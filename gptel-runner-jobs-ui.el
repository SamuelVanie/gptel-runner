;;; gptel-runner-jobs-ui.el --- Background jobs and run controls -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Observe durable invocation manifests without restoring executable state.
;; Controls are sent to the owning worker through the jobs command API.

;;; Code:

(require 'pp)
(require 'gptel-runner-ui)
(require 'gptel-runner-jobs)

(declare-function gptel-runner-launchd-install "gptel-runner-launchd")
(declare-function gptel-runner-launchd-disable "gptel-runner-launchd")
(declare-function gptel-runner-launchd-remove "gptel-runner-launchd")
(declare-function gptel-runner-launchd-diagnostics "gptel-runner-launchd")
(declare-function gptel-runner-launchd-reap "gptel-runner-launchd")
(declare-function gptel-runner-invocation-outputs "gptel-runner-jobs")
(declare-function gptel-runner-invocation-call "gptel-runner-jobs")
(declare-function gptel-runner-invocation-commands "gptel-runner-jobs")
(declare-function gptel-runner-invocation-list "gptel-runner-jobs")
(declare-function gptel-runner-invocation-get "gptel-runner-jobs")
(declare-function gptel-runner-job-list "gptel-runner-jobs")
(declare-function gptel-runner-job-get "gptel-runner-jobs")
(declare-function gptel-runner-job-save "gptel-runner-jobs")
(declare-function gptel-runner-job-delete "gptel-runner-jobs")
(declare-function gptel-runner-job-run "gptel-runner-jobs")
(declare-function gptel-runner-job-command "gptel-runner-jobs")

(defvar gptel-runner-jobs-buffer "*gptel-runner jobs*"
  "Name of the background jobs dashboard.")

(defvar gptel-runner-jobs-ui--reap-process nil
  "Subprocess currently removing completed worker registrations, if any.")

(defvar gptel-runner-jobs-ui--last-reap-time nil
  "Time of the last attempted asynchronous registration cleanup.")

(defvar-local gptel-runner-job-edit--id nil
  "Identity of the job being edited, or nil for a new job.")

(defvar-local gptel-runner-background--target nil
  "External invocation and call identity associated with this buffer.")

(defun gptel-runner-jobs-ui--elapsed (record)
  "Format elapsed time from an invocation or call RECORD."
  (let ((start (plist-get record :started-at)))
    (if (numberp start)
        (format "%.1fs" (- (or (plist-get record :finished-at) (float-time))
                           start))
      "-")))

(defun gptel-runner-jobs-ui--entries ()
  "Return external invocation rows using only their compact manifests."
  (let (entries)
    (dolist (run (gptel-runner-invocation-list))
      (let ((id (plist-get run :id))
            (job (or (plist-get (plist-get run :job) :name)
                     (plist-get run :job-id) "")))
        (push
         (list (list 'background-run id)
               (gptel-runner-ui--row-vector
                `((workflow . ,(format "%s" (plist-get run :workflow)))
                  (run . ,id)
                  (job . ,job)
                  (state . ,(gptel-runner-ui--state (plist-get run :state)))
                  (elapsed . ,(gptel-runner-jobs-ui--elapsed run)))))
         entries)
        (dolist (call (plist-get run :calls))
          (push
           (list (list 'background-call id (plist-get call :epoch)
                       (plist-get call :id))
                 (gptel-runner-ui--row-vector
                  `((node . ,(format "%s" (plist-get call :node)))
                    (job . ,job)
                    (agent . ,(cond ((eq (plist-get call :kind) 'elisp) "Elisp")
                                   ((plist-get call :agent)
                                    (format "%s" (plist-get call :agent)))
                                   (t "")))
                    (call . ,(plist-get call :id))
                    (state . ,(gptel-runner-ui--state (plist-get call :state)))
                    (elapsed . ,(gptel-runner-jobs-ui--elapsed call))
                    (attempts . ,(format "%s" (or (plist-get call :attempts) 0))))))
           entries))))
    (nreverse entries)))

(defun gptel-runner-jobs-ui--target (&optional row)
  "Return the external identity at ROW or point without loading a run."
  (let ((identity (or row gptel-runner-background--target
                      (and (derived-mode-p 'tabulated-list-mode)
                           (tabulated-list-get-id)))))
    (when (memq (car-safe identity) '(background-run background-call))
      identity)))

(defun gptel-runner-jobs-ui--selection ()
  "Return fresh (INVOCATION CALL TARGET) data for the selected external row."
  (let* ((target (or (gptel-runner-jobs-ui--target)
                     (user-error "Select a background run or call")))
         (run (or (gptel-runner-invocation-get (nth 1 target))
                  (user-error "Background invocation no longer exists")))
         (call (when (eq (car target) 'background-call)
                 (cl-find-if
                  (lambda (item)
                    (and (equal (plist-get item :id) (nth 3 target))
                         (equal (plist-get item :epoch) (nth 2 target))))
                  (plist-get run :calls)))))
    (when (and (eq (car target) 'background-call) (null call))
      (user-error "Background call no longer exists"))
    (list run call target)))

(defun gptel-runner-jobs-ui--display (name data &optional target)
  "Display DATA read-only in buffer NAME with optional external TARGET."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pp-to-string data))
        (special-mode)
        (setq-local gptel-runner-background--target target)
        (when target
          (use-local-map (copy-keymap (current-local-map)))
          (local-set-key (kbd "?") #'gptel-runner-background-menu))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun gptel-runner-jobs-ui--artifact-path (run relative)
  "Resolve a readable RELATIVE artifact strictly within RUN's directory."
  (unless (and (stringp relative) (not (file-name-absolute-p relative)))
    (user-error "Invalid invocation artifact path"))
  (let* ((directory (file-name-as-directory
                     (file-truename (plist-get run :directory))))
         (path (file-truename (expand-file-name relative directory))))
    (unless (and (file-in-directory-p path directory)
                 (file-regular-p path) (file-readable-p path))
      (user-error "Invocation artifact is unavailable: %s" relative))
    path))

(defun gptel-runner-background-inspect ()
  "Inspect the selected background run or call without starting execution."
  (interactive)
  (pcase-let ((`(,run ,call ,target) (gptel-runner-jobs-ui--selection)))
    (gptel-runner-jobs-ui--display
     (format "*gptel-runner background:%s*" (plist-get run :id))
     (if call (gptel-runner-invocation-call run call) run) target)))

(defun gptel-runner-background-outputs ()
  "Inspect saved outputs for the selected background invocation."
  (interactive)
  (pcase-let ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection)))
    (gptel-runner-jobs-ui--display
     (format "*gptel-runner outputs:%s*" (plist-get run :id))
     (gptel-runner-outputs run) target)))

(defun gptel-runner-background-commands ()
  "Inspect submitted command receipts for the selected invocation."
  (interactive)
  (pcase-let ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection)))
    (gptel-runner-jobs-ui--display
     (format "*gptel-runner commands:%s*" (plist-get run :id))
     (gptel-runner-invocation-commands run) target)
    (setq-local revert-buffer-function
                (lambda (&rest _ignored) (gptel-runner-background-commands)))
    (local-set-key (kbd "g") #'revert-buffer)))

(defun gptel-runner-background-transcript ()
  "Open the selected background call's persisted transcript read-only."
  (interactive)
  (pcase-let ((`(,run ,call ,target) (gptel-runner-jobs-ui--selection)))
    (unless call (user-error "Select a background call"))
    (let ((path (gptel-runner-jobs-ui--artifact-path
                 run (plist-get call :transcript))))
      (let ((enable-local-variables nil) (enable-local-eval nil))
        (pop-to-buffer (find-file-noselect path)))
      (read-only-mode 1)
      (setq-local gptel-runner-background--target target))))

(defun gptel-runner-background-events ()
  "Open the persisted event journal for the selected invocation."
  (interactive)
  (pcase-let ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection)))
    (let ((path (gptel-runner-jobs-ui--artifact-path run "events.el"))
          (enable-local-variables nil) (enable-local-eval nil))
      (pop-to-buffer (find-file-noselect path))
      (read-only-mode 1)
      (setq-local gptel-runner-background--target target))))

(defun gptel-runner-background-artifact ()
  "Open a selected artifact belonging to the background invocation."
  (interactive)
  (pcase-let* ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection))
               (choices
                (mapcar (lambda (item)
                          (cons (or (plist-get item :name)
                                    (plist-get item :file)) item))
                        (gptel-runner-artifacts run))))
    (unless choices (user-error "This invocation has no artifacts"))
    (let* ((choice (completing-read "Artifact: " choices nil t))
           (path (gptel-runner-artifact-file
                  run (cdr (assoc choice choices)))))
      (let ((enable-local-variables nil) (enable-local-eval nil))
        (pop-to-buffer (find-file-noselect path)))
      (read-only-mode 1)
      (setq-local gptel-runner-background--target target))))

(defun gptel-runner-jobs-ui--interaction (run call)
  "Select an active interaction from RUN, optionally restricted to CALL."
  (let* ((interactions
          (cl-remove-if-not
           (lambda (item)
             (or (null call)
                 (and (equal (plist-get item :call-id) (plist-get call :id))
                      (or (null (plist-get item :epoch))
                          (equal (plist-get item :epoch)
                                 (plist-get call :epoch))))))
           (plist-get run :interactions)))
         (choices (mapcar (lambda (item)
                            (cons (plist-get item :id) item)) interactions)))
    (unless choices (user-error "No pending interaction on this row"))
    (if (= (length choices) 1) (cdar choices)
      (cdr (assoc (completing-read "Interaction: " choices nil t) choices)))))

(defun gptel-runner-jobs-ui--budget-additions ()
  "Read requested background budget additions without loading runtime data."
  (let (arguments)
    (dolist (spec '((:additional-requests . "Additional requests")
                    (:additional-calls . "Additional calls")
                    (:additional-duration . "Additional active seconds")))
      (let ((value (read-number (concat (cdr spec) " (0 for none): ") 0)))
        (when (> value 0)
          (setq arguments (plist-put arguments (car spec) value)))))
    (unless arguments (user-error "No budget addition requested"))
    arguments))

(defun gptel-runner-background-command (operation &optional arguments)
  "Send OPERATION with ARGUMENTS to the selected invocation's worker.
When ARGUMENTS is nil, read the arguments needed by OPERATION.  The returned
receipt acknowledges submission; only the worker can acknowledge application."
  (interactive
   (list (intern
          (completing-read
           "Background action: "
           '(pause resume cancel retry continue extend extend-repeat decision
             pause-call cancel-call feedback accept approve reject)
           nil t))))
  (pcase-let* ((`(,run ,call ,target) (gptel-runner-jobs-ui--selection))
               (arguments (copy-sequence arguments)))
    (when (memq operation '(pause-call cancel-call feedback accept))
      (unless call (user-error "Select a background call for this action")))
    (unless arguments
      (setq arguments
            (pcase operation
              ('resume (list :feedback (read-string "Resume feedback: ")))
              ('feedback (list :feedback (read-string "Feedback for the agent: ")))
              ('accept (let ((text (read-string
                                   "Accepted result (empty uses latest response): ")))
                         (unless (string-empty-p text) (list :result text))))
              ('retry (append
                       (when call (list :from-node (plist-get call :node)))
                       (list :feedback (read-string "Retry feedback: "))))
              ('continue (list :observation (read-string "Observation and new goal: ")
                               :reset-budget
                               (y-or-n-p "Reset consumed budget for this cycle? ")))
              ('extend (gptel-runner-jobs-ui--budget-additions))
              ('extend-repeat
               (list :node (intern (read-string "Repeat node: "))
                     :additional-iterations (read-number "Additional iterations: " 1)))
              ('decision (list :text (read-string "Workflow decision: ")
                               :reason (read-string "Rationale (optional): "))))))
    (when (and call (memq operation '(pause-call cancel-call feedback accept)))
      (setq arguments (plist-put arguments :call-id (plist-get call :id))
            arguments (plist-put arguments :call-epoch (plist-get call :epoch))))
    (when (memq operation '(approve reject))
      (let* ((interaction (gptel-runner-jobs-ui--interaction run call))
             (owner (cl-find (plist-get interaction :call-id) (plist-get run :calls)
                             :key (lambda (item) (plist-get item :id))
                             :test #'equal)))
        (gptel-runner-jobs-ui--display "*gptel-runner pending interaction*"
                                       interaction target)
        (unless (yes-or-no-p (format "%s this pending tool request? "
                                    (capitalize (symbol-name operation))))
          (user-error "No tool decision submitted"))
        (setq arguments
              (append (list :interaction-id (plist-get interaction :id)
                            :call-id (plist-get interaction :call-id)
                            :call-epoch (or (plist-get interaction :epoch)
                                            (plist-get owner :epoch))
                            :generation (plist-get interaction :generation))
                      arguments))))
    (let ((receipt (gptel-runner-job-command run operation arguments)))
      (gptel-runner-jobs-ui--display "*gptel-runner command receipt*" receipt target)
      (message "Background command %s: %s"
               (plist-get receipt :id) (or (plist-get receipt :status) 'queued))
      receipt)))

(defun gptel-runner-background-pause ()
  "Pause the selected invocation in its worker."
  (interactive) (gptel-runner-background-command 'pause))

(defun gptel-runner-background-resume ()
  "Resume the selected invocation in an isolated worker."
  (interactive) (gptel-runner-background-command 'resume))

(defun gptel-runner-background-cancel ()
  "Cancel the selected invocation."
  (interactive) (gptel-runner-background-command 'cancel))

(defun gptel-runner-background-retry ()
  "Retry the invocation or selected call's node."
  (interactive) (gptel-runner-background-command 'retry))

(defun gptel-runner-background-continue ()
  "Continue the invocation with a new goal."
  (interactive) (gptel-runner-background-command 'continue))

(defun gptel-runner-background-extend ()
  "Extend the invocation's finite budgets."
  (interactive) (gptel-runner-background-command 'extend))

(defun gptel-runner-background-feedback ()
  "Ask the selected agent to revise its response."
  (interactive) (gptel-runner-background-command 'feedback))

(defun gptel-runner-background-accept ()
  "Accept the selected call's latest or supplied response."
  (interactive) (gptel-runner-background-command 'accept))

(defun gptel-runner-background-approve ()
  "Review and approve a pending tool request."
  (interactive) (gptel-runner-background-command 'approve))

(defun gptel-runner-background-reject ()
  "Review and reject a pending tool request."
  (interactive) (gptel-runner-background-command 'reject))

(transient-define-prefix gptel-runner-background-menu ()
  "Inspect and control the selected background invocation."
  [["Inspect"
    ("i" "Run/call details" gptel-runner-background-inspect)
    ("e" "Event journal" gptel-runner-background-events)
    ("v" "Transcript" gptel-runner-background-transcript)
    ("o" "Saved outputs" gptel-runner-background-outputs)
    ("h" "Command receipts" gptel-runner-background-commands)
    ("A" "Artifact" gptel-runner-background-artifact)]
   ["Run"
    ("P" "Pause" gptel-runner-background-pause)
    ("r" "Resume" gptel-runner-background-resume)
    ("t" "Retry / retry node" gptel-runner-background-retry)
    ("c" "Continue" gptel-runner-background-continue)
    ("b" "Extend budgets" gptel-runner-background-extend)
    ("K" "Cancel" gptel-runner-background-cancel)]
   ["Interaction"
    ("f" "Agent feedback" gptel-runner-background-feedback)
    ("a" "Accept response" gptel-runner-background-accept)
    ("y" "Approve tools" gptel-runner-background-approve)
    ("n" "Reject tools" gptel-runner-background-reject)
    ("x" "All actions" gptel-runner-background-command)]])

(defun gptel-runner-jobs-ui--row-action (action row)
  "Handle ACTION for external dashboard ROW and report whether handled."
  (when (gptel-runner-jobs-ui--target row)
    (pcase action
      ('open (gptel-runner-background-inspect) t)
      ('menu (gptel-runner-background-menu) t))))

(defun gptel-runner-jobs-ui--schedule (job)
  "Describe JOB's configured schedule."
  (let ((schedule (plist-get job :schedule)))
    (cond ((null schedule) "manual")
          ((plist-get schedule :interval)
           (format "every %ss" (plist-get schedule :interval)))
          (t (format "%S" schedule)))))

(defun gptel-runner-jobs-ui--job-entries ()
  "Return rows for the jobs dashboard."
  (mapcar
   (lambda (job)
     (list (plist-get job :id)
           (vector (or (plist-get job :name) (plist-get job :id))
                   (format "%s" (plist-get job :workflow))
                   (format "%s" (or (plist-get (plist-get job :scheduler) :state)
                                     (if (plist-get job :enabled)
                                         'enabled 'disabled)))
                   (gptel-runner-jobs-ui--schedule job)
                   (format "%s" (or (plist-get job :revision) 1)))))
   (gptel-runner-job-list)))

(defun gptel-runner-jobs-ui--reap-sentinel (process _event)
  "Retain PROCESS's diagnostic log on failed registration cleanup."
  (when (memq (process-status process) '(exit signal))
    (if (= (process-exit-status process) 0)
        (when (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process)))
      (message "Worker registration cleanup failed; see %s"
               (if (buffer-live-p (process-buffer process))
                   (buffer-name (process-buffer process)) "cleanup log")))))

(defun gptel-runner-jobs-ui--maybe-reap ()
  "Lazily clean completed worker registrations outside the editor process.
Start at most one subprocess, no more frequently than every thirty seconds.
No timer is installed: cleanup is requested only when the jobs view refreshes."
  (when (and (eq system-type 'darwin)
             (not (and (processp gptel-runner-jobs-ui--reap-process)
                       (process-live-p gptel-runner-jobs-ui--reap-process)))
             (or (null gptel-runner-jobs-ui--last-reap-time)
                 (>= (- (float-time) gptel-runner-jobs-ui--last-reap-time) 30)))
    (setq gptel-runner-jobs-ui--last-reap-time (float-time))
    (let ((buffer (get-buffer-create "*gptel-runner cleanup log*"))
          (form `(let ((gptel-runner-job-directory
                        ,(expand-file-name gptel-runner-job-directory)))
                   (require 'gptel-runner-launchd)
                   (gptel-runner-launchd-reap))))
      (condition-case err
          (setq gptel-runner-jobs-ui--reap-process
                (make-process
                 :name "gptel-runner-cleanup" :buffer buffer :noquery t
                 :connection-type 'pipe
                 :command (list gptel-runner-job-emacs-program "-Q" "--batch"
                                "-L" gptel-runner-jobs--package-directory
                                "-l" "gptel-runner-jobs"
                                "--eval" (prin1-to-string form))
                 :sentinel #'gptel-runner-jobs-ui--reap-sentinel))
        (error
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert (error-message-string err) "\n"))
         (message "Could not start worker cleanup; see %s" (buffer-name buffer)))))))

(defun gptel-runner-jobs-refresh ()
  "Refresh saved jobs and request asynchronous completed-worker cleanup."
  (interactive)
  (gptel-runner-jobs-ui--maybe-reap)
  (tabulated-list-print t))

(defun gptel-runner-jobs-ui--job ()
  "Return the selected saved job or report a missing selection."
  (or (when-let* ((id (tabulated-list-get-id))) (gptel-runner-job-get id))
      (user-error "Select a saved background job")))

(define-derived-mode gptel-runner-jobs-mode tabulated-list-mode "Runner-Jobs"
  "Manage background jobs and their macOS schedules."
  (setq tabulated-list-format
        [("Job" 26 t) ("Workflow" 24 t) ("State" 10 t)
         ("Schedule" 35 nil) ("Revision" 8 t)]
        tabulated-list-entries #'gptel-runner-jobs-ui--job-entries
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'gptel-runner-jobs-refresh nil t)
  (tabulated-list-init-header))

(define-derived-mode gptel-runner-job-edit-mode emacs-lisp-mode "Runner-Job"
  "Edit one job data plist.
Use `gptel-runner-job-edit-save' to parse and save without evaluating forms."
  (setq-local header-line-format "Job data — C-c C-c save; C-c C-k close"))

(defun gptel-runner-jobs-ui--edit (job)
  "Open JOB in a dedicated data editor."
  (let ((buffer (generate-new-buffer "*gptel-runner job definition*")))
    (with-current-buffer buffer
      (gptel-runner-job-edit-mode)
      (setq-local gptel-runner-job-edit--id (plist-get job :id))
      (insert (pp-to-string job))
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)))

(defun gptel-runner-jobs-create ()
  "Create a background job by editing its data definition."
  (interactive)
  (gptel-runner-jobs-ui--edit
   (list :name "New pipeline job" :workflow 'my-workflow
         :init-file (expand-file-name "runner-init.el" user-emacs-directory)
         :arguments (list :goal "Describe the pipeline goal"
                          :workspace (expand-file-name default-directory))
         :schedule nil :enabled nil
         :emacs (or (executable-find "emacs") invocation-name))))

(defun gptel-runner-jobs-edit ()
  "Edit the selected job's saved data definition."
  (interactive)
  (gptel-runner-jobs-ui--edit (gptel-runner-jobs-ui--job)))

(defun gptel-runner-job-edit-save ()
  "Parse and save the job data without evaluating any buffer forms."
  (interactive)
  (unless (derived-mode-p 'gptel-runner-job-edit-mode)
    (user-error "Run this command from a job definition buffer"))
  (let* ((data (save-excursion
                 (goto-char (point-min))
                 (let ((value (read (current-buffer))))
                   (forward-comment (point-max))
                   (unless (eobp) (user-error "Expected exactly one job plist"))
                   value)))
         (id (and (listp data) (plist-get data :id))))
    (unless (and (listp data) (keywordp (car data)))
      (user-error "Expected a job property list"))
    (when (and gptel-runner-job-edit--id
               (not (equal id gptel-runner-job-edit--id)))
      (user-error "Editing a job cannot change its identity"))
    (let ((saved (gptel-runner-job-save data)))
      (setq-local gptel-runner-job-edit--id (plist-get saved :id))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pp-to-string saved))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (message "Saved job %s; use Apply schedule to install schedule changes"
               (plist-get saved :name))
      saved)))

(defun gptel-runner-jobs-run ()
  "Start the selected job in an isolated Emacs worker now."
  (interactive)
  (let ((run (gptel-runner-job-run (gptel-runner-jobs-ui--job) 'manual)))
    (if (eq (plist-get run :state) 'skipped)
        (message "Skipped background invocation %s: another invocation is active"
                 (plist-get run :id))
      (message "Started background invocation %s" (plist-get run :id)))
    (gptel-runner-show-dashboard)))

(defun gptel-runner-jobs-install ()
  "Enable and install the selected job's user LaunchAgent schedule."
  (interactive)
  (require 'gptel-runner-launchd)
  (gptel-runner-launchd-install (gptel-runner-jobs-ui--job))
  (gptel-runner-jobs-refresh))

(defun gptel-runner-jobs-disable ()
  "Disable the selected job's installed schedule."
  (interactive)
  (require 'gptel-runner-launchd)
  (gptel-runner-launchd-disable (gptel-runner-jobs-ui--job))
  (gptel-runner-jobs-refresh))

(defun gptel-runner-jobs-remove ()
  "Remove the selected job and its schedule while preserving run history."
  (interactive)
  (let ((job (gptel-runner-jobs-ui--job)))
    (when (yes-or-no-p (format "Remove job %s and its schedule? "
                              (plist-get job :name)))
      (gptel-runner-job-delete (plist-get job :id))
      (gptel-runner-jobs-refresh))))

(defun gptel-runner-jobs-diagnostics ()
  "Show scheduler diagnostics for the selected job."
  (interactive)
  (require 'gptel-runner-launchd)
  (gptel-runner-jobs-ui--display
   "*gptel-runner schedule diagnostics*"
   (gptel-runner-launchd-diagnostics (gptel-runner-jobs-ui--job))))

(transient-define-prefix gptel-runner-jobs-menu ()
  "Manage background jobs and schedules."
  [["Job"
    ("n" "New job" gptel-runner-jobs-create)
    ("e" "Edit definition" gptel-runner-jobs-edit)
    ("r" "Run now" gptel-runner-jobs-run)
    ("x" "Remove job" gptel-runner-jobs-remove)]
   ["Schedule"
    ("a" "Enable / apply schedule" gptel-runner-jobs-install)
    ("d" "Disable schedule" gptel-runner-jobs-disable)
    ("i" "Diagnostics" gptel-runner-jobs-diagnostics)]
   ["View"
    ("g" "Refresh" gptel-runner-jobs-refresh)
    ("R" "Runs" gptel-runner-show-dashboard)]])

(let ((map gptel-runner-jobs-mode-map))
  (define-key map (kbd "?") #'gptel-runner-jobs-menu)
  (define-key map (kbd "RET") #'gptel-runner-jobs-edit)
  (define-key map (kbd "g") #'gptel-runner-jobs-refresh)
  (define-key map (kbd "n") #'gptel-runner-jobs-create))

(define-key gptel-runner-job-edit-mode-map (kbd "C-c C-c")
            #'gptel-runner-job-edit-save)
(define-key gptel-runner-job-edit-mode-map (kbd "C-c C-k") #'kill-current-buffer)

;;;###autoload
(defun gptel-runner-show-jobs ()
  "Show saved background jobs and their schedule management controls."
  (interactive)
  (let ((buffer (get-buffer-create gptel-runner-jobs-buffer)))
    (with-current-buffer buffer
      (gptel-runner-jobs-mode)
      (gptel-runner-jobs-refresh))
    (pop-to-buffer buffer)))

(add-hook 'gptel-runner-dashboard-entry-functions
          #'gptel-runner-jobs-ui--entries)
(add-hook 'gptel-runner-dashboard-row-action-functions
          #'gptel-runner-jobs-ui--row-action)

(provide 'gptel-runner-jobs-ui)
;;; gptel-runner-jobs-ui.el ends here
