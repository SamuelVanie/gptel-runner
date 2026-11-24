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

(declare-function gptel-runner-invocation-outputs "gptel-runner-jobs")
(declare-function gptel-runner-invocation-call "gptel-runner-jobs")
(declare-function gptel-runner-invocation-commands "gptel-runner-jobs")
(declare-function gptel-runner-invocation-list "gptel-runner-jobs")
(declare-function gptel-runner-invocation-get "gptel-runner-jobs")
(declare-function gptel-runner-job-list "gptel-runner-jobs")
(declare-function gptel-runner-job-get "gptel-runner-jobs")
(declare-function gptel-runner-job-run "gptel-runner-jobs")
(declare-function gptel-runner-job-command "gptel-runner-jobs")
(autoload 'gptel-runner-background-open "gptel-runner-jobs-panel" nil t)
(declare-function gptel-runner-store-open-chat "gptel-runner-store")

(defvar gptel-runner-jobs-buffer "*gptel-runner jobs*"
  "Name of the background jobs dashboard.")

(defvar gptel-runner-jobs-ui--reap-process nil
  "Subprocess currently removing completed worker registrations, if any.")

(defvar gptel-runner-jobs-ui--last-reap-time nil
  "Time of the last attempted asynchronous registration cleanup.")

(defvar-local gptel-runner-background--target nil
  "External invocation and call identity associated with this buffer.")

(defvar-local gptel-runner-jobs-ui--chat-revision nil
  "Source call, file and publication time of this local chat copy.")

(defun gptel-runner-jobs-ui--elapsed (record)
  "Format elapsed time from an invocation or call RECORD."
  (let ((start (plist-get record :started-at)))
    (if (numberp start)
        (format "%.1fs" (- (or (plist-get record :finished-at) (float-time))
                           start))
      "-")))

(defun gptel-runner-jobs-ui--visible-invocations ()
  "Return background history that has not been forgotten."
  (cl-remove-if #'gptel-runner-invocation-forgotten-p
                (gptel-runner-invocation-list)))

(defun gptel-runner-jobs-ui--call-interactions (run call)
  "Return pending requests for CALL's exact epoch in RUN."
  (cl-remove-if-not
   (lambda (item)
     (and (equal (plist-get item :call-id) (plist-get call :id))
          (equal (plist-get item :epoch) (plist-get call :epoch))))
   (plist-get run :interactions)))

(defun gptel-runner-jobs-ui--interaction-summary (pending)
  "Describe the questions and approval requests in PENDING."
  (let ((questions 0) (approvals 0))
    (dolist (entry pending)
      (if (eq (plist-get entry :kind) 'question)
          (cl-incf questions (length (plist-get entry :questions)))
        (cl-incf approvals)))
    (if (zerop questions)
        (gptel-runner-ui--approval-count approvals)
      (propertize (concat (format "%d question%s" questions (if (= questions 1) "" "s"))
                          (if (zerop approvals) " pending"
                            (format " · %d approval%s" approvals (if (= approvals 1) "" "s"))))
                  'face 'warning))))

(defun gptel-runner-jobs-ui--entries ()
  "Return external invocation rows using only their compact manifests."
  (let (entries)
    (dolist (run (gptel-runner-jobs-ui--visible-invocations))
      (let ((id (plist-get run :id))
            (job (or (plist-get (plist-get run :job) :name)
                     (plist-get run :job-id) "")))
        (push
         (list (list 'background-run id)
               (gptel-runner-ui--row-vector
                `((item . ,(propertize
                            (gptel-runner-ui--tree-label
                             (list 'background-run id)
                             (format "%s · %s" (if (string-empty-p job)
                                                    (plist-get run :workflow) job) id))
                            'face 'bold))
                  (workflow . ,(format "%s" (plist-get run :workflow)))
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
                  `((item . ,(format "    %s" (plist-get call :node)))
                    (node . ,(format "%s" (plist-get call :node)))
                    (job . ,job)
                    (agent . ,(cond ((eq (plist-get call :kind) 'elisp) "Elisp")
                                   ((plist-get call :agent)
                                    (format "%s" (plist-get call :agent)))
                                   (t "")))
                    (call . ,(plist-get call :id))
                    (state . ,(let ((pending (gptel-runner-jobs-ui--call-interactions run call)))
                                (if pending
                                    (gptel-runner-jobs-ui--interaction-summary pending)
                                  (gptel-runner-ui--state (plist-get call :state)))))
                    (elapsed . ,(gptel-runner-jobs-ui--elapsed call))
                    (attempts . ,(format "%s" (or (plist-get call :attempts) 0)))
                    (iteration . ,(or (plist-get call :iteration) "-")))))
           entries))))
    (nreverse entries)))

(defun gptel-runner-jobs-ui--target (&optional row)
  "Return the external identity at ROW or point without loading a run."
  (let ((identity (or row gptel-runner-background--target
                      (and (derived-mode-p 'tabulated-list-mode)
                           (tabulated-list-get-id)))))
    (when (memq (car-safe identity) '(background-run background-call))
      identity)))

(defun gptel-runner-jobs-ui--selection (&optional target)
  "Return fresh (INVOCATION CALL TARGET) data for TARGET or the selected row."
  (let* ((target (or target (gptel-runner-jobs-ui--target)
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

(defconst gptel-runner-jobs-ui--controls-link
  (propertize "Worker controls" 'face 'link 'mouse-face 'highlight
              'help-echo "Answer questions, review approvals, or send feedback to the worker"
              'local-map (define-keymap
                           "<header-line> <mouse-1>"
                           (lambda (event)
                             (interactive "e")
                             (select-window (posn-window (event-start event)))
                             (gptel-runner-background-open))))
  "Header link from a local chat copy to the worker that produced it.")

(defun gptel-runner-jobs-ui--chat-header ()
  "Keep worker controls in this local chat copy's header.
Leave native gptel's status slot and the installed header list intact.
Optional conversation views can replace or restore the header when toggled."
  (when (and gptel-runner-jobs-ui--chat-revision
             (not (memq gptel-runner-jobs-ui--controls-link
                        (flatten-tree header-line-format))))
    (let ((label (list " Local chat copy · "
                       gptel-runner-jobs-ui--controls-link "    ")))
      (if (and (bound-and-true-p gptel-mode)
               (bound-and-true-p gptel-use-header-line)
               (consp header-line-format))
          ;; gptel updates slot 1 in place.  Atelier also uses list identity
          ;; to carry that status back to its saved header on disable.
          (setcar header-line-format (list (car header-line-format) label))
        (setq-local header-line-format (list label header-line-format))))))

(defun gptel-runner-background-transcript (&optional selection)
  "Visit the selected background call's most recently saved chat.
Edits and ordinary gptel sends belong to this local conversation.  Use
`gptel-runner-background-feedback' to send feedback to the worker.
Legacy text transcripts open read-only.  SELECTION defaults to the row
at point."
  (interactive)
  (pcase-let ((`(,run ,call ,target) (or selection (gptel-runner-jobs-ui--selection))))
    (unless call (user-error "Select a background call"))
    (if-let* ((chat (plist-get call :chat)))
        (let* ((path (gptel-runner-jobs-ui--artifact-path run chat))
               (revision (list target path
                               (file-attribute-modification-time (file-attributes path))))
               (existing (cl-find revision (buffer-list) :test #'equal
                                  :key (lambda (buffer)
                                         (buffer-local-value
                                          'gptel-runner-jobs-ui--chat-revision buffer)))))
          (pop-to-buffer
           (or existing
               (with-current-buffer
                   (gptel-runner-store-open-chat path (plist-get call :chat-format))
                 (setq-local gptel-runner-background--target target
                             gptel-runner-jobs-ui--chat-revision revision)
                 (add-hook 'gptel-mode-hook
                           #'gptel-runner-jobs-ui--chat-header nil t)
                 ;; Register locally without loading the optional view.
                 (add-hook 'gptel-ui-mode-hook
                           #'gptel-runner-jobs-ui--chat-header nil t)
                 (gptel-runner-jobs-ui--chat-header)
                 (current-buffer)))))
      (let ((path (gptel-runner-jobs-ui--artifact-path
                   run (or (plist-get call :transcript)
                           (user-error "This call has no saved chat"))))
            (enable-local-variables nil) (enable-local-eval nil))
        (pop-to-buffer (find-file-noselect path))
        (read-only-mode 1)
        (setq-local gptel-runner-background--target target)))))

(defun gptel-runner-background-visit (&optional target)
  "Visit TARGET's controls for pending interactions, or its available chat.
TARGET defaults to the selected background run or node."
  (interactive)
  (let* ((selection (gptel-runner-jobs-ui--selection target))
         (run (car selection))
         (call (nth 1 selection)))
    (if (and call (or (plist-get call :chat) (plist-get call :transcript))
             (not (gptel-runner-jobs-ui--call-interactions run call)))
        (gptel-runner-background-transcript selection)
      (gptel-runner-background-open (nth 2 selection)))))

(defun gptel-runner-background-child-transcripts ()
  "Open a child transcript published by the selected background node or run."
  (interactive)
  (pcase-let* ((`(,run ,call ,_) (gptel-runner-jobs-ui--selection))
               (children
                (cl-loop for owner in (plist-get run :calls)
                         when (or (null call)
                                  (equal (plist-get owner :node) (plist-get call :node)))
                         append (mapcar
                                 (lambda (record)
                                   (append (list :target (list 'background-call (plist-get run :id)
                                                              (plist-get owner :epoch)
                                                              (plist-get owner :id)))
                                           record))
                                 (plist-get owner :children))))
               (child (gptel-runner-ui--read-child children)))
    (gptel-runner-background-transcript (list run child (plist-get child :target)))))

(defun gptel-runner-background-events ()
  "Open the persisted event journal for the selected invocation."
  (interactive)
  (pcase-let ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection)))
    (let ((path (gptel-runner-jobs-ui--artifact-path run "events.el"))
          (enable-local-variables nil) (enable-local-eval nil))
      (pop-to-buffer (find-file-noselect path))
      (read-only-mode 1)
      (setq-local gptel-runner-background--target target))))

(defun gptel-runner-jobs-ui--visit-message-node (run epoch node)
  "Open the published conversation for NODE in RUN at or before EPOCH."
  (let ((call
         (cl-find-if (lambda (c)
                       (and (equal node (plist-get c :node))
                            (<= (plist-get c :epoch) epoch)
                            (or (plist-get c :chat) (plist-get c :transcript))))
                     (plist-get run :calls) :from-end t)))
    (unless call (user-error "No published chat for node %s" node))
    (gptel-runner-background-visit
     (list 'background-call (plist-get run :id) (plist-get call :epoch)
           (plist-get call :id)))))

(defun gptel-runner-jobs-ui--insert-messages (run node)
  "Read RUN's published journal and display messages involving NODE."
  (when (file-exists-p (expand-file-name "events.el" (plist-get run :directory)))
    (let ((display (current-buffer)))
      (with-temp-buffer
        (insert-file-contents (gptel-runner-jobs-ui--artifact-path run "events.el"))
        (goto-char (point-min))
        ;; A worker may still be appending the last record.
        (condition-case nil
            (while t
              (let* ((record (read (current-buffer)))
                     (epoch (plist-get record :epoch)))
                (with-current-buffer display
                  (gptel-runner-ui--insert-message
                   (plist-get record :time) (plist-get record :type)
                   (plist-get record :data) node
                   (lambda (id)
                     (gptel-runner-jobs-ui--visit-message-node run epoch id))))))
          (end-of-file nil))))))

(defun gptel-runner-background-messages ()
  "Show node messages exchanged in the selected background invocation."
  (interactive)
  (pcase-let* ((`(,run ,call ,target) (gptel-runner-jobs-ui--selection))
               (id (plist-get run :id))
               (node (plist-get call :node)))
    (gptel-runner-ui--show-messages
     (format "*gptel-runner background messages:%s*" id) node
     (lambda ()
       (gptel-runner-jobs-ui--insert-messages (gptel-runner-invocation-get id) node)))
    (setq-local gptel-runner-background--target target)))

(defun gptel-runner-jobs-ui--artifact-choices (artifacts)
  "Return a completion alist with distinct labels for ARTIFACTS.
Keep unique names unchanged.  Qualify repeated names with the epoch, when
available, and retained filename.  Resolve remaining label collisions in
publication order, reserving literal names even when they resemble labels."
  (let ((names (make-hash-table :test #'equal))
        (used (make-hash-table :test #'equal))
        (choices (mapcar (lambda (item)
                           (cons (or (plist-get item :name)
                                     (plist-get item :file)) item))
                         artifacts)))
    (dolist (choice choices)
      (puthash (car choice) (1+ (gethash (car choice) names 0)) names))
    (dolist (choice choices)
      (when (> (gethash (car choice) names) 1)
        (let* ((item (cdr choice))
               (epoch (plist-get item :epoch))
               (base (format "%s [%s%s]" (car choice)
                             (if epoch (format "epoch %s; " epoch) "")
                             (plist-get item :file)))
               (label base)
               (number 1))
          ;; Filenames distinguish normal publications, including repeated
          ;; names in one epoch.  Also handle duplicate imported descriptors
          ;; and literal names that happen to match a generated label.
          (while (or (gethash label names) (gethash label used))
            (setq label (format "%s #%d" base (cl-incf number))))
          (setcar choice label)))
      (puthash (car choice) t used))
    choices))

(defun gptel-runner-background-artifact ()
  "Open a selected artifact belonging to the background invocation."
  (interactive)
  (pcase-let* ((`(,run ,_call ,target) (gptel-runner-jobs-ui--selection))
               (choices (gptel-runner-jobs-ui--artifact-choices
                         (gptel-runner-artifacts run))))
    (unless choices (user-error "This invocation has no artifacts"))
    (let* ((choice (completing-read "Artifact: " choices nil t))
           (path (gptel-runner-artifact-file
                  run (cdr (assoc choice choices)))))
      (let ((enable-local-variables nil) (enable-local-eval nil))
        (pop-to-buffer (find-file-noselect path)))
      (read-only-mode 1)
      (setq-local gptel-runner-background--target target))))

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
receipt acknowledges submission; only the worker can acknowledge application.
Question and approval actions open the node panel for review before submission."
  (interactive
   (list (intern
          (completing-read
           "Background action: "
           gptel-runner-jobs--operations
           nil t))))
  (if (memq operation '(approve approve-all deny reject answer cancel-questions))
      (gptel-runner-background-open)
    (pcase-let* ((`(,run ,call ,_) (gptel-runner-jobs-ui--selection))
		 (arguments (copy-sequence arguments)))
      (when (memq operation '(pause-call cancel-call feedback accept revoke-outside))
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
      (when (and call (memq operation '(pause-call cancel-call feedback accept revoke-outside)))
	(setq arguments (plist-put arguments :call-id (plist-get call :id))
              arguments (plist-put arguments :call-epoch (plist-get call :epoch))))
      (when (eq operation 'revoke-outside)
	(setq arguments (plist-put arguments :generation (plist-get call :generation))))
      (let ((receipt (gptel-runner-job-command run operation arguments)))
	(message "Background command %s: %s"
		 (plist-get receipt :id) (or (plist-get receipt :status) 'queued))
	receipt))))

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

(defun gptel-runner-background-rerun ()
  "Rerun the selected finished invocation from its first step."
  (interactive) (gptel-runner-background-command 'rerun))

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
  "Open the node panel to review pending tools together."
  (interactive) (gptel-runner-background-command 'approve))

(defun gptel-runner-background-reject ()
  "Open the node panel to review and reject pending tools."
  (interactive) (gptel-runner-background-command 'reject))

(defun gptel-runner-jobs-ui--forgettable-at-point-p ()
  "Return non-nil when the selected background run can be forgotten."
  (ignore-errors
    (gptel-runner-invocation-forgettable-p
     (car (gptel-runner-jobs-ui--selection)))))

(defun gptel-runner-jobs-ui--workflow-invocations ()
  "Return visible background history for the workflow at point."
  (let ((name (plist-get (car (gptel-runner-jobs-ui--selection)) :workflow)))
    (cl-remove-if-not (lambda (run) (eq (plist-get run :workflow) name))
                      (gptel-runner-jobs-ui--visible-invocations))))

(defun gptel-runner-jobs-ui--forgettable-workflow-p ()
  "Return non-nil when the selected workflow's background history is stopped."
  (ignore-errors
    (let ((runs (gptel-runner-jobs-ui--workflow-invocations)))
      (and runs (cl-every #'gptel-runner-invocation-forgettable-p runs)))))

(defun gptel-runner-jobs-ui--finished-invocations (&optional job-id)
  "Return visible stopped invocations, limiting them to JOB-ID when given."
  (cl-loop for run in (gptel-runner-jobs-ui--visible-invocations)
           when (and (or (null job-id) (equal job-id (plist-get run :job-id)))
                     (gptel-runner-invocation-forgettable-p run))
           collect run))

(defun gptel-runner-jobs-ui--forget (runs delete-checkpoints description)
  "Confirm forgetting RUNS with DESCRIPTION and optional DELETE-CHECKPOINTS."
  (unless runs (user-error "There are no background runs to forget"))
  (unless (cl-every #'gptel-runner-invocation-forgettable-p runs)
    (user-error "Background workers are still active; cancel them before cleanup"))
  (when (yes-or-no-p
         (format "Forget %s%s? " description
                 (if delete-checkpoints " and delete recovery checkpoints" "")))
    (dolist (run runs)
      (gptel-runner-invocation-forget run delete-checkpoints)
      (dolist (buffer (buffer-list))
        (when (with-current-buffer buffer
                (and (equal (nth 1 gptel-runner-background--target)
                            (plist-get run :id))
                     (not (buffer-modified-p))))
          (kill-buffer buffer))))
    (when-let* ((buffer (get-buffer gptel-runner-dashboard-buffer)))
      (with-current-buffer buffer (gptel-runner-dashboard-refresh)))
    (message "Forgot %d background run%s" (length runs)
             (if (= (length runs) 1) "" "s"))))

(defun gptel-runner-background-forget-run (&optional delete-checkpoint)
  "Forget the selected background run, preserving its saved history.
With prefix argument DELETE-CHECKPOINT, also delete its recovery checkpoint."
  (interactive "P")
  (let ((run (car (gptel-runner-jobs-ui--selection))))
    (gptel-runner-jobs-ui--forget
     (list run) delete-checkpoint (format "background run %s" (plist-get run :id)))))

(defun gptel-runner-background-forget-workflow (&optional delete-checkpoints)
  "Forget the selected workflow's background runs, keeping jobs and schedules.
With prefix argument DELETE-CHECKPOINTS, also delete recovery checkpoints.
All workers belonging to the selected history must have stopped."
  (interactive "P")
  (let ((runs (gptel-runner-jobs-ui--workflow-invocations)))
    (gptel-runner-jobs-ui--forget
     runs delete-checkpoints
     (format "%d background runs for workflow %S"
             (length runs) (plist-get (car runs) :workflow)))))

(defun gptel-runner-background-clear-finished (&optional delete-checkpoints)
  "Forget all stopped background runs, preserving their saved history.
With prefix argument DELETE-CHECKPOINTS, also delete recovery checkpoints."
  (interactive "P")
  (let ((runs (gptel-runner-jobs-ui--finished-invocations)))
    (gptel-runner-jobs-ui--forget
     runs delete-checkpoints (format "%d finished background runs" (length runs)))))

(transient-define-prefix gptel-runner-background-menu ()
  "Inspect and control the selected background invocation."
  [["Inspect"
    ("i" "Run/call details" gptel-runner-background-inspect)
    ("w" "Worker controls" gptel-runner-background-open)
    ("m" "Node messages" gptel-runner-background-messages)
    ("j" "Child transcripts" gptel-runner-background-child-transcripts)
    ("e" "Event journal" gptel-runner-background-events)
    ("v" "Open node chat" gptel-runner-background-transcript)
    ("o" "Saved outputs" gptel-runner-background-outputs)
    ("h" "Command receipts" gptel-runner-background-commands)
    ("A" "Artifact" gptel-runner-background-artifact)]
   ["Run"
    ("P" "Pause" gptel-runner-background-pause)
    ("r" "Resume" gptel-runner-background-resume)
    ("t" "Retry / retry node" gptel-runner-background-retry)
    ("R" "Rerun with same goal" gptel-runner-background-rerun)
    ("c" "Continue" gptel-runner-background-continue)
    ("b" "Extend budgets" gptel-runner-background-extend)
    ("K" "Cancel" gptel-runner-background-cancel)]
   ["Interaction"
    ("f" "Agent feedback" gptel-runner-background-feedback)
    ("a" "Accept response" gptel-runner-background-accept)
    ("y" "Review pending tools" gptel-runner-background-approve)
    (":" "All actions" gptel-runner-background-command)]]
  [["Manage"
    ("x" "Forget run" gptel-runner-background-forget-run
     :inapt-if-not gptel-runner-jobs-ui--forgettable-at-point-p)
    ("X" "Forget workflow runs" gptel-runner-background-forget-workflow
     :inapt-if-not gptel-runner-jobs-ui--forgettable-workflow-p)
    ("C" "Clear finished background runs" gptel-runner-background-clear-finished
     :inapt-if-not gptel-runner-jobs-ui--finished-invocations)
    ("V" "Toggle column" gptel-runner-dashboard-toggle-column)
    ("g" "Refresh" gptel-runner-dashboard-refresh)]])

(defun gptel-runner-jobs-ui--row-action (action row)
  "Handle ACTION for external dashboard ROW and report whether handled."
  (when (gptel-runner-jobs-ui--target row)
    (pcase action
      ('open (gptel-runner-background-visit row) t)
      ('menu (call-interactively #'gptel-runner-background-menu) t))))

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
                                    'uninstalled))
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
  (when (and (or (gptel-runner-jobs--scheduler) gptel-runner-job-scheduler-program)
             (not (and (processp gptel-runner-jobs-ui--reap-process)
                       (process-live-p gptel-runner-jobs-ui--reap-process)))
             (or (null gptel-runner-jobs-ui--last-reap-time)
                 (>= (- (float-time) gptel-runner-jobs-ui--last-reap-time) 30)))
    (setq gptel-runner-jobs-ui--last-reap-time (float-time))
    (let ((buffer (get-buffer-create "*gptel-runner cleanup log*"))
          (form `(let ((gptel-runner-job-directory
                        ,(expand-file-name gptel-runner-job-directory)))
                   (gptel-runner-jobs--scheduler-call 'reap))))
      (condition-case err
          (setq gptel-runner-jobs-ui--reap-process
                (make-process
                 :name "gptel-runner-cleanup" :buffer buffer :noquery t
                 :connection-type 'pipe
                 :command (append (list gptel-runner-job-emacs-program "-Q" "--batch"
                                        "-L" gptel-runner-jobs--package-directory
                                        "-l" "gptel-runner-jobs")
                                  (gptel-runner-jobs--native-settings-arguments)
                                  (list "--eval" (prin1-to-string form)))
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
  "Manage registered background jobs and their run history."
  (setq tabulated-list-format
        [("Job" 26 t) ("Workflow" 24 t) ("State" 12 t)
         ("Schedule" 35 nil) ("Revision" 8 t)]
        tabulated-list-entries #'gptel-runner-jobs-ui--job-entries
        tabulated-list-padding 2)
  (setq-local mode-line-process "  ? Actions · x Delete job · C Clear job's finished runs")
  (add-hook 'tabulated-list-revert-hook #'gptel-runner-jobs-refresh nil t)
  (tabulated-list-init-header))

(defun gptel-runner-jobs-run ()
  "Start the selected job in an isolated Emacs worker now."
  (interactive)
  (let ((run (gptel-runner-job-run (gptel-runner-jobs-ui--job) 'manual)))
    (if (eq (plist-get run :state) 'skipped)
        (message "Skipped background invocation %s: another invocation is active"
                 (plist-get run :id))
      (message "Started background invocation %s" (plist-get run :id)))
    (gptel-runner-show-dashboard)))

(defun gptel-runner-jobs-diagnostics ()
  "Show scheduler diagnostics for the selected job."
  (interactive)
  (gptel-runner-jobs-ui--display
   "*gptel-runner schedule diagnostics*"
   (gptel-runner-jobs--scheduler-call 'diagnostics (gptel-runner-jobs-ui--job))))

(defun gptel-runner-jobs-remove ()
  "Delete the selected job and its schedule, preserving its runs."
  (interactive)
  (let ((id (plist-get (gptel-runner-jobs-ui--job) :id)))
    (when (yes-or-no-p (format "Delete job %s and its schedule, keeping existing runs? " id))
      (gptel-runner-job-delete id)
      (gptel-runner-jobs-refresh)
      (message "Deleted job %s" id))))

(defun gptel-runner-jobs-clear-finished (&optional delete-checkpoints)
  "Clear the selected job's finished runs from the run dashboard.
With prefix argument DELETE-CHECKPOINTS, also delete their recovery checkpoints."
  (interactive "P")
  (let* ((id (plist-get (gptel-runner-jobs-ui--job) :id))
         (runs (gptel-runner-jobs-ui--finished-invocations id)))
    (unless runs (user-error "No finished runs to clear for job %s" id))
    (gptel-runner-jobs-ui--forget
     runs delete-checkpoints
     (format "%d finished %s for job %s"
             (length runs) (if (cdr runs) "runs" "run") id))))

(defun gptel-runner-jobs-ui--job-has-finished-runs-p ()
  "Return non-nil when the selected job has runs to clear."
  (when-let* ((id (tabulated-list-get-id)))
    (gptel-runner-jobs-ui--finished-invocations id)))

(transient-define-prefix gptel-runner-jobs-menu ()
  "Run and manage jobs, schedules, and dashboard history."
  [["Job"
    ("r" "Run now" gptel-runner-jobs-run :inapt-if-not tabulated-list-get-id)
    ("i" "Schedule diagnostics" gptel-runner-jobs-diagnostics :inapt-if-not tabulated-list-get-id)
    ("x" "Delete job and schedule" gptel-runner-jobs-remove :inapt-if-not tabulated-list-get-id)
    ("C" "Clear this job's finished runs" gptel-runner-jobs-clear-finished
     :inapt-if-not gptel-runner-jobs-ui--job-has-finished-runs-p)]
   ["View"
    ("g" "Refresh" gptel-runner-jobs-refresh)
    ("R" "Runs" gptel-runner-show-dashboard)]])

(let ((map gptel-runner-jobs-mode-map))
  (define-key map (kbd "?") #'gptel-runner-jobs-menu)
  (define-key map (kbd "RET") #'gptel-runner-jobs-menu)
  (define-key map (kbd "x") #'gptel-runner-jobs-remove)
  (define-key map (kbd "C") #'gptel-runner-jobs-clear-finished)
  (define-key map (kbd "g") #'gptel-runner-jobs-refresh))

;;;###autoload
(defun gptel-runner-show-jobs ()
  "Show registered background jobs, their schedules, and controls to run them."
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
