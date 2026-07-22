;;; gptel-runner-store.el --- Durable snapshots for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Compact, versioned execution checkpoints for named workflows.  Automatic
;; checkpoints are debounced and all snapshots are serialized in short timer
;; slices so persistence does not monopolize the Emacs event loop.  Snapshots
;; contain only the state required to resume execution; calls, events, and
;; worker transcripts remain session-local inspection data.

;;; Code:

(require 'cl-lib)
(require 'gptel-runner-core)
(require 'gptel-runner-flow)

(defgroup gptel-runner-store nil
  "Persistent snapshots for gptel-runner."
  :group 'gptel-runner)

(defcustom gptel-runner-snapshot-directory
  (expand-file-name "gptel-runner/snapshots/" user-emacs-directory)
  "Directory in which durable runner snapshots are stored."
  :type 'directory)

(defcustom gptel-runner-checkpoint-delay 5.0
  "Seconds of inactivity before an automatic checkpoint starts.
Repeated safe checkpoints reset this delay.  Explicit saves, pauses, and
terminal run transitions bypass it, but their writes are still asynchronous."
  :type 'number)

(defconst gptel-runner-snapshot-version 2
  "Current on-disk runner snapshot format version.")

(defconst gptel-runner-store--slice-seconds 0.01
  "Approximate maximum serialization time in one event-loop slice.")

(defconst gptel-runner-store--chunk-bytes (* 256 1024)
  "Approximate maximum buffered output before a snapshot write.")

(cl-defstruct (gptel-runner-store--token
               (:constructor gptel-runner-store--token-create))
  "One snapshot VALUE and the CONTEXT used for serialization errors."
  value context)

(cl-defstruct (gptel-runner-store--writer
               (:constructor gptel-runner-store--writer-create))
  "Incremental writer state for one snapshot generation."
  revision file temporary tokens parts (part-bytes 0) notify)

(cl-defstruct (gptel-runner-store--coordinator
               (:constructor gptel-runner-store--coordinator-create))
  "Save coordination state belonging to one runner RUN."
  run file (revision 0) (committed-revision 0) (state 'clean)
  timer writer error urgent notify)

(defvar gptel-runner-store--coordinators (make-hash-table :test #'eq)
  "Save coordinators indexed by live run object.")

(defun gptel-runner-store--alist-hash (alist &optional test)
  "Return a hash table populated from ALIST using TEST."
  (let ((table (make-hash-table :test (or test #'equal))))
    (dolist (entry alist table)
      (puthash (car entry) (cdr entry) table))))

(defun gptel-runner-store--default-file (run)
  "Return the default snapshot filename for RUN."
  (expand-file-name
   (concat (replace-regexp-in-string
            "[^[:alnum:]_.-]" "_" (gptel-runner-run-id run))
           ".snapshot.el")
   gptel-runner-snapshot-directory))

(defun gptel-runner-store--validate-run (run)
  "Validate that RUN can be queued for persistent storage."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (unless (gptel-runner-workflow-name (gptel-runner-run-workflow run))
    (user-error "Persistent runs require a named workflow")))

(defun gptel-runner-store--table-entries (table)
  "Return a shallow alist snapshot of hash TABLE."
  (let (entries)
    (maphash (lambda (key value) (push (cons key value) entries)) table)
    (nreverse entries)))

(defun gptel-runner-store--duration-remaining (run)
  "Return RUN's duration remaining at snapshot capture time."
  (let ((remaining (gptel-runner-run-duration-remaining run)))
    (if (and (numberp remaining)
             (numberp (gptel-runner-run-active-started-at run)))
        (max 0 (- remaining
                  (- (float-time)
                     (gptel-runner-run-active-started-at run))))
      remaining)))

(defun gptel-runner-store--snapshot-tokens (run)
  "Return incremental serialization tokens for a compact snapshot of RUN."
  (let ((budget (gptel-runner-run-budget run)) tokens)
    (cl-labels
        ((literal (text) (push text tokens))
         (value (object context)
           (push (gptel-runner-store--token-create
                  :value object :context context)
                 tokens))
         (table (name object strict)
           (literal (format "\n  :%s (" name))
           (dolist (entry (gptel-runner-store--table-entries object))
             (literal "\n   ")
             (value entry
                    (if strict
                        (format "blackboard entry %S" (car entry))
                      (format "%s entry %S" name (car entry)))))
           (literal ")")))
      (literal ";;; gptel-runner snapshot -- execution data only\n")
      (literal "(:format gptel-runner-snapshot\n :version 2\n :saved-at ")
      (value (float-time) "save time")
      (literal "\n :run (:id ")
      (value (gptel-runner-run-id run) "run identifier")
      (literal "\n  :workflow ")
      (value (gptel-runner-workflow-name
              (gptel-runner-run-workflow run))
             "workflow name")
      (literal "\n  :goal ")
      (value (gptel-runner-run-goal run) "run goal")
      (literal "\n  :workspace ")
      (value (gptel-runner-run-workspace run) "workspace")
      (literal "\n  :state ")
      (value (gptel-runner-run-state run) "run state")
      (table "blackboard" (gptel-runner-run-blackboard run) t)
      (table "node-states" (gptel-runner-run-node-states run) nil)
      (table "iterations" (gptel-runner-run-iterations run) nil)
      (literal "\n  :budget ")
      (value
       (list :max-requests (gptel-runner-budget-max-requests budget)
             :max-calls (gptel-runner-budget-max-calls budget)
             :max-duration (gptel-runner-budget-max-duration budget)
             :requests (gptel-runner-budget-requests budget)
             :calls (gptel-runner-budget-calls budget))
       "budget")
      (literal "\n  :started-at ")
      (value (gptel-runner-run-started-at run) "start time")
      (literal "\n  :finished-at ")
      (value (gptel-runner-run-finished-at run) "finish time")
      (literal "\n  :paused-at ")
      (value (gptel-runner-run-paused-at run) "pause time")
      (literal "\n  :duration-remaining ")
      (value (gptel-runner-store--duration-remaining run)
             "remaining duration")
      (literal "\n  :generation ")
      (value (gptel-runner-run-generation run) "run generation")
      (literal "\n  :options ")
      (value (copy-sequence (gptel-runner-run-options run)) "run options")
      (literal "))\n")
      (nreverse tokens))))

(defun gptel-runner-store--serialize-token (token)
  "Serialize TOKEN and verify that the result can be read back."
  (condition-case err
      (let* ((print-circle t)
             (print-level nil)
             (print-length nil)
             (printed (prin1-to-string
                       (gptel-runner-store--token-value token)))
             (parsed (read-from-string printed)))
        (unless (= (cdr parsed) (length printed))
          (error "Serialized value has trailing unreadable data"))
        printed)
    (error
     (error "Snapshot %s cannot be persisted: %s"
            (gptel-runner-store--token-context token)
            (error-message-string err)))))

(defun gptel-runner-store--coordinator (run)
  "Return RUN's existing or newly allocated save coordinator."
  (or (gethash run gptel-runner-store--coordinators)
      (let ((coordinator
             (gptel-runner-store--coordinator-create :run run)))
        (puthash run coordinator gptel-runner-store--coordinators)
        coordinator)))

(defun gptel-runner-store-save-status (run)
  "Return the persistence status of RUN.
The result is one of `clean', `pending', `writing', or `error'."
  (if-let ((coordinator
            (gethash run gptel-runner-store--coordinators)))
      (gptel-runner-store--coordinator-state coordinator)
    'clean))

(defun gptel-runner-store--cancel-timer (coordinator)
  "Cancel COORDINATOR's outstanding timer, if any."
  (when (timerp (gptel-runner-store--coordinator-timer coordinator))
    (cancel-timer (gptel-runner-store--coordinator-timer coordinator)))
  (setf (gptel-runner-store--coordinator-timer coordinator) nil))

(defun gptel-runner-store--delete-temporary (writer)
  "Delete WRITER's incomplete temporary snapshot."
  (when (and writer
             (gptel-runner-store--writer-temporary writer)
             (file-exists-p
              (gptel-runner-store--writer-temporary writer)))
    (delete-file (gptel-runner-store--writer-temporary writer))))

(defun gptel-runner-store-cancel-save (run)
  "Cancel pending persistence work for RUN and remove temporary data."
  (when-let ((coordinator
              (gethash run gptel-runner-store--coordinators)))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary
     (gptel-runner-store--coordinator-writer coordinator))
    (remhash run gptel-runner-store--coordinators)
    t))

(defun gptel-runner-store--fail (coordinator err)
  "Record ERR and stop COORDINATOR's current save attempt."
  (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
         (notify (or (gptel-runner-store--coordinator-notify coordinator)
                     (and writer
                          (gptel-runner-store--writer-notify writer)))))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary writer)
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-state coordinator) 'error
          (gptel-runner-store--coordinator-error coordinator) err
          (gptel-runner-store--coordinator-urgent coordinator) nil
          (gptel-runner-store--coordinator-notify coordinator) nil)
    (condition-case nil
        (gptel-runner--emit
         (gptel-runner-store--coordinator-run coordinator)
         'snapshot-error nil nil err)
      (error nil))
    (when notify
      (message "Snapshot save failed: %s" (error-message-string err)))))

(defun gptel-runner-store--flush-parts (writer)
  "Append WRITER's buffered text parts to its temporary file."
  (when (gptel-runner-store--writer-parts writer)
    (let ((text (apply #'concat
                       (nreverse (gptel-runner-store--writer-parts writer)))))
      (write-region text nil
                    (gptel-runner-store--writer-temporary writer)
                    t 'silent))
    (setf (gptel-runner-store--writer-parts writer) nil
          (gptel-runner-store--writer-part-bytes writer) 0)))

(defun gptel-runner-store--append-part (writer text)
  "Add TEXT to WRITER's buffered output."
  (push text (gptel-runner-store--writer-parts writer))
  (cl-incf (gptel-runner-store--writer-part-bytes writer)
           (string-bytes text))
  (when (>= (gptel-runner-store--writer-part-bytes writer)
            gptel-runner-store--chunk-bytes)
    (gptel-runner-store--flush-parts writer)))

(defun gptel-runner-store--schedule-slice (coordinator)
  "Arrange another writer slice for COORDINATOR."
  (setf (gptel-runner-store--coordinator-timer coordinator)
        (run-at-time 0 nil #'gptel-runner-store--write-slice coordinator)))

(defun gptel-runner-store--commit (coordinator)
  "Commit COORDINATOR's completed temporary snapshot atomically."
  (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
         (run (gptel-runner-store--coordinator-run coordinator))
         (revision (gptel-runner-store--writer-revision writer))
         (file (gptel-runner-store--writer-file writer))
         (notify (gptel-runner-store--writer-notify writer)))
    (gptel-runner-store--flush-parts writer)
    (set-file-modes (gptel-runner-store--writer-temporary writer) #o600)
    (rename-file (gptel-runner-store--writer-temporary writer) file t)
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-committed-revision coordinator)
          revision
          (gptel-runner-store--coordinator-error coordinator) nil)
    (if (< revision (gptel-runner-store--coordinator-revision coordinator))
        (progn
          (setf (gptel-runner-store--coordinator-state coordinator) 'pending
                (gptel-runner-store--coordinator-urgent coordinator) t)
          (gptel-runner-store--schedule-begin coordinator 0))
      (setf (gptel-runner-store--coordinator-state coordinator) 'clean))
    (condition-case nil
        (gptel-runner--emit run 'snapshot-saved nil nil
                            (list :file file :revision revision))
      (error nil))
    (when notify
      (message "Snapshot saved to %s" file))))

(defun gptel-runner-store--write-slice (coordinator &optional drain)
  "Write one time slice for COORDINATOR.
When DRAIN is non-nil, finish all remaining work synchronously."
  (setf (gptel-runner-store--coordinator-timer coordinator) nil)
  (condition-case err
      (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
             (deadline (+ (float-time) gptel-runner-store--slice-seconds))
             processed)
        (unless writer
          (error "Snapshot writer disappeared"))
        (while (and (gptel-runner-store--writer-tokens writer)
                    (or drain (not processed) (< (float-time) deadline)))
          (let ((token (pop (gptel-runner-store--writer-tokens writer))))
            (gptel-runner-store--append-part
             writer
             (if (stringp token)
                 token
               (gptel-runner-store--serialize-token token))))
          (setq processed t))
        (gptel-runner-store--flush-parts writer)
        (if (gptel-runner-store--writer-tokens writer)
            (gptel-runner-store--schedule-slice coordinator)
          (gptel-runner-store--commit coordinator)))
    (error (gptel-runner-store--fail coordinator err))))

(defun gptel-runner-store--begin (coordinator)
  "Begin an incremental snapshot for COORDINATOR's latest revision."
  (gptel-runner-store--cancel-timer coordinator)
  (condition-case err
      (let* ((run (gptel-runner-store--coordinator-run coordinator))
             (file (gptel-runner-store--coordinator-file coordinator))
             (directory (file-name-directory file)))
        (make-directory directory t)
        (setf
         (gptel-runner-store--coordinator-writer coordinator)
         (gptel-runner-store--writer-create
          :revision (gptel-runner-store--coordinator-revision coordinator)
          :file file
          :temporary
          (make-temp-file (expand-file-name ".gptel-runner-" directory)
                          nil ".snapshot")
          :tokens (gptel-runner-store--snapshot-tokens run)
          :notify (gptel-runner-store--coordinator-notify coordinator))
         (gptel-runner-store--coordinator-state coordinator) 'writing
         (gptel-runner-store--coordinator-error coordinator) nil
         (gptel-runner-store--coordinator-urgent coordinator) nil
         (gptel-runner-store--coordinator-notify coordinator) nil)
        (gptel-runner-store--schedule-slice coordinator))
    (error (gptel-runner-store--fail coordinator err))))

(defun gptel-runner-store--schedule-begin (coordinator delay)
  "Begin saving COORDINATOR after DELAY seconds."
  (gptel-runner-store--cancel-timer coordinator)
  (setf (gptel-runner-store--coordinator-timer coordinator)
        (run-at-time delay nil #'gptel-runner-store--begin coordinator)))

(defun gptel-runner-store--queue (run file immediate notify)
  "Queue RUN for FILE, honoring IMMEDIATE and user NOTIFY flags."
  (gptel-runner-store--validate-run run)
  (setq file (expand-file-name
              (or file (gptel-runner-run-snapshot-file run)
                  (gptel-runner-store--default-file run))))
  (setf (gptel-runner-run-snapshot-file run) file
        (gptel-runner-run-options run)
        (plist-put (gptel-runner-run-options run) :persist t))
  (let* ((coordinator (gptel-runner-store--coordinator run))
         (already-queued
          (memq (gptel-runner-store--coordinator-state coordinator)
                '(pending writing))))
    (setf (gptel-runner-store--coordinator-file coordinator) file
          (gptel-runner-store--coordinator-notify coordinator)
          (or notify (gptel-runner-store--coordinator-notify coordinator)))
    (cl-incf (gptel-runner-store--coordinator-revision coordinator))
    (when immediate
      (setf (gptel-runner-store--coordinator-urgent coordinator) t))
    (unless (eq (gptel-runner-store--coordinator-state coordinator) 'writing)
      (setf (gptel-runner-store--coordinator-state coordinator) 'pending)
      (gptel-runner-store--schedule-begin
       coordinator
       (if (gptel-runner-store--coordinator-urgent coordinator)
           0
         (max 0 gptel-runner-checkpoint-delay))))
    (unless already-queued
      (gptel-runner--emit
       run 'snapshot-save-queued nil nil
       (list :file file
             :revision (gptel-runner-store--coordinator-revision coordinator)
             :immediate immediate)))
    file))

(defun gptel-runner-schedule-save (run &optional immediate)
  "Queue an automatic snapshot of RUN.
When IMMEDIATE is non-nil, bypass `gptel-runner-checkpoint-delay'."
  (gptel-runner-store--queue run nil immediate nil))

(defun gptel-runner-save-run (run &optional file)
  "Queue an atomic snapshot of RUN to FILE and return its absolute filename.
The command returns before serialization or disk I/O completes.  Completion
is reported through a `snapshot-saved' event; failures use `snapshot-error'."
  (gptel-runner-store--queue run file t t))

(defun gptel-runner-store--flush-coordinator (coordinator)
  "Synchronously commit COORDINATOR's latest queued revision."
  (when (memq (gptel-runner-store--coordinator-state coordinator)
              '(pending writing))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary
     (gptel-runner-store--coordinator-writer coordinator))
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-state coordinator) 'pending)
    (gptel-runner-store--begin coordinator)
    (gptel-runner-store--cancel-timer coordinator)
    (when (gptel-runner-store--coordinator-writer coordinator)
      (gptel-runner-store--write-slice coordinator t))))

(defun gptel-runner-store-flush-pending ()
  "Synchronously finish all queued snapshots.
This is used during normal Emacs shutdown; interactive saves remain
asynchronous."
  (maphash (lambda (_run coordinator)
             (gptel-runner-store--flush-coordinator coordinator))
           gptel-runner-store--coordinators))

(defun gptel-runner-store--node (root id)
  "Find node ID below ROOT."
  (if (equal (gptel-runner-node-id root) id) root
    (cl-loop for child in (gptel-runner-node-children root)
             thereis (gptel-runner-store--node child id))))

(defun gptel-runner-store--notice-id (id)
  "Advance the process-local identifier counter beyond string ID."
  (when (and (stringp id) (string-match "-\\([0-9]+\\)\\'" id))
    (setq gptel-runner--next-id
          (max gptel-runner--next-id
               (string-to-number (match-string 1 id))))))

(defun gptel-runner-store--read-file (file)
  "Read and validate v2 snapshot FILE as data."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (unless (eq (plist-get data :format) 'gptel-runner-snapshot)
        (user-error "Not a gptel-runner snapshot: %s" file))
      (unless (equal (plist-get data :version)
                     gptel-runner-snapshot-version)
        (user-error "Unsupported snapshot version %S (expected v2)"
                    (plist-get data :version)))
      data)))

(defun gptel-runner-load-run (file &optional callback driver)
  "Load paused run from v2 snapshot FILE using CALLBACK and DRIVER.
The workflow and all referenced agents must already be registered.  Historical
calls, events, and transcripts are deliberately not restored."
  (setq file (expand-file-name file))
  (let* ((snapshot (gptel-runner-store--read-file file))
         (data (plist-get snapshot :run))
         (run-id (plist-get data :id))
         (workflow-name (plist-get data :workflow))
         (workflow (or (gethash workflow-name gptel-runner--workflows)
                       (user-error "Define workflow %S before loading snapshot"
                                   workflow-name)))
         (budget-data (plist-get data :budget))
         (budget
          (gptel-runner-budget-create
           :max-requests (plist-get budget-data :max-requests)
           :max-calls (plist-get budget-data :max-calls)
           :max-duration (plist-get budget-data :max-duration)
           :requests (or (plist-get budget-data :requests) 0)
           :calls (or (plist-get budget-data :calls) 0)))
         (saved-state (plist-get data :state))
         (run
          (gptel-runner-run-create
           :id run-id :workflow workflow
           :goal (plist-get data :goal) :workspace (plist-get data :workspace)
           :state (if (gptel-runner--terminal-p saved-state)
                      saved-state 'paused)
           :blackboard (gptel-runner-store--alist-hash
                        (plist-get data :blackboard))
           :node-states (gptel-runner-store--alist-hash
                         (plist-get data :node-states))
           :iterations (gptel-runner-store--alist-hash
                        (plist-get data :iterations))
           :events nil :budget budget
           :driver (or driver gptel-runner-default-driver)
           :queue nil :active-calls nil :calls nil
           :started-at (plist-get data :started-at)
           :finished-at (plist-get data :finished-at)
           :paused-at (or (plist-get data :paused-at) (float-time))
           :duration-remaining (plist-get data :duration-remaining)
           :generation (or (plist-get data :generation) 0)
           :callback callback :snapshot-file file
           :options (plist-put (copy-sequence (plist-get data :options))
                               :persist t))))
    (when (gethash run-id gptel-runner--runs)
      (user-error "Run %s is already loaded in this Emacs session" run-id))
    (unless (gptel-runner-run-driver run)
      (user-error "No driver configured for restored run"))
    (let ((root (gptel-runner-workflow-root workflow)))
      (gptel-runner--validate-workflow
       root (plist-get (gptel-runner-run-options run) :allow-writes))
      (dolist (entry (plist-get data :node-states))
        (unless (gptel-runner-store--node root (car entry))
          (user-error "Snapshot node %S is absent from workflow %S"
                      (car entry) workflow-name)))
      (dolist (entry (plist-get data :iterations))
        (unless (gptel-runner-store--node root (car entry))
          (user-error "Snapshot iteration node %S is absent from workflow %S"
                      (car entry) workflow-name))))
    (gptel-runner-store--notice-id (gptel-runner-run-id run))
    (puthash (gptel-runner-run-id run) run gptel-runner--runs)
    (gptel-runner--emit run 'snapshot-loaded nil nil file)
    run))

(defun gptel-runner-resume-snapshot
    (file &optional feedback callback driver)
  "Load FILE and resume it with FEEDBACK, CALLBACK, and DRIVER."
  (gptel-runner-resume-run
   (gptel-runner-load-run file callback driver) feedback callback))

(defun gptel-runner-list-snapshots ()
  "Return known snapshot files, newest first."
  (when (file-directory-p gptel-runner-snapshot-directory)
    (sort (directory-files gptel-runner-snapshot-directory t
                           "\\.snapshot\\.el\\'")
          (lambda (a b)
            (time-less-p (file-attribute-modification-time
                          (file-attributes b))
                         (file-attribute-modification-time
                          (file-attributes a)))))))

(add-hook 'kill-emacs-hook #'gptel-runner-store-flush-pending)

(provide 'gptel-runner-store)
;;; gptel-runner-store.el ends here
