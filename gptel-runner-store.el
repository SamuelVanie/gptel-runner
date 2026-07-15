;;; gptel-runner-store.el --- Durable snapshots for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Versioned, atomic snapshots of named workflows.  Snapshots contain data,
;; never executable workflow definitions: users must load the same workflow
;; and agent registrations before restoring a run.

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'gptel-runner-core)
(require 'gptel-runner-flow)

(declare-function gptel-runner-gptel-restore-worker-buffer
                  "gptel-runner-gptel")

(defgroup gptel-runner-store nil
  "Persistent snapshots for gptel-runner."
  :group 'gptel-runner)

(defcustom gptel-runner-snapshot-directory
  (expand-file-name "gptel-runner/snapshots/" user-emacs-directory)
  "Directory in which durable runner snapshots are stored."
  :type 'directory)

(defconst gptel-runner-snapshot-version 1
  "Current on-disk runner snapshot format version.")

(defun gptel-runner-store--readable-copy (value &optional fallback)
  "Return a reader-round-tripped copy of VALUE, or FALLBACK on failure."
  (condition-case nil
      (let* ((print-circle t)
             (print-level nil)
             (print-length nil)
             (printed (prin1-to-string value))
             (parsed (read-from-string printed)))
        (if (string-match-p "\\`[[:space:]]*\\'"
                            (substring printed (cdr parsed)))
            (car parsed)
          fallback))
    (error fallback)))

(defun gptel-runner-store--hash-alist (table &optional strict)
  "Convert hash TABLE to a readable alist.
When STRICT is non-nil, signal if any key or value cannot be serialized."
  (let ((unreadable (make-symbol "unreadable")) result)
    (maphash
     (lambda (key value)
       (let ((saved-key (gptel-runner-store--readable-copy key unreadable))
             (saved-value (gptel-runner-store--readable-copy value unreadable)))
         (when (and strict
                    (or (eq saved-key unreadable)
                        (eq saved-value unreadable)))
           (user-error "Blackboard entry %S cannot be persisted" key))
         (push (cons saved-key saved-value) result)))
     table)
    (nreverse result)))

(defun gptel-runner-store--alist-hash (alist &optional test)
  "Return a hash table populated from ALIST using TEST."
  (let ((table (make-hash-table :test (or test #'equal))))
    (dolist (entry alist table)
      (puthash (car entry) (cdr entry) table))))

(defun gptel-runner-store--event (event)
  "Convert EVENT to snapshot data."
  (list :time (gptel-runner-event-time event)
        :run-id (gptel-runner-event-run-id event)
        :node-id (gptel-runner-event-node-id event)
        :call-id (gptel-runner-event-call-id event)
        :type (gptel-runner-event-type event)
        :data (gptel-runner-store--readable-copy
               (gptel-runner-event-data event)
               (list :unreadable
                     (format "%S" (gptel-runner-event-data event))))))

(defun gptel-runner-store--restore-event (data)
  "Restore an event struct from snapshot DATA."
  (gptel-runner-event-create
   :time (plist-get data :time) :run-id (plist-get data :run-id)
   :node-id (plist-get data :node-id) :call-id (plist-get data :call-id)
   :type (plist-get data :type) :data (plist-get data :data)))

(defun gptel-runner-store--transcript (call)
  "Return a readable transcript string for CALL, when available."
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (with-current-buffer (gptel-runner-call-buffer call)
      (or (gptel-runner-store--readable-copy
           (buffer-substring (point-min) (point-max)))
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun gptel-runner-store--call (call)
  "Convert CALL to snapshot data."
  (list
   :id (gptel-runner-call-id call)
   :node-id (gptel-runner-node-id (gptel-runner-call-node call))
   :agent (gptel-runner-agent-name (gptel-runner-call-agent call))
   :prompt (gptel-runner-call-prompt call)
   :workspace (gptel-runner-call-workspace call)
   :state (gptel-runner-call-state call)
   :request-attempt (gptel-runner-call-request-attempt call)
   :generation (gptel-runner-call-generation call)
   :result (gptel-runner-store--readable-copy
            (gptel-runner-call-result call)
            (format "%S" (gptel-runner-call-result call)))
   :error (gptel-runner-store--readable-copy
           (gptel-runner-call-error call)
           (format "%S" (gptel-runner-call-error call)))
   :started-at (gptel-runner-call-started-at call)
   :finished-at (gptel-runner-call-finished-at call)
   :repair-p (gptel-runner-call-repair-p call)
   :transcript (gptel-runner-store--transcript call)))

(defun gptel-runner-store--snapshot-data (run)
  "Return versioned snapshot data for RUN."
  (let* ((unreadable (make-symbol "unreadable"))
         (workflow-name
         (gptel-runner-workflow-name (gptel-runner-run-workflow run)))
        (budget (gptel-runner-run-budget run))
        (goal (gptel-runner-store--readable-copy
               (gptel-runner-run-goal run) unreadable))
        (duration-remaining (gptel-runner-run-duration-remaining run)))
    (unless workflow-name
      (user-error "Persistent runs require a named workflow"))
    (when (eq goal unreadable)
      (user-error "Run goal cannot be persisted"))
    (when (and (numberp duration-remaining)
               (numberp (gptel-runner-run-active-started-at run)))
      (setq duration-remaining
            (max 0 (- duration-remaining
                      (- (float-time)
                         (gptel-runner-run-active-started-at run))))))
    (list
     :format 'gptel-runner-snapshot
     :version gptel-runner-snapshot-version
     :saved-at (float-time)
     :run
     (list
      :id (gptel-runner-run-id run)
      :workflow workflow-name
      :goal goal
      :workspace (gptel-runner-run-workspace run)
      :state (gptel-runner-run-state run)
      :blackboard (gptel-runner-store--hash-alist
                   (gptel-runner-run-blackboard run) t)
      :node-states (gptel-runner-store--hash-alist
                    (gptel-runner-run-node-states run))
      :iterations (gptel-runner-store--hash-alist
                   (gptel-runner-run-iterations run))
      :events (mapcar #'gptel-runner-store--event
                      (gptel-runner-run-events run))
      :calls (mapcar #'gptel-runner-store--call
                     (gptel-runner-run-calls run))
      :budget
      (list :max-requests (gptel-runner-budget-max-requests budget)
            :max-calls (gptel-runner-budget-max-calls budget)
            :max-duration (gptel-runner-budget-max-duration budget)
            :requests (gptel-runner-budget-requests budget)
            :calls (gptel-runner-budget-calls budget))
      :started-at (gptel-runner-run-started-at run)
      :finished-at (gptel-runner-run-finished-at run)
      :paused-at (gptel-runner-run-paused-at run)
      :duration-remaining duration-remaining
      :generation (gptel-runner-run-generation run)
      :options (gptel-runner-store--readable-copy
                (gptel-runner-run-options run) nil)))))

(defun gptel-runner-store--default-file (run)
  "Return the default snapshot filename for RUN."
  (expand-file-name
   (concat (replace-regexp-in-string
            "[^[:alnum:]_.-]" "_" (gptel-runner-run-id run))
           ".snapshot.el")
   gptel-runner-snapshot-directory))

(defun gptel-runner-save-run (run &optional file)
  "Atomically save RUN to FILE and return its absolute filename."
  (setq file (expand-file-name
              (or file (gptel-runner-run-snapshot-file run)
                  (gptel-runner-store--default-file run))))
  (make-directory (file-name-directory file) t)
  (setf (gptel-runner-run-snapshot-file run) file
        (gptel-runner-run-options run)
        (plist-put (gptel-runner-run-options run) :persist t))
  (let* ((data (gptel-runner-store--snapshot-data run))
         (temporary (make-temp-file
                     (expand-file-name ".gptel-runner-" (file-name-directory file))
                     nil ".snapshot"))
         (text (concat ";;; gptel-runner snapshot -- data, not executable code\n"
                       (pp-to-string data))))
    (unwind-protect
        (progn
          (write-region text nil temporary nil 'silent)
          (set-file-modes temporary #o600)
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary))))
  file)

(defun gptel-runner-store--node (root id)
  "Find node ID below ROOT."
  (if (equal (gptel-runner-node-id root) id) root
    (cl-loop for child in (gptel-runner-node-children root)
             thereis (gptel-runner-store--node child id))))

(defun gptel-runner-store--restore-call (run root data)
  "Restore one call belonging to RUN and ROOT from DATA."
  (let* ((node-id (plist-get data :node-id))
         (node (or (gptel-runner-store--node root node-id)
                   (user-error "Snapshot references unknown node %S" node-id)))
         (agent-name (plist-get data :agent))
         (agent (or (gethash agent-name gptel-runner--agents)
                    (user-error "Register agent %S before loading snapshot"
                                agent-name)))
         (saved-state (plist-get data :state))
         (state (if (gptel-runner--terminal-p saved-state)
                    saved-state
                  (if (eq saved-state 'waiting-feedback)
                      'waiting-feedback
                    'paused)))
         (call
          (gptel-runner-call-create
           :id (plist-get data :id) :run run :node node :agent agent
           :prompt (plist-get data :prompt)
           :workspace (or (plist-get data :workspace)
                          (gptel-runner-run-workspace run))
           :state state :request-attempt (plist-get data :request-attempt)
           :generation (or (plist-get data :generation) 0)
           :result (plist-get data :result) :error (plist-get data :error)
           :started-at (plist-get data :started-at)
           :finished-at (plist-get data :finished-at)
           :repair-p (plist-get data :repair-p))))
    (when-let ((transcript (plist-get data :transcript)))
      (when (fboundp 'gptel-runner-gptel-restore-worker-buffer)
        (gptel-runner-gptel-restore-worker-buffer call transcript)))
    call))

(defun gptel-runner-store--notice-id (id)
  "Advance the process-local identifier counter beyond string ID."
  (when (and (stringp id) (string-match "-\\([0-9]+\\)\\'" id))
    (setq gptel-runner--next-id
          (max gptel-runner--next-id
               (string-to-number (match-string 1 id))))))

(defun gptel-runner-store--read-file (file)
  "Read and validate snapshot FILE as data."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (unless (eq (plist-get data :format) 'gptel-runner-snapshot)
        (user-error "Not a gptel-runner snapshot: %s" file))
      (unless (equal (plist-get data :version)
                     gptel-runner-snapshot-version)
        (user-error "Unsupported snapshot version %S (expected %S)"
                    (plist-get data :version) gptel-runner-snapshot-version))
      data)))

(defun gptel-runner-load-run (file &optional callback driver)
  "Load paused run from snapshot FILE using CALLBACK and DRIVER.
The workflow and all referenced agents must already be registered."
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
           :events (mapcar #'gptel-runner-store--restore-event
                           (plist-get data :events))
           :budget budget :driver (or driver gptel-runner-default-driver)
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
                      (car entry) workflow-name)))
      (setf (gptel-runner-run-calls run)
            (mapcar (lambda (call-data)
                      (gptel-runner-store--restore-call run root call-data))
                    (plist-get data :calls))))
    (gptel-runner-store--notice-id (gptel-runner-run-id run))
    (dolist (call (gptel-runner-run-calls run))
      (gptel-runner-store--notice-id (gptel-runner-call-id call)))
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

(provide 'gptel-runner-store)
;;; gptel-runner-store.el ends here
