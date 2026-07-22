;;; gptel-runner-ui.el --- Session dashboard for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A tabulated projection of runtime objects and their append-only journals.

;;; Code:

(require 'tabulated-list)
(require 'gptel-runner-core)

(declare-function gptel-runner-save-run "gptel-runner-store")
(declare-function gptel-runner-load-run "gptel-runner-store")
(declare-function gptel-runner-resume-run "gptel-runner-flow")
(declare-function gptel-runner-complete-call-from-buffer
                  "gptel-runner-gptel")

(defconst gptel-runner-dashboard--column-specs
  '((workflow "Workflow" 22)
    (run "Run" 16)
    (node "Node" 20)
    (call "Call" 16)
    (state "State" 20)
    (elapsed "Elapsed" 9)
    (attempts "Attempts" 9)
    (iteration "Iteration" 9)
    (requests "Requests left" 13)
    (calls "Calls left" 10))
  "Canonical dashboard column definitions and display order.")

(defvaralias 'gptel-runner-dashboards-column
  'gptel-runner-dashboard-columns
  "Alias for `gptel-runner-dashboard-columns'.")

(defcustom gptel-runner-dashboard-columns
  '(workflow run node state elapsed)
  "Columns visible in the runner dashboard.
List order is ignored; columns always use the canonical dashboard order."
  :type '(set
          (const :tag "Workflow" workflow)
          (const :tag "Run" run)
          (const :tag "Node" node)
          (const :tag "Call" call)
          (const :tag "State" state)
          (const :tag "Elapsed time" elapsed)
          (const :tag "Request attempts" attempts)
          (const :tag "Workflow iteration" iteration)
          (const :tag "Requests remaining" requests)
          (const :tag "Calls remaining" calls))
  :group 'gptel-runner)

(defcustom gptel-runner-dashboard-refresh-interval 2.0
  "Seconds between automatic dashboard refreshes.
Set this to nil to disable automatic refresh.  Changes take effect the next
time the dashboard refreshes or its mode is activated."
  :type '(choice
          (const :tag "Disable automatic refresh" nil)
          (number :tag "Refresh interval in seconds"))
  :group 'gptel-runner)

(defvar gptel-runner-dashboard-buffer "*gptel-runner*"
  "Name of the session dashboard buffer.")

(defvar-local gptel-runner-dashboard--refresh-timer nil
  "Timer responsible for refreshing the current dashboard buffer.")

(defvar-local gptel-runner-dashboard--timer-interval nil
  "Refresh interval used by the current dashboard timer.")

(defun gptel-runner-ui--selected-column-specs ()
  "Return selected column specifications in canonical display order."
  (let* ((known (mapcar #'car gptel-runner-dashboard--column-specs))
         (unknown (cl-set-difference gptel-runner-dashboard-columns known))
         (selected
          (cl-remove-if-not
           (lambda (spec) (memq (car spec) gptel-runner-dashboard-columns))
           gptel-runner-dashboard--column-specs)))
    (when unknown
      (user-error "Unknown dashboard column%s: %S"
                  (if (= (length unknown) 1) "" "s") unknown))
    (unless selected
      (user-error "Select at least one dashboard column"))
    selected))

(defun gptel-runner-ui--format ()
  "Build `tabulated-list-format' from the selected dashboard columns."
  (vconcat
   (mapcar (lambda (spec)
             (list (nth 1 spec) (nth 2 spec) nil))
           (gptel-runner-ui--selected-column-specs))))

(defun gptel-runner-ui--truncate-cell (value width)
  "Return VALUE constrained to display WIDTH, preserving its properties.
An elided value retains its complete unpropertized text as hover help."
  (let ((text (if (stringp value) value (format "%s" value))))
    (if (<= (string-width text) width)
        text
      (let ((short (truncate-string-to-width text width 0 nil "…")))
        (add-text-properties
         0 (length short)
         (list 'help-echo (substring-no-properties text)) short)
        short))))

(defun gptel-runner-ui--row-vector (values)
  "Build a dashboard row vector from column alist VALUES."
  (vconcat
   (mapcar (lambda (spec)
             (gptel-runner-ui--truncate-cell
              (or (alist-get (car spec) values) "") (nth 2 spec)))
           (gptel-runner-ui--selected-column-specs))))

(defun gptel-runner-ui--state (state)
  "Return STATE as a dashboard string with an appropriate face."
  (propertize
   (format "%s" state)
   'face
   (pcase state
     ('succeeded 'success)
     ((or 'failed 'blocked 'stalled 'cancelled) 'error)
     ((or 'waiting-confirmation 'waiting-feedback 'paused 'retry-wait)
      'warning)
     ((or 'running 'ready) 'font-lock-keyword-face)
     (_ 'shadow))))

(defun gptel-runner-ui--elapsed (run &optional call)
  "Return elapsed seconds for RUN or CALL as a compact string."
  (let* ((start (if call (gptel-runner-call-started-at call)
                  (gptel-runner-run-started-at run)))
         (finish (if call (gptel-runner-call-finished-at call)
                   (gptel-runner-run-finished-at run))))
    (if start (format "%.1fs" (- (or finish (float-time)) start)) "-")))

(defun gptel-runner-ui--remaining (run accessor maximum-accessor)
  "Format remaining RUN budget using ACCESSOR and MAXIMUM-ACCESSOR."
  (let* ((budget (gptel-runner-run-budget run))
         (maximum (funcall maximum-accessor budget)))
    (if maximum
        (number-to-string (max 0 (- maximum (funcall accessor budget))))
      "∞")))

(defun gptel-runner-ui--workflow-name (run)
  "Return the registered workflow name associated with RUN, or nil."
  (gptel-runner-workflow-name (gptel-runner-run-workflow run)))

(defun gptel-runner-ui--workflow-keys (run-list)
  "Build sorted workflow keys from RUN-LIST and the workflow registry."
  (let (keys)
    (maphash (lambda (name _workflow) (cl-pushnew name keys :test #'eq))
             gptel-runner--workflows)
    (dolist (run run-list)
      (cl-pushnew (gptel-runner-ui--workflow-name run) keys :test #'eq))
    (sort keys
          (lambda (a b)
            (string-lessp (if a (symbol-name a) "<anonymous>")
                          (if b (symbol-name b) "<anonymous>"))))))

(defun gptel-runner-ui--workflow-entry (name run-count)
  "Create a workflow NAME dashboard header containing RUN-COUNT."
  (list
   (list 'workflow name)
   (gptel-runner-ui--row-vector
    `((workflow . ,(propertize
                    (if name (symbol-name name) "<anonymous>")
                    'face 'font-lock-function-name-face))
      (state . ,(propertize
                 (format "%d run%s" run-count (if (= run-count 1) "" "s"))
                 'face 'shadow))))))

(defun gptel-runner-ui--run-entry (run)
  "Return the dashboard summary entry for RUN."
  (list
   (list 'run (gptel-runner-run-id run))
   (gptel-runner-ui--row-vector
    `((run . ,(gptel-runner-run-id run))
      (state . ,(gptel-runner-ui--state (gptel-runner-run-state run)))
      (elapsed . ,(gptel-runner-ui--elapsed run))
      (attempts . "-")
      (iteration . "-")
      (requests . ,(gptel-runner-ui--remaining
                    run #'gptel-runner-budget-requests
                    #'gptel-runner-budget-max-requests))
      (calls . ,(gptel-runner-ui--remaining
                 run #'gptel-runner-budget-calls
                 #'gptel-runner-budget-max-calls))))))

(defun gptel-runner-ui--call-entry (run call)
  "Return the dashboard detail entry for CALL belonging to RUN."
  (let ((node (gptel-runner-call-node call)))
    (list
     (list 'call (gptel-runner-call-id call))
     (gptel-runner-ui--row-vector
      `((node . ,(format "%s" (gptel-runner-node-id node)))
        (call . ,(gptel-runner-call-id call))
        (state . ,(gptel-runner-ui--state
                   (gptel-runner-call-state call)))
        (elapsed . ,(gptel-runner-ui--elapsed run call))
        (attempts . ,(number-to-string
                      (gptel-runner-call-request-attempt call)))
        (iteration . ,(number-to-string
                       (gptel-runner-iteration
                        run (gptel-runner-node-id node))))
        (requests . ,(gptel-runner-ui--remaining
                      run #'gptel-runner-budget-requests
                      #'gptel-runner-budget-max-requests))
        (calls . ,(gptel-runner-ui--remaining
                   run #'gptel-runner-budget-calls
                   #'gptel-runner-budget-max-calls)))))))

(defun gptel-runner-ui--entries ()
  "Return dashboard entries grouped as workflow, run, and call rows."
  (let* ((runs (gptel-runner-list-runs)) entries)
    (dolist (name (gptel-runner-ui--workflow-keys runs) entries)
      (let ((workflow-runs
             (cl-remove-if-not
              (lambda (run)
                (eq name (gptel-runner-ui--workflow-name run)))
              runs)))
        (when (memq 'workflow gptel-runner-dashboard-columns)
          (setq entries
                (nconc entries
                       (list (gptel-runner-ui--workflow-entry
                              name (length workflow-runs))))))
        (dolist (run workflow-runs)
          (setq entries
                (nconc entries
                       (list (gptel-runner-ui--run-entry run))
                       (mapcar (lambda (call)
                                 (gptel-runner-ui--call-entry run call))
                               (gptel-runner-run-calls run)))))))))

(defun gptel-runner-dashboard--configured-refresh-interval ()
  "Return the configured dashboard refresh interval after validation."
  (unless (or (null gptel-runner-dashboard-refresh-interval)
              (and (numberp gptel-runner-dashboard-refresh-interval)
                   (> gptel-runner-dashboard-refresh-interval 0)))
    (user-error
     "Dashboard refresh interval must be a positive number or nil: %S"
     gptel-runner-dashboard-refresh-interval))
  gptel-runner-dashboard-refresh-interval)

(defun gptel-runner-dashboard--cancel-refresh-timer ()
  "Cancel the automatic refresh timer for the current buffer."
  (when (timerp gptel-runner-dashboard--refresh-timer)
    (cancel-timer gptel-runner-dashboard--refresh-timer))
  (setq gptel-runner-dashboard--refresh-timer nil
        gptel-runner-dashboard--timer-interval nil))

(defun gptel-runner-dashboard--refresh-buffer (buffer)
  "Refresh dashboard BUFFER when it is still live and in dashboard mode."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'gptel-runner-dashboard-mode)
        (gptel-runner-dashboard-refresh)))))

(defun gptel-runner-dashboard--configure-refresh-timer ()
  "Make the current buffer's refresh timer match the configured interval."
  (let ((interval (gptel-runner-dashboard--configured-refresh-interval)))
    (unless (and (timerp gptel-runner-dashboard--refresh-timer)
                 (equal interval
                        gptel-runner-dashboard--timer-interval))
      (gptel-runner-dashboard--cancel-refresh-timer)
      (when interval
        (setq gptel-runner-dashboard--timer-interval interval
              gptel-runner-dashboard--refresh-timer
              (run-with-timer
               interval interval #'gptel-runner-dashboard--refresh-buffer
               (current-buffer)))))))

(define-derived-mode gptel-runner-dashboard-mode tabulated-list-mode
  "Runner-Dashboard"
  "Major mode for inspecting session-local gptel-runner state."
  (setq tabulated-list-format (gptel-runner-ui--format)
        tabulated-list-padding 2
        tabulated-list-entries #'gptel-runner-ui--entries)
  (setq-local truncate-lines t)
  (add-hook 'change-major-mode-hook
            #'gptel-runner-dashboard--cancel-refresh-timer nil t)
  (add-hook 'kill-buffer-hook
            #'gptel-runner-dashboard--cancel-refresh-timer nil t)
  (hl-line-mode 1)
  (tabulated-list-init-header)
  (gptel-runner-dashboard--configure-refresh-timer))

(defun gptel-runner-dashboard-refresh ()
  "Refresh dashboard data and apply the current column selection."
  (interactive)
  (gptel-runner-dashboard--configure-refresh-timer)
  (setq tabulated-list-format (gptel-runner-ui--format))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defun gptel-runner-dashboard-toggle-column (column)
  "Toggle visibility of dashboard COLUMN and refresh the table."
  (interactive
   (list
    (intern
     (completing-read
      "Toggle dashboard column: "
      (mapcar (lambda (spec) (symbol-name (car spec)))
              gptel-runner-dashboard--column-specs)
      nil t))))
  (unless (assq column gptel-runner-dashboard--column-specs)
    (user-error "Unknown dashboard column: %S" column))
  (if (memq column gptel-runner-dashboard-columns)
      (if (= (length gptel-runner-dashboard-columns) 1)
          (user-error "The dashboard must retain at least one column")
        (setq gptel-runner-dashboard-columns
              (delq column (copy-sequence
                            gptel-runner-dashboard-columns))))
    (push column gptel-runner-dashboard-columns))
  (gptel-runner-dashboard-refresh)
  (message "%s column %s"
           (if (memq column gptel-runner-dashboard-columns)
               "Showing" "Hiding")
           column))

(defun gptel-runner-ui--call-at-point ()
  "Return runner call represented by the current dashboard row."
  (pcase (tabulated-list-get-id)
    (`(call ,id)
     (cl-loop for run in (gptel-runner-list-runs)
              thereis (cl-find id (gptel-runner-run-calls run)
                               :key #'gptel-runner-call-id :test #'equal)))))

(defun gptel-runner-ui--run-at-point ()
  "Return runner run represented by the current dashboard row."
  (pcase (tabulated-list-get-id)
    (`(run ,id) (gethash id gptel-runner--runs))
    (`(call ,_) (when-let ((call (gptel-runner-ui--call-at-point)))
                  (gptel-runner-call-run call)))))

(defun gptel-runner-ui--workflow-name-at-point ()
  "Return the workflow name represented by the current dashboard row."
  (pcase (tabulated-list-get-id)
    (`(workflow ,name) name)
    (_ (when-let ((run (gptel-runner-ui--run-at-point)))
         (gptel-runner-ui--workflow-name run)))))

(defun gptel-runner-dashboard-inspect-events ()
  "Display the event journal for the run at point."
  (interactive)
  (let ((run (or (gptel-runner-ui--run-at-point)
                 (user-error "No run on this row"))))
    (with-current-buffer
        (get-buffer-create (format "*gptel-runner events:%s*"
                                   (gptel-runner-run-id run)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (dolist (event (gptel-runner-run-events run))
          (insert (format-time-string "%H:%M:%S.%3N"
                                      (seconds-to-time
                                       (gptel-runner-event-time event)))
                  (format "  %-28s node=%-18s call=%-12s %S\n"
                          (gptel-runner-event-type event)
                          (or (gptel-runner-event-node-id event) "-")
                          (or (gptel-runner-event-call-id event) "-")
                          (gptel-runner-event-data event)))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun gptel-runner-dashboard-visit-worker ()
  "Visit the retained or active worker transcript for the call at point."
  (interactive)
  (let* ((call (or (gptel-runner-ui--call-at-point)
                   (user-error "No call on this row")))
         (buffer (gptel-runner-call-buffer call)))
    (unless (buffer-live-p buffer)
      (user-error "Call has no retained worker transcript"))
    (pop-to-buffer buffer)))

(defun gptel-runner-dashboard-abort-call ()
  "Abort the unfinished call at point."
  (interactive)
  (gptel-runner-abort-call
   (or (gptel-runner-ui--call-at-point) (user-error "No call on this row")))
  (revert-buffer))

(defun gptel-runner-dashboard-abort-run ()
  "Abort the unfinished run at point."
  (interactive)
  (gptel-runner-abort-run
   (or (gptel-runner-ui--run-at-point) (user-error "No run on this row")))
  (revert-buffer))

(defun gptel-runner-dashboard-pause-call ()
  "Pause the active call at point and visit its feedback buffer."
  (interactive)
  (let ((call (or (gptel-runner-ui--call-at-point)
                  (user-error "No call on this row"))))
    (gptel-runner-pause-call call 'dashboard)
    (if (buffer-live-p (gptel-runner-call-buffer call))
        (pop-to-buffer (gptel-runner-call-buffer call))
      (revert-buffer))))

(defun gptel-runner-dashboard-complete-call ()
  "Complete the feedback call at point from its latest gptel response."
  (interactive)
  (gptel-runner-complete-call-from-buffer
   nil (or (gptel-runner-ui--call-at-point)
           (user-error "No call on this row")))
  (revert-buffer))

(defun gptel-runner-dashboard-pause-run ()
  "Pause and durably snapshot the run at point."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "No run on this row")))
         (file (progn (gptel-runner-pause-run run 'dashboard)
                      (gptel-runner-run-snapshot-file run))))
    (revert-buffer)
    (message "Paused %s; snapshot queued for %s"
             (gptel-runner-run-id run) file)))

(defun gptel-runner-dashboard-save-run ()
  "Save a snapshot of the run at point without pausing it."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "No run on this row")))
         (file (gptel-runner-save-run run)))
    (message "Snapshot queued for %s" file)))

(defun gptel-runner-dashboard-resume-run ()
  "Resume the paused run at point with optional human feedback."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "No run on this row")))
         (feedback (read-string "Feedback for the next unfinished agent: ")))
    (gptel-runner-resume-run run (unless (string-empty-p feedback) feedback))
    (revert-buffer)))

(defun gptel-runner-dashboard-load-snapshot (file)
  "Load paused run from snapshot FILE into the dashboard."
  (interactive "fSnapshot file: ")
  (gptel-runner-load-run file)
  (revert-buffer))

(defun gptel-runner-dashboard-forget-run (&optional delete-snapshot)
  "Forget the run at point and remove its retained buffers.
With prefix argument DELETE-SNAPSHOT, also delete its durable snapshot."
  (interactive "P")
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "Select a run or call row")))
         (id (gptel-runner-run-id run)))
    (when (yes-or-no-p
           (format "Forget run %s%s? " id
                   (if delete-snapshot " and delete its snapshot" "")))
      (gptel-runner-forget-run run delete-snapshot)
      (tabulated-list-print t)
      (message "Forgot run %s" id))))

(defun gptel-runner-dashboard-forget-workflow (&optional delete-snapshots)
  "Remove the workflow group at point and its retained run history.
With prefix argument DELETE-SNAPSHOTS, also delete their durable snapshots.
Active runs prevent the operation."
  (interactive "P")
  (let ((name (gptel-runner-ui--workflow-name-at-point)))
    (unless name
      (user-error "Anonymous workflows cannot be unregistered as a group"))
    (when (yes-or-no-p
           (format "Unregister workflow %S, forget its runs%s? "
                   name (if delete-snapshots ", and delete snapshots" "")))
      (gptel-runner-forget-workflow name delete-snapshots)
      (tabulated-list-print t)
      (message "Forgot workflow %S" name))))

(defun gptel-runner-dashboard-clear-finished (&optional delete-snapshots)
  "Remove all terminal run history currently retained by the dashboard.
With prefix argument DELETE-SNAPSHOTS, also delete their durable snapshots."
  (interactive "P")
  (let ((runs (cl-remove-if-not #'gptel-runner--run-terminal-p
                                (gptel-runner-list-runs))))
    (unless runs
      (user-error "There are no completed runs to clear"))
    (when (yes-or-no-p
           (format "Forget %d completed run%s%s? "
                   (length runs) (if (= (length runs) 1) "" "s")
                   (if delete-snapshots " and delete their snapshots" "")))
      (dolist (run runs)
        (gptel-runner-forget-run run delete-snapshots))
      (tabulated-list-print t)
      (message "Forgot %d completed run%s"
               (length runs) (if (= (length runs) 1) "" "s")))))

(let ((map gptel-runner-dashboard-mode-map))
  (define-key map (kbd "RET") #'gptel-runner-dashboard-inspect-events)
  (define-key map (kbd "v") #'gptel-runner-dashboard-visit-worker)
  (define-key map (kbd "p") #'gptel-runner-dashboard-pause-call)
  (define-key map (kbd "x") #'gptel-runner-dashboard-complete-call)
  (define-key map (kbd "P") #'gptel-runner-dashboard-pause-run)
  (define-key map (kbd "s") #'gptel-runner-dashboard-save-run)
  (define-key map (kbd "r") #'gptel-runner-dashboard-resume-run)
  (define-key map (kbd "l") #'gptel-runner-dashboard-load-snapshot)
  (define-key map (kbd "d") #'gptel-runner-dashboard-forget-run)
  (define-key map (kbd "D") #'gptel-runner-dashboard-forget-workflow)
  (define-key map (kbd "C") #'gptel-runner-dashboard-clear-finished)
  (define-key map (kbd "V") #'gptel-runner-dashboard-toggle-column)
  (define-key map (kbd "c") #'gptel-runner-dashboard-abort-call)
  (define-key map (kbd "a") #'gptel-runner-dashboard-abort-run)
  (define-key map (kbd "g") #'gptel-runner-dashboard-refresh))

;;;###autoload
(defun gptel-runner-show-dashboard ()
  "Show and refresh the session-local runner dashboard."
  (interactive)
  (let ((buffer (get-buffer-create gptel-runner-dashboard-buffer)))
    (with-current-buffer buffer
      (gptel-runner-dashboard-mode)
      (gptel-runner-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'gptel-runner-ui)
;;; gptel-runner-ui.el ends here
