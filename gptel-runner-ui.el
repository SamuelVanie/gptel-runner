;;; gptel-runner-ui.el --- Session dashboard for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A tabulated projection of runtime objects and their append-only journals.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'button)
(require 'gptel-runner-core)

(declare-function gptel-runner-save-run "gptel-runner-store")
(declare-function gptel-runner-load-run "gptel-runner-store")
(declare-function gptel-runner-list-snapshots "gptel-runner-store")
(declare-function gptel-runner-store-session-title "gptel-runner-store")
(defvar gptel-runner-snapshot-directory)
(declare-function gptel-runner-call-chat-buffer "gptel-runner-store")
(declare-function gptel-runner-node-buffer "gptel-runner-store")
(declare-function gptel-runner-child-buffer "gptel-runner-store")
(defvar gptel-runner-store--child-run)
(defvar gptel-runner-store--child-id)
(declare-function gptel-runner-resume-run "gptel-runner-flow")
(declare-function gptel-runner-continue "gptel-runner-flow")
(declare-function gptel-runner-extend "gptel-runner-flow")
(declare-function gptel-runner--exhausted-budget-kinds "gptel-runner-flow")
(declare-function gptel-runner-retry "gptel-runner-flow")
(declare-function gptel-runner-rerun "gptel-runner-flow")
(declare-function gptel-runner-complete-call-from-buffer
                  "gptel-runner-gptel")
(declare-function gptel-runner-gptel-feedback "gptel-runner-gptel")

(defconst gptel-runner-dashboard--column-specs
  '((item "Workflow / node" 38)
    (workflow "Workflow" 22)
    (run "Run" 16)
    (node "Node" 20)
    (job "Job" 24)
    (agent "Agent" 18)
    (call "Call" 16)
    (state "State" 28)
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
  '(item agent state elapsed)
  "Columns visible in the runner dashboard.
List order is ignored; columns always use the canonical dashboard order."
  :type '(set
          (const :tag "Workflow / node tree" item)
          (const :tag "Workflow" workflow)
          (const :tag "Run" run)
          (const :tag "Node" node)
          (const :tag "Background job" job)
          (const :tag "Agent" agent)
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

(defvar gptel-runner-dashboard-entry-functions nil
  "Functions returning additional dashboard entries.
External sources must use distinct row identifiers and must not register
observed runs in the local execution registry.")

(defvar gptel-runner-dashboard-row-action-functions nil
  "Functions handling actions on externally supplied dashboard rows.
Each receives ACTION and the row identifier.  Return non-nil when handled.
Supported actions are `open' and `menu'.")

(defvar-local gptel-runner-dashboard--refresh-timer nil
  "Timer responsible for refreshing the current dashboard buffer.")

(defvar-local gptel-runner-dashboard--timer-interval nil
  "Refresh interval used by the current dashboard timer.")

(defvar-local gptel-runner-dashboard--collapsed nil
  "Run row identities whose node rows are collapsed in this dashboard.")

(defun gptel-runner-ui--tree-label (row label)
  "Return a fold indicator and LABEL for run ROW."
  (concat (if (member row gptel-runner-dashboard--collapsed) "▸ " "▾ ") label))

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
     ((or 'waiting-tool 'waiting-confirmation 'waiting-feedback 'paused
          'retry-wait)
      'warning)
     ((or 'running 'ready) 'font-lock-keyword-face)
     (_ 'shadow))))

(defun gptel-runner-ui--call-state (call)
  "Return CALL's dashboard state, including an active tool name."
  (let ((state (gptel-runner-call-state call)))
    (if (eq state 'waiting-tool)
        (propertize
         (format "Calling %s..."
                 (if-let* ((names (gptel-runner-call-tool-names call)))
                     (mapconcat (lambda (name) (format "%s" name))
                                (ensure-list names) ", ")
                   "tool"))
         'face 'warning)
      (gptel-runner-ui--state state))))

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
    `((item . ,(propertize (format "%s" (or name "<anonymous>")) 'face 'bold))
      (workflow . ,(propertize
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
    `((item . ,(gptel-runner-ui--tree-label
                (list 'run (gptel-runner-run-id run))
                (gptel-runner-run-id run)))
      (run . ,(gptel-runner-run-id run))
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
      `((item . ,(format "    %s" (gptel-runner-node-id node)))
        (node . ,(format "%s" (gptel-runner-node-id node)))
        (agent . ,(if (gptel-runner--elisp-call-p call) "Elisp"
                    (when-let* ((agent (gptel-runner-call-agent call)))
                      (format "%s" (gptel-runner-agent-name agent)))))
        (call . ,(gptel-runner-call-id call))
        (state . ,(gptel-runner-ui--call-state call))
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

(defun gptel-runner-ui--local-entries ()
  "Return dashboard entries grouped as workflow, run, and call rows."
  (let* ((runs (gptel-runner-list-runs)) entries)
    (dolist (name (gptel-runner-ui--workflow-keys runs) entries)
      (let ((workflow-runs
             (cl-remove-if-not
              (lambda (run)
                (eq name (gptel-runner-ui--workflow-name run)))
              runs)))
        (when (or (memq 'workflow gptel-runner-dashboard-columns)
                  (memq 'item gptel-runner-dashboard-columns))
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

(defun gptel-runner-ui--entries ()
  "Return local dashboard entries followed by external source entries."
  (let (hidden)
    (cl-remove-if
     (lambda (entry)
       (pcase (car-safe (car entry))
         ((or 'run 'background-run)
          (setq hidden (member (car entry) gptel-runner-dashboard--collapsed)) nil)
         ('workflow (setq hidden nil))
         ((or 'call 'background-call) hidden)))
     (append (gptel-runner-ui--local-entries)
             (apply #'append
                    (mapcar #'funcall gptel-runner-dashboard-entry-functions))))))

(defun gptel-runner-dashboard-toggle-nodes ()
  "Expand or collapse the run at point, or the selected node's parent run."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (target (pcase row
                   ((or `(run ,_) `(background-run ,_)) row)
                   (`(background-call ,id . ,_) (list 'background-run id))
                   (`(call ,_)
                    (list 'run (gptel-runner-run-id (gptel-runner-ui--run-at-point)))))))
    (unless target (user-error "Select a run or node to fold"))
    (if (member target gptel-runner-dashboard--collapsed)
        (setq gptel-runner-dashboard--collapsed
              (remove target gptel-runner-dashboard--collapsed))
      (push target gptel-runner-dashboard--collapsed))
    (gptel-runner-dashboard-refresh)
    (goto-char (gptel-runner-dashboard--position-from-state
                (list :id target :line 1 :column 2)))))

(defun gptel-runner-dashboard-mouse-open (event)
  "Open the dashboard node clicked in mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (when (tabulated-list-get-id) (gptel-runner-dashboard-open-at-point)))

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

(defun gptel-runner-dashboard--position-state (position)
  "Capture dashboard location information at buffer POSITION."
  (save-excursion
    (goto-char position)
    (list :id (tabulated-list-get-id)
          :line (line-number-at-pos)
          :column (current-column))))

(defun gptel-runner-dashboard--position-from-state (state)
  "Return the buffer position described by dashboard location STATE."
  (save-excursion
    (let ((id (plist-get state :id))
          found)
      (when id
        (goto-char (point-min))
        (while (and (< (point) (point-max))
                    (not (equal id (tabulated-list-get-id))))
          (goto-char
           (next-single-property-change
            (point) 'tabulated-list-id nil (point-max))))
        (setq found (equal id (tabulated-list-get-id))))
      (unless found
        (goto-char (point-min))
        (forward-line (1- (plist-get state :line))))
      (move-to-column (plist-get state :column))
      (point))))

(defun gptel-runner-dashboard--window-states ()
  "Capture positions of windows currently displaying this dashboard."
  (mapcar
   (lambda (window)
     (list :window window
           :point (gptel-runner-dashboard--position-state
                   (window-point window))
           :start (gptel-runner-dashboard--position-state
                   (window-start window))
           :hscroll (window-hscroll window)
           :vscroll (window-vscroll window t)))
   (get-buffer-window-list (current-buffer) nil t)))

(defun gptel-runner-dashboard--restore-window-states (states)
  "Restore dashboard window positions captured in STATES."
  (dolist (state states)
    (let ((window (plist-get state :window)))
      (when (and (window-live-p window)
                 (eq (window-buffer window) (current-buffer)))
        (set-window-point
         window
         (gptel-runner-dashboard--position-from-state
          (plist-get state :point)))
        (set-window-start
         window
         (gptel-runner-dashboard--position-from-state
          (plist-get state :start))
         t)
        (set-window-hscroll window (plist-get state :hscroll))
        (set-window-vscroll window (plist-get state :vscroll) t)))))

(defun gptel-runner-dashboard--print ()
  "Print the dashboard without moving its buffer or window positions."
  (let ((point-state (gptel-runner-dashboard--position-state (point)))
        (window-states (gptel-runner-dashboard--window-states)))
    (tabulated-list-print t)
    (gptel-runner-dashboard--restore-window-states window-states)
    (goto-char (gptel-runner-dashboard--position-from-state point-state))))

(defun gptel-runner-dashboard--revert (&rest _ignored)
  "Refresh the dashboard in response to `revert-buffer'."
  (run-hooks 'tabulated-list-revert-hook)
  (gptel-runner-dashboard-refresh))

(define-derived-mode gptel-runner-dashboard-mode tabulated-list-mode
  "Runner-Dashboard"
  "Major mode for inspecting session-local gptel-runner state."
  (setq tabulated-list-format (gptel-runner-ui--format)
        tabulated-list-padding 2
        tabulated-list-entries #'gptel-runner-ui--entries)
  (setq-local truncate-lines t)
  (setq-local mode-line-process "  RET Open · TAB Fold · ? Actions · g Refresh")
  (setq-local revert-buffer-function #'gptel-runner-dashboard--revert)
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
  (gptel-runner-dashboard--print))

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
    (`(call ,_) (when-let* ((call (gptel-runner-ui--call-at-point)))
                  (gptel-runner-call-run call)))))

(defun gptel-runner-ui--run-context ()
  "Return (RUN . CALL) for the current node chat or dashboard row."
  (cond
   ((and (boundp 'gptel-runner--call)
         (gptel-runner-call-p gptel-runner--call))
    (cons (gptel-runner-call-run gptel-runner--call) gptel-runner--call))
   ((bound-and-true-p gptel-runner-store--child-run)
    (let* ((run gptel-runner-store--child-run)
           (child (gethash gptel-runner-store--child-id (gptel-runner-run-child-chats run))))
      (cons run (cl-find (plist-get child :call) (gptel-runner-run-calls run)
                         :key #'gptel-runner-call-id :test #'equal))))
   ((derived-mode-p 'gptel-runner-dashboard-mode)
    (let ((call (gptel-runner-ui--call-at-point)))
      (when-let* ((run (or (and call (gptel-runner-call-run call))
                          (gptel-runner-ui--run-at-point))))
        (cons run call))))))

(defun gptel-runner-ui--read-run ()
  "Read and return a session-local run for an interactive command."
  (let ((runs (gptel-runner-list-runs)))
    (unless runs
      (user-error "There are no gptel-runner runs in this session"))
    (if (= (length runs) 1)
        (car runs)
      (let* ((choices
              (mapcar (lambda (run)
                        (cons (gptel-runner-run-id run) run))
                      runs))
             (id (completing-read "Run: " choices nil t)))
        (cdr (assoc id choices))))))

(defun gptel-runner-add-decision (run text &optional rationale call)
  "Interactively record decision TEXT with optional RATIONALE for RUN.
From a runner worker, use the current call as provenance.  From the dashboard,
use the selected run or call row.  Elsewhere, prompt for a session-local run.
CALL may supply explicit provenance when this function is called from Lisp."
  (interactive
   (let* ((context (gptel-runner-ui--run-context))
          (run (or (car context) (gptel-runner-ui--read-run)))
          (text (read-string "Workflow decision: "))
          (rationale (read-string "Rationale (optional): ")))
     (list run text (unless (string-empty-p rationale) rationale)
           (cdr context))))
  (when (stringp run)
    (setq run (gptel-runner-find-run run)))
  (let ((entry (gptel-runner-record-decision run text rationale call)))
    (message "Recorded workflow decision %s"
             (plist-get entry :id))
    entry))

(defun gptel-runner-ui--workflow-name-at-point ()
  "Return the workflow name represented by the current dashboard row."
  (pcase (tabulated-list-get-id)
    (`(workflow ,name) name)
    (_ (when-let* ((run (gptel-runner-ui--run-at-point)))
         (gptel-runner-ui--workflow-name run)))))

(defun gptel-runner-ui--run-at-point-p ()
  "Return non-nil when point represents a dashboard run or call."
  (and (gptel-runner-ui--run-at-point) t))

(defun gptel-runner-ui--chat-available-p (call)
  "Whether CALL has a live or saved conversation, without opening it."
  (and call (not (gptel-runner--elisp-call-p call))
       (or (buffer-live-p (gptel-runner-call-buffer call))
           (gethash (gptel-runner-node-id (gptel-runner-call-node call))
                    (gptel-runner-run-chats (gptel-runner-call-run call))))))

(defun gptel-runner-ui--worker-at-point-p ()
  "Return non-nil when the call at point has a live or saved chat."
  (gptel-runner-ui--chat-available-p (gptel-runner-ui--call-at-point)))

(defun gptel-runner-ui--pausable-call-at-point-p ()
  "Return non-nil when the call at point can be paused for feedback."
  (let ((call (gptel-runner-ui--call-at-point)))
    (and call
         (not (gptel-runner--elisp-call-p call))
         (not (gptel-runner--call-terminal-p call))
         (not (memq (gptel-runner-call-state call)
                    '(paused waiting-feedback))))))

(defun gptel-runner-ui--completable-call-at-point-p ()
  "Return non-nil when the call at point can accept transcript feedback."
  (let ((call (gptel-runner-ui--call-at-point)))
    (and call
         (memq (gptel-runner-call-state call) '(paused waiting-feedback))
         (gptel-runner-ui--chat-available-p call))))

(defun gptel-runner-ui--abortable-call-at-point-p ()
  "Return non-nil when the call at point can be aborted."
  (let ((call (gptel-runner-ui--call-at-point)))
    (and call (not (gptel-runner--call-terminal-p call)))))

(defun gptel-runner-ui--persistable-run-at-point-p ()
  "Return non-nil when the run at point has a persistent workflow name."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (gptel-runner-workflow-name
              (gptel-runner-run-workflow run)))))

(defun gptel-runner-ui--pausable-run-at-point-p ()
  "Return non-nil when the run at point can be paused."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run
         (gptel-runner-workflow-name (gptel-runner-run-workflow run))
         (not (gptel-runner--run-terminal-p run))
         (not (eq (gptel-runner-run-state run) 'paused)))))

(defun gptel-runner-ui--resumable-run-at-point-p ()
  "Return non-nil when the run at point can be resumed."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (eq (gptel-runner-run-state run) 'paused))))

(defun gptel-runner-ui--retryable-run-at-point-p ()
  "Return non-nil when the run at point is in a retryable state."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (memq (gptel-runner-run-state run)
                   '(failed blocked stalled cancelled)))))

(defun gptel-runner-ui--retryable-node-at-point-p ()
  "Return non-nil when an unsuccessful run has a call selected at point."
  (and (gptel-runner-ui--call-at-point)
       (gptel-runner-ui--retryable-run-at-point-p)))

(defun gptel-runner-ui--continuable-run-at-point-p ()
  "Return non-nil when the run at point can be continued with a new goal."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (gptel-runner--run-terminal-p run))))

(defun gptel-runner-ui--extendable-run-at-point-p ()
  "Return non-nil when the run at point can have finite budgets extended."
  (let ((run (gptel-runner-ui--run-at-point)))
    (when run
      (let ((state (gptel-runner-run-state run))
            (budget (gptel-runner-run-budget run)))
        (if (eq state 'failed)
            (and (gptel-runner--exhausted-budget-kinds run) t)
          (and (memq state '(running paused))
               (or (gptel-runner-budget-max-requests budget)
                   (gptel-runner-budget-max-calls budget)
                   (gptel-runner-budget-max-duration budget))))))))

(defun gptel-runner-ui--abortable-run-at-point-p ()
  "Return non-nil when the run at point can be aborted."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (not (gptel-runner--run-terminal-p run)))))

(defun gptel-runner-ui--forgettable-run-at-point-p ()
  "Return non-nil when the run at point can be forgotten safely."
  (let ((run (gptel-runner-ui--run-at-point)))
    (and run (gptel-runner--forgettable-run-p run))))

(defun gptel-runner-ui--forgettable-workflow-at-point-p ()
  "Return non-nil when the workflow at point can be forgotten safely."
  (let ((name (gptel-runner-ui--workflow-name-at-point)))
    (and name
         (not
          (cl-find-if
           (lambda (run)
             (and (eq name (gptel-runner-workflow-name
                            (gptel-runner-run-workflow run)))
                  (not (gptel-runner--forgettable-run-p run))))
           (gptel-runner-list-runs))))))

(defun gptel-runner-ui--finished-runs-p ()
  "Return non-nil when the dashboard retains at least one terminal run."
  (cl-some #'gptel-runner--run-terminal-p (gptel-runner-list-runs)))

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

(defun gptel-runner-ui--insert-message (time type data node visit)
  "Insert the message in DATA for NODE at TIME, if TYPE is a message.
When NODE is nil, include every participant.  VISIT opens a node chat."
  (when-let* ((kind (pcase type
                      ('node-handoff "Handoff")
                      ('node-follow-up "Follow-up")
                      ('node-reply "Reply")))
              (from (plist-get data :from)))
    (let ((to (plist-get data :to)))
      (when (or (null node) (equal node from) (equal node to))
        (insert (propertize (format-time-string "%H:%M:%S" (seconds-to-time time))
                            'face 'shadow)
                "  " (propertize kind 'face 'bold) "  ")
        (cl-flet ((link (id)
                    (insert-text-button (format "%s" id) 'follow-link t
                                        'action (lambda (_) (funcall visit id)))))
          (link from)
          (insert " → ")
          (if to (link to) (insert "Workflow")))
        (insert "\n" (substring-no-properties (plist-get data :message)) "\n\n")))))

(defun gptel-runner-ui--show-messages (buffer-name node render)
  "Show messages for NODE in BUFFER-NAME, calling RENDER on refresh."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (special-mode)
      (setq-local header-line-format
                  (list " Node messages" (and node (format " · %s" node))
                        "    g Refresh"))
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (let ((position (point)) (inhibit-read-only t))
                      (erase-buffer)
                      (funcall render)
                      (when (= (point-min) (point-max))
                        (insert "No node messages yet.\n"))
                      (goto-char (min position (point-max))))))
      (revert-buffer)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun gptel-runner-dashboard-messages ()
  "Show workflow messages for the run or node at point."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point) (user-error "Select a run or node")))
         (call (gptel-runner-ui--call-at-point))
         (node (and call (gptel-runner-node-id (gptel-runner-call-node call))))
         (visit (lambda (id) (gptel-runner-visit-node-chat run id))))
    (gptel-runner-ui--show-messages
     (format "*gptel-runner events:%s*" (gptel-runner-run-id run)) node
     (lambda ()
       (dolist (event (gptel-runner-run-events run))
         (gptel-runner-ui--insert-message
          (gptel-runner-event-time event) (gptel-runner-event-type event)
          (gptel-runner-event-data event) node visit))))))

(defun gptel-runner-dashboard-open-at-point ()
  "Open the selected agent's chat or the run's event journal."
  (interactive)
  (unless (run-hook-with-args-until-success
           'gptel-runner-dashboard-row-action-functions
           'open (tabulated-list-get-id))
    (let ((call (gptel-runner-ui--call-at-point)))
      (if (and call (not (gptel-runner--elisp-call-p call)))
          (gptel-runner-dashboard-visit-worker)
        (gptel-runner-dashboard-inspect-events)))))

(defun gptel-runner-dashboard-visit-worker ()
  "Visit the live or saved node chat for the call at point."
  (interactive)
  (let ((call (or (gptel-runner-ui--call-at-point)
                  (user-error "Select an agent node"))))
    (pop-to-buffer (or (gptel-runner-call-chat-buffer call)
                       (user-error "No chat was retained for this node")))))

(defun gptel-runner-ui--read-child (children)
  "Select a transcript in CHILDREN by its node, parent, name and identifier."
  (unless children (user-error "No child transcripts for this selection"))
  (if (null (cdr children)) (car children)
    (let ((choices
           (mapcar (lambda (child)
                     (cons (format "%s%s · %s · %s [%s]"
                                   (plist-get child :node)
                                   (if-let* ((parent (plist-get child :parent-id)))
                                       (format " / %s" parent) "")
                                   (plist-get child :kind)
                                   (string-trim (plist-get child :name))
                                   (plist-get child :id))
                           child))
                   children)))
      (cdr (assoc (completing-read "Child transcript: " choices nil t) choices)))))

(defun gptel-runner-dashboard-child-transcripts ()
  "Open a child agent or discussion transcript for the selected node or run."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point) (user-error "Select a run or node")))
         (call (gptel-runner-ui--call-at-point))
         (node (and call (gptel-runner-node-id (gptel-runner-call-node call))))
         (child (gptel-runner-ui--read-child
                 (cl-loop for child being the hash-values of (gptel-runner-run-child-chats run)
                          when (or (null node) (equal node (plist-get child :node)))
                          collect child))))
    (pop-to-buffer (gptel-runner-child-buffer run (plist-get child :id)))))

;;;###autoload
(defun gptel-runner-visit-node-chat (run node)
  "Visit NODE's conversation in RUN.
From a node chat or dashboard row, use its run.  Elsewhere, read a run
from the local session.  NODE may be a node object or its ID."
  (interactive
   (let* ((context (gptel-runner-ui--run-context))
          (run (or (car context) (gptel-runner-ui--read-run)))
          (nodes (delete-dups
                  (cl-loop for call in (gptel-runner-run-calls run)
                           when (gptel-runner-ui--chat-available-p call)
                           collect (gptel-runner-call-node call))))
          (choices (mapcar (lambda (node)
                             (cons (format "%s" (gptel-runner-node-id node)) node))
                           nodes)))
     (unless choices (user-error "No agent has started a chat in this run"))
     (list run (if (cdr choices)
                   (cdr (assoc (completing-read "Node chat: " choices nil t) choices))
                 (cdar choices)))))
  (pop-to-buffer (or (gptel-runner-node-buffer run node)
                     (user-error "No chat was retained for this node"))))

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

(defun gptel-runner-dashboard-send-feedback ()
  "Send another message in the selected waiting node's conversation."
  (interactive)
  (unless (gptel-runner-ui--completable-call-at-point-p)
    (user-error "Select an agent waiting for feedback"))
  (let ((call (gptel-runner-ui--call-at-point)))
    (gptel-runner-gptel-feedback
     call (read-string (format "Feedback for %s: "
                               (gptel-runner-node-id (gptel-runner-call-node call)))))
    (revert-buffer)))

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

;;;###autoload
(defun gptel-runner-save-session ()
  "Save the current run and its chats without pausing execution.
From a node chat or the dashboard, use the corresponding run.  Elsewhere,
read a run from the current session."
  (interactive)
  (let* ((run (or (car (gptel-runner-ui--run-context))
                  (gptel-runner-ui--read-run)))
         (file (gptel-runner-save-run run)))
    (message "Saving run %s to %s" (gptel-runner-run-id run) file)))

(defalias 'gptel-runner-dashboard-save-run #'gptel-runner-save-session)

(defun gptel-runner-dashboard-resume-run ()
  "Resume the paused run at point with optional human feedback."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "No run on this row")))
         (feedback (read-string "Feedback for the next unfinished agent: ")))
    (gptel-runner-resume-run run (unless (string-empty-p feedback) feedback))
    (revert-buffer)))

(defun gptel-runner-ui--budget-additions (run)
  "Read and return finite budget additions for RUN as a keyword plist."
  (let* ((budget (gptel-runner-run-budget run))
         (requests
          (and (gptel-runner-budget-max-requests budget)
               (read-number "Additional requests (0 for none): " 0)))
         (calls
          (and (gptel-runner-budget-max-calls budget)
               (read-number "Additional calls (0 for none): " 0)))
         (duration
          (and (gptel-runner-budget-max-duration budget)
               (read-number "Additional active seconds (0 for none): " 0)))
         additions)
    (when (and requests (> requests 0))
      (setq additions (plist-put additions :additional-requests requests)))
    (when (and calls (> calls 0))
      (setq additions (plist-put additions :additional-calls calls)))
    (when (and duration (> duration 0))
      (setq additions (plist-put additions :additional-duration duration)))
    (unless additions
      (user-error "No finite budget was increased"))
    additions))

(defun gptel-runner-dashboard-continue-run ()
  "Continue the finished run at point with a new human observation."
  (interactive)
  (let* ((run (or (gptel-runner-ui--run-at-point)
                  (user-error "No run on this row")))
         (observation (read-string "Observation and new goal: "))
         (budget-action
          (completing-read
           "Budget for the new cycle: "
           '("keep remaining" "reset accounting" "increase limits")
           nil t nil nil "keep remaining"))
         (arguments
          (pcase budget-action
            ("reset accounting" '(:reset-budget t))
            ("increase limits" (gptel-runner-ui--budget-additions run))
            (_ nil))))
    (apply #'gptel-runner-continue run observation arguments)
    (revert-buffer)))

(defun gptel-runner-dashboard-retry-run ()
  "Retry the unsuccessful run at point without changing its goal."
  (interactive)
  (gptel-runner-retry
   (or (gptel-runner-ui--run-at-point)
       (user-error "No run on this row")))
  (revert-buffer))

(defun gptel-runner-dashboard-rerun ()
  "Rerun the finished run at point from its first step with the same goal."
  (interactive)
  (gptel-runner-rerun (or (gptel-runner-ui--run-at-point)
                        (user-error "No run on this row")))
  (revert-buffer))

(defun gptel-runner-dashboard-retry-node ()
  "Retry from the selected call's workflow node with optional feedback.
The node is rerun in the current workflow state, even for a historical call."
  (interactive)
  (let* ((call (or (gptel-runner-ui--call-at-point)
                   (user-error "No agent call on this row")))
         (feedback (read-string "What changed since this call? (optional): ")))
    (gptel-runner-retry
     (gptel-runner-call-run call)
     :from-node (gptel-runner-node-id (gptel-runner-call-node call))
     :feedback feedback)
    (revert-buffer)))

(defun gptel-runner-dashboard-extend-run ()
  "Increase finite budgets for the active, paused, or exhausted run at point."
  (interactive)
  (let ((run (or (gptel-runner-ui--run-at-point)
                 (user-error "No run on this row"))))
    (apply #'gptel-runner-extend run
           (gptel-runner-ui--budget-additions run))
    (revert-buffer)
    (message "Extended limits for %s" (gptel-runner-run-id run))))

(defun gptel-runner-ui--read-session (&optional browse)
  "Choose a saved run by its description or, with BROWSE, by filename."
  (require 'gptel-runner-store)
  (let* ((files (unless browse (gptel-runner-list-snapshots)))
         (choices
          (mapcar (lambda (file)
                    (let ((name (file-name-nondirectory file))
                          (title (gptel-runner-store-session-title file)))
                      (cons (if (equal name title) name (format "%s  [%s]" title name))
                            file)))
                  files))
         (picked (and choices
                      (completing-read
                       "Open run session: " (append choices '(("Choose another file…")))
                       nil t nil nil (caar choices)))))
    (or (cdr (assoc picked choices))
        (read-file-name "Session file: "
                        (when (file-directory-p gptel-runner-snapshot-directory)
                          gptel-runner-snapshot-directory)
                        nil t))))

;;;###autoload
(defun gptel-runner-open-session (file)
  "Open the run saved in FILE and select it in the dashboard.
Interactively choose from saved sessions.  A prefix argument opens the
file picker directly."
  (interactive (list (gptel-runner-ui--read-session current-prefix-arg)))
  (let ((run (or (cl-find-if
                  (lambda (candidate)
                    (when-let* ((saved (gptel-runner-run-snapshot-file candidate)))
                      (file-equal-p file saved)))
                  (gptel-runner-list-runs))
                 (gptel-runner-load-run file))))
    (if (derived-mode-p 'gptel-runner-dashboard-mode)
        (gptel-runner-dashboard-refresh)
      (gptel-runner-show-dashboard))
    (goto-char (gptel-runner-dashboard--position-from-state
                (list :id (list 'run (gptel-runner-run-id run))
                      :line 1 :column 2)))
    run))

(defalias 'gptel-runner-dashboard-load-snapshot #'gptel-runner-open-session)

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
      (gptel-runner-dashboard-refresh)
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
      (gptel-runner-dashboard-refresh)
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
      (gptel-runner-dashboard-refresh)
      (message "Forgot %d completed run%s"
               (length runs) (if (= (length runs) 1) "" "s")))))

(transient-define-prefix gptel-runner-dashboard-menu ()
  "Show actions for the selected dashboard row."
  [["Inspect"
    ("j" "Child transcripts" gptel-runner-dashboard-child-transcripts
     :inapt-if-not gptel-runner-ui--run-at-point-p)
    ("m" "Node messages" gptel-runner-dashboard-messages
     :inapt-if-not gptel-runner-ui--run-at-point-p)
    ("e" "Event journal" gptel-runner-dashboard-inspect-events
     :inapt-if-not gptel-runner-ui--run-at-point-p)
    ("v" "Open node chat" gptel-runner-dashboard-visit-worker
     :inapt-if-not gptel-runner-ui--worker-at-point-p)]
   ["Call"
    ("p" "Pause for feedback" gptel-runner-dashboard-pause-call
     :inapt-if-not gptel-runner-ui--pausable-call-at-point-p)
    ("f" "Send feedback" gptel-runner-dashboard-send-feedback
     :inapt-if-not gptel-runner-ui--completable-call-at-point-p)
    ("a" "Accept response" gptel-runner-dashboard-complete-call
     :inapt-if-not gptel-runner-ui--completable-call-at-point-p)
    ("T" "Retry from this node" gptel-runner-dashboard-retry-node
     :inapt-if-not gptel-runner-ui--retryable-node-at-point-p)
    ("k" "Abort call" gptel-runner-dashboard-abort-call
     :inapt-if-not gptel-runner-ui--abortable-call-at-point-p)]
   ["Run"
    ("P" "Pause and save" gptel-runner-dashboard-pause-run
     :inapt-if-not gptel-runner-ui--pausable-run-at-point-p)
    ("r" "Resume" gptel-runner-dashboard-resume-run
     :inapt-if-not gptel-runner-ui--resumable-run-at-point-p)
    ("t" "Retry" gptel-runner-dashboard-retry-run
     :inapt-if-not gptel-runner-ui--retryable-run-at-point-p)
    ("R" "Rerun with same goal" gptel-runner-dashboard-rerun
     :inapt-if-not gptel-runner-ui--continuable-run-at-point-p)
    ("b" "Extend limits" gptel-runner-dashboard-extend-run
     :inapt-if-not gptel-runner-ui--extendable-run-at-point-p)
    ("c" "Continue with new goal" gptel-runner-dashboard-continue-run
     :inapt-if-not gptel-runner-ui--continuable-run-at-point-p)
    ("K" "Abort run" gptel-runner-dashboard-abort-run
     :inapt-if-not gptel-runner-ui--abortable-run-at-point-p)]]
  [["Persistence"
    ("s" "Save run and chats" gptel-runner-dashboard-save-run
     :inapt-if-not gptel-runner-ui--persistable-run-at-point-p)
    ("l" "Open saved run" gptel-runner-dashboard-load-snapshot)]
   ["Manage"
    ("d" "Record decision" gptel-runner-add-decision
     :inapt-if-not gptel-runner-ui--run-at-point-p)
    ("V" "Toggle column" gptel-runner-dashboard-toggle-column)
    ("x" "Forget run" gptel-runner-dashboard-forget-run
     :inapt-if-not gptel-runner-ui--forgettable-run-at-point-p)
    ("X" "Forget workflow" gptel-runner-dashboard-forget-workflow
     :inapt-if-not gptel-runner-ui--forgettable-workflow-at-point-p)
    ("C" "Clear finished" gptel-runner-dashboard-clear-finished
     :inapt-if-not gptel-runner-ui--finished-runs-p)
    ("g" "Refresh" gptel-runner-dashboard-refresh)]])

(defun gptel-runner-ui--dispatch-menu (original &rest arguments)
  "Dispatch the row's external menu, or invoke ORIGINAL with ARGUMENTS."
  (unless (run-hook-with-args-until-success
           'gptel-runner-dashboard-row-action-functions
           'menu (tabulated-list-get-id))
    (apply original arguments)))

(advice-add 'gptel-runner-dashboard-menu :around
            #'gptel-runner-ui--dispatch-menu)

(let ((map gptel-runner-dashboard-mode-map))
  ;; Clear the historical direct bindings when this file is reevaluated.
  (dolist (key '("RET" "?" "a" "b" "c" "d" "e" "f" "k" "K" "l"
                 "p" "P" "r" "R" "s" "t" "v" "V" "x" "C" "D"))
    (define-key map (kbd key) nil))
  (define-key map (kbd "RET") #'gptel-runner-dashboard-open-at-point)
  (define-key map (kbd "TAB") #'gptel-runner-dashboard-toggle-nodes)
  (define-key map [mouse-1] #'gptel-runner-dashboard-mouse-open)
  (define-key map (kbd "?") #'gptel-runner-dashboard-menu)
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
