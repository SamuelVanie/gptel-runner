;;; gptel-runner-jobs-panel.el --- Background node controls -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A readable, live view of one invocation or node.  Approval buttons capture
;; exactly the displayed requests; refreshes never expand a submitted grant.

;;; Code:

(require 'button)
(require 'gptel-runner-jobs-ui)

(defvar-local gptel-runner-panel--timer nil)
(defvar-local gptel-runner-panel--receipts nil
  "Commands submitted from this panel, with their displayed interaction IDs.")
(defvar-local gptel-runner-panel--actions nil
  "Keyboard actions for the displayed node's approval snapshot.")

(defun gptel-runner-panel--location (position)
  "Describe POSITION relative to its stable section, if available."
  (let ((section (get-text-property position 'runner-section)))
    (list section
          (if section
              (- position (or (previous-single-property-change
                               (1+ position) 'runner-section) (point-min)))
            position))))

(defun gptel-runner-panel--position (location)
  "Resolve a saved LOCATION after rerendering this panel."
  (pcase-let ((`(,section ,offset) location))
    (let ((candidate (point-min)) start)
      (when section
        (while (and (< candidate (point-max)) (not start))
          (if (equal section (get-text-property candidate 'runner-section))
              (setq start candidate)
            (setq candidate (next-single-property-change
                             candidate 'runner-section nil (point-max))))))
      (if start
          (min (+ start offset) (1- (next-single-property-change
                                    start 'runner-section nil (point-max))))
        (min offset (point-max))))))

(defun gptel-runner-panel--button (label key function)
  "Insert a button with LABEL and stable KEY calling FUNCTION."
  (insert-text-button label 'follow-link t 'runner-action key
                      'action (lambda (_) (funcall function)))
  (insert "   "))

(defun gptel-runner-panel--target (run call)
  "Return the durable row identity for CALL in RUN."
  (if call
      (list 'background-call (plist-get run :id)
            (plist-get call :epoch) (plist-get call :id))
    (list 'background-run (plist-get run :id))))

(defun gptel-runner-panel--interactions (run call)
  "Return only RUN's current requests owned by CALL's exact epoch."
  (cl-remove-if-not
   (lambda (item)
     (and (equal (plist-get item :call-id) (plist-get call :id))
          (equal (plist-get item :epoch) (plist-get call :epoch))))
   (plist-get run :interactions)))

(defun gptel-runner-panel--submitted-p (id)
  "Whether displayed interaction ID already has a submitted decision."
  (cl-some (lambda (entry)
             (and (member id (plist-get entry :ids))
                  (not (eq (plist-get (plist-get entry :receipt) :status)
                           'rejected))))
           gptel-runner-panel--receipts))

(defun gptel-runner-panel--submit (run call interactions operation &optional outside)
  "Submit OPERATION for displayed INTERACTIONS on CALL in RUN.
OUTSIDE explicitly requests gptel-agent's conversation-scoped file grant."
  (let* ((ids (mapcar (lambda (item) (plist-get item :id)) interactions))
         (arguments (list :call-id (plist-get call :id)
                          :call-epoch (plist-get call :epoch)
                          :generation (if interactions
                                          (plist-get (car interactions) :generation)
                                        (plist-get call :generation)))))
    (when (cl-some #'gptel-runner-panel--submitted-p ids)
      (user-error "A decision for these requests has already been submitted"))
    (pcase operation
      ('approve-all
       (setq arguments (append arguments (list :interaction-ids ids
                                               :allow-outside (and outside t)))))
      ((or 'approve 'reject)
       (setq arguments (append arguments (list :interaction-id (car ids))))
       (when outside (setq arguments (append arguments '(:allow-outside t))))))
    (let ((receipt (gptel-runner-job-command run operation arguments)))
      (push (list :ids ids :node (plist-get call :node) :operation operation :receipt receipt)
            gptel-runner-panel--receipts)
      (gptel-runner-panel-refresh)
      (message "%s submitted; worker status appears in this panel"
               (if (eq operation 'revoke-outside) "Revocation" "Tool decision")))))

(defun gptel-runner-panel--command (run call operation)
  "Send a non-approval OPERATION for CALL in RUN and keep its receipt inline."
  (let* ((gptel-runner-background--target (gptel-runner-panel--target run call))
         (receipt (gptel-runner-background-command operation)))
    (push (list :node (plist-get call :node) :operation operation :receipt receipt)
          gptel-runner-panel--receipts)
    (gptel-runner-panel-refresh)))

(defun gptel-runner-panel--links (run call)
  "Insert inspection links and available controls for CALL in RUN."
  (let ((target (gptel-runner-panel--target run call)))
    (dolist (spec (append
                  (cond ((plist-get call :chat)
                         '(("Open node chat" . gptel-runner-background-transcript)))
                        ((plist-get call :transcript)
                         '(("Transcript" . gptel-runner-background-transcript))))
                  '(("Outputs" . gptel-runner-background-outputs)
                    ("Events" . gptel-runner-background-events)
                    ("More actions…" . gptel-runner-background-menu))))
      (let ((command (cdr spec)))
        (gptel-runner-panel--button
         (car spec) (list target command)
         (lambda ()
           (let ((gptel-runner-background--target target))
             (call-interactively command))))))
    (insert "\n\n"))
  (let ((state (plist-get (or call run) :state)))
    (dolist (spec
             (if call
                 (cond
                  ((memq state '(paused waiting-feedback))
                   '(("Send feedback" . feedback) ("Accept response" . accept)))
                  ((not (memq state gptel-runner-terminal-states))
                   '(("Pause for feedback" . pause-call) ("Cancel node" . cancel-call))))
               (cond
                ((eq state 'paused) '(("Resume run" . resume)))
                ((memq state '(failed blocked stalled cancelled)) '(("Retry run" . retry)))
                ((eq state 'succeeded) '(("Continue with a new goal" . continue)))
                ((memq state '(running ready waiting-confirmation waiting-tool waiting-feedback))
                 '(("Pause run" . pause) ("Cancel run" . cancel))))))
      (let ((operation (cdr spec)))
        (gptel-runner-panel--button
         (car spec) (list (plist-get call :id) operation)
         (lambda () (gptel-runner-panel--command run call operation)))))
    (insert "\n")))

(defun gptel-runner-panel--approval-buttons (run call interactions)
  "Insert group controls for the displayed INTERACTIONS of CALL in RUN."
  (let* ((pending (cl-remove-if (lambda (item)
                                (gptel-runner-panel--submitted-p (plist-get item :id)))
                              interactions))
         (outside (cl-some (lambda (item) (plist-get item :outside-access)) pending)))
    (when pending
      (let ((approve (lambda () (gptel-runner-panel--submit run call pending 'approve-all)))
            (allow (lambda () (gptel-runner-panel--submit run call pending 'approve-all t))))
        (gptel-runner-panel--button
         (format "Approve all %d pending" (length pending))
         (list (plist-get call :id) 'approve-all) approve)
        (when (equal gptel-runner-background--target (gptel-runner-panel--target run call))
          (push (cons 'approve approve) gptel-runner-panel--actions))
        (when outside
          (insert "\n")
          (gptel-runner-panel--button
           "Approve all + allow outside paths" (list (plist-get call :id) 'outside) allow)
          (when (equal gptel-runner-background--target (gptel-runner-panel--target run call))
            (push (cons 'outside allow) gptel-runner-panel--actions))
          (insert "\nAllow future outside file operations in the requesting conversations.\n"
                  "Ends on pause, completion, restart, directory change, or revocation.\n"
                  "Other tool confirmations and file reviews still apply.\n")))
      (insert "\n"))))

(defun gptel-runner-panel--request (run call interaction)
  "Insert a readable INTERACTION and its individual actions for CALL in RUN."
  (let ((id (plist-get interaction :id)) (start (point)))
    (insert (propertize
             (format "  %s%s\n" (or (plist-get interaction :source-buffer) "Agent")
                     (if (plist-get interaction :outside-access) " · outside directory" ""))
             'face 'bold))
    (dolist (tool (plist-get interaction :tools))
      (insert (propertize (format "  %s\n" (plist-get tool :name))
                          'face 'font-lock-function-name-face))
      (let ((args (plist-get tool :args)))
        (if (and (proper-list-p args) (cl-evenp (length args)))
            (cl-loop for (key value) on args by #'cddr do
                     (insert (format "    %s: %s\n" (string-remove-prefix ":" (format "%s" key))
                                     (if (stringp value) value (pp-to-string value)))))
          (insert (pp-to-string args)))))
    (if (gptel-runner-panel--submitted-p id)
        (insert (propertize "  Decision submitted — awaiting worker update\n" 'face 'shadow))
      (insert "  ")
      (gptel-runner-panel--button
       "Approve once" (list id 'approve)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'approve)))
      (when (plist-get interaction :outside-access)
        (gptel-runner-panel--button
         "Approve + allow outside paths" (list id 'outside)
         (lambda () (gptel-runner-panel--submit run call (list interaction) 'approve t))))
      (gptel-runner-panel--button
       "Reject & cancel node" (list id 'reject)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'reject)))
      (insert "\n"))
    (insert "\n")
    (put-text-property start (point) 'runner-section id)))

(defun gptel-runner-panel--call (run call)
  "Insert CALL's state and approval group in RUN."
  (let ((interactions (gptel-runner-panel--interactions run call)))
    (insert (propertize (format "%s" (plist-get call :node)) 'face 'bold)
            (format "  ·  %s  ·  " (or (plist-get call :agent) "Elisp"))
            (gptel-runner-ui--state (plist-get call :state))
            (format "  ·  %s\n" (gptel-runner-jobs-ui--elapsed call)))
    (when (plist-get call :outside-access)
      (insert (propertize "Outside access granted through this node.  " 'face 'warning))
      (gptel-runner-panel--button
       "Revoke outside access" (list (plist-get call :id) 'revoke)
       (lambda () (gptel-runner-panel--submit run call nil 'revoke-outside)))
      (insert "\n"))
    (insert "\n")
    (gptel-runner-panel--links run call)
    (if interactions
        (progn
          (insert (propertize (format "Needs your approval · %d requests\n\n" (length interactions))
                              'face 'warning))
          (gptel-runner-panel--approval-buttons run call interactions)
          (dolist (interaction interactions)
            (gptel-runner-panel--request run call interaction)))
      (insert (propertize "No pending tool approvals.\n\n" 'face 'shadow)))))

(defun gptel-runner-panel--receipts (run)
  "Update submitted command statuses from RUN and display them inline."
  (when gptel-runner-panel--receipts
    (insert (propertize "Recent actions\n" 'face 'bold))
    (dolist (entry gptel-runner-panel--receipts)
      (let* ((old (plist-get entry :receipt))
             (receipt (if (memq (plist-get old :status) '(applied rejected indeterminate))
                          old
                        (or (ignore-errors
                              (gptel-runner-job-command-status run (plist-get old :id))) old)))
             (status (or (plist-get receipt :status) 'queued)))
        (setf (plist-get entry :receipt) receipt)
        (insert (if-let* ((node (plist-get entry :node))) (format "%s · " node) "")
                (format "%s · " (plist-get entry :operation))
                (propertize (format "%s" status)
                            'face (pcase status ('applied 'success)
                                    ((or 'rejected 'indeterminate) 'error) (_ 'shadow)))
                (if-let* ((error (plist-get receipt :error))) (format " — %s" error) "")
                "\n")))
    (insert "\n")))

(defun gptel-runner-panel-refresh ()
  "Refresh this node panel without changing its selected target or focus."
  (interactive)
  (pcase-let* ((`(,run ,call ,_) (gptel-runner-jobs-ui--selection))
               (location (gptel-runner-panel--location (point)))
               (windows (mapcar (lambda (window)
                                  (cons window (gptel-runner-panel--location (window-start window))))
                                (get-buffer-window-list (current-buffer) nil t)))
               (button (button-at (point)))
               (key (and button (button-get button 'runner-action)))
               (inhibit-read-only t))
    (erase-buffer)
    (setq gptel-runner-panel--actions nil)
    (insert (propertize (format "%s\n" (or (plist-get (plist-get run :job) :name)
                                           (plist-get run :workflow)))
                        'face '(:inherit bold :height 1.25))
            (format "Run %s  ·  " (plist-get run :id))
            (gptel-runner-ui--state (plist-get run :state)) "\n\n")
    (gptel-runner-panel--receipts run)
    (if call
        (progn
          (gptel-runner-panel--button
           "All nodes" 'all-nodes
           (lambda () (gptel-runner-background-open (gptel-runner-panel--target run nil))))
          (insert "\n\n")
          (gptel-runner-panel--call run call)
          (when-let* ((record (gptel-runner-invocation-call run call)))
            (dolist (spec '((:workspace . "Workspace") (:prompt . "Task")
                            (:result . "Result") (:error . "Error")))
              (when-let* ((value (plist-get record (car spec))))
                (insert (propertize (concat (cdr spec) "\n") 'face 'bold)
                        (if (stringp value) value (pp-to-string value)) "\n\n")))))
      (gptel-runner-panel--links run nil)
      (dolist (item (plist-get run :calls))
        (let ((target (gptel-runner-panel--target run item)))
          (gptel-runner-panel--button
           (format "Open %s" (plist-get item :node)) target
           (lambda () (gptel-runner-background-visit target)))
          (insert (format " · %s · " (or (plist-get item :agent) "Elisp")))
          (insert (gptel-runner-ui--state (plist-get item :state)))
          (let ((count (length (gptel-runner-panel--interactions run item))))
            (when (> count 0)
              (insert (propertize (format " · %d approvals" count) 'face 'warning))))
          (insert "\n\n"))))
    (goto-char (gptel-runner-panel--position location))
    (when key
      (let ((candidate (point-min)) found)
        (while (and (< candidate (point-max)) (not found))
          (if (equal key (get-text-property candidate 'runner-action))
              (setq found candidate)
            (setq candidate (next-single-property-change
                             candidate 'runner-action nil (point-max)))))
        ;; A disappearing button must not transfer RET to a different action.
        (goto-char (or found (point-min)))))
    (dolist (entry windows)
      (when (window-live-p (car entry))
        (set-window-start (car entry) (gptel-runner-panel--position (cdr entry)) t)))))

(defun gptel-runner-panel-approve-all ()
  "Approve all pending requests displayed for this node, once."
  (interactive)
  (funcall (or (alist-get 'approve gptel-runner-panel--actions)
               (user-error "Open a node with pending approvals first"))))

(defun gptel-runner-panel-allow-outside ()
  "Approve displayed requests and remember their outside file access."
  (interactive)
  (funcall (or (alist-get 'outside gptel-runner-panel--actions)
               (user-error "No displayed request needs outside access"))))

(defun gptel-runner-panel-next-node (&optional previous)
  "Visit the next node in this invocation, or PREVIOUS when non-nil."
  (interactive)
  (pcase-let* ((`(,run ,call ,_) (gptel-runner-jobs-ui--selection))
               (calls (plist-get run :calls))
               (index (cl-position call calls :test #'equal))
               (next (and calls (nth (mod (+ (or index (if previous 0 -1))
                                             (if previous -1 1)) (length calls)) calls))))
    (unless next (user-error "This invocation has no nodes yet"))
    (gptel-runner-background-open (gptel-runner-panel--target run next))))

(defun gptel-runner-panel-previous-node ()
  "Visit the previous node in this invocation."
  (interactive)
  (gptel-runner-panel-next-node t))

(defun gptel-runner-panel--stop-timer ()
  "Stop the current panel's refresh timer."
  (when (timerp gptel-runner-panel--timer) (cancel-timer gptel-runner-panel--timer))
  (setq gptel-runner-panel--timer nil))

(defvar gptel-runner-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'gptel-runner-panel-refresh)
    (define-key map (kbd "a") #'gptel-runner-panel-approve-all)
    (define-key map (kbd "A") #'gptel-runner-panel-allow-outside)
    (define-key map (kbd "n") #'gptel-runner-panel-next-node)
    (define-key map (kbd "p") #'gptel-runner-panel-previous-node)
    (define-key map (kbd "?") #'gptel-runner-background-menu)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map))

(define-derived-mode gptel-runner-panel-mode special-mode "Runner-Node"
  "Inspect a background node and act on its displayed pending requests."
  (setq-local header-line-format
              "  n/p Nodes   TAB Actions   a Approve all shown   A Allow outside   g Refresh   q Back")
  (setq-local revert-buffer-function (lambda (&rest _) (gptel-runner-panel-refresh)))
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'gptel-runner-panel--stop-timer nil t)
  (add-hook 'change-major-mode-hook #'gptel-runner-panel--stop-timer nil t)
  (when-let* ((interval (gptel-runner-dashboard--configured-refresh-interval)))
    (let ((buffer (current-buffer)))
      (setq gptel-runner-panel--timer
            (run-with-timer
             interval interval
             (lambda ()
               (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
                 (with-current-buffer buffer
                   (condition-case err (gptel-runner-panel-refresh)
                     (error (setq header-line-format (error-message-string err))))))))))))

(defun gptel-runner-background-open (&optional target)
  "Open a live node panel for TARGET or the background row at point."
  (interactive)
  (let* ((target (or target (gptel-runner-jobs-ui--target)
                     (user-error "Select a background run or node")))
         (buffer (get-buffer-create (format "*gptel-runner node:%s*" (nth 1 target)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gptel-runner-panel-mode) (gptel-runner-panel-mode))
      (setq-local gptel-runner-background--target target)
      (gptel-runner-panel-refresh)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(provide 'gptel-runner-jobs-panel)
;;; gptel-runner-jobs-panel.el ends here
