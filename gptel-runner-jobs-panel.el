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
      (cond
       (start (min (+ start offset) (1- (next-single-property-change
                                         start 'runner-section nil (point-max)))))
       (section (point-min))
       (t (min offset (point-max)))))))

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

(defun gptel-runner-panel--submitted-p (id)
  "Whether displayed interaction ID already has a submitted decision."
  (cl-some (lambda (entry)
             (and (member id (plist-get entry :ids))
                  (not (eq (plist-get (plist-get entry :receipt) :status)
                           'rejected))))
           gptel-runner-panel--receipts))

(defun gptel-runner-panel--submit (run call interactions operation &optional outside answers)
  "Submit OPERATION for displayed INTERACTIONS on CALL in RUN.
OUTSIDE requests a conversation-scoped file grant.  ANSWERS accompany `answer'."
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
      ('deny
       (setq arguments (append arguments (list :interaction-ids ids))))
      ((or 'approve 'reject 'answer 'cancel-questions)
       (setq arguments (append arguments (list :interaction-id (car ids))))
       (when answers (setq arguments (append arguments (list :answers answers))))
       (when outside (setq arguments (append arguments '(:allow-outside t))))))
    (let ((receipt (gptel-runner-job-command run operation arguments)))
      (push (list :ids ids :node (plist-get call :node) :operation operation :receipt receipt)
            gptel-runner-panel--receipts)
      (gptel-runner-panel-refresh)
      (message "Submitted %s to the worker" operation))))

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
                  (when (if call (plist-get call :children)
                          (cl-some (lambda (c) (plist-get c :children)) (plist-get run :calls)))
                    '(("Child transcripts" . gptel-runner-background-child-transcripts)))
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
                ((memq state '(failed blocked stalled cancelled))
                 '(("Retry run" . retry) ("Rerun with same goal" . rerun)))
                ((eq state 'succeeded)
                 '(("Continue with a new goal" . continue) ("Rerun with same goal" . rerun)))
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
            (deny (lambda () (gptel-runner-panel--submit run call pending 'deny)))
            (allow (lambda () (gptel-runner-panel--submit run call pending 'approve-all t))))
        (gptel-runner-panel--button
         (format "Approve all %d pending" (length pending))
         (list (plist-get call :id) 'approve-all) approve)
        (gptel-runner-panel--button
         (format "Deny all %d pending" (length pending))
         (list (plist-get call :id) 'deny-all) deny)
        (when (equal gptel-runner-background--target (gptel-runner-panel--target run call))
          (push (cons 'approve approve) gptel-runner-panel--actions)
          (push (cons 'deny deny) gptel-runner-panel--actions)
          (push (cons 'decide (lambda (requests operation)
                                (gptel-runner-panel--submit run call requests operation)))
                gptel-runner-panel--actions))
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
  (let* ((id (plist-get interaction :id))
         (submitted (gptel-runner-panel--submitted-p id))
         (start (point)))
    (gptel-runner-ui--insert-tool-request interaction)
    (if submitted
        (insert (propertize "  Decision submitted — awaiting worker update\n" 'face 'shadow))
      (insert "  ")
      (gptel-runner-panel--button
       "Approve once" (list id 'approve)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'approve)))
      (gptel-runner-panel--button
       "Deny once" (list id 'deny)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'deny)))
      (when (plist-get interaction :outside-access)
        (gptel-runner-panel--button
         "Approve + allow outside paths" (list id 'outside)
         (lambda () (gptel-runner-panel--submit run call (list interaction) 'approve t))))
      (gptel-runner-panel--button
       "Reject & cancel node" (list id 'reject)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'reject)))
      (insert "\n"))
    (insert "\n")
    (put-text-property start (point) 'runner-section id)
    (unless submitted
      (put-text-property start (point) 'runner-request interaction))))

(defun gptel-runner-panel--read-answer (question)
  "Read a suggested or free-text answer to QUESTION."
  (let* ((choices (mapcar (lambda (choice) (plist-get choice :value))
                          (plist-get question :choices)))
         (prompt (concat (plist-get question :question) " "))
         (answer (if choices (completing-read prompt choices) (read-string prompt))))
    (when (string-blank-p answer) (user-error "Supply an answer for each question"))
    (substring-no-properties answer)))

(defun gptel-runner-panel--questions (run call interaction)
  "Display INTERACTION's question batch from CALL in RUN."
  (let ((id (plist-get interaction :id))
        (questions (plist-get interaction :questions))
        (start (point)))
    (insert (propertize (format "Questions from %s\n" (plist-get interaction :source-buffer))
                        'face 'bold))
    (cl-loop for item in questions for number from 1 do
             (insert (format "  %d. %s\n" number (plist-get item :question)))
             (dolist (choice (plist-get item :choices))
               (insert "     " (plist-get choice :value)
                       (if (eq (plist-get choice :recommended) t) " (recommended)" "")
                       (if-let* ((detail (plist-get choice :description)))
                           (concat " — " detail) "")
                       "\n")))
    (if (gptel-runner-panel--submitted-p id)
        (insert (propertize "  Submitted — awaiting worker update\n" 'face 'shadow))
      (insert "  Choose a suggested answer or enter your own for each question.\n  ")
      (gptel-runner-panel--button
       "Answer questions" (list id 'answer)
       (lambda ()
         (gptel-runner-panel--submit run call (list interaction) 'answer nil
                                     (mapcar #'gptel-runner-panel--read-answer questions))))
      (gptel-runner-panel--button
       "Cancel questions" (list id 'cancel-questions)
       (lambda () (gptel-runner-panel--submit run call (list interaction) 'cancel-questions)))
      (insert "\n"))
    (insert "\n")
    (put-text-property start (point) 'runner-section id)))

(defun gptel-runner-panel--call (run call)
  "Insert CALL's state and pending interactions in RUN."
  (let* ((interactions (gptel-runner-jobs-ui--call-interactions run call))
         (approvals (cl-remove 'question interactions
                               :key (lambda (item) (plist-get item :kind)))))
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
    (dolist (item interactions)
      (when (eq (plist-get item :kind) 'question)
        (gptel-runner-panel--questions run call item)))
    (if approvals
        (progn
          (insert (propertize (format "Needs your approval · %d requests\n\n" (length approvals))
                              'face 'warning))
          (insert "Use y to approve or d to deny the request at point or the requests in a region.\n"
                  "Use a to approve all shown or D to deny all shown.\n\n")
          (gptel-runner-panel--approval-buttons run call approvals)
          (dolist (interaction approvals)
            (gptel-runner-panel--request run call interaction)))
      (unless interactions
        (insert (propertize "No pending questions or tool approvals.\n\n" 'face 'shadow))))))

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
    ;; An explicit refresh ends a selection made against the old rendering.
    (when (region-active-p) (deactivate-mark))
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
          (when-let* ((pending (gptel-runner-jobs-ui--call-interactions run item)))
            (insert " · " (gptel-runner-jobs-ui--interaction-summary pending)))
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

(defun gptel-runner-panel-deny-all ()
  "Deny all pending requests displayed for this node and let the agent continue."
  (interactive)
  (funcall (or (alist-get 'deny gptel-runner-panel--actions)
               (user-error "Open a node with pending approvals first"))))

(defun gptel-runner-panel--decide-selection (operation)
  "Send OPERATION for displayed requests at point or in the active region."
  (let* ((region (use-region-p))
         (start (if region (region-beginning) (point)))
         (end (if region (region-end) (min (1+ start) (point-max))))
         (submit (alist-get 'decide gptel-runner-panel--actions))
         requests)
    (while (< start end)
      (when-let* ((request (get-text-property start 'runner-request)))
        (push request requests))
      (setq start (next-single-property-change start 'runner-request nil end)))
    (unless (and submit requests)
      (user-error "Place point on a pending request or select requests with the region"))
    (setq requests (nreverse requests))
    (funcall submit requests
             (if (and (eq operation 'approve) (cdr requests)) 'approve-all operation))))

(defun gptel-runner-panel-approve-selection ()
  "Approve the displayed request at point or the requests in the active region."
  (interactive)
  (gptel-runner-panel--decide-selection 'approve))

(defun gptel-runner-panel-deny-selection ()
  "Deny the displayed request at point or in the active region and continue."
  (interactive)
  (gptel-runner-panel--decide-selection 'deny))

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
    (define-key map (kbd "y") #'gptel-runner-panel-approve-selection)
    (define-key map (kbd "d") #'gptel-runner-panel-deny-selection)
    (define-key map (kbd "D") #'gptel-runner-panel-deny-all)
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
                   ;; Keep the selected requests fixed while the user reviews a region.
                   (unless (region-active-p)
                     (condition-case err (gptel-runner-panel-refresh)
                       (error (setq header-line-format (error-message-string err)))))))))))))

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
