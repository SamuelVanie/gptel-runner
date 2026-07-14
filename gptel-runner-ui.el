;;; gptel-runner-ui.el --- Session dashboard for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A tabulated projection of runtime objects and their append-only journals.

;;; Code:

(require 'tabulated-list)
(require 'gptel-runner-core)

(defvar gptel-runner-dashboard-buffer "*gptel-runner*"
  "Name of the session dashboard buffer.")

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

(defun gptel-runner-ui--entries ()
  "Return a list of dashboard entries for session-local state."
  (apply
   #'append
   (mapcar
    (lambda (run)
      (let ((calls (gptel-runner-run-calls run)))
        (if calls
            (mapcar
             (lambda (call)
               (let ((node (gptel-runner-call-node call)))
                 (list (gptel-runner-call-id call)
                       (vector
                        (gptel-runner-run-id run)
                        (format "%s" (gptel-runner-node-id node))
                        (gptel-runner-call-id call)
                        (format "%s" (gptel-runner-call-state call))
                        (gptel-runner-ui--elapsed run call)
                        (number-to-string
                         (gptel-runner-call-request-attempt call))
                        (number-to-string
                         (gptel-runner-iteration run
                                                 (gptel-runner-node-id node)))
                        (gptel-runner-ui--remaining
                         run #'gptel-runner-budget-requests
                         #'gptel-runner-budget-max-requests)
                        (gptel-runner-ui--remaining
                         run #'gptel-runner-budget-calls
                         #'gptel-runner-budget-max-calls)))))
             calls)
          (list
           (list (gptel-runner-run-id run)
                 (vector (gptel-runner-run-id run) "-" "-"
                         (format "%s" (gptel-runner-run-state run))
                         (gptel-runner-ui--elapsed run) "0" "0"
                         (gptel-runner-ui--remaining
                          run #'gptel-runner-budget-requests
                          #'gptel-runner-budget-max-requests)
                         (gptel-runner-ui--remaining
                          run #'gptel-runner-budget-calls
                          #'gptel-runner-budget-max-calls)))))))
    (gptel-runner-list-runs))))

(define-derived-mode gptel-runner-dashboard-mode tabulated-list-mode
  "Runner-Dashboard"
  "Major mode for inspecting session-local gptel-runner state."
  (setq tabulated-list-format
        [("Run" 12 t) ("Node" 20 t) ("Call" 12 t) ("State" 20 t)
         ("Elapsed" 9 nil) ("Attempts" 9 nil) ("Iteration" 9 nil)
         ("Requests left" 13 nil) ("Calls left" 10 nil)])
  (setq tabulated-list-padding 2
        tabulated-list-entries #'gptel-runner-ui--entries)
  (tabulated-list-init-header))

(defun gptel-runner-ui--call-at-point ()
  "Return runner call represented by the current dashboard row."
  (let ((id (tabulated-list-get-id)) found)
    (dolist (run (gptel-runner-list-runs))
      (dolist (call (gptel-runner-run-calls run))
        (when (equal id (gptel-runner-call-id call)) (setq found call))))
    found))

(defun gptel-runner-ui--run-at-point ()
  "Return runner run represented by the current dashboard row."
  (or (and-let* ((call (gptel-runner-ui--call-at-point)))
        (gptel-runner-call-run call))
      (gethash (tabulated-list-get-id) gptel-runner--runs)))

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
  "Visit the live worker buffer for the call at point."
  (interactive)
  (let* ((call (or (gptel-runner-ui--call-at-point)
                   (user-error "No call on this row")))
         (buffer (gptel-runner-call-buffer call)))
    (unless (buffer-live-p buffer) (user-error "Call has no live worker"))
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

(let ((map gptel-runner-dashboard-mode-map))
  (define-key map (kbd "RET") #'gptel-runner-dashboard-inspect-events)
  (define-key map (kbd "v") #'gptel-runner-dashboard-visit-worker)
  (define-key map (kbd "c") #'gptel-runner-dashboard-abort-call)
  (define-key map (kbd "a") #'gptel-runner-dashboard-abort-run)
  (define-key map (kbd "g") #'revert-buffer))

;;;###autoload
(defun gptel-runner-show-dashboard ()
  "Show and refresh the session-local runner dashboard."
  (interactive)
  (let ((buffer (get-buffer-create gptel-runner-dashboard-buffer)))
    (with-current-buffer buffer
      (gptel-runner-dashboard-mode)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(provide 'gptel-runner-ui)
;;; gptel-runner-ui.el ends here
