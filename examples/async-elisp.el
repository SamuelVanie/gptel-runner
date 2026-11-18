;;; async-elisp.el --- Native asynchronous local work -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file and start from a Git workspace, without a model connection:
;; (setq my-run (gptel-runner-start 'async-git-status
;;                                 :workspace "/path/to/project/"))
;; Once the run succeeds, read (gptel-runner-get my-run 'status).
;; Pause with `gptel-runner-pause-run'; resume starts a fresh subprocess.

;;; Code:

(require 'gptel-runner)

(defun gptel-runner-example-async-git-status (_run _node resolve reject)
  "Start git status, invoking RESOLVE with output or REJECT with error data.
Return a cleanup function that stops the subprocess and releases its buffer."
  (let ((buffer (generate-new-buffer " *runner-git-status*"))
        process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "runner-git-status" :buffer buffer :noquery t
                 :command '("git" "status" "--short")
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (let ((output (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (buffer-string))))
                           (status (process-exit-status process)))
                       (if (and (eq (process-status process) 'exit)
                                (zerop status))
                           (funcall resolve output)
                         (funcall reject
                                  (list :exit-status status :output output))))))))
          (lambda (_reason)
            (when (process-live-p process) (delete-process process))
            (when (buffer-live-p buffer) (kill-buffer buffer))))
      ;; A startup error occurs before the runner can receive our cleanup.
      (unless process
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(gptel-runner-defworkflow async-git-status (:max-calls 0 :max-requests 0)
  (gptel-runner-elisp-step
   :id 'status :async t :timeout 10 :save-as 'status
   :function #'gptel-runner-example-async-git-status))

(provide 'async-elisp)
;;; async-elisp.el ends here
