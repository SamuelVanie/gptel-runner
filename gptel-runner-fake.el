;;; gptel-runner-fake.el --- Deterministic fake driver for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A network-free scripted driver used by tests and workflow prototyping.

;;; Code:

(require 'cl-lib)
(require 'gptel-runner-core)

(cl-defstruct (gptel-runner-fake-driver
               (:constructor gptel-runner-fake-driver-create))
  "A scripted fake driver.
SCRIPTS maps agent names to outcome queues.  PENDING maps call IDs to manual
completion closures."
  (scripts (make-hash-table :test #'eq))
  (pending (make-hash-table :test #'equal))
  (timers (make-hash-table :test #'equal))
  starts (active 0) (max-active 0))

(defun gptel-runner-fake-queue (driver agent &rest outcomes)
  "Append OUTCOMES to DRIVER's script for AGENT and return DRIVER.
An outcome is a plist with `:status', `:value', `:metadata', `:delay',
`:manual', `:observations', and optional `:duplicate'."
  (let ((scripts (gptel-runner-fake-driver-scripts driver)))
    (puthash agent (nconc (gethash agent scripts) (copy-sequence outcomes))
             scripts))
  driver)

(defun gptel-runner-fake-release (driver call-or-id &optional outcome)
  "Release a manual CALL-OR-ID on DRIVER, optionally replacing its OUTCOME."
  (let* ((id (if (gptel-runner-call-p call-or-id)
                 (gptel-runner-call-id call-or-id)
               call-or-id))
         (pending (gethash id (gptel-runner-fake-driver-pending driver))))
    (unless pending (user-error "No manual fake outcome for %S" id))
    (remhash id (gptel-runner-fake-driver-pending driver))
    (funcall pending outcome)))

(defun gptel-runner-fake--next (driver call)
  "Pop DRIVER's next scripted outcome for CALL."
  (let* ((name (gptel-runner-agent-name (gptel-runner-call-agent call)))
         (scripts (gptel-runner-fake-driver-scripts driver))
         (queue (gethash name scripts)))
    (unless queue
      (error "No fake outcome queued for agent %S" name))
    (puthash name (cdr queue) scripts)
    (car queue)))

(cl-defmethod gptel-runner-driver-start
  ((driver gptel-runner-fake-driver) call complete observe)
  "Start scripted CALL with fake DRIVER."
  (let* ((raw (gptel-runner-fake--next driver call))
         (outcome (if (functionp raw) (funcall raw call) raw))
         (id (gptel-runner-call-id call))
         finished)
    (setf (gptel-runner-fake-driver-starts driver)
          (nconc (gptel-runner-fake-driver-starts driver) (list call)))
    (cl-incf (gptel-runner-fake-driver-active driver))
    (setf (gptel-runner-fake-driver-max-active driver)
          (max (gptel-runner-fake-driver-max-active driver)
               (gptel-runner-fake-driver-active driver)))
    (dolist (observation (plist-get outcome :observations))
      (funcall observe (car observation) (cdr observation)))
    (cl-labels
        ((deliver
          (&optional replacement)
          (let ((result (or replacement outcome)))
            (unless finished
              (setq finished t)
              (cl-decf (gptel-runner-fake-driver-active driver)))
            (funcall complete
                     (or (plist-get result :status) 'success)
                     (plist-get result :value)
                     (plist-get result :metadata))
            (when (plist-get result :duplicate)
              (funcall complete
                       (or (plist-get result :status) 'success)
                       (plist-get result :value)
                       (plist-get result :metadata))))))
      (cond
       ((plist-get outcome :manual)
        (puthash id #'deliver (gptel-runner-fake-driver-pending driver)))
       ((plist-get outcome :delay)
        (puthash id
                 (cons (run-at-time (plist-get outcome :delay) nil #'deliver)
                       (plist-get outcome :late))
                 (gptel-runner-fake-driver-timers driver)))
       (t (deliver))))))

(cl-defmethod gptel-runner-driver-cancel
  ((driver gptel-runner-fake-driver) call)
  "Cancel fake CALL timers in DRIVER unless modeling lateness."
  (let* ((id (gptel-runner-call-id call))
         (entry (gethash id (gptel-runner-fake-driver-timers driver)))
         (timer (car-safe entry)))
    (when (and (timerp timer) (not (cdr entry)))
      (cancel-timer timer)
      (remhash id (gptel-runner-fake-driver-timers driver))
      (when (> (gptel-runner-fake-driver-active driver) 0)
        (cl-decf (gptel-runner-fake-driver-active driver))))
    (when (gethash id (gptel-runner-fake-driver-pending driver))
      (remhash id (gptel-runner-fake-driver-pending driver))
      (when (> (gptel-runner-fake-driver-active driver) 0)
        (cl-decf (gptel-runner-fake-driver-active driver))))))

(provide 'gptel-runner-fake)
;;; gptel-runner-fake.el ends here
