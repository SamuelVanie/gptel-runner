;;; gptel-runner-startup-reconciliation-test.el --- Abandoned launches -*- lexical-binding: t; -*-

;;; Commentary:
;; Private stores and mocked launchctl, including a real dead launcher.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-runner-jobs)
(require 'gptel-runner-launchd)

(defmacro gptel-runner-startup-test--isolated (&rest body)
  "Run BODY with a private store and a service confirmed absent by default."
  (declare (indent 0) (debug t))
  `(let* ((gptel-runner-job-directory (make-temp-file "runner-startup-" t))
          (gptel-runner-launchd-directory
           (expand-file-name "agents" gptel-runner-job-directory))
          (gptel-runner-job-launch-function #'gptel-runner-jobs--launch)
          (system-type 'darwin)
          (job (list :id "startup-test" :workflow 'unused
                     :base-directory gptel-runner-job-directory))
          (probe-exit 113) probe-hook commands)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-runner-launchd--invoke)
                    (lambda (&rest args)
                      (push args commands)
                      (should (equal (car args) "print"))
                      (when probe-hook (funcall probe-hook))
                      (if (eq probe-exit 'error) (error "Probe unavailable")
                        (list :exit probe-exit :output "mock launchctl")))))
           ,@body)
       (delete-directory gptel-runner-job-directory t))))

(defun gptel-runner-startup-test--save (invocation)
  "Write INVOCATION and its reservation, returning INVOCATION."
  (gptel-runner-jobs--write
   (expand-file-name "owner.el"
                     (gptel-runner-jobs--job-lock (plist-get invocation :job-id)))
   (plist-get invocation :worker))
  (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation))

(defun gptel-runner-startup-test--dead-reservation (job)
  "Reserve JOB with a provably dead exact process identity."
  (let ((invocation (gptel-runner-jobs--new-invocation job 'test)))
    (gptel-runner-startup-test--save
     (plist-put invocation :worker
                (plist-put (plist-get invocation :worker) :start '(0 0 0 0))))))

(ert-deftest gptel-runner-startup-reconciles-real-dead-launcher-without-registration ()
  (gptel-runner-startup-test--isolated
    (let ((form `(let ((gptel-runner-job-directory ,gptel-runner-job-directory))
                   (gptel-runner-jobs--new-invocation ',job 'test 'launchd))))
      (should (zerop
               (call-process gptel-runner-job-emacs-program nil nil nil
                             "-Q" "--batch" "-L" gptel-runner-jobs--package-directory
                             "-l" "gptel-runner-jobs" "--eval" (prin1-to-string form)))))
    (let* ((invocation (car (gptel-runner-invocation-list)))
           (id (plist-get invocation :id))
           (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id))))
      (should (eq (gptel-runner-jobs--lock-owner-state
                   (plist-get invocation :worker)) 'dead))
      (should-not (gptel-runner-invocation-forgettable-p invocation))
      (should-not (file-exists-p (expand-file-name "worker.plist"
                                                  (plist-get invocation :directory))))
      (gptel-runner-launchd-reap)
      (setq invocation (gptel-runner-invocation-get id))
      (should (eq (plist-get invocation :phase) 'closed))
      (should (eq (plist-get invocation :state) 'startup-error))
      (should (eq (plist-get invocation :worker-state) 'exited))
      (should-not (file-exists-p lock))
      (should (gptel-runner-invocation-forgettable-p invocation))
      (should (gptel-runner-invocation-forgotten-p
               (gptel-runner-invocation-forget invocation)))
      (should (equal commands
                     (list (list "print" (gptel-runner-launchd--target
                                           (gptel-runner-launchd--label "worker" id 1))))))
      ;; A second reap is inert, and a delayed worker cannot adopt the old token.
      (setq commands nil)
      (gptel-runner-launchd-reap)
      (should-not commands)
      (should-error (gptel-runner-jobs-worker (plist-get invocation :directory))))))

(ert-deftest gptel-runner-startup-list-projects-dead-reservation-without-writing ()
  (gptel-runner-startup-test--isolated
    (let ((invocation (gptel-runner-startup-test--dead-reservation job)))
      (should (eq (plist-get (car (gptel-runner-invocation-list)) :state) 'interrupted))
      (should (eq (plist-get (gptel-runner-invocation-get
                             (plist-get invocation :id)) :state) 'starting))
      (should-not commands))))

(ert-deftest gptel-runner-startup-external-scheduler-reaps-only-direct-runs ()
  (gptel-runner-startup-test--isolated
    (cl-letf (((symbol-function 'gptel-runner-jobs--scheduler) #'ignore))
      (dolist (launcher '(direct custom))
        (let* ((definition (plist-put (copy-sequence job) :id (symbol-name launcher)))
               (run (gptel-runner-startup-test--dead-reservation definition)))
          (gptel-runner-startup-test--save (plist-put run :launcher launcher))
          (gptel-runner-jobs--scheduler-call 'reap)
          (should (eq (plist-get (gptel-runner-invocation-get (plist-get run :id)) :phase)
                      (if (eq launcher 'direct) 'closed 'starting)))))
      (should-not commands))))

(ert-deftest gptel-runner-startup-keeps-loaded-and-unverifiable-services ()
  (dolist (result '(0 5 error))
    (gptel-runner-startup-test--isolated
      (let ((invocation (gptel-runner-startup-test--dead-reservation job)))
        (setq probe-exit result)
        (gptel-runner-launchd-reap)
        (should commands)
        (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))
        (should-not (gptel-runner-invocation-forgettable-p invocation))
        (should-error (gptel-runner-invocation-forget invocation) :type 'user-error)))))

(ert-deftest gptel-runner-startup-keeps-live-and-unknown-launchers ()
  (dolist (kind '(live remote incomplete))
    (gptel-runner-startup-test--isolated
      (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
             (owner (plist-get invocation :worker)))
        (pcase kind
          ('remote (setq owner (plist-put owner :host "another-host")))
          ('incomplete (setq owner (plist-put owner :start nil))))
        (setq invocation (gptel-runner-startup-test--save (plist-put invocation :worker owner)))
        (gptel-runner-launchd-reap)
        (should-not commands)
        (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))
        (should-not (gptel-runner-invocation-forgettable-p invocation))))))

(ert-deftest gptel-runner-startup-reconciles-uncertain-without-registration-only-if-absent ()
  (gptel-runner-startup-test--isolated
    (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
           (receipt (gptel-runner-job-command invocation 'pause))
           (id (plist-get receipt :id)))
      (gptel-runner-jobs--launch-failed
       invocation '(gptel-runner-launchd-cleanup-error "Unconfirmed"))
      (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
      (dolist (result '(0 5 error))
        (setq probe-exit result)
        (gptel-runner-launchd-reap)
        (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))
        (should (equal receipt (gptel-runner-job-command-status invocation id))))
      (setq probe-exit 113)
      (gptel-runner-launchd-reap)
      (should (eq (plist-get (gptel-runner-invocation-get
                             (plist-get invocation :id)) :phase) 'closed))
      (should (eq (plist-get (gptel-runner-job-command-status invocation id) :status)
                  'rejected)))))

(ert-deftest gptel-runner-startup-rechecks-handoff-after-service-probe ()
  (dolist (change '(adopt epoch token))
    (gptel-runner-startup-test--isolated
      (let* ((invocation (gptel-runner-startup-test--dead-reservation job))
             (replacement (copy-tree invocation)))
        (pcase change
          ('adopt
           (setq replacement (plist-put replacement :phase 'open)
                 replacement (plist-put replacement :worker-state 'running)
                 replacement (plist-put replacement :worker
                                        (gptel-runner-jobs--owner
                                         (plist-get (plist-get invocation :worker) :token)
                                         (plist-get invocation :id)))))
          ('epoch (setq replacement (plist-put replacement :epoch 2)))
          ('token (setq replacement (plist-put replacement :worker
                                               (plist-put (plist-get replacement :worker)
                                                          :token "replacement")))))
        (setq probe-hook (lambda () (gptel-runner-startup-test--save replacement)))
        (gptel-runner-launchd-reap)
        (should commands)
        (should (equal replacement
                       (gptel-runner-invocation-get (plist-get invocation :id))))
        (should (equal (plist-get replacement :worker)
                       (gptel-runner-jobs--read
                        (expand-file-name "owner.el"
                                          (gptel-runner-jobs--job-lock
                                           (plist-get invocation :job-id))))))))))

(ert-deftest gptel-runner-startup-keeps-partially-adopted-or-unreadable-lock ()
  (dolist (kind '(live unknown unreadable))
    (gptel-runner-startup-test--isolated
      (let* ((invocation (gptel-runner-startup-test--dead-reservation job))
             (file (expand-file-name "owner.el"
                                     (gptel-runner-jobs--job-lock (plist-get invocation :job-id))))
             (owner (gptel-runner-jobs--owner
                     (plist-get (plist-get invocation :worker) :token)
                     (plist-get invocation :id))))
        (when (eq kind 'unknown) (setq owner (plist-put owner :start nil)))
        (if (eq kind 'unreadable)
            (with-temp-file file (insert "("))
          (gptel-runner-jobs--write file owner))
        (gptel-runner-launchd-reap)
        (should commands)
        (should (file-exists-p file))
        (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))))))

(ert-deftest gptel-runner-startup-preserves-successor-reservation ()
  (gptel-runner-startup-test--isolated
    (let* ((invocation (gptel-runner-startup-test--dead-reservation job))
           (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id)))
           (successor (gptel-runner-jobs--new-invocation job 'test)))
      (gptel-runner-launchd-reap)
      (should (eq (plist-get (gptel-runner-invocation-get
                             (plist-get invocation :id)) :phase) 'closed))
      (should (equal (plist-get successor :worker)
                     (gptel-runner-jobs--read (expand-file-name "owner.el" lock)))))))

(ert-deftest gptel-runner-startup-keeps-custom-launcher-reservations ()
  (gptel-runner-startup-test--isolated
    (let* ((gptel-runner-job-launch-function #'ignore)
           (invocation (gptel-runner-startup-test--dead-reservation job)))
      (gptel-runner-launchd-reap)
      (should-not commands)
      (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))
      (should-not (gptel-runner-invocation-forgettable-p invocation)))))

(ert-deftest gptel-runner-startup-legacy-needs-current-launchd-evidence ()
  (dolist (evidence '(none stale-plist plist registration))
    (gptel-runner-startup-test--isolated
      (let* ((invocation (gptel-runner-startup-test--dead-reservation job))
             (directory (plist-get invocation :directory))
             (label (gptel-runner-launchd--label "worker" (plist-get invocation :id)
                                                  (if (eq evidence 'stale-plist) 2 1))))
        (cl-remf invocation :launcher)
        (gptel-runner-startup-test--save invocation)
        (pcase evidence
          ((or 'plist 'stale-plist)
           (gptel-runner-launchd--write
            (expand-file-name "worker.plist" directory)
            (gptel-runner-launchd--plist (list (cons "Label" label)))))
          ('registration
           (gptel-runner-jobs--write
            (expand-file-name "worker-registration-1.el" directory)
            (list :epoch 1 :label label))))
        (gptel-runner-launchd-reap)
        (if (memq evidence '(plist registration))
            (progn
              (should commands)
              (should (eq (plist-get (gptel-runner-invocation-get
                                     (plist-get invocation :id)) :phase) 'closed))
              (should-not (file-exists-p
                           (expand-file-name "worker-registration-1.el" directory))))
          (should-not commands)
          (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id)))))))))

(ert-deftest gptel-runner-startup-restart-refreshes-launcher-evidence ()
  (gptel-runner-startup-test--isolated
    (let ((invocation (gptel-runner-startup-test--dead-reservation job)))
      (gptel-runner-launchd-reap)
      (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
      (gptel-runner-jobs--write
       (expand-file-name "checkpoint.el" (plist-get invocation :directory)) nil)
      (let ((gptel-runner-job-launch-function #'ignore))
        (gptel-runner-job-command invocation 'resume))
      (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
      (should (= (plist-get invocation :epoch) 2))
      (should (eq (plist-get invocation :state) 'starting))
      (should-not (plist-get invocation :error))
      ;; Simulate a second launcher dying before its custom worker adopts.
      (setq invocation
            (gptel-runner-startup-test--save
             (plist-put invocation :worker
                        (plist-put (plist-get invocation :worker) :start '(0 0 0 0)))))
      (setq commands nil)
      (gptel-runner-launchd-reap)
      (should-not commands)
      (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id)))))))

(ert-deftest gptel-runner-startup-delayed-worker-cannot-adopt-restarted-generation ()
  (gptel-runner-startup-test--isolated
    (let* ((invocation (gptel-runner-startup-test--dead-reservation job))
           (directory (plist-get invocation :directory)))
      (gptel-runner-launchd-reap)
      (gptel-runner-jobs--write (expand-file-name "checkpoint.el" directory) nil)
      (let ((gptel-runner-job-launch-function #'ignore))
        (gptel-runner-job-command invocation 'resume))
      (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
      (should (= (plist-get invocation :epoch) 2))
      ;; This old worker has not yet read any manifest.  It must not use the
      ;; new generation's token just because the directory is the same.
      (should (= 2 (call-process
                    gptel-runner-job-emacs-program nil nil nil
                    "-Q" "--batch" "-L" gptel-runner-jobs--package-directory
                    "-l" "gptel-runner-jobs" "-f" "gptel-runner-jobs-batch-main"
                    "--" "worker" directory "--epoch" "1")))
      (should (equal invocation (gptel-runner-invocation-get (plist-get invocation :id))))
      (should (equal (plist-get invocation :worker)
                     (gptel-runner-jobs--read
                      (expand-file-name "owner.el"
                                        (gptel-runner-jobs--job-lock
                                         (plist-get invocation :job-id)))))))))

(ert-deftest gptel-runner-startup-delayed-launch-error-after-successful-rerun ()
  (gptel-runner-startup-test--isolated
    (let* ((init (expand-file-name "workflow.el" gptel-runner-job-directory))
           (gptel-runner-job-preload-features nil)
           completed receipt
           (gptel-runner-job-launch-function
            (lambda (invocation)
              (let ((epoch (plist-get invocation :epoch)))
                (should (zerop
                         (call-process
                          gptel-runner-job-emacs-program nil nil nil
                          "-Q" "--batch" "-L" gptel-runner-jobs--package-directory
                          "-l" "gptel-runner-jobs" "-f" "gptel-runner-jobs-batch-main"
                          "--" "worker" (plist-get invocation :directory)
                          "--epoch" (number-to-string epoch))))
                (when (= epoch 1)
                  (setq receipt (gptel-runner-job-command invocation 'rerun)
                        completed (gptel-runner-invocation-get (plist-get invocation :id)))
                  (error "Delayed launch failure"))))))
      (with-temp-file init
        (prin1 '(gptel-runner-defworkflow example ()
                  (gptel-runner-elisp-step :id 'work :function #'ignore))
               (current-buffer)))
      (let ((job (gptel-runner-register-job
                  "example" :workflow 'example :init-file init
                  :base-directory gptel-runner-job-directory)))
        (should (equal (should-error (gptel-runner-job-run job))
                       '(error "Delayed launch failure"))))
      (should (= (plist-get completed :epoch) 2))
      (should (eq (plist-get completed :phase) 'closed))
      (should (eq (plist-get completed :state) 'succeeded))
      (should (equal completed (gptel-runner-invocation-get (plist-get completed :id))))
      (should (eq (plist-get (gptel-runner-job-command-status completed (plist-get receipt :id))
                             :status)
                  'applied)))))

(ert-deftest gptel-runner-startup-delayed-launch-error-rechecks-epoch-and-token ()
  (dolist (change '(epoch token))
    (dolist (failure '((error "Launch failed") (gptel-runner-launchd-cleanup-error "Unconfirmed")))
      (gptel-runner-startup-test--isolated
        (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
               (successor (copy-tree invocation))
               (owner-file (expand-file-name
                            "owner.el" (gptel-runner-jobs--job-lock (plist-get job :id))))
               (control (symbol-function 'gptel-runner-jobs--with-control-lock)))
          (if (eq change 'epoch)
              (setq successor (plist-put successor :epoch 2))
            (setf (plist-get (plist-get successor :worker) :token) "replacement"))
          ;; A controller wins the lock after failure notification but before
          ;; the old launcher can record the outcome.
          (cl-letf (((symbol-function 'gptel-runner-jobs--with-control-lock)
                     (lambda (record function)
                       (funcall control record
                                (lambda () (gptel-runner-startup-test--save successor)))
                       (funcall control record function))))
            (gptel-runner-jobs--launch-failed invocation failure))
          (should (equal successor (gptel-runner-invocation-get (plist-get invocation :id))))
          (should (equal (plist-get successor :worker) (gptel-runner-jobs--read owner-file))))))))

(ert-deftest gptel-runner-startup-matching-launch-error-preserves-manifest-metadata ()
  (gptel-runner-startup-test--isolated
    (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
           (fresh (gptel-runner-invocation-get (plist-get invocation :id))))
      (gptel-runner-startup-test--save (plist-put fresh :outputs '(report)))
      (gptel-runner-jobs--launch-failed invocation '(error "Launch failed"))
      (setq fresh (gptel-runner-invocation-get (plist-get invocation :id)))
      (should (eq (plist-get fresh :state) 'startup-error))
      (should (equal (plist-get fresh :error) "Launch failed"))
      (should (equal (plist-get fresh :outputs) '(report))))))

(ert-deftest gptel-runner-startup-failure-continues-after-receipt-io-error ()
  (gptel-runner-startup-test--isolated
    (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
           (receipt (gptel-runner-job-command invocation 'pause))
           (id (plist-get receipt :id))
           (file (gptel-runner-jobs--receipt-file invocation id))
           (write (symbol-function 'gptel-runner-jobs--write))
           interrupted)
      (with-temp-file (gptel-runner-jobs--receipt-file invocation "broken")
        (insert "("))
      (cl-letf (((symbol-function 'gptel-runner-jobs--write)
                 (lambda (path data)
                   (when (and (equal path file) (not interrupted))
                     (setq interrupted t)
                     (signal 'file-error '("Temporary receipt error")))
                   (funcall write path data))))
        (gptel-runner-jobs--launch-failed invocation '(error "Launch failed")))
      (should interrupted)
      (should (eq (plist-get (gptel-runner-job-command-status invocation id) :status)
                  'rejected))
      (let ((fresh (gptel-runner-invocation-get (plist-get invocation :id))))
        (should (eq (plist-get fresh :phase) 'closed))
        (should (string-match-p "broken\\.el" (plist-get fresh :error)))))))

(provide 'gptel-runner-startup-reconciliation-test)
;;; gptel-runner-startup-reconciliation-test.el ends here
