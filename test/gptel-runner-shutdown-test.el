;;; gptel-runner-shutdown-test.el --- Snapshot shutdown protection -*- lexical-binding: t; -*-

(require 'gptel-runner-dispatch-test)

(defmacro gptel-runner-shutdown-test--with-run (&rest body)
  "Run BODY with a private RUN whose durable progress is initially old."
  (declare (indent 0) (debug t))
  `(gptel-runner-dispatch-test--isolated
     (let ((gptel-runner-checkpoint-delay 3600)
           (driver (gptel-runner-fake-driver-create)))
       (gptel-runner-register-agent 'worker :preset 'p)
       (gptel-runner-defworkflow shutdown-save ()
         (gptel-runner-agent-step
          :id 'work :agent 'worker :prompt "work"))
       (gptel-runner-fake-queue driver 'worker '(:value "done"))
       (let ((run (gptel-runner-start 'shutdown-save :driver driver)))
         (gptel-runner-put run 'progress "old")
         (gptel-runner-schedule-save run t)
         (gptel-runner-store-flush-pending)
         (should (eq (gptel-runner-store-save-status run) 'clean))
         ,@body))))

(defun gptel-runner-shutdown-test--progress (run)
  "Read RUN's durable progress from its snapshot."
  (alist-get 'progress
             (plist-get (plist-get (gptel-runner-store--read-file
                                    (gptel-runner-run-snapshot-file run))
                                   :run)
                        :blackboard)))

(defun gptel-runner-shutdown-test--write-error (run)
  "Fail RUN's queued automatic writer deterministically."
  (let ((coordinator (gethash run gptel-runner-store--coordinators)))
    (gptel-runner-store--begin coordinator)
    (gptel-runner-store--cancel-timer coordinator)
    (cl-letf (((symbol-function 'gptel-runner-store--flush-parts)
               (lambda (_writer) (signal 'file-error '("Disk full")))))
      (gptel-runner-store--write-slice coordinator t)))
  (should (eq (gptel-runner-store-save-status run) 'error)))

(defun gptel-runner-shutdown-test--normal-exit ()
  "Try normal interactive shutdown, returning whether Emacs would exit.
Only runner query hooks are relevant here; never actually kill Emacs."
  (let ((noninteractive nil)
        (confirm-kill-emacs nil)
        (confirm-kill-processes nil)
        (save-some-buffers-functions nil)
        killed)
    (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) nil))
              ((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (ert-fail "Unexpected shutdown prompt")))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "Unexpected shutdown prompt")))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq killed t))))
      (save-buffers-kill-emacs t))
    killed))

(ert-deftest gptel-runner-shutdown-flush-retries-failed-automatic-save ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "new")
    (gptel-runner-shutdown-test--write-error run)
    (should (equal (gptel-runner-shutdown-test--progress run) "old"))
    ;; No new checkpoint is queued between the failed write and the retry.
    (gptel-runner-store-flush-pending)
    (should (eq (gptel-runner-store-save-status run) 'clean))
    (should (equal (gptel-runner-shutdown-test--progress run) "new"))
    (should-not (directory-files gptel-runner-snapshot-directory nil
                                "\\`\\.gptel-runner-"))))

(ert-deftest gptel-runner-shutdown-normal-exit-recovers-failed-save ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "new")
    (gptel-runner-shutdown-test--write-error run)
    (should (gptel-runner-shutdown-test--normal-exit))
    (should (eq (gptel-runner-store-save-status run) 'clean))
    (should (equal (gptel-runner-shutdown-test--progress run) "new"))))

(ert-deftest gptel-runner-shutdown-normal-exit-blocks-unresolved-save ()
  ;; Both a previously failed autosave and the first failure during exit
  ;; must veto shutdown, with one bounded attempt per exit request.
  (dolist (already-failed '(nil t))
    (gptel-runner-shutdown-test--with-run
      (gptel-runner-put run 'progress "new")
      (when already-failed (gptel-runner-shutdown-test--write-error run))
      (let ((attempts 0) messages)
        (cl-letf (((symbol-function 'gptel-runner-store--flush-parts)
                   (lambda (_writer)
                     (cl-incf attempts)
                     (signal 'file-error '("Disk full"))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (should-not (gptel-runner-shutdown-test--normal-exit)))
        (should (= attempts 1))
        (should (eq (gptel-runner-store-save-status run) 'error))
        (should (equal (gptel-runner-shutdown-test--progress run) "old"))
        (should (cl-some
                 (lambda (text)
                   (and (string-match-p "[Ee]xit.*cancel" text)
                        (string-search (gptel-runner-run-snapshot-file run) text)
                        (string-search "Disk full" text)))
                 messages)))
      ;; The user can repair the problem and repeat the same exit command.
      (should (gptel-runner-shutdown-test--normal-exit))
      (should (equal (gptel-runner-shutdown-test--progress run) "new")))))

(ert-deftest gptel-runner-shutdown-normal-exit-drains-pending-and-writing ()
  (dolist (writing '(nil t))
    (gptel-runner-shutdown-test--with-run
      (gptel-runner-put run 'progress "intermediate")
      (when writing
        (gptel-runner-store--begin
         (gethash run gptel-runner-store--coordinators)))
      (gptel-runner-put run 'progress "new")
      (should (gptel-runner-shutdown-test--normal-exit))
      (should (eq (gptel-runner-store-save-status run) 'clean))
      (should (equal (gptel-runner-shutdown-test--progress run) "new")))))

(ert-deftest gptel-runner-shutdown-normal-exit-does-not-rewrite-clean-saves ()
  (gptel-runner-shutdown-test--with-run
    (cl-letf (((symbol-function 'gptel-runner-store--begin)
               (lambda (_coordinator) (ert-fail "Rewrote a clean snapshot"))))
      (should (gptel-runner-shutdown-test--normal-exit))))
  (gptel-runner-dispatch-test--isolated
    (should (gptel-runner-shutdown-test--normal-exit))))

(ert-deftest gptel-runner-shutdown-normal-exit-checks-new-revisions-after-flush ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "intermediate")
    (let ((gptel-runner-event-hook
           (list (lambda (event)
                   (when (eq (gptel-runner-event-type event) 'snapshot-saved)
                     (gptel-runner-put run 'progress "new"))))))
      (should-not (gptel-runner-shutdown-test--normal-exit)))
    (should (eq (gptel-runner-store-save-status run) 'pending))
    (should (equal (gptel-runner-shutdown-test--progress run) "intermediate"))
    (should (gptel-runner-shutdown-test--normal-exit))
    (should (equal (gptel-runner-shutdown-test--progress run) "new"))))

(ert-deftest gptel-runner-shutdown-batch-query-does-not-prompt-or-veto ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "new")
    (gptel-runner-shutdown-test--write-error run)
    (let ((noninteractive t))
      (cl-letf (((symbol-function 'gptel-runner-store-flush-pending)
                 (lambda () (ert-fail "Batch query attempted to save")))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (ert-fail "Batch query prompted")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Batch query prompted"))))
        (should (run-hook-with-args-until-failure 'kill-emacs-query-functions))))))

(ert-deftest gptel-runner-shutdown-kill-hook-remains-best-effort ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "new")
    (gptel-runner-shutdown-test--write-error run)
    (dolist (noninteractive '(nil t))
      (cl-letf (((symbol-function 'gptel-runner-store--flush-parts)
                 (lambda (_writer) (signal 'file-error '("Disk full"))))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (ert-fail "Kill hook prompted")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Kill hook prompted"))))
        ;; Direct kill-emacs uses only this hook, even interactively.
        (run-hooks 'kill-emacs-hook)
        (should (eq (gptel-runner-store-save-status run) 'error))))
    (run-hooks 'kill-emacs-hook)
    (should (eq (gptel-runner-store-save-status run) 'clean))
    (should (equal (gptel-runner-shutdown-test--progress run) "new"))))

(ert-deftest gptel-runner-shutdown-cancel-detaches-on-temporary-cleanup-error ()
  (gptel-runner-shutdown-test--with-run
    (gptel-runner-put run 'progress "new")
    (let ((coordinator (gethash run gptel-runner-store--coordinators)))
      (gptel-runner-store--begin coordinator)
      (cl-letf (((symbol-function 'gptel-runner-store--delete-temporary)
                 (lambda (_writer) (signal 'file-error '("Cannot remove temp")))))
        (should-error (gptel-runner-store-cancel-save run) :type 'file-error))
      (should-not (gptel-runner-store--coordinator-timer coordinator))
      (should-not (gethash run gptel-runner-store--coordinators))
      ;; Even failed cleanup must relinquish ownership: a shutdown hook
      ;; cannot retry this run after its worker has handed it off.
      (gptel-runner-store-flush-pending)
      (should (equal (gptel-runner-shutdown-test--progress run) "old")))))

(provide 'gptel-runner-shutdown-test)
;;; gptel-runner-shutdown-test.el ends here
