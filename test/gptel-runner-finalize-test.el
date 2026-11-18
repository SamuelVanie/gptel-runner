;;; gptel-runner-finalize-test.el --- Bounded run finalization -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(defun gptel-runner-finalize-test--step (id function)
  "Return a local step with ID and FUNCTION."
  (gptel-runner-elisp-step :id id :function function))

(defun gptel-runner-finalize-test--agent (id)
  "Return a worker agent step with ID."
  (gptel-runner-agent-step :id id :agent 'worker :prompt "work" :save-as id))

(ert-deftest gptel-runner-finalize-preserves-body-outcomes-and-publishes-first ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (status '(success permanent blocked cancelled))
      (dolist (cleanup-fails '(nil t))
        (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
          (gptel-runner-fake-queue driver 'worker
                                   (list :status status :value "body" :duplicate t))
          (let* ((run
                  (gptel-runner-start
                   (gptel-runner-finalize
                    :id 'run :max-duration 30 :on-cancel 'finalize
                    :body (gptel-runner-finalize-test--agent 'body)
                    :finally
                    (gptel-runner-elisp-step
                     :id 'publish :save-as 'published
                     :function
                     (lambda (run _node)
                       (should (eq (plist-get (gptel-runner-finalization-outcome run)
                                             :phase) 'finalizing))
                       (when cleanup-fails (error "publication failed"))
                       (gptel-runner-node-outcome run 'body))))
                   :driver driver
                   :callback
                   (lambda (run)
                     (cl-incf callbacks)
                     (should (eq (plist-get (gptel-runner-finalization-outcome run)
                                           :phase) 'complete))
                     (unless cleanup-fails
                       (should (gptel-runner-get run 'published))))))
                 (record (gptel-runner-finalization-outcome run)))
            (should (= callbacks 1))
            (should (eq (gptel-runner-run-state run)
                        (pcase status ('success 'succeeded) ('blocked 'blocked)
                               ('cancelled 'cancelled) (_ 'failed))))
            (should (eq (plist-get (plist-get record :result) :state)
                        (if cleanup-fails 'failed 'succeeded)))
            (should (equal (gptel-runner-run-terminal-data run)
                           (plist-get (plist-get record :body) :value)))
            (should-not (gptel-runner-run-duration-timer run))))))))

(ert-deftest gptel-runner-finalize-uses-separate-budget-after-exhaustion ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (kind '(calls requests))
      (let ((driver (gptel-runner-fake-driver-create)))
        (gptel-runner-fake-queue driver 'worker '(:value "partial") '(:value "published"))
        (let* ((run (gptel-runner-start
                     (gptel-runner-finalize
                      :max-duration 30
                      :body (gptel-runner-sequence
                             (gptel-runner-finalize-test--agent 'first)
                             (gptel-runner-finalize-test--agent 'second))
                      :finally (gptel-runner-finalize-test--agent 'publication))
                     :driver driver :max-calls (if (eq kind 'calls) 1 5)
                     :max-requests 1))
               (record (gptel-runner-finalization-outcome run)))
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind) kind))
          (should (equal (gptel-runner-get run 'first) "partial"))
          (should (equal (gptel-runner-get run 'publication) "published"))
          (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 1))
          (should (= (gptel-runner-budget-calls (plist-get record :budget)) 1))
          (should (= (gptel-runner-budget-requests (plist-get record :budget)) 1)))))))

(ert-deftest gptel-runner-finalize-duration-cancels-body-and-ignores-late-results ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (gptel-runner-fake-queue driver 'worker '(:manual t) '(:manual t))
      (let* ((run (gptel-runner-start
                   (gptel-runner-finalize
                    :max-duration 30
                    :body (gptel-runner-try
                           :body (gptel-runner-finalize-test--agent 'body)
                           :finally (gptel-runner-finalize-test--step
                                     'try-cleanup (lambda (&rest _) (ert-fail "try cleanup"))))
                    :finally (gptel-runner-finalize-test--agent 'publication))
                   :driver driver :max-duration 10
                   :callback (lambda (_run) (cl-incf callbacks))))
             (body-call (car (gptel-runner-run-calls run)))
             (late (gethash (gptel-runner-call-id body-call)
                            (gptel-runner-fake-driver-pending driver)))
             (generation (gptel-runner-run-generation run)))
        (gptel-runner--duration-expired run generation)
        (should (eq (gptel-runner-call-state body-call) 'cancelled))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (zerop callbacks))
        (funcall late '(:value "stale" :duplicate t))
        (gptel-runner--duration-expired run generation)
        (should-not (gptel-runner-get run 'body))
        (should (= (length (gptel-runner-run-calls run)) 2))
        (gptel-runner-fake-release driver (cadr (gptel-runner-run-calls run))
                                   '(:value "partial report" :duplicate t))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind) 'duration))
        (should (= (gptel-runner-run-duration-remaining run) 0))
        (should (= callbacks 1))
        (should (equal (gptel-runner-get run 'publication) "partial report"))))))

(ert-deftest gptel-runner-finalize-cancellation-policy-and-second-abort ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (policy '(nil skip finalize))
      (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
        (gptel-runner-fake-queue driver 'worker '(:manual t) '(:manual t))
        (let ((run (gptel-runner-start
                    (gptel-runner-finalize
                     :max-duration 30 :on-cancel policy
                     :body (gptel-runner-finalize-test--agent 'body)
                     :finally (gptel-runner-finalize-test--agent 'publication))
                    :driver driver :callback (lambda (_run) (cl-incf callbacks)))))
          (gptel-runner-abort-run run 'user)
          (if (eq policy 'finalize)
              (progn
                (should (eq (gptel-runner-run-state run) 'running))
                (should (zerop callbacks))
                (let* ((call (cadr (gptel-runner-run-calls run)))
                       (late (gethash (gptel-runner-call-id call)
                                      (gptel-runner-fake-driver-pending driver))))
                  (gptel-runner-abort-run run 'hard-stop)
                  (funcall late '(:value "late publication")))
                (should (eq (plist-get (plist-get (gptel-runner-finalization-outcome run)
                                                 :result) :state) 'cancelled)))
            (should (= (length (gptel-runner-run-calls run)) 1))
            (should (eq (gptel-runner-node-state run 'publication) 'skipped)))
          (gptel-runner-abort-run run)
          (should (eq (gptel-runner-run-state run) 'cancelled))
          (should (eq (gptel-runner-run-terminal-data run) 'user))
          (should-not (gptel-runner-get run 'publication))
          (should (= callbacks 1)))))))

(ert-deftest gptel-runner-finalize-enforces-its-own-limits ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (kind '(calls requests duration))
      (let ((driver (gptel-runner-fake-driver-create)))
        (gptel-runner-fake-queue driver 'worker '(:manual t))
        (let* ((run (gptel-runner-start
                     (gptel-runner-finalize
                      :max-duration 30 :max-calls (if (eq kind 'calls) 0 1)
                      :max-requests (if (eq kind 'requests) 0 1)
                      :body (gptel-runner-finalize-test--step 'body #'ignore)
                      :finally (gptel-runner-finalize-test--agent 'publication))
                     :driver driver))
               (generation (gptel-runner-run-generation run)))
          (when (eq kind 'duration)
            (gptel-runner--duration-expired run generation))
          (let ((result (plist-get (gptel-runner-finalization-outcome run) :result)))
            (should (eq (plist-get result :state) 'failed))
            (should (eq (plist-get (plist-get result :value) :kind) kind)))
          (gptel-runner--duration-expired run generation)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should-not (gptel-runner-run-terminal-data run))
          (should (zerop (gptel-runner-run-active-count run)))
          (should-not (gptel-runner-run-duration-timer run)))))))

(ert-deftest gptel-runner-finalize-restores-phase-budget-and-failed-body ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) (bodies 0))
      (gptel-runner-fake-queue driver 'worker '(:manual t) '(:value "published"))
      (gptel-runner-defworkflow publication ()
        (gptel-runner-finalize
         :max-duration 30 :max-calls 2 :max-requests 2
         :body (gptel-runner-finalize-test--step
                'body (lambda (&rest _) (cl-incf bodies) (error "body failed")))
         :finally (gptel-runner-finalize-test--agent 'publication)))
      (let ((run (gptel-runner-start 'publication :driver driver :max-duration 60)))
        (gptel-runner-pause-run run)
        (gptel-runner-store-flush-pending)
        (let ((file (gptel-runner-run-snapshot-file run)))
          (gptel-runner-forget-run run)
          (setq run (gptel-runner-load-run file nil driver)))
        (should (= (gptel-runner-budget-calls (gptel-runner-run-budget run)) 1))
        (gptel-runner-resume-run run)
        (should (= bodies 1))
        (should (eq (gptel-runner-node-state run 'body) 'failed))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (equal (gptel-runner-get run 'publication) "published"))
        (should (= (gptel-runner-budget-max-duration (gptel-runner-run-budget run)) 60))
        (should (zerop (gptel-runner-budget-calls (gptel-runner-run-budget run))))
        (should (= (gptel-runner-budget-calls
                    (plist-get (gptel-runner-finalization-outcome run) :budget)) 2))))))

(ert-deftest gptel-runner-finalize-runs-again-on-retry-and-continuation ()
  (gptel-runner-elisp-test--isolated
    (let ((bodies 0) (publications 0))
      (let ((run (gptel-runner-start
                  (gptel-runner-finalize
                   :max-duration 30
                   :body (gptel-runner-finalize-test--step
                          'body (lambda (&rest _)
                                  (when (= (cl-incf bodies) 1) (error "first attempt"))
                                  'done))
                   :finally (gptel-runner-elisp-step
                             :id 'publication :save-as 'report
                             :function (lambda (&rest _) (cl-incf publications)))))))
        (should-error (gptel-runner-retry run :from-node 'publication) :type 'user-error)
        (gptel-runner-retry run)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= bodies 2))
        (should (= publications 2))
        (gptel-runner-continue run "Next cycle")
        (should (= bodies 3))
        (should (= publications 3))
        (should (= (gptel-runner-get run 'report) 3))))))

(ert-deftest gptel-runner-finalize-validates-bounds-location-and-capabilities ()
  (gptel-runner-elisp-test--isolated
    (let* ((body (gptel-runner-finalize-test--step 'body #'ignore))
           (cleanup (gptel-runner-finalize-test--step 'publication #'ignore))
           (valid (list :body body :finally cleanup :max-duration 30)))
      (dolist (bad '((:max-duration nil) (:max-duration 0) (:max-duration 1.0e+INF)
                     (:max-calls nil) (:max-calls -1) (:max-requests 0.5)
                     (:on-cancel always) (:body nil) (:finally ignore)))
        (should-error (gptel-runner-start
                       (apply #'gptel-runner-finalize (append bad valid)))
                      :type 'user-error))
      (should-error (gptel-runner-start
                     (gptel-runner-sequence (apply #'gptel-runner-finalize valid)))
                    :type 'user-error)
      (should-error (gptel-runner-start
                     (gptel-runner-finalize
                      :max-duration 30 :body body
                      :finally (gptel-runner-elisp-step
                                :function #'ignore :workspace-mode 'write)))
                    :type 'user-error))))

(ert-deftest gptel-runner-finalize-repeat-exhaustion-and-stall-keep-partials ()
  (gptel-runner-elisp-test--isolated
    (dolist (stalled '(nil t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-finalize
                :max-duration 30
                :body (gptel-runner-repeat-until
                       :id 'repeat :max 2 :progress-key (and stalled (lambda (_) 'same))
                       :collect-keys '(item) :save-history-as 'history
                       :body (gptel-runner-elisp-step
                              :id 'item :save-as 'item :function (lambda (&rest _) 'partial)))
                :finally (gptel-runner-elisp-step
                          :id 'publish :save-as 'report
                          :function (lambda (run _) (gptel-runner-get run 'history)))))))
        (should (eq (gptel-runner-run-state run) (if stalled 'stalled 'failed)))
        (should (= (length (gptel-runner-get run 'report)) 2))
        (should (equal (gptel-runner-get run 'report) (gptel-runner-get run 'history)))))))

(ert-deftest gptel-runner-finalize-expired-elisp-result-is-not-published ()
  (gptel-runner-elisp-test--isolated
    (let ((now 100.0) resolver cleaned (callbacks 0))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (let ((run
               (gptel-runner-start
                (gptel-runner-finalize
                 :max-duration 5
                 :body (gptel-runner-finalize-test--step 'body #'ignore)
                 :finally (gptel-runner-elisp-step
                           :id 'publish :async t :save-as 'report
                           :function (lambda (_run _node resolve _reject)
                                       (setq resolver resolve)
                                       (lambda (reason) (push reason cleaned)))))
                :callback (lambda (_) (cl-incf callbacks)))))
          (setq now 106.0)
          (funcall resolver 'too-late)
          (funcall resolver 'duplicate)
          (should (= callbacks 1))
          (should (equal cleaned '(cancelled)))
          (should-not (gptel-runner-get run 'report))
          (should (eq (plist-get (plist-get (plist-get
                                            (gptel-runner-finalization-outcome run)
                                            :result) :value) :kind) 'duration)))))))

(ert-deftest gptel-runner-finalize-resume-exhausted-body-dispatches-once ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:manual t) '(:manual t))
      (gptel-runner-defworkflow publication ()
        (gptel-runner-finalize
         :max-duration 30 :max-calls 3 :max-requests 3
         :body (gptel-runner-finalize-test--agent 'body)
         :finally (gptel-runner-finalize-test--agent 'publication)))
      (let ((run (gptel-runner-start 'publication :driver driver :max-duration 10)))
        (gptel-runner-pause-run run)
        (should-not (gptel-runner-finalization-outcome run))
        (setf (gptel-runner-run-duration-remaining run) 0)
        (gptel-runner-resume-run run)
        (should (= (length (gptel-runner-run-calls run)) 2))
        (should-error (gptel-runner-extend run :additional-duration 5) :type 'user-error)
        (gptel-runner-fake-release driver (cadr (gptel-runner-run-calls run))
                                   '(:value "partial"))
        (should (eq (gptel-runner-run-state run) 'failed))))))

(ert-deftest gptel-runner-finalize-extension-retries-body-with-new-finalizer-budget ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "partial")
                               '(:value "body") '(:value "complete"))
      (let ((run (gptel-runner-start
                  (gptel-runner-finalize
                   :max-duration 30
                   :body (gptel-runner-finalize-test--agent 'body)
                   :finally (gptel-runner-finalize-test--agent 'publication))
                  :driver driver :max-calls 0 :max-requests 0)))
        (should (equal (gptel-runner-get run 'publication) "partial"))
        (gptel-runner-extend run :additional-calls 1 :additional-requests 1)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'publication) "complete"))
        (should (= (gptel-runner-budget-calls (gptel-runner-run-budget run)) 1))
        (should (= (gptel-runner-budget-calls
                    (plist-get (gptel-runner-finalization-outcome run) :budget)) 1))))))

(ert-deftest gptel-runner-finalize-ignores-result-parsed-across-phase-change ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) run)
      (gptel-runner-fake-queue driver 'worker '(:manual t) '(:manual t))
      (setq run
            (gptel-runner-start
             (gptel-runner-finalize
              :max-duration 30
              :body (gptel-runner-agent-step
                     :id 'body :agent 'worker :prompt "body" :save-as 'body
                     :parser (lambda (value)
                               (gptel-runner--duration-expired
                                run (gptel-runner-run-generation run))
                               value))
              :finally (gptel-runner-finalize-test--agent 'publication))
             :driver driver :max-duration 10))
      (gptel-runner-fake-release driver (car (gptel-runner-run-calls run))
                                 '(:value "obsolete"))
      (should-not (gptel-runner-get run 'body))
      (gptel-runner-fake-release driver (cadr (gptel-runner-run-calls run))
                                 '(:value "partial"))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should (equal (gptel-runner-get run 'publication) "partial")))))

(ert-deftest gptel-runner-finalize-ignores-elisp-result-after-cleanup-cancels ()
  (gptel-runner-elisp-test--isolated
    (let (resolve)
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 30 :on-cancel 'finalize
               :body (gptel-runner-elisp-step
                      :id 'body :save-as 'body :async t
                      :function (lambda (run _node done _reject)
                                  (setq resolve done)
                                  (lambda (_reason) (gptel-runner-abort-run run))))
               :finally (gptel-runner-elisp-step
                         :id 'publication :save-as 'report
                         :function (lambda (&rest _) 'partial))))))
        (funcall resolve 'obsolete)
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should-not (gptel-runner-get run 'body))
        (should (eq (gptel-runner-get run 'report) 'partial))))))

(provide 'gptel-runner-finalize-test)
;;; gptel-runner-finalize-test.el ends here
