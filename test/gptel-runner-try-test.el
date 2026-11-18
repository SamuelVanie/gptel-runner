;;; gptel-runner-try-test.el --- Workflow recovery tests -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(defun gptel-runner-try-test--step (id function)
  "Return an Elisp step with ID that invokes FUNCTION."
  (gptel-runner-elisp-step :id id :function function))

(ert-deftest gptel-runner-try-recovers-and-finishes-in-order ()
  (gptel-runner-elisp-test--isolated
    (let ((attempts 0) order (completions 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               (gptel-runner-try
                :id 'attempt :error-as 'failure :save-as 'result
                :body (gptel-runner-elisp-step
                       :id 'body :retries 1
                       :function (lambda (_run _node)
                                   (cl-incf attempts) (error "operation failed")))
                :catch (gptel-runner-try-test--step
                        'handler
                        (lambda (run _node)
                          (push 'catch order)
                          (let ((failure (gptel-runner-get run 'failure)))
                            (should (eq (plist-get failure :node) 'body))
                            (should (eq (plist-get failure :state) 'failed))
                            (should (eq (plist-get (plist-get failure :value) :type)
                                        'elisp-error)))
                          'recovered))
                :finally (gptel-runner-try-test--step
                          'cleanup (lambda (_run _node) (push 'finally order) 'clean)))
               (gptel-runner-try-test--step
                'next (lambda (_run _node) (push 'next order))))
              :callback (lambda (_run) (cl-incf completions)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= attempts 2))
        (should (= completions 1))
        (should (equal (reverse order) '(catch finally next)))
        (should (eq (gptel-runner-get run 'result) 'recovered))
        (should (eq (gethash 'body (gptel-runner-run-node-states run)) 'failed))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-try-success-skips-catch-and-preserves-nil ()
  (gptel-runner-elisp-test--isolated
    (let ((run (gptel-runner-start
                (gptel-runner-try
                 :error-as 'failure :save-as 'result
                 :body (gptel-runner-try-test--step 'body #'ignore)
                 :catch (gptel-runner-try-test--step
                         'handler (lambda (_run _node) (ert-fail "Unexpected catch")))
                 :finally (gptel-runner-try-test--step
                           'cleanup (lambda (_run _node) 'cleanup-value)))
                :inputs '((failure . stale) (result . stale)))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should-not (gptel-runner-get run 'result 'missing))
      (should-not (gptel-runner-get run 'failure 'missing))
      (should (eq (gethash 'handler (gptel-runner-run-node-states run)) 'skipped)))))

(ert-deftest gptel-runner-try-propagates-handler-and-cleanup-failures ()
  (gptel-runner-elisp-test--isolated
    (dolist (failing '(body handler cleanup))
      (let (cleaned)
        (let ((run
               (gptel-runner-start
                (gptel-runner-try
                 :save-as 'result
                 :body (gptel-runner-try-test--step
                        'body (lambda (_run _node) (error "body")))
                 :catch (unless (eq failing 'body)
                          (gptel-runner-try-test--step
                           'handler (lambda (_run _node) (error "handler"))))
                 :finally (gptel-runner-try-test--step
                           'cleanup (lambda (_run _node)
                                      (setq cleaned t)
                                      (when (eq failing 'cleanup) (error "cleanup"))))))))
          (should cleaned)
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (equal (plist-get (gptel-runner-run-terminal-data run) :error)
                         (list 'error (symbol-name failing))))
          (should-not (gptel-runner-get run 'result)))))))

(ert-deftest gptel-runner-try-nests-and-catches-async-agent-failure-once ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) (handled 0) (cleaned 0))
      (gptel-runner-fake-queue driver 'worker '(:manual t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-try
                :save-as 'result
                :body (gptel-runner-try
                       :body (gptel-runner-agent-step :agent 'worker :prompt "work")
                       :catch (gptel-runner-try-test--step
                               'inner (lambda (_run _node) (error "handler failed")))
                       :finally (gptel-runner-try-test--step
                                 'cleanup (lambda (_run _node) (cl-incf cleaned))))
                :catch (gptel-runner-try-test--step
                        'outer (lambda (_run _node) (cl-incf handled) 'fallback)))
               :driver driver))
             (call (car (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-run-state run) 'running))
        (gptel-runner-fake-release
         driver call '(:status permanent :value "bad" :duplicate t))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (eq (gptel-runner-get run 'result) 'fallback))
        (should (= handled 1))
        (should (= cleaned 1))))))

(ert-deftest gptel-runner-try-catch-states-are-explicit ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (catch-states '(nil (blocked)))
      (let ((driver (gptel-runner-fake-driver-create)) caught cleaned)
        (gptel-runner-fake-queue driver 'worker '(:status blocked :value "blocked"))
        (let ((run (gptel-runner-start
                    (gptel-runner-try
                     :catch-states catch-states
                     :body (gptel-runner-agent-step :agent 'worker :prompt "work")
                     :catch (gptel-runner-try-test--step
                             'catch (lambda (_run _node) (setq caught t)))
                     :finally (gptel-runner-try-test--step
                               'finally (lambda (_run _node) (setq cleaned t))))
                    :driver driver)))
          (should cleaned)
          (should (eq (not (null caught)) (not (null catch-states))))
          (should (eq (gptel-runner-run-state run)
                      (if catch-states 'succeeded 'blocked))))))))

(ert-deftest gptel-runner-try-cancellation-never-launches-recovery ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (mode '(call run duration sibling))
      (let* ((driver (gptel-runner-fake-driver-create))
             (attempt
              (gptel-runner-try
               :body (gptel-runner-agent-step :agent 'worker :prompt "work")
               :catch (gptel-runner-try-test--step
                       'catch (lambda (_run _node) (ert-fail "Cancelled catch")))
               :finally (gptel-runner-try-test--step
                         'cleanup (lambda (_run _node) (ert-fail "Cancelled cleanup"))))))
        (gptel-runner-fake-queue driver 'worker '(:manual t))
        (let* ((run (gptel-runner-start
                     (if (eq mode 'sibling)
                         (gptel-runner-parallel
                          attempt (gptel-runner-try-test--step
                                   'sibling (lambda (_run _node) (error "sibling"))))
                       attempt)
                     :driver driver :max-concurrency 2))
               (call (car (gptel-runner-run-calls run))))
          (pcase mode
            ('call (gptel-runner-abort-call call))
            ('run (gptel-runner-abort-run run))
            ('duration (gptel-runner--duration-expired
                        run (gptel-runner-run-generation run))))
          (should (eq (gptel-runner-run-state run)
                      (if (memq mode '(duration sibling)) 'failed 'cancelled)))
          (should (= (length (gptel-runner-run-calls run))
                     (if (eq mode 'sibling) 2 1)))
          (should (zerop (gptel-runner-run-active-count run))))))))

(ert-deftest gptel-runner-try-recovers-exhausted-agent-budget-with-elisp ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((run (gptel-runner-start
                (gptel-runner-try
                 :error-as 'failure :save-as 'result
                 :body (gptel-runner-agent-step :agent 'worker :prompt "work")
                 :catch (gptel-runner-try-test--step
                         'handler (lambda (_run _node) 'fallback)))
                :driver (gptel-runner-fake-driver-create) :max-calls 0)))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (eq (gptel-runner-get run 'result) 'fallback))
      (should (eq (plist-get (plist-get (gptel-runner-get run 'failure) :value) :type)
                  'budget)))))

(ert-deftest gptel-runner-try-retry-resumes-the-failed-phase ()
  (gptel-runner-elisp-test--isolated
    (dolist (phase '(body catch finally))
      (let ((body-count 0) (catch-count 0) (cleanup-count 0) repaired)
        (let ((run
               (gptel-runner-start
                (gptel-runner-try
                 :body (gptel-runner-try-test--step
                        'body (lambda (_run _node)
                                (cl-incf body-count)
                                (unless (and repaired (eq phase 'body))
                                  (error "body")) 'body-ok))
                 :catch (unless (eq phase 'body)
                          (gptel-runner-try-test--step
                           'catch (lambda (_run _node)
                                    (cl-incf catch-count)
                                    (when (and (eq phase 'catch) (not repaired))
                                      (error "catch")) 'catch-ok)))
                 :finally (gptel-runner-try-test--step
                           'finally (lambda (_run _node)
                                      (cl-incf cleanup-count)
                                      (when (and (eq phase 'finally) (not repaired))
                                        (error "finally"))))))))
          (should (eq (gptel-runner-run-state run) 'failed))
          (setq repaired t)
          (gptel-runner-retry run)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= body-count (if (eq phase 'body) 2 1)))
          (should (= catch-count (pcase phase ('body 0) ('catch 2) (_ 1))))
          (should (= cleanup-count 2)))))))

(ert-deftest gptel-runner-try-snapshot-resumes-handler-or-cleanup ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (phase '(catch finally))
      (let ((body-count 0) (handler-count 0)
            (driver (gptel-runner-fake-driver-create)))
        (gptel-runner-fake-queue driver 'worker '(:manual t) '(:value "resumed"))
        (gptel-runner-defworkflow recovery ()
          (gptel-runner-try
           :id 'try :error-as 'failure :save-as 'result
           :body (gptel-runner-try-test--step
                  'body (lambda (_run _node) (cl-incf body-count) (error "body")))
           :catch (if (eq phase 'catch)
                      (gptel-runner-agent-step :id 'handler :agent 'worker :prompt "recover")
                    (gptel-runner-try-test--step
                     'handler (lambda (_run _node) (cl-incf handler-count) 'recovered)))
           :finally (if (eq phase 'finally)
                        (gptel-runner-agent-step :id 'cleanup :agent 'worker :prompt "clean")
                      (gptel-runner-try-test--step 'cleanup #'ignore))))
        (let ((run (gptel-runner-start 'recovery :driver driver)))
          (gptel-runner-pause-run run)
          (gptel-runner-store-flush-pending)
          (let ((file (gptel-runner-run-snapshot-file run)))
            (gptel-runner-forget-run run)
            (setq run (gptel-runner-load-run file nil driver)))
          (gptel-runner-resume-run run)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= body-count 1))
          (should (= handler-count (if (eq phase 'finally) 1 0)))
          (should (eq (gethash 'body (gptel-runner-run-node-states run)) 'failed))
          (should (equal (gptel-runner-get run 'result)
                         (if (eq phase 'finally) 'recovered "resumed"))))))))

(ert-deftest gptel-runner-try-repeat-clears-old-recovery ()
  (gptel-runner-elisp-test--isolated
    (let ((count 0) (handled 0) (cleaned 0))
      (let ((run (gptel-runner-start
                  (gptel-runner-repeat-until
                   :id 'repeat :max 2 :until (lambda (_run) (= count 2))
                   :collect-keys '(result) :save-history-as 'history
                   :body (gptel-runner-try
                          :save-as 'result :error-as 'failure
                          :body (gptel-runner-try-test--step
                                 'body (lambda (_run _node)
                                         (cl-incf count)
                                         (if (= count 1) (error "first") 'ok)))
                          :catch (gptel-runner-try-test--step
                                  'catch (lambda (_run _node) (cl-incf handled) 'fallback))
                          :finally (gptel-runner-try-test--step
                                    'cleanup (lambda (_run _node) (cl-incf cleaned))))))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= count 2))
        (should (= handled 1))
        (should (= cleaned 2))
        (should-not (gptel-runner-get run 'failure))
        (should (equal (gptel-runner-get run 'history)
                       '((:iteration 1 :values ((result . fallback)))
                         (:iteration 2 :values ((result . ok))))))))))

(ert-deftest gptel-runner-try-validates-shape-and-parallel-error-keys ()
  (gptel-runner-elisp-test--isolated
    (let ((body (gptel-runner-try-test--step 'body #'ignore))
          (handler (gptel-runner-try-test--step 'handler #'ignore)))
      (dolist (props (list (list :catch handler)
                          (list :body body)
                          (list :body body :catch #'ignore)
                          (list :body body :finally t)
                          (list :body body :catch handler :catch-states '(cancelled))
                          (list :body body :catch handler :catch-states 'failed)
                          (list :body body :catch handler :error-as 'x :save-as 'x)))
        (should-error (gptel-runner-start (apply #'gptel-runner-try props))
                      :type 'user-error))
      (should-error
       (gptel-runner-start
        (gptel-runner-parallel
         (gptel-runner-try :body body :catch handler :error-as 'shared)
         (gptel-runner-elisp-step :function #'ignore :save-as 'shared)))
       :type 'user-error))))

(ert-deftest gptel-runner-try-catches-flow-functions-after-async-completion ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (kind '(branch until stop progress))
      (let* ((driver (gptel-runner-fake-driver-create))
             (step (gptel-runner-agent-step :id 'agent :agent 'worker :prompt "work"))
             (bad (lambda (_run) (error "predicate")))
             (body (pcase kind
                     ('branch (gptel-runner-sequence
                               step (gptel-runner-branch
                                     :predicate bad
                                     :then (gptel-runner-try-test--step 'unused #'ignore))))
                     (_ (apply #'gptel-runner-repeat-until
                               :max 2 :body step
                               (pcase kind
                                 ('until (list :until
                                               (lambda (run)
                                                 (when (gptel-runner-run-calls run)
                                                   (funcall bad run)))))
                                 ('stop (list :stop-when bad))
                                 ('progress (list :progress-key bad))))))))
        (gptel-runner-fake-queue driver 'worker '(:manual t))
        (let ((run (gptel-runner-start
                    (gptel-runner-try
                     :body body :error-as 'failure
                     :catch (gptel-runner-try-test--step 'catch #'ignore))
                    :driver driver)))
          (gptel-runner-fake-release driver (car (gptel-runner-run-calls run))
                                     '(:value "done"))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (eq (plist-get (plist-get (gptel-runner-get run 'failure) :value)
                                :type) 'flow-error))
          (should (eq (gethash 'agent (gptel-runner-run-node-states run))
                      'succeeded)))))))

(ert-deftest gptel-runner-try-retry-from-node-invalidates-dependent-phases ()
  (gptel-runner-elisp-test--isolated
    (dolist (target '(body handler cleanup))
      (let ((body-count 0) (handler-count 0) (cleanup-count 0) repaired)
        (let ((run (gptel-runner-start
                    (gptel-runner-sequence
                     (gptel-runner-try
                      :save-as 'result
                      :body (gptel-runner-try-test--step
                             'body (lambda (_run _node)
                                     (cl-incf body-count) (error "body")))
                      :catch (gptel-runner-try-test--step
                              'handler (lambda (_run _node)
                                         (cl-incf handler-count) 'recovered))
                      :finally (gptel-runner-try-test--step
                                'cleanup (lambda (_run _node) (cl-incf cleanup-count))))
                     (gptel-runner-try-test--step
                      'next (lambda (_run _node) (unless repaired (error "next"))))))))
          (setq repaired t)
          (gptel-runner-retry run :from-node target)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= body-count (if (eq target 'body) 2 1)))
          (should (= handler-count (if (memq target '(body handler)) 2 1)))
          (should (= cleanup-count 2))
          (should (eq (gptel-runner-get run 'result) 'recovered)))))))

(ert-deftest gptel-runner-try-resume-preserves-a-handled-blocked-repeat ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:manual t) '(:value "recovered"))
      (gptel-runner-defworkflow blocked-recovery ()
        (gptel-runner-try
         :catch-states '(blocked)
         :body (gptel-runner-repeat-until
                :id 'repeat :max 1 :stop-when (lambda (_run) t)
                :body (gptel-runner-try-test--step 'step #'ignore))
         :catch (gptel-runner-agent-step :id 'handler :agent 'worker :prompt "recover")))
      (let ((run (gptel-runner-start 'blocked-recovery :driver driver)))
        (gptel-runner-pause-run run)
        (gptel-runner-resume-run run)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (eq (gethash 'repeat (gptel-runner-run-node-states run)) 'blocked))
        (should (eq (gethash 'step (gptel-runner-run-node-states run)) 'succeeded))
        (should (= (gptel-runner-iteration run 'repeat) 1))))))

(provide 'gptel-runner-try-test)
;;; gptel-runner-try-test.el ends here
