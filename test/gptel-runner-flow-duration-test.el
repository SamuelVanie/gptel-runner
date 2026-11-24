;;; gptel-runner-flow-duration-test.el --- Synchronous flow budgets -*- lexical-binding: t; -*-

(require 'gptel-runner-dispatch-test)

(ert-deftest gptel-runner-flow-duration-rejects-invalid-limits-before-start ()
  (gptel-runner-dispatch-test--isolated
    (let* ((workflow (gptel-runner-workflow-create
                      :options '(:max-duration -1)
                      :root (gptel-runner-sequence)))
           (events nil)
           (gptel-runner-event-hook
            (list (lambda (event) (push (gptel-runner-event-type event) events)))))
      (dolist (overrides '(nil (:max-duration 0) (:max-duration "1")
                           (:max-duration 1.0e+INF) (:max-duration 0.0e+NaN)))
        (ert-info ((format "Starting with overrides %S" overrides))
          (should-error (apply #'gptel-runner-start workflow overrides)
                        :type 'user-error)
          (should-not events)
          (should-not (gptel-runner-list-runs))))
      (should (eq (gptel-runner-run-state
                   (gptel-runner-start workflow :max-duration nil))
                  'succeeded)))))

(defun gptel-runner-flow-duration-test--return (outcome)
  "Return OUTCOME, or signal it when it is error or quit."
  (pcase outcome
    ('error (error "Flow function failed"))
    ('quit (signal 'quit nil))
    (_ outcome)))

(defun gptel-runner-flow-duration-test--node (site function body)
  "Return a flow invoking FUNCTION at SITE around BODY."
  (if (eq site 'branch)
      (gptel-runner-branch :id 'flow :predicate function :then body)
    (apply #'gptel-runner-repeat-until
           :id 'flow :max 2 :body body
           :collect-keys '(result) :save-history-as 'history
           (pcase site
             ('entry (list :until function))
             ('until (list :until (lambda (run)
                                   (when (> (gptel-runner-iteration run 'flow) 0)
                                     (funcall function run)))))
             ('stop (list :stop-when function))
             ('progress (list :progress-key function))))))

(defun gptel-runner-flow-duration-test--expired (site outcome elapsed finalizer)
  "Check OUTCOME at SITE after ELAPSED seconds, optionally with FINALIZER."
  (gptel-runner-dispatch-test--isolated
    (let ((now 100.0) (body-starts 0) (callbacks 0) timer)
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (let* ((body
                    (gptel-runner-flow-duration-test--node
                     site
                     (lambda (run)
                       ;; Advance time without yielding to the duration timer.
                       (setq timer (gptel-runner-run-duration-timer run))
                       (cl-incf now elapsed)
                       (gptel-runner-flow-duration-test--return outcome))
                     (gptel-runner-elisp-step
                      :id 'body :save-as 'result
                      :function (lambda (_run _node) (cl-incf body-starts)))))
                   (root
                    (if finalizer
                        (gptel-runner-finalize
                         :max-duration 30 :body body
                         :finally
                         (gptel-runner-elisp-step
                          :id 'finalizer :save-as 'published
                          :function
                          (lambda (run _node)
                            (should (= (gptel-runner-run-duration-remaining run) 30))
                            (should (= (gptel-runner-run-active-started-at run) now))
                            (cl-incf now 1)
                            'finished)))
                      body))
                   (run (gptel-runner-start
                         root :max-duration 10
                         :callback (lambda (_run) (cl-incf callbacks))))
                   (iterations (if (memq site '(until stop)) 1 0)))
              (should (eq (gptel-runner-run-state run) 'failed))
              (should (eq (gptel-runner-node-state run 'flow) 'failed))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                          'budget))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind)
                          'duration))
              (should (= (plist-get (gptel-runner-run-terminal-data run) :limit) 10))
              (should (= (gptel-runner-run-duration-remaining run) 0))
              (should (= callbacks 1))
              (should (= body-starts (if (memq site '(branch entry)) 0 1)))
              (should (= (gptel-runner-iteration run 'flow) iterations))
              (should (= (length (gptel-runner-get run 'history)) iterations))
              (should-not (gptel-runner-get run '(gptel-runner-branch flow)))
              (should-not (gptel-runner-get run '(gptel-runner-progress flow)))
              (should-not (gptel-runner-run-duration-timer run))
              (should (zerop (gptel-runner-run-active-count run)))
              (when finalizer
                (should (eq (gptel-runner-get run 'published) 'finished))
                (should (eq (plist-get
                             (plist-get (gptel-runner-finalization-outcome run)
                                        :result) :state)
                            'succeeded)))))
        ;; Direct elapsed-time expiry can detach the scheduled timer.
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest gptel-runner-flow-duration-expires-before-accepting-outcomes ()
  (dolist (site '(branch entry until stop progress))
    (dolist (outcome '(nil t error quit))
      (dolist (elapsed '(10 11))
        (ert-info ((format "%s returning %s at %s seconds" site outcome elapsed))
          (gptel-runner-flow-duration-test--expired site outcome elapsed nil))))))

(ert-deftest gptel-runner-flow-duration-finalizer-keeps-separate-allowance ()
  (dolist (site '(branch entry until stop progress))
    (ert-info ((format "Finalizing expired %s" site))
      (gptel-runner-flow-duration-test--expired site t 10 t))))

(ert-deftest gptel-runner-flow-duration-expired-stop-skips-until ()
  (gptel-runner-dispatch-test--isolated
    (let ((now 100.0) (until-calls 0) timer)
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (let ((run
                   (gptel-runner-start
                    (gptel-runner-repeat-until
                     :max 2 :body (gptel-runner-sequence)
                     :until (lambda (_run) (cl-incf until-calls) nil)
                     :stop-when (lambda (run)
                                  (setq timer (gptel-runner-run-duration-timer run))
                                  (cl-incf now 10)
                                  nil))
                    :max-duration 10)))
              (should (eq (gptel-runner-run-state run) 'failed))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind)
                          'duration))
              (should (= until-calls 1))))
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest gptel-runner-flow-duration-under-budget-keeps-outcomes ()
  (dolist (limit '(nil 10))
    (dolist (outcome '(nil t error quit))
      (gptel-runner-dispatch-test--isolated
        (let ((now 100.0) (callbacks 0))
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (let ((run
                   (gptel-runner-start
                    (gptel-runner-branch
                     :id 'flow
                     :predicate (lambda (_run)
                                  (cl-incf now 9)
                                  (gptel-runner-flow-duration-test--return outcome))
                     :then (gptel-runner-sequence))
                    :max-duration limit
                    :callback (lambda (_run) (cl-incf callbacks)))))
              (should (= callbacks 1))
              (should (eq (gptel-runner-run-state run)
                          (pcase outcome
                            ('error 'failed) ('quit 'cancelled) (_ 'succeeded))))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                          (pcase outcome
                            ('error 'flow-error) ('quit 'flow-quit) (_ nil))))
              (should (equal (gptel-runner-run-duration-remaining run)
                             (and limit 1.0))))))))))

(ert-deftest gptel-runner-flow-duration-does-not-catch-continuation-errors ()
  (gptel-runner-dispatch-test--isolated
    (let* ((node (gptel-runner-elisp-step :id 'owner :async t :function #'ignore))
           (run (gptel-runner-start node :max-duration 60))
           done)
      (should-error
       (gptel-runner--evaluate-flow-function
        run node (lambda (_run) t)
        (lambda (_value) (error "Downstream failure"))
        (lambda (&rest _) (setq done t)))
       :type 'error)
      (should-not done)
      (should (eq (gptel-runner-node-state run 'owner) 'running)))))

(defun gptel-runner-flow-duration-test--agent (site outcome finalizer)
  "Exhaust the body or FINALIZER allowance at SITE, returning OUTCOME."
  (gptel-runner-dispatch-test--isolated
    (gptel-runner-register-agent 'worker)
    (let* ((driver (gptel-runner-fake-driver-create))
           (building (memq site '(prompt context)))
           (limit (if building 0.02 0.05))
           (elapsed (if building 0.15 0.3))
           (now 100.0) (callbacks 0) visits)
      (gptel-runner-fake-queue driver 'worker '(:value "answer"))
      (cl-labels ((visit (where value)
                    (push where visits)
                    (if (eq site where)
                        (progn
                          (cl-incf now elapsed)
                          (gptel-runner-flow-duration-test--return outcome))
                      value)))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
          (let* ((agent (gptel-runner-agent-step
                         :id 'worker :agent 'worker :save-as 'answer
                         :prompt (lambda (&rest _) (visit 'prompt "task"))
                         :context (list (lambda (&rest _) (visit 'context "context"))
                                        (lambda (&rest _) (visit 'more "more")))
                         :parser (lambda (value) (visit 'parser value))
                         :validator (lambda (_value) (visit 'validator t))
                         :repair-invalid t :retries 1))
                 (other (gptel-runner-elisp-step
                         :id 'other :save-as 'other :function (lambda (&rest _) 'done)))
                 (run (gptel-runner-start
                       (gptel-runner-finalize
                        :max-duration (if finalizer limit 10)
                        :body (if finalizer other agent)
                        :finally (if finalizer agent other))
                       :max-duration (if finalizer 10 limit) :driver driver
                       :callback (lambda (_run) (cl-incf callbacks))))
                 (record (gptel-runner-finalization-outcome run))
                 (result (plist-get record :result))
                 (failure (if finalizer (plist-get result :value)
                            (gptel-runner-run-terminal-data run))))
            (should (equal (nreverse visits)
                           (pcase site
                             ('prompt '(prompt))
                             ('context '(prompt context))
                             ('parser '(prompt context more parser))
                             ('validator '(prompt context more parser validator)))))
            (should (eq (plist-get failure :kind) 'duration))
            (should (equal (plist-get failure :limit) limit))
            (should (eq (gptel-runner-node-state run 'worker) 'failed))
            (should (eq (gptel-runner-run-state run) (if finalizer 'succeeded 'failed)))
            (should (eq (plist-get result :state) (if finalizer 'failed 'succeeded)))
            (should (eq (gptel-runner-get run 'other) 'done))
            (should-not (gptel-runner-get run 'answer))
            (should (= callbacks 1))
            (should (= (length (gptel-runner-run-calls run)) (if building 1 2)))
            (should (= (length (gptel-runner-fake-driver-starts driver))
                       (if building 0 1)))
            (should (zerop (gptel-runner-run-active-count run)))
            (should-not (gptel-runner-run-duration-timer run))))))))

(ert-deftest gptel-runner-flow-duration-agent-functions-obey-phase-deadlines ()
  (dolist (site '(prompt context parser validator))
    ;; Include an accepted value, a rejection, and both nonlocal exits.
    (dolist (outcome '("result" nil error quit))
      (dolist (finalizer '(nil t))
        (ert-info ((format "%s returning %S in %s" site outcome
                           (if finalizer "finalizer" "body")))
          (gptel-runner-flow-duration-test--agent site outcome finalizer))))))

(ert-deftest gptel-runner-flow-duration-dispatch-rechecks-after-observers ()
  (dolist (site '(admission call-started request-started))
    (gptel-runner-dispatch-test--isolated
      (gptel-runner-register-agent 'worker :workspace-mode 'write)
      (let* ((now 100.0)
             (driver (gptel-runner-fake-driver-create))
             (acquired 0) (released 0)
             (gptel-runner-call-admission-functions
              (list (lambda (_call)
                      (cl-incf acquired)
                      (when (eq site 'admission) (cl-incf now 1))
                      t)))
             (gptel-runner-call-release-hook
              (list (lambda (_call) (cl-incf released))))
             (gptel-runner-event-hook
              (list (lambda (event)
                      (when (eq (gptel-runner-event-type event) site)
                        (cl-incf now 1))))))
        (gptel-runner-fake-queue driver 'worker '(:value "late"))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
          (let ((run (gptel-runner-start
                      (gptel-runner-agent-step :agent 'worker :prompt "work")
                      :driver driver :max-duration 1 :allow-writes t)))
            (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind)
                        'duration))
            (should-not (gptel-runner-fake-driver-starts driver))
            (should (= acquired released 1))
            (should (zerop (gptel-runner-run-writer-active run)))
            (should (zerop (gptel-runner-budget-requests (gptel-runner-run-budget run))))
            (should (zerop (gptel-runner-run-active-count run)))))))))

(ert-deftest gptel-runner-flow-duration-manual-acceptance-uses-active-clock ()
  (dolist (paused '(nil t))
    (gptel-runner-dispatch-test--isolated
      (gptel-runner-register-agent 'worker)
      (let ((now 100.0) (driver (gptel-runner-fake-driver-create)))
        (gptel-runner-fake-queue driver 'worker '(:value "draft"))
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
          (let* ((root (gptel-runner-agent-step
                        :id 'worker :agent 'worker :prompt "review" :save-as 'answer
                        :pause-after t :parser (lambda (value) (cl-incf now 1) value)))
                 (workflow (gptel-runner-workflow-create :name 'approval :root root))
                 (run (gptel-runner-start workflow :driver driver :max-duration 0.05))
                 (call (car (gptel-runner-run-calls run))))
            (should (eq (gptel-runner-call-state call) 'waiting-feedback))
            (when paused (gptel-runner-pause-run run))
            (gptel-runner-complete-call-from-buffer "approved" call)
            (if paused
                (progn
                  (should (eq (gptel-runner-run-state run) 'succeeded))
                  (should (equal (gptel-runner-get run 'answer) "approved")))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind)
                          'duration))
              (should-not (gptel-runner-get run 'answer)))))))))

(provide 'gptel-runner-flow-duration-test)
;;; gptel-runner-flow-duration-test.el ends here
