;;; gptel-runner-flow-generation-test.el --- Flow callback ownership -*- lexical-binding: t; -*-

(require 'gptel-runner-dispatch-test)

(defun gptel-runner-flow-generation-test--return (outcome value)
  "Return VALUE or signal the requested OUTCOME after a transition."
  (pcase outcome
    ('error (error "Obsolete flow error"))
    ('quit (signal 'quit nil))
    (_ value)))

(defun gptel-runner-flow-generation-test--node (site function body)
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

(defun gptel-runner-flow-generation-test--finalize (site transition outcome)
  "Invalidate a flow at SITE via TRANSITION before returning OUTCOME.
Invoke the duration callback explicitly, so no wall-clock sleep is needed."
  (gptel-runner-dispatch-test--isolated
    (gptel-runner-register-agent 'finalizer)
    (let ((driver (gptel-runner-fake-driver-create))
          (body-starts 0) (callbacks 0) before-generation)
      (gptel-runner-fake-queue driver 'finalizer '(:manual t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-finalize
                :max-duration 60 :on-cancel 'finalize
                :body
                (gptel-runner-flow-generation-test--node
                 site
                 (lambda (run)
                   (setq before-generation (gptel-runner-run-generation run))
                   (if (eq transition 'timeout)
                       (gptel-runner--duration-expired run before-generation)
                     (gptel-runner-abort-run run 'stop))
                   (gptel-runner-flow-generation-test--return
                    outcome (memq site '(branch progress))))
                 (gptel-runner-elisp-step
                  :id 'body :save-as 'result
                  :function (lambda (_run _node) (cl-incf body-starts))))
                :finally (gptel-runner-agent-step
                          :id 'finalizer :agent 'finalizer :prompt "finalize"))
               :driver driver :max-concurrency 2
               :callback (lambda (_run) (cl-incf callbacks))))
             (state (if (eq transition 'timeout) 'failed 'cancelled))
             (iterations (if (memq site '(until stop)) 1 0)))
        (should (> (gptel-runner-run-generation run) before-generation))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (eq (plist-get (gptel-runner-finalization-outcome run) :phase)
                    'finalizing))
        (should (eq (gptel-runner-node-state run 'flow) state))
        (should (= body-starts (if (memq site '(branch entry)) 0 1)))
        (should (= (gptel-runner-iteration run 'flow) iterations))
        (should (= (length (gptel-runner-get run 'history)) iterations))
        (should-not (gptel-runner-get run '(gptel-runner-branch flow)))
        (should-not (gptel-runner-get run '(gptel-runner-progress flow)))
        (should (equal (mapcar (lambda (call)
                                (gptel-runner-node-id (gptel-runner-call-node call)))
                              (gptel-runner-fake-driver-starts driver))
                       '(finalizer)))
        (should (zerop callbacks))
        (gptel-runner-fake-release
         driver (car (gptel-runner-fake-driver-starts driver)) '(:value "finished"))
        (should (eq (gptel-runner-run-state run) state))
        (should (eq (plist-get (plist-get (gptel-runner-finalization-outcome run)
                                        :result) :state)
                    'succeeded))
        (should (equal (gptel-runner-run-terminal-data run)
                       (plist-get (gptel-runner-node-outcome run 'flow) :value)))
        (should (= callbacks 1))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-flow-generation-branch-cannot-enter-finalizer-phase ()
  (dolist (transition '(timeout cancel))
    (dolist (outcome '(value error quit))
      (gptel-runner-flow-generation-test--finalize 'branch transition outcome))))

(ert-deftest gptel-runner-flow-generation-repeat-entry-cannot-enter-finalizer-phase ()
  (dolist (transition '(timeout cancel))
    (dolist (outcome '(value error quit))
      (gptel-runner-flow-generation-test--finalize 'entry transition outcome))))

(ert-deftest gptel-runner-flow-generation-repeat-until-cannot-enter-finalizer-phase ()
  (dolist (transition '(timeout cancel))
    (dolist (outcome '(value error quit))
      (gptel-runner-flow-generation-test--finalize 'until transition outcome))))

(ert-deftest gptel-runner-flow-generation-repeat-stop-cannot-enter-finalizer-phase ()
  (dolist (transition '(timeout cancel))
    (dolist (outcome '(value error quit))
      (gptel-runner-flow-generation-test--finalize 'stop transition outcome))))

(ert-deftest gptel-runner-flow-generation-repeat-progress-cannot-publish-after-expiry ()
  (dolist (transition '(timeout cancel))
    (dolist (outcome '(value error quit))
      (gptel-runner-flow-generation-test--finalize 'progress transition outcome))))

(ert-deftest gptel-runner-flow-generation-resume-keeps-new-branch-decision ()
  (dolist (outcome '(value error quit))
    (gptel-runner-dispatch-test--isolated
      (gptel-runner-register-agent 'worker)
      (let ((driver (gptel-runner-fake-driver-create)) (predicates 0) obsolete)
        (gptel-runner-fake-queue driver 'worker '(:manual t))
        (let* ((workflow
                (gptel-runner-workflow-create
                 :name 'flow-generation
                 :root (gptel-runner-branch
                        :id 'flow
                        :predicate
                        (lambda (run)
                          (when (= (cl-incf predicates) 1)
                            (gptel-runner-pause-run run)
                            (gptel-runner-resume-run run)
                            (gptel-runner-flow-generation-test--return outcome t)))
                        :then (gptel-runner-elisp-step
                               :id 'obsolete
                               :function (lambda (_run _node) (setq obsolete t)))
                        :else (gptel-runner-agent-step
                               :id 'current :agent 'worker :prompt "work"))))
               (run (gptel-runner-start workflow :driver driver :max-concurrency 2)))
          (should (= predicates 2))
          (should-not obsolete)
          (should (eq (gptel-runner-node-state run 'flow) 'running))
          (should (eq (gptel-runner-node-state run 'obsolete) 'skipped))
          (should (eq (gptel-runner-get run '(gptel-runner-branch flow)) :else))
          (should (= (length (gptel-runner-run-calls run)) 1))
          (gptel-runner-fake-release
           driver (car (gptel-runner-run-calls run)) '(:value "current result"))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (eq (gptel-runner-node-state run 'flow) 'succeeded)))))))

(ert-deftest gptel-runner-flow-generation-cancelled-owner-discards-all-outcomes ()
  (dolist (outcome '(value error quit))
    (gptel-runner-dispatch-test--isolated
      (let* ((node (gptel-runner-elisp-step :id 'owner :async t :function #'ignore))
             (run (gptel-runner-start node))
             (generation (gptel-runner-run-generation run))
             next done)
        (gptel-runner--evaluate-flow-function
         run node
         (lambda (run)
           (gptel-runner--with-deferred-dispatch run
             (gptel-runner--cancel-subtree-calls run node 'cancelled-owner))
           (gptel-runner-flow-generation-test--return outcome 'obsolete))
         (lambda (_value) (setq next t))
         (lambda (_state _value) (setq done t)))
        (should (= generation (gptel-runner-run-generation run)))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (eq (gptel-runner-node-state run 'owner) 'cancelled))
        (should-not next)
        (should-not done)))))

(ert-deftest gptel-runner-flow-generation-unrelated-cancellation-keeps-outcomes ()
  (dolist (outcome '(value error quit))
    (gptel-runner-dispatch-test--isolated
      (let* ((node (gptel-runner-elisp-step :id 'owner :async t :function #'ignore))
             (other (gptel-runner-elisp-step :id 'other :async t :function #'ignore))
             (run (gptel-runner-start
                   (gptel-runner-parallel node other) :max-concurrency 2))
             next done)
        (gptel-runner--evaluate-flow-function
         run node
         (lambda (run)
           (gptel-runner--with-deferred-dispatch run
             (gptel-runner--cancel-subtree-calls run other 'unrelated))
           (gptel-runner-flow-generation-test--return outcome 'current))
         (lambda (value) (setq next value))
         (lambda (state value) (setq done (list state (plist-get value :type)))))
        (if (eq outcome 'value)
            (progn (should (eq next 'current)) (should-not done))
          (should-not next)
          (should (equal done (if (eq outcome 'error)
                                 '(failed flow-error) '(cancelled flow-quit)))))))))

(ert-deftest gptel-runner-flow-generation-stale-stop-skips-until ()
  (gptel-runner-dispatch-test--isolated
    (let ((until-calls 0) resolve)
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 60
               :body (gptel-runner-repeat-until
                      :max 2
                      :until (lambda (_run) (cl-incf until-calls) nil)
                      :stop-when (lambda (run)
                                   (gptel-runner--duration-expired
                                    run (gptel-runner-run-generation run))
                                   nil)
                      :body (gptel-runner-elisp-step :function #'ignore))
               :finally (gptel-runner-elisp-step
                         :async t :function
                         (lambda (_run _node succeed _reject)
                           (setq resolve succeed) nil))))))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= until-calls 1))
        (funcall resolve 'finished)
        (should (eq (gptel-runner-run-state run) 'failed))))))

(provide 'gptel-runner-flow-generation-test)
;;; gptel-runner-flow-generation-test.el ends here
