;;; gptel-runner-flow-duration-test.el --- Synchronous flow budgets -*- lexical-binding: t; -*-

(require 'gptel-runner-dispatch-test)

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

(provide 'gptel-runner-flow-duration-test)
;;; gptel-runner-flow-duration-test.el ends here
