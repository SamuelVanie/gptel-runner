;;; gptel-runner-finalization-errors-test.el --- Finalization error boundaries -*- lexical-binding: t; -*-

(require 'gptel-runner-dispatch-test)

(defun gptel-runner-finalization-errors-test--terminal (run state)
  "Check that RUN has fully terminalized as STATE."
  (should (eq (gptel-runner-run-state run) state))
  (should (eq (plist-get (gptel-runner-finalization-outcome run) :phase) 'complete))
  (should (gptel-runner-run-callback-called run))
  (should (zerop (gptel-runner-run-active-count run)))
  (should-not (gptel-runner-run-queue run))
  (should-not (gptel-runner-run-continuations run))
  (should-not (gptel-runner-run-dispatching run))
  (should-not (gptel-runner-run-duration-timer run)))

(defun gptel-runner-finalization-errors-test--callback (async-body async-finalizer)
  "Check terminal callback errors with ASYNC-BODY and ASYNC-FINALIZER."
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) run resolve-body resolve-finalizer)
      (cl-labels
          ((start ()
             (setq run
                   (gptel-runner-start
                    (gptel-runner-finalize
                     :max-duration 10
                     :body (gptel-runner-elisp-step
                            :async async-body
                            :function
                            (lambda (_run _node &optional resolve _reject)
                              (when async-body (setq resolve-body resolve))
                              nil))
                     :finally (gptel-runner-elisp-step
                               :async async-finalizer
                               :function
                               (lambda (_run _node &optional resolve _reject)
                                 (when async-finalizer
                                   (setq resolve-finalizer resolve))
                                 nil)))
                    :callback (lambda (completed)
                                (setq run completed)
                                (cl-incf callbacks)
                                (error "Terminal callback failed")))))
           (finish ()
             (if (or async-body async-finalizer)
                 (progn
                   (start)
                   (should (eq (gptel-runner-run-state run) 'running))
                   (when async-body (funcall resolve-body 'body-result))
                   (when async-finalizer
                     (funcall resolve-finalizer 'finalizer-result)))
               (start))))
        (should (equal (should-error (finish) :type 'error)
                       '(error "Terminal callback failed"))))
      (gptel-runner-finalization-errors-test--terminal run 'succeeded)
      (should (= callbacks 1))
      (should (eq (plist-get (plist-get (gptel-runner-finalization-outcome run)
                                      :result) :state)
                  'succeeded))
      (when resolve-body (funcall resolve-body 'duplicate))
      (when resolve-finalizer (funcall resolve-finalizer 'duplicate))
      (should (= callbacks 1)))))

(ert-deftest gptel-runner-finalization-errors-synchronous-callback ()
  (gptel-runner-finalization-errors-test--callback nil nil))

(ert-deftest gptel-runner-finalization-errors-async-body-callback ()
  (gptel-runner-finalization-errors-test--callback t nil))

(ert-deftest gptel-runner-finalization-errors-async-finalizer-callback ()
  (gptel-runner-finalization-errors-test--callback nil t))

(ert-deftest gptel-runner-finalization-errors-async-body-and-finalizer-callback ()
  (gptel-runner-finalization-errors-test--callback t t))

(ert-deftest gptel-runner-finalization-errors-start-observer-finishes-run ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) finalizer-ran
          (gptel-runner-event-hook
           (list (lambda (event)
                   (when (eq (gptel-runner-event-type event) 'finalization-started)
                     (error "Finalization start observer failed"))))))
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 10
               :body (gptel-runner-elisp-step :function #'ignore)
               :finally (gptel-runner-elisp-step
                         :function (lambda (_run _node) (setq finalizer-ran t))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (gptel-runner-finalization-errors-test--terminal run 'succeeded)
        (should (= callbacks 1))
        (should-not finalizer-ran)
        (let ((result (plist-get (gptel-runner-finalization-outcome run) :result)))
          (should (eq (plist-get result :state) 'failed))
          (should (equal (plist-get result :value)
                         '(:type execution-error
                           :error (error "Finalization start observer failed")))))))))

(ert-deftest gptel-runner-finalization-errors-completion-observer-finishes-run ()
  (gptel-runner-dispatch-test--isolated
    (dolist (async '(nil t))
      (let ((callbacks 0) run resolve
            (gptel-runner-event-hook
             (list (lambda (event)
                     (when (eq (gptel-runner-event-type event) 'finalization-completed)
                       (error "Finalization completion observer failed"))))))
        (cl-labels
            ((start ()
               (setq run
                     (gptel-runner-start
                      (gptel-runner-finalize
                       :max-duration 10
                       :body (gptel-runner-elisp-step :function #'ignore)
                       :finally
                       (gptel-runner-elisp-step
                        :async async :function
                        (lambda (_run _node &optional complete _reject)
                          (when async (setq resolve complete))
                          nil)))
                      :callback (lambda (completed)
                                  (setq run completed)
                                  (cl-incf callbacks)))))
             (finish ()
               (start)
               (when async (funcall resolve 'published))))
          (should (equal (should-error (finish) :type 'error)
                         '(error "Finalization completion observer failed"))))
        (gptel-runner-finalization-errors-test--terminal run 'succeeded)
        (should (= callbacks 1))
        (should (eq (plist-get (plist-get (gptel-runner-finalization-outcome run)
                                        :result) :state)
                    'succeeded))))))

(ert-deftest gptel-runner-finalization-errors-skipped-observer-finishes-run ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 10 :on-cancel 'skip
               :body (gptel-runner-elisp-step :async t :function #'ignore)
               :finally (gptel-runner-elisp-step :function #'ignore))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (let ((gptel-runner-event-hook
               (list (lambda (event)
                       (when (eq (gptel-runner-event-type event) 'finalization-skipped)
                         (error "Finalization skip observer failed"))))))
          (should (equal (should-error (gptel-runner-abort-run run 'stop) :type 'error)
                         '(error "Finalization skip observer failed"))))
        (gptel-runner-finalization-errors-test--terminal run 'cancelled)
        (should (= callbacks 1))
        (should (eq (gptel-runner-run-terminal-data run) 'stop))
        (should (eq (plist-get (plist-get (gptel-runner-finalization-outcome run)
                                        :result) :state)
                    'skipped))))))

(ert-deftest gptel-runner-finalization-errors-initial-quiesce-preserves-body ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) (cleanups 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 10 :on-cancel 'finalize
               :body (gptel-runner-elisp-step
                      :id 'body :async t :function
                      (lambda (_run _node _resolve _reject)
                        (lambda (_reason) (cl-incf cleanups))))
               :finally (gptel-runner-elisp-step :function #'ignore))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (let ((gptel-runner-event-hook
               (list (lambda (event)
                       (when (and (eq (gptel-runner-event-type event) 'node-cancelled)
                                  (eq (gptel-runner-event-node-id event) 'body))
                         (error "Finalization quiesce observer failed"))))))
          (gptel-runner-abort-run run 'stop))
        (gptel-runner-finalization-errors-test--terminal run 'cancelled)
        (should (= callbacks 1))
        (should (= cleanups 1))
        (should (eq (gptel-runner-run-terminal-data run) 'stop))
        (let ((result (plist-get (gptel-runner-finalization-outcome run) :result)))
          (should (eq (plist-get result :state) 'failed))
          (should (equal (plist-get result :value)
                         '(:type execution-error
                           :error (error "Finalization quiesce observer failed")))))))))

(ert-deftest gptel-runner-finalization-errors-obsolete-transition-keeps-new-finalizer ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) resumed resolve)
      (let* ((workflow
              (gptel-runner-workflow-create
               :name 'phase-error-resume
               :root (gptel-runner-finalize
                      :max-duration 10
                      :body (gptel-runner-elisp-step :function #'ignore)
                      :finally (gptel-runner-elisp-step
                                :async t :function
                                (lambda (_run _node complete _reject)
                                  (setq resolve complete) nil)))))
             (gptel-runner-event-hook
              (list (lambda (event)
                      (when (and (not resumed)
                                 (eq (gptel-runner-event-type event)
                                     'finalization-started))
                        (setq resumed t)
                        (let ((run (gptel-runner-find-run
                                    (gptel-runner-event-run-id event))))
                          (gptel-runner-pause-run run)
                          (gptel-runner-resume-run run))
                        (error "Obsolete transition observer failed")))))
             (run (gptel-runner-start
                   workflow :callback (lambda (_run) (cl-incf callbacks)))))
        (should resumed)
        (should resolve)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (zerop callbacks))
        (funcall resolve 'published)
        (gptel-runner-finalization-errors-test--terminal run 'succeeded)
        (should (= callbacks 1))
        (should (equal (plist-get (gptel-runner-finalization-outcome run) :result)
                       '(:state succeeded :value published)))))))

(provide 'gptel-runner-finalization-errors-test)
;;; gptel-runner-finalization-errors-test.el ends here
