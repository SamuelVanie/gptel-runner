;;; gptel-runner-dispatch-test.el --- Bounded synchronous dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-dispatch-test--isolated (&rest body)
  "Run BODY with private registries and a bounded Lisp evaluation stack."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner-default-driver nil)
         (gptel-runner-event-hook nil)
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner-snapshot-directory
          (make-temp-file "runner-dispatch-" t))
         (max-lisp-eval-depth 1000))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run) (gptel-runner-abort-run run))
                gptel-runner--runs)
       (maphash (lambda (run _coordinator)
                  (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(defun gptel-runner-dispatch-test--repeat (id maximum body)
  "Return a repeat with ID running BODY exactly MAXIMUM times."
  (gptel-runner-repeat-until
   :id id :max maximum :body body
   :until (lambda (run) (= (gptel-runner-iteration run id) maximum))))

(ert-deftest gptel-runner-dispatch-synchronous-repeat-finishes-inline ()
  (gptel-runner-dispatch-test--isolated
    (let ((iterations 0) (callbacks 0) returned callback-before-return
          callback-state)
      (let ((run
             (gptel-runner-start
              (gptel-runner-dispatch-test--repeat
               'repeat 200
               (gptel-runner-elisp-step
                :id 'body :save-as 'result
                :function (lambda (_run _node) (cl-incf iterations))))
              :callback (lambda (run)
                          (setq callback-before-return (not returned)
                                callback-state (gptel-runner-run-state run))
                          (cl-incf callbacks)))))
        (setq returned t)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= iterations 200))
        (should (= (gptel-runner-get run 'result) 200))
        (should (= callbacks 1))
        (should callback-before-return)
        (should (eq callback-state 'succeeded))
        (should (zerop (gptel-runner-run-active-count run)))
        (should-not (gptel-runner-run-queue run))))))

(ert-deftest gptel-runner-dispatch-long-sequence-needs-no-call-boundary ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0))
      (let ((run
             (gptel-runner-start
              (apply #'gptel-runner-sequence
                     (cl-loop for index below 300
                              collect
                              (if (zerop (% index 2))
                                  (gptel-runner-sequence)
                                (gptel-runner-branch
                                 :predicate #'ignore
                                 :then (gptel-runner-elisp-step
                                        :function
                                        (lambda (&rest _)
                                          (ert-fail "Unselected branch ran")))))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= callbacks 1))
        (should-not (gptel-runner-run-calls run))))))

(ert-deftest gptel-runner-dispatch-repeat-needs-no-call-boundary ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-dispatch-test--repeat
               'empty-repeat 300 (gptel-runner-sequence))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-iteration run 'empty-repeat) 300))
        (should (= callbacks 1))
        (should-not (gptel-runner-run-calls run))))))

(ert-deftest gptel-runner-dispatch-inline-async-cleans-before-next-step ()
  (gptel-runner-dispatch-test--isolated
    (let ((starts 0) (cleanups 0) (callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-dispatch-test--repeat
               'repeat 150
               (gptel-runner-elisp-step
                :async t :save-as 'result
                :function
                (lambda (_run _node resolve reject)
                  (should (= starts cleanups))
                  (cl-incf starts)
                  (funcall resolve starts)
                  (funcall reject 'duplicate)
                  (lambda (reason)
                    (should (eq reason 'succeeded))
                    (cl-incf cleanups)))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= starts 150))
        (should (= cleanups 150))
        (should (= (gptel-runner-get run 'result) 150))
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-dispatch-elisp-retries-have-bounded-stack ()
  (gptel-runner-dispatch-test--isolated
    (let ((attempts 0) (callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-elisp-step
               :retries 150 :save-as 'result
               :function (lambda (_run _node)
                           (cl-incf attempts)
                           (if (<= attempts 150) (error "Retry me") 'done)))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= attempts 151))
        (should (eq (gptel-runner-get run 'result) 'done))
        (should (= callbacks 1))
        (should (= (length (gptel-runner-run-calls run)) 151))))))

(ert-deftest gptel-runner-dispatch-agent-retries-have-bounded-stack ()
  (gptel-runner-dispatch-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (apply #'gptel-runner-fake-queue driver 'worker
             (append (make-list 100 '(:status permanent :value "Retry me"))
                     '((:value "done" :duplicate t))))
      (let ((run
             (gptel-runner-start
              (gptel-runner-agent-step
               :agent 'worker :prompt "work" :retries 100 :save-as 'result)
              :driver driver :max-calls 101 :max-requests 101
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'result) "done"))
        (should (= callbacks 1))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 101))
        (should (zerop (gptel-runner-fake-driver-active driver)))))))

(ert-deftest gptel-runner-dispatch-driver-inline-result-precedes-startup-error ()
  (gptel-runner-dispatch-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((callbacks 0))
      (cl-letf (((symbol-function 'gptel-runner-driver-start)
                 (lambda (_driver _call complete _observe)
                   (funcall complete 'success "accepted")
                   (error "Driver failed after delivering its result"))))
        (let ((run
               (gptel-runner-start
                (gptel-runner-agent-step
                 :agent 'worker :prompt "work" :save-as 'result)
                :driver (gptel-runner-fake-driver-create)
                :callback (lambda (_run) (cl-incf callbacks)))))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (equal (gptel-runner-get run 'result) "accepted"))
          (should (= callbacks 1))
          (should (zerop (gptel-runner-run-active-count run))))))))

(ert-deftest gptel-runner-dispatch-terminal-callback-error-remains-visible ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0))
      (should-error
       (gptel-runner-start
        (gptel-runner-elisp-step :function #'ignore)
        :callback (lambda (_run)
                    (cl-incf callbacks)
                    (error "Terminal callback failed")))
       :type 'error)
      (should (= callbacks 1))
      (should (= (hash-table-count gptel-runner--runs) 1))
      (maphash (lambda (_id run)
                 (should (eq (gptel-runner-run-state run) 'succeeded))
                 (should (gptel-runner-run-callback-called run))
                 (should (zerop (gptel-runner-run-active-count run))))
               gptel-runner--runs))))

(ert-deftest gptel-runner-dispatch-budget-denied-retries-have-bounded-stack ()
  (gptel-runner-dispatch-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-agent-step
               :agent 'worker :prompt "work" :retries 150)
              :driver driver :max-calls 0
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind) 'calls))
        (should (= callbacks 1))
        (should (= (cl-count 'agent-step-retry (gptel-runner-run-events run)
                            :key #'gptel-runner-event-type)
                   150))
        (should-not (gptel-runner-run-calls run))
        (should-not (gptel-runner-fake-driver-starts driver))))))

(ert-deftest gptel-runner-dispatch-manual-completion-drains-synchronous-tail ()
  (gptel-runner-dispatch-test--isolated
    (let ((iterations 0) (callbacks 0) resolve)
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               (gptel-runner-elisp-step
                :async t :function (lambda (_run _node success _failure)
                                     (setq resolve success) nil))
               (gptel-runner-dispatch-test--repeat
                'tail 200
                (gptel-runner-elisp-step
                 :function (lambda (_run _node) (cl-incf iterations)))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (zerop callbacks))
        (funcall resolve 'ready)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= iterations 200))
        (should (= callbacks 1))
        (funcall resolve 'duplicate)
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-dispatch-nested-start-remains-synchronous ()
  (gptel-runner-dispatch-test--isolated
    (let ((children 0) (callbacks 0))
      (let ((run
             (gptel-runner-start
              (gptel-runner-dispatch-test--repeat
               'outer 50
               (gptel-runner-elisp-step
                :function
                (lambda (_run _node)
                  (let ((child
                         (gptel-runner-start
                          (gptel-runner-dispatch-test--repeat
                           'inner 3
                           (gptel-runner-elisp-step
                            :function (lambda (_run _node) (cl-incf children)))))))
                    (should (eq (gptel-runner-run-state child) 'succeeded))
                    (should (= (gptel-runner-iteration child 'inner) 3))))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= children 150))
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-dispatch-resume-drains-many-cached-successes ()
  (gptel-runner-dispatch-test--isolated
    (let ((starts 0) (gate-starts 0) (callbacks 0) resolve gate-resolve)
      (let* ((workflow
              (gptel-runner-workflow-create
               :name 'cached-resume
               :root
               (apply
                #'gptel-runner-sequence
                (append
                 (cl-loop repeat 300 collect
                          (gptel-runner-elisp-step
                           :async t
                           :function (lambda (_run _node success _failure)
                                       (cl-incf starts)
                                       (setq resolve success) nil)))
                 (list
                  (gptel-runner-elisp-step
                   :id 'gate :async t
                   :function (lambda (_run _node success _failure)
                               (cl-incf gate-starts)
                               (setq gate-resolve success) nil)))))))
             (run (gptel-runner-start
                   workflow :callback (lambda (_run) (cl-incf callbacks)))))
        ;; Finish each prefix step from an independent external callback, so
        ;; only the later replay of cached successes stresses the stack.
        (dotimes (_ 300) (funcall resolve 'done))
        (should (= starts 300))
        (should (= gate-starts 1))
        (let ((old-gate-resolve gate-resolve))
          (gptel-runner-pause-run run)
          (gptel-runner-resume-run run)
          (should (eq (gptel-runner-run-state run) 'running))
          (should (= starts 300))
          (should (= gate-starts 2))
          (funcall old-gate-resolve 'obsolete)
          (should (zerop callbacks))
          (funcall gate-resolve 'fresh))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= callbacks 1))
        (should (eq (gptel-runner-node-result run 'gate) 'fresh))))))

(ert-deftest gptel-runner-dispatch-pause-resumes-long-inline-repeat ()
  (gptel-runner-dispatch-test--isolated
    (let ((starts 0) (callbacks 0) old-resolve cleanup-reasons)
      (let ((run
             (gptel-runner-start
              (gptel-runner-workflow-create
               :name 'repeat-resume
               :root
               (gptel-runner-dispatch-test--repeat
                'repeat 200
                (gptel-runner-elisp-step
                 :id 'body :async t :save-as 'result
                 :function
                 (lambda (run _node resolve _reject)
                   (cl-incf starts)
                   (if (= starts 41)
                       (progn
                         (setq old-resolve resolve)
                         (gptel-runner-pause-run run))
                     (funcall resolve starts))
                   (lambda (reason) (push reason cleanup-reasons))))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'paused))
        (should (= (gptel-runner-iteration run 'repeat) 40))
        (should (zerop callbacks))
        (gptel-runner-resume-run run)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= starts 201))
        (should (= (gptel-runner-iteration run 'repeat) 200))
        (should (= (gptel-runner-get run 'result) 201))
        (should (= (cl-count 'paused cleanup-reasons) 1))
        (should (= (cl-count 'succeeded cleanup-reasons) 200))
        (funcall old-resolve 'obsolete)
        (should (= (gptel-runner-get run 'result) 201))
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-dispatch-cancellation-drains-only-finalizer-generation ()
  (gptel-runner-dispatch-test--isolated
    (let ((finalizations 0) (callbacks 0) resolve cleanup-reasons)
      (let ((run
             (gptel-runner-start
              (gptel-runner-finalize
               :max-duration 30 :on-cancel 'finalize
               :body (gptel-runner-elisp-step
                      :id 'body :async t :save-as 'obsolete
                      :function
                      (lambda (_run _node success _failure)
                        (setq resolve success)
                        (lambda (reason) (push reason cleanup-reasons))))
               :finally
               (gptel-runner-dispatch-test--repeat
                'publication 150
                (gptel-runner-elisp-step
                 :function (lambda (_run _node) (cl-incf finalizations)))))
              :callback (lambda (_run) (cl-incf callbacks)))))
        (gptel-runner-abort-run run 'stop)
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should (= finalizations 150))
        (should (= callbacks 1))
        (should (equal cleanup-reasons '(cancelled)))
        (should (eq (plist-get
                     (plist-get (gptel-runner-finalization-outcome run) :result)
                     :state)
                    'succeeded))
        (funcall resolve 'stale)
        (should-not (gptel-runner-get run 'obsolete))
        (should (= callbacks 1))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-dispatch-completion-precedes-next-parallel-admission ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) started)
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               ;; Enter the parallel from a completion continuation.
               (gptel-runner-sequence)
               (gptel-runner-parallel
                :policy 'fail-fast
                (gptel-runner-elisp-step
                 :function (lambda (_run _node)
                             (push 'failure started) (error "Stop")))
                (gptel-runner-elisp-step
                 :function (lambda (_run _node) (push 'unwanted started)))))
              :max-concurrency 2
              :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (equal started '(failure)))
        (should (= callbacks 1))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-dispatch-cancellation-invalidates-queued-node-completions ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) reject-a resolve-b resolve-c resolve-other advanced)
      (let ((run
             (gptel-runner-start
              (gptel-runner-parallel
               :id 'outer :policy 'collect
               (gptel-runner-parallel
                :id 'group :policy 'fail-fast
                (gptel-runner-elisp-step
                 :id 'a :async t
                 :function (lambda (_run _node _resolve reject)
                             (setq reject-a reject) nil))
                (gptel-runner-sequence
                 :id 'sibling
                 (gptel-runner-parallel
                  :id 'inner :policy 'collect
                  (gptel-runner-elisp-step
                   :id 'b :async t
                   :function (lambda (_run _node resolve _reject)
                               (setq resolve-b resolve) nil))
                  (gptel-runner-elisp-step
                   :id 'c :async t
                   :function (lambda (_run _node resolve _reject)
                               (setq resolve-c resolve) nil)))
                 (gptel-runner-branch
                  :id 'after :predicate (lambda (_run) (setq advanced t))
                  :then (gptel-runner-sequence))))
               (gptel-runner-elisp-step
                :id 'independent :async t
                :function (lambda (_run _node resolve _reject)
                            (setq resolve-other resolve) nil)))
              :max-concurrency 4
              :callback (lambda (_run) (cl-incf callbacks)))))
        (let ((gptel-runner-event-hook
               (list (lambda (event)
                       (when (and (eq (gptel-runner-event-type event) 'call-failed)
                                  (eq (gptel-runner-event-node-id event) 'a))
                         ;; Their calls succeed before A's failure reaches the
                         ;; group.  Their node completions are still queued
                         ;; when cancellation invalidates the sibling subtree.
                         (funcall resolve-b 'done)
                         (funcall resolve-c 'done))))))
          (funcall reject-a 'failure))
        (should-not advanced)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (eq (gptel-runner-node-state run 'group) 'failed))
        (dolist (id '(sibling inner))
          (should (eq (gptel-runner-node-state run id) 'cancelled)))
        (dolist (id '(b c))
          (should (eq (gptel-runner-node-state run id) 'succeeded)))
        (should (eq (gptel-runner-node-state run 'after) 'skipped))
        (should (eq (gptel-runner-node-state run 'independent) 'running))
        (should (zerop callbacks))
        (funcall resolve-b 'stale)
        (funcall resolve-c 'stale)
        (funcall resolve-other 'unaffected)
        (should-not advanced)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= callbacks 1))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-dispatch-unexpected-continuation-error-cleans-run ()
  (gptel-runner-dispatch-test--isolated
    (let ((callbacks 0) cleaned)
      (cl-letf (((symbol-function 'gptel-runner--collect-repeat-history)
                 (lambda (&rest _) (error "Unexpected continuation error"))))
        (let ((run
               (gptel-runner-start
                (gptel-runner-parallel
                 (gptel-runner-elisp-step
                  :async t :function
                  (lambda (_run _node _resolve _reject)
                    (lambda (_reason) (setq cleaned t))))
                 (gptel-runner-dispatch-test--repeat
                  'repeat 2 (gptel-runner-elisp-step :function #'ignore)))
                :max-concurrency 2
                :callback (lambda (_run) (cl-incf callbacks)))))
          (should (eq (gptel-runner-run-state run) 'failed))
          (should cleaned)
          (should (= callbacks 1))
          (should (zerop (gptel-runner-run-active-count run)))
          (should-not (gptel-runner-run-queue run))
          (should (cl-every #'gptel-runner--call-terminal-p
                            (gptel-runner-run-calls run))))))))

(ert-deftest gptel-runner-dispatch-async-observer-error-cannot-strand-run ()
  (gptel-runner-dispatch-test--isolated
    (dolist (observation '(call-succeeded elisp-cleanup-error))
      (let ((callbacks 0) resolve cleaned)
        (let ((run
               (gptel-runner-start
                (gptel-runner-parallel
                 (gptel-runner-elisp-step
                  :id 'source :async t
                  :function
                  (lambda (_run _node success _failure)
                    (setq resolve success)
                    (when (eq observation 'elisp-cleanup-error)
                      (lambda (_reason) (error "Cleanup failed")))))
                 (gptel-runner-elisp-step
                  :id 'sibling :async t
                  :function (lambda (_run _node _resolve _reject)
                              (lambda (_reason) (setq cleaned t)))))
                :max-concurrency 2
                :callback (lambda (_run) (cl-incf callbacks)))))
          (let ((gptel-runner-event-hook
                 (list (lambda (event)
                         (when (memq (gptel-runner-event-type event)
                                     (list observation 'run-failed))
                           (error "Observer failed during completion or teardown"))))))
            ;; This delivery starts outside the dispatch pump.  The same hook
            ;; also fails while recovery publishes the failed run's outcome.
            (funcall resolve 'ready))
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                      'execution-error))
          (should cleaned)
          (should (= callbacks 1))
          (should (zerop (gptel-runner-run-active-count run)))
          (should-not (gptel-runner-run-queue run))
          (should (cl-every #'gptel-runner--call-terminal-p
                            (gptel-runner-run-calls run)))
          (funcall resolve 'duplicate)
          (should (= callbacks 1)))))))

(provide 'gptel-runner-dispatch-test)
;;; gptel-runner-dispatch-test.el ends here

(ert-deftest gptel-runner-dispatch-observer-stop-prevents-provider-start ()
  (dolist (event-type '(call-started request-started))
    (dolist (action '(cancel pause))
      (gptel-runner-dispatch-test--isolated
        (gptel-runner-register-agent 'worker)
        (gptel-runner-defworkflow observer-stop ()
          (gptel-runner-agent-step :id 'work :agent 'worker :prompt "work"))
        (let* ((driver (gptel-runner-fake-driver-create))
               (gptel-runner-event-hook
                (list (lambda (event)
                        (when (eq (gptel-runner-event-type event) event-type)
                          (let ((run (gptel-runner-find-run
                                      (gptel-runner-event-run-id event))))
                            (if (eq action 'cancel)
                                (gptel-runner-abort-run run)
                              (gptel-runner-pause-run run))))))))
          (gptel-runner-fake-queue driver 'worker '(:value "done"))
          (let ((run (gptel-runner-start 'observer-stop :driver driver)))
            (should (eq (gptel-runner-run-state run)
                        (if (eq action 'cancel) 'cancelled 'paused)))
            (should-not (gptel-runner-fake-driver-starts driver))
            (should-not (gptel-runner-run-active-calls run))
            (should (zerop (gptel-runner-run-active-count run)))
            (when (eq action 'pause)
              (let ((gptel-runner-event-hook nil))
                (gptel-runner-resume-run run))
              (should (eq (gptel-runner-run-state run) 'succeeded))
              (should (= (length (gptel-runner-fake-driver-starts driver)) 1)))))))))
