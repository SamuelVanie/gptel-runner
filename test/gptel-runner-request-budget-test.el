;;; gptel-runner-request-budget-test.el --- Provider send budgets -*- lexical-binding: t; -*-

(require 'gptel-runner-gptel-test)

(defun gptel-runner-request-budget-test--count (run)
  "Return the provider request count for RUN."
  (gptel-runner-budget-requests (gptel-runner-run-budget run)))

(defun gptel-runner-request-budget-test--events (run type)
  "Count RUN events of TYPE."
  (cl-count type (gptel-runner-run-events run)
            :key #'gptel-runner-event-type))

(defun gptel-runner-request-budget-test--tools (fsm)
  "Deliver a successful read tool invocation through FSM."
  (gptel-runner-gptel-test--reply
   fsm nil "tool_calls"
   [(:id "read-budget" :type "function"
         :function (:name "runner_read" :arguments "{}"))]))

(defun gptel-runner-request-budget-test--transient (fsm)
  "Deliver a retryable transport error through FSM."
  (let ((info (gptel-fsm-info fsm)))
    (plist-put info :http-status "503")
    (plist-put info :error "Temporarily unavailable")
    (gptel--fsm-transition fsm 'ERRS)))

(defun gptel-runner-request-budget-test--exhausted (run call limit)
  "Assert RUN and CALL failed cleanly at request LIMIT."
  (should (eq (gptel-runner-run-state run) 'failed))
  (should (eq (gptel-runner-call-state call) 'failed))
  (should (equal (gptel-runner-call-error call)
                 (list :type 'budget :kind 'requests :limit limit
                       :requests limit :calls 1)))
  (should (= (gptel-runner-request-budget-test--count run) limit))
  (should (zerop (gptel-runner-run-active-count run)))
  (should (zerop (gptel-runner-run-writer-active run)))
  (should-not (gptel-runner-run-active-calls run))
  (should-not (gptel-runner-call-retry-timer call))
  (should-not (plist-get (gptel-runner-call-driver-data call) :feedback-active)))

(ert-deftest gptel-runner-request-budget-zero-does-not-send ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                 :completion 'response :max-requests 0))
           (call (car (gptel-runner-run-calls run))))
      (should-not requests)
      (gptel-runner-request-budget-test--exhausted run call 0)
      (should (zerop (gptel-runner-request-budget-test--events run 'request-started))))))

(ert-deftest gptel-runner-request-budget-exact-limit-can-succeed ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let ((run (gptel-runner-start
                (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                :completion 'response :max-requests 1)))
      (should (= (length requests) 1))
      (gptel-runner-gptel-test--reply (pop requests) "Done" "stop")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-request-budget-test--count run) 1))
      (should (= (gptel-runner-request-budget-test--events run 'request-started) 1))
      (should-not requests))))

(ert-deftest gptel-runner-request-budget-tool-exhaustion-retires-once ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader :workspace-mode 'write)
    (let* ((posts 0) (completed 0)
           (run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                 :completion 'response :max-requests 1
                 :allow-writes t :allow-unconfirmed-tools t
                 :callback (lambda (_run) (cl-incf completed))))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests))
           (info (gptel-fsm-info fsm)))
      (plist-put info :post
                 (cons (lambda (_info) (cl-incf posts)) (plist-get info :post)))
      (gptel-runner-request-budget-test--tools fsm)
      (should-not requests)
      (gptel-runner-request-budget-test--exhausted run call 1)
      (should (= posts 1))
      (should (= completed 1))
      ;; A late tool callback must not revive transport or run cleanup twice.
      (gptel--fsm-transition fsm 'WAIT)
      (gptel--fsm-transition fsm 'ERRS)
      (should-not requests)
      (should (= posts 1))
      (should (= completed 1))
      (should-not (cl-some #'cdr (gptel-fsm-handlers fsm))))))

(ert-deftest gptel-runner-request-budget-counts-each-successful-tool-round ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                 :completion 'response :max-requests 3))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests)))
      (dotimes (round 2)
        (gptel-runner-request-budget-test--tools fsm)
        (should (eq (pop requests) fsm))
        (should (= (gptel-runner-request-budget-test--count run) (+ round 2))))
      (gptel-runner-gptel-test--reply fsm "Done" "stop")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-call-request-attempt call) 1))
      (should (= (gptel-runner-request-budget-test--events run 'request-started) 3))
      (should-not requests))))

(ert-deftest gptel-runner-request-budget-review-feedback-cannot-bypass-limit ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :pause-after t)
                 :completion 'response :max-requests 1))
           (call (car (gptel-runner-run-calls run))))
      (gptel-runner-gptel-test--reply (pop requests) "First answer" "stop")
      (gptel-runner-gptel-feedback call "Check again")
      (should-not requests)
      (gptel-runner-request-budget-test--exhausted run call 1))))

(ert-deftest gptel-runner-request-budget-review-feedback-counts-and-keeps-gate ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :pause-after t)
                 :completion 'response :max-requests 3))
           (call (car (gptel-runner-run-calls run))))
      (gptel-runner-gptel-test--reply (pop requests) "First answer" "stop")
      (gptel-runner-gptel-feedback call "Check again")
      (should (= (gptel-runner-request-budget-test--count run) 2))
      (let ((fsm (pop requests)))
        (gptel-runner-request-budget-test--tools fsm)
        (should (eq (pop requests) fsm))
        (should (= (gptel-runner-request-budget-test--count run) 3))
        (gptel-runner-gptel-test--reply fsm "Revised answer" "stop"))
      (should (eq (gptel-runner-call-state call) 'waiting-feedback))
      (should (zerop (gptel-runner-run-active-count run)))
      (gptel-runner-complete-call-from-buffer nil call)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-request-budget-test--events run 'request-started) 3))
      (should-not requests))))

(ert-deftest gptel-runner-request-budget-review-tool-exhaustion-cleans-up ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader :workspace-mode 'write)
    (let* ((posts 0) (completed 0)
           (run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :pause-after t)
                 :completion 'response :max-requests 2
                 :allow-writes t :allow-unconfirmed-tools t
                 :callback (lambda (_run) (cl-incf completed))))
           (call (car (gptel-runner-run-calls run))))
      (gptel-runner-gptel-test--reply (pop requests) "First answer" "stop")
      (gptel-runner-gptel-feedback call "Check again")
      (let* ((fsm (pop requests)) (info (gptel-fsm-info fsm)))
        (plist-put info :post
                   (cons (lambda (_info) (cl-incf posts)) (plist-get info :post)))
        (gptel-runner-request-budget-test--tools fsm)
        (should-not requests)
        (gptel-runner-request-budget-test--exhausted run call 2)
        (gptel--fsm-transition fsm 'WAIT)
        (gptel--fsm-transition fsm 'ERRS)
        (should-not requests)
        (should (= posts 1))
        (should (= completed 1))
        (should-not (cl-some #'cdr (gptel-fsm-handlers fsm)))))))

(ert-deftest gptel-runner-request-budget-transport-retry-counted-once ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                 :completion 'response :max-requests 2))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests)))
      (gptel-runner-request-budget-test--transient fsm)
      (should (= (gptel-runner-request-budget-test--count run) 1))
      (gptel-runner-gptel-test--retry call)
      (should (eq (pop requests) fsm))
      (should (= (gptel-runner-request-budget-test--count run) 2))
      (gptel-runner-gptel-test--reply fsm "Recovered" "stop")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-call-request-attempt call) 2))
      (should-not requests))))

(ert-deftest gptel-runner-request-budget-tool-round-keeps-transport-retry ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent
     'reader :retry-policy (gptel-runner-retry-policy-create :max-retries 1))
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                 :completion 'response :max-requests 3))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests)))
      (gptel-runner-request-budget-test--tools fsm)
      (should (eq (pop requests) fsm))
      (gptel-runner-request-budget-test--transient fsm)
      (should (eq (gptel-runner-call-state call) 'retry-wait))
      (gptel-runner-gptel-test--retry call)
      (should (eq (pop requests) fsm))
      (should (= (gptel-runner-request-budget-test--count run) 3))
      (gptel-runner-gptel-test--reply fsm "Recovered" "stop")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-call-request-attempt call) 2))
      (should-not requests))))

(ert-deftest gptel-runner-request-budget-finalizer-uses-own-tool-allowance ()
  (dolist (finalizer-limit '(1 2))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader)
      (let* ((completed 0)
             (run (gptel-runner-start
                   (gptel-runner-finalize
                    :max-duration 30 :max-requests finalizer-limit
                    :body (gptel-runner-agent-step
                           :id 'body :agent 'reader :prompt "Read")
                    :finally (gptel-runner-agent-step
                              :id 'report :agent 'reader :prompt "Report"))
                   :completion 'response :max-requests 1
                   :callback (lambda (_run) (cl-incf completed))))
             (body (pop requests)))
        (gptel-runner-request-budget-test--tools body)
        (should (eq (gptel-runner-node-state run 'body) 'failed))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= (length requests) 1))
        (let ((finalizer (pop requests)))
          (should-not (eq finalizer body))
          (gptel-runner-request-budget-test--tools finalizer)
          (when (= finalizer-limit 2)
            (should (eq (pop requests) finalizer))
            (gptel-runner-gptel-test--reply finalizer "Published" "stop")))
        (let* ((record (gptel-runner-finalization-outcome run))
               (result (plist-get record :result)))
          (should (eq (plist-get record :phase) 'complete))
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (eq (plist-get result :state)
                      (if (= finalizer-limit 1) 'failed 'succeeded)))
          (when (= finalizer-limit 1)
            (should (eq (plist-get (plist-get result :value) :kind) 'requests)))
          (should (= (gptel-runner-budget-requests
                      (plist-get record :body-budget)) 1))
          (should (= (gptel-runner-budget-requests
                      (plist-get record :budget)) finalizer-limit)))
        (should (= completed 1))
        (should (zerop (gptel-runner-run-active-count run)))
        (should-not (gptel-runner-run-duration-timer run))
        (should-not requests)))))

(ert-deftest gptel-runner-request-budget-failed-transform-is-not-a-send ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-prompt-transform-functions
           (list (lambda (_fsm) (error "Prompt preparation failed")))))
      (gptel-runner-register-agent 'reader)
      (let ((run (gptel-runner-start
                  (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                  :completion 'response :max-requests 1)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-not requests)
        (should (zerop (gptel-runner-request-budget-test--count run)))
        (should (zerop (gptel-runner-request-budget-test--events run 'request-started)))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-request-budget-async-transform-charges-at-send ()
  (gptel-runner-gptel-test--with-transport
    (let* (resume
           (gptel-prompt-transform-functions
            (list (lambda (continue _fsm) (setq resume continue)))))
      (gptel-runner-register-agent 'reader)
      (let ((run (gptel-runner-start
                  (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                  :completion 'response :max-requests 1)))
        (should (functionp resume))
        (should-not requests)
        (should (zerop (gptel-runner-request-budget-test--count run)))
        (should (zerop (gptel-runner-request-budget-test--events run 'request-started)))
        (funcall resume)
        (should (= (gptel-runner-request-budget-test--count run) 1))
        (should (= (length requests) 1))
        (gptel-runner-gptel-test--reply (pop requests) "Ready" "stop")
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-request-budget-concurrent-transforms-share-allowance ()
  (gptel-runner-gptel-test--with-transport
    (let* (continuations
           (gptel-prompt-transform-functions
            (list (lambda (continue fsm)
                    (let ((call (plist-get (gptel-fsm-info fsm) :context)))
                      (push (cons (gptel-runner-node-id (gptel-runner-call-node call))
                                  continue)
                            continuations))))))
      (gptel-runner-register-agent 'reader)
      (let* ((run (gptel-runner-start
                   (gptel-runner-parallel
                    :policy 'collect
                    (gptel-runner-agent-step :id 'first :agent 'reader :prompt "First")
                    (gptel-runner-agent-step :id 'second :agent 'reader :prompt "Second"))
                   :completion 'response :max-requests 1 :max-concurrency 2))
             (second (cadr (gptel-runner-run-calls run))))
        (should (= (length continuations) 2))
        (should-not requests)
        (should (zerop (gptel-runner-request-budget-test--count run)))
        (funcall (alist-get 'first continuations))
        (let ((request (pop requests)))
          (should (= (gptel-runner-request-budget-test--count run) 1))
          (funcall (alist-get 'second continuations))
          (should-not requests)
          (should (eq (gptel-runner-call-state second) 'failed))
          (should (eq (plist-get (gptel-runner-call-error second) :kind) 'requests))
          (should (= (gptel-runner-run-active-count run) 1))
          (gptel-runner-gptel-test--reply request "First answer" "stop"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-request-budget-test--count run) 1))
        (should (= (gptel-runner-request-budget-test--events run 'request-started) 1))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-request-budget-retry-rechecks-shared-allowance ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((posts 0)
           (run (gptel-runner-start
                 (gptel-runner-parallel
                  :policy 'collect
                  (gptel-runner-agent-step :id 'first :agent 'reader :prompt "First")
                  (gptel-runner-agent-step :id 'second :agent 'reader :prompt "Second"))
                 :completion 'response :max-requests 2 :max-concurrency 1))
           (first (car (gptel-runner-run-calls run)))
           (fsm (pop requests))
           (info (gptel-fsm-info fsm)))
      (plist-put info :post
                 (cons (lambda (_info) (cl-incf posts)) (plist-get info :post)))
      (gptel-runner-request-budget-test--transient fsm)
      (should (eq (gptel-runner-call-state first) 'retry-wait))
      (should (zerop posts))
      ;; The other branch consumes the remaining allowance during backoff.
      (should (= (length requests) 1))
      (should (= (gptel-runner-request-budget-test--count run) 2))
      (gptel-runner-gptel-test--reply (pop requests) "Second answer" "stop")
      (gptel-runner-gptel-test--retry first)
      (should-not requests)
      (should (eq (gptel-runner-call-state first) 'failed))
      (should (eq (plist-get (gptel-runner-call-error first) :kind) 'requests))
      (should-not (gptel-runner-call-retry-timer first))
      (should-not (plist-get (gptel-runner-call-driver-data first) :agent-supervision))
      (should (= posts 1))
      (should-not (cl-some #'cdr (gptel-fsm-handlers fsm)))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-request-budget-test--count run) 2))
      (should (= (gptel-runner-request-budget-test--events run 'request-started) 2))
      (should (zerop (gptel-runner-run-active-count run))))))

(ert-deftest gptel-runner-request-budget-feedback-post-error-stays-terminal ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :pause-after t)
                 :completion 'response :max-requests 1))
           (call (car (gptel-runner-run-calls run))))
      (gptel-runner-gptel-test--reply (pop requests) "First answer" "stop")
      (with-current-buffer (gptel-runner-call-buffer call)
        (setq-local gptel-prompt-transform-functions
                    (list (lambda (fsm)
                            (let ((info (gptel-fsm-info fsm)))
                              (plist-put info :post
                                         (cons (lambda (_info)
                                                 (error "Post cleanup failed"))
                                               (plist-get info :post))))))))
      (should-error (gptel-runner-gptel-feedback call "Check again"))
      (should-not requests)
      (gptel-runner-request-budget-test--exhausted run call 1))))

(ert-deftest gptel-runner-request-budget-start-hook-can-cancel-send ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let ((gptel-runner-event-hook
           (list (lambda (event)
                   (when (eq (gptel-runner-event-type event) 'request-started)
                     (gptel-runner-abort-run
                      (gptel-runner-find-run (gptel-runner-event-run-id event))))))))
      (let ((run (gptel-runner-start
                  (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                  :completion 'response :max-requests 1)))
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should-not requests)
        (should (zerop (gptel-runner-run-active-count run)))))))

(provide 'gptel-runner-request-budget-test)
;;; gptel-runner-request-budget-test.el ends here
