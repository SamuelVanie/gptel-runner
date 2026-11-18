;;; gptel-runner-elisp-test.el --- Elisp workflow steps -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-elisp-test--isolated (&rest body)
  "Run BODY with private registries, snapshots, and no default driver."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner-default-driver nil)
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner-snapshot-directory (make-temp-file "runner-elisp-" t)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run) (gptel-runner-abort-run run))
                gptel-runner--runs)
       (maphash (lambda (run _coordinator) (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(ert-deftest gptel-runner-elisp-mixes-with-agents-and-passes-structured-output ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) observed-prompt)
      (gptel-runner-fake-queue
       driver 'worker (lambda (call)
                        (setq observed-prompt (gptel-runner-call-prompt call))
                        '(:value "answer")))
      (let ((run (gptel-runner-start
                  (gptel-runner-sequence
                   (gptel-runner-elisp-step
                    :id 'prepare :save-as 'prepared
                    :function (lambda (run _node)
                                (list :count (length (gptel-runner-get run 'items)))))
                   (gptel-runner-agent-step
                    :id 'agent :agent 'worker :save-as 'answer
                    :prompt (lambda (run _node)
                              (format "Count: %s"
                                      (plist-get (gptel-runner-get run 'prepared) :count))))
                   (gptel-runner-elisp-step
                    :id 'process :save-as 'result
                    :function (lambda (run _node)
                                (list :answer (upcase (gptel-runner-get run 'answer))))))
                  :inputs '((items a b)) :driver driver
                  :max-calls 1 :max-requests 1)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal observed-prompt "Count: 2"))
        (should (equal (gptel-runner-get run 'result) '(:answer "ANSWER")))
        (should (= (length (gptel-runner-run-calls run)) 3))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 1))
        (should (= (gptel-runner-budget-calls (gptel-runner-run-budget run)) 1))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 1))))))

(ert-deftest gptel-runner-elisp-accepts-empty-and-structured-values-without-driver ()
  (gptel-runner-elisp-test--isolated
    (dolist (value '(nil "" " " 0 (:ok t) [1 2]))
      (let* ((run (gptel-runner-start
                   (gptel-runner-elisp-step
                    :save-as 'result :function (lambda (_run _node) value))
                   :max-requests 0 :max-calls 0))
             (call (car (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'result 'missing) value))
        (should (equal (gptel-runner-call-result call) value))
        (should (zerop (gptel-runner-call-request-attempt call)))
        (should-not (gptel-runner-call-agent call))
        (should-not (gptel-runner-run-driver run))))))

(ert-deftest gptel-runner-elisp-validates-all-steps-before-executing ()
  (gptel-runner-elisp-test--isolated
    (let (executed)
      (dolist (properties '((:function nil) (:function missing-function)
                            (:function ignore :workspace 42)
                            (:function ignore :workspace-mode invalid)
                            (:function ignore :retries -1)
                            (:function ignore :retries 1.5)
                            (:function ignore :async yes)
                            (:function ignore :timeout 0)
                            (:function ignore :timeout -1)
                            (:function ignore :timeout "1")
                            (:function ignore :timeout 1.0e+INF)
                            (:function ignore :timeout 0.0e+NaN)
                            (:function ignore :pause-after t)
                            (:function ignore :parser identity)
                            (:function ignore :workspace-mode write)))
        (should-error
         (gptel-runner-start
          (gptel-runner-sequence
           (gptel-runner-elisp-step
            :function (lambda (_run _node) (setq executed t)))
           (apply #'gptel-runner-elisp-step properties)))
         :type 'user-error))
      (should-not executed)
      (should (zerop (hash-table-count gptel-runner--runs)))
      (should-error
       (gptel-runner-start
        (gptel-runner-parallel
         (gptel-runner-elisp-step :function #'ignore :save-as 'same)
         (gptel-runner-elisp-step :function #'ignore :save-as 'same)))
       :type 'user-error))))

(ert-deftest gptel-runner-elisp-shares-writer-locks-and-workspace-with-agents ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'writer :workspace-mode 'write)
    (let* ((directory (file-name-as-directory
                       (file-truename gptel-runner-snapshot-directory)))
           (driver (gptel-runner-fake-driver-create))
           executed)
      (gptel-runner-fake-queue driver 'writer '(:manual t))
      (let* ((run (gptel-runner-start
                   (gptel-runner-parallel
                    (gptel-runner-agent-step :agent 'writer :prompt "work")
                    (gptel-runner-elisp-step
                     :workspace "." :workspace-mode 'write :save-as 'local
                     :function (lambda (run node)
                                 (setq executed t)
                                 (should (equal (gptel-runner-node-workspace run node)
                                                directory))
                                 (should (equal default-directory directory))
                                 (with-temp-file "output.txt" (insert "written"))
                                 'written)))
                   :workspace directory :driver driver :max-concurrency 2
                   :allow-writes t))
             (calls (gptel-runner-run-calls run)))
        (should-not executed)
        (should (eq (gptel-runner-call-state (cadr calls)) 'ready))
        (gptel-runner-fake-release driver (car calls) '(:value "agent done"))
        (should executed)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (file-exists-p (expand-file-name "output.txt" directory)))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (zerop (gptel-runner-run-writer-active run)))))))

(ert-deftest gptel-runner-elisp-honors-admission-and-cancels-queued-work ()
  (gptel-runner-elisp-test--isolated
    (let* ((gptel-runner-call-admission-functions (list (lambda (_call) nil)))
           executed
           (run (gptel-runner-start
                 (gptel-runner-elisp-step
                  :function (lambda (_run _node) (setq executed t)))))
           (call (car (gptel-runner-run-calls run))))
      (should (eq (gptel-runner-call-state call) 'ready))
      (should-error (gptel-runner-pause-call call) :type 'user-error)
      (gptel-runner-abort-run run)
      (let ((gptel-runner-call-admission-functions nil))
        (gptel-runner--drain-queue run))
      (should-not executed)
      (should (eq (gptel-runner-call-state call) 'cancelled)))))

(ert-deftest gptel-runner-elisp-fails-fast-and-retries-only-when-requested ()
  (gptel-runner-elisp-test--isolated
    (dolist (retries '(0 1))
      (let* ((attempts 0) (completions 0)
             (run (gptel-runner-start
                   (gptel-runner-sequence
                    (gptel-runner-elisp-step
                     :id 'fail :save-as 'output :retries retries
                     :function (lambda (_run _node)
                                 (cl-incf attempts)
                                 (if (= attempts 1) (error "Expected failure") 42)))
                    (gptel-runner-elisp-step :id 'next :function #'ignore))
                   :callback (lambda (_run) (cl-incf completions)))))
        (should (= attempts (1+ retries)))
        (should (= completions 1))
        (if (zerop retries)
            (progn
              (should (eq (gptel-runner-run-state run) 'failed))
              (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                          'elisp-error))
              (should (eq (gptel-runner-get run 'output 'absent) 'absent))
              (should (eq (gethash 'next (gptel-runner-run-node-states run)) 'skipped)))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= (gptel-runner-get run 'output) 42)))))))

(ert-deftest gptel-runner-elisp-discards-results-after-cancellation-or-quit ()
  (gptel-runner-elisp-test--isolated
    (dolist (quitp '(nil t))
      (let* ((attempts 0) (completions 0)
             (run (gptel-runner-start
                   (gptel-runner-elisp-step
                    :save-as 'output :retries 2
                    :function (lambda (run _node)
                                (cl-incf attempts)
                                (if quitp (signal 'quit nil)
                                  (gptel-runner-abort-run run))
                                'discarded))
                   :callback (lambda (_run) (cl-incf completions)))))
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should (= attempts 1))
        (should (= completions 1))
        (should (eq (gptel-runner-get run 'output 'absent) 'absent))
        (should (zerop (gptel-runner-run-active-count run)))))))

(ert-deftest gptel-runner-elisp-checks-duration-before-publishing-output ()
  (gptel-runner-elisp-test--isolated
    (let ((run (gptel-runner-start
                (gptel-runner-elisp-step
                 :save-as 'output
                 :function (lambda (run _node)
                             ;; Advance the active clock without a timer race.
                             (cl-decf (gptel-runner-run-active-started-at run) 20)
                             'late))
                :max-duration 10)))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind) 'duration))
      (should (eq (gptel-runner-get run 'output 'absent) 'absent)))))

(ert-deftest gptel-runner-elisp-participates-in-branch-repeat-and-history ()
  (gptel-runner-elisp-test--isolated
    (let ((run (gptel-runner-start
                (gptel-runner-repeat-until
                 :id 'loop :max 2 :collect-keys '(count) :save-history-as 'history
                 :until (lambda (run) (equal (gptel-runner-get run 'count) 2))
                 :body (gptel-runner-branch
                        :predicate (lambda (_run) t)
                        :then (gptel-runner-elisp-step
                               :save-as 'count
                               :function (lambda (run _node)
                                           (1+ (gptel-runner-iteration run 'loop))))
                        :else (gptel-runner-elisp-step
                               :function (lambda (_run _node) (error "Skipped"))))))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'history)
                     '((:iteration 1 :values ((count . 1)))
                       (:iteration 2 :values ((count . 2)))))))))

(ert-deftest gptel-runner-elisp-parallel-collects-values-and-failures ()
  (gptel-runner-elisp-test--isolated
    (let ((run (gptel-runner-start
                (gptel-runner-parallel
                 :policy 'collect :save-as 'results
                 (gptel-runner-elisp-step :id 'nil :function #'ignore)
                 (gptel-runner-elisp-step
                  :id 'error :function (lambda (_run _node) (error "Expected")))))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (let ((results (gptel-runner-get run 'results)))
        (should (= (length results) 2))
        (should (eq (plist-get (car results) :state) 'succeeded))
        (should-not (plist-get (car results) :value))
        (should (eq (plist-get (cadr results) :state) 'failed))))))

(ert-deftest gptel-runner-elisp-snapshot-retry-retains-completed-outputs ()
  (gptel-runner-elisp-test--isolated
    (let ((prepared 0) (attempts 0))
      (gptel-runner-defworkflow local-recovery
          (:persist t :max-calls 0 :max-requests 0)
        (gptel-runner-sequence
         :id 'root
         (gptel-runner-elisp-step
          :id 'prepare :save-as 'prepared
          :function (lambda (_run _node) (cl-incf prepared)))
         (gptel-runner-elisp-step
          :id 'work :save-as 'result
          :function (lambda (run _node)
                      (cl-incf attempts)
                      (if (= attempts 1) (error "Expected")
                        (1+ (gptel-runner-get run 'prepared)))))))
      (let ((run (gptel-runner-start 'local-recovery)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (gptel-runner-store-flush-pending)
        (remhash (gptel-runner-run-id run) gptel-runner--runs)
        (let ((restored (gptel-runner-load-run (gptel-runner-run-snapshot-file run))))
          (gptel-runner-retry restored :from-node 'work)
          (should (eq (gptel-runner-run-state restored) 'succeeded))
          (should (= prepared 1))
          (should (= (gptel-runner-get restored 'result) 2))
          (gptel-runner-continue restored "Recompute")
          (should (eq (gptel-runner-run-state restored) 'succeeded))
          (should (= prepared 2))
          (should (= (gptel-runner-get restored 'result) 3))
          (should (equal (plist-get (car (gptel-runner-continuations restored)) :results)
                         '((prepared . 1) (result . 2)))))))))

(ert-deftest gptel-runner-elisp-pause-discards-inflight-result-and-resumes ()
  (gptel-runner-elisp-test--isolated
    (let ((attempts 0))
      (gptel-runner-defworkflow local-pause nil
        (gptel-runner-elisp-step
         :id 'work :save-as 'result
         :function (lambda (run _node)
                     (when (= (cl-incf attempts) 1)
                       (gptel-runner-pause-run run))
                     attempts)))
      (let ((run (gptel-runner-start 'local-pause)))
        (should (eq (gptel-runner-run-state run) 'paused))
        (should (eq (gptel-runner-get run 'result 'absent) 'absent))
        (gptel-runner-resume-run run)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-get run 'result) 2))))))

(ert-deftest gptel-runner-elisp-async-holds-slots-and-writer-lock-until-cleanup ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'writer :workspace-mode 'write)
    (let ((driver (gptel-runner-fake-driver-create)) resolve read-resolve cleaned)
      (gptel-runner-fake-queue driver 'writer '(:value "agent done"))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-parallel
                :save-as 'results
                (gptel-runner-elisp-step
                 :async t :timeout 3600 :workspace-mode 'write :save-as 'local
                 :function
                 (lambda (run node complete _reject)
                   (setq resolve complete)
                   (should (equal default-directory
                                  (gptel-runner-node-workspace run node)))
                   (lambda (reason)
                     (should (eq reason 'succeeded))
                     (should (= (gptel-runner-run-writer-active run) 1))
                     (should-not (gptel-runner-fake-driver-starts driver))
                     (should (equal default-directory
                                    (gptel-runner-node-workspace run node)))
                     (setq cleaned t))))
                (gptel-runner-agent-step :agent 'writer :prompt "work")
                (gptel-runner-elisp-step
                 :async t
                 :function (lambda (_run _node complete _reject)
                             (setq read-resolve complete) nil)))
               :driver driver :max-concurrency 2 :allow-writes t
               :max-calls 1 :max-requests 1))
             (call (car (gptel-runner-run-calls run)))
             (timer (gptel-runner-call-elisp-timer call)))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= (gptel-runner-run-active-count run) 2))
        (should-not (gptel-runner-fake-driver-starts driver))
        (should (funcall resolve nil))
        (should cleaned)
        (should-not (memq timer timer-list))
        (should-not (gptel-runner-call-elisp-timer call))
        (should-not (funcall resolve 'duplicate))
        (should-not (gptel-runner-get run 'local 'absent))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 1))
        (funcall read-resolve 'read)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (zerop (gptel-runner-run-writer-active run)))))))

(ert-deftest gptel-runner-elisp-async-inline-first-result-wins ()
  (gptel-runner-elisp-test--isolated
    (dolist (fail '(nil t))
      (let (cleaned resolve reject)
        (let ((run
               (gptel-runner-start
                (gptel-runner-sequence
                 (gptel-runner-elisp-step
                  :async t :save-as 'result
                  :function
                  (lambda (_run _node complete failure)
                    (setq resolve complete reject failure)
                    (should (funcall (if fail failure complete) 'first))
                    (should-not (funcall failure 'second))
                    (should-not (funcall complete 'third))
                    (lambda (reason)
                      (should (eq reason (if fail 'failed 'succeeded)))
                      (should-not (funcall complete 'from-cleanup))
                      (setq cleaned t))))
                 (gptel-runner-elisp-step
                  :function (lambda (_run _node) (should cleaned)))))))
          (should cleaned)
          (should (eq (gptel-runner-run-state run) (if fail 'failed 'succeeded)))
          (should-not (funcall resolve 'late))
          (should-not (funcall reject 'late))
          (if fail
              (should (equal (gptel-runner-run-terminal-data run)
                             '(:type elisp-error :error first)))
            (should (eq (gptel-runner-get run 'result) 'first))))))))

(ert-deftest gptel-runner-elisp-async-startup-errors-and-quits ()
  (gptel-runner-elisp-test--isolated
    (dolist (behavior '(error quit invalid complete-then-error))
      (let ((run
             (gptel-runner-start
              (gptel-runner-elisp-step
               :async t :timeout 3600 :save-as 'result
               :function
               (lambda (_run _node resolve _reject)
                 (pcase behavior
                   ('error (error "startup"))
                   ('quit (signal 'quit nil))
                   ('invalid 'not-a-function)
                   (_ (funcall resolve 42) (error "too late"))))))))
        (should (eq (gptel-runner-run-state run)
                    (pcase behavior ('quit 'cancelled)
                           ('complete-then-error 'succeeded) (_ 'failed))))
        (should-not (gptel-runner-call-elisp-timer
                     (car (gptel-runner-run-calls run))))
        (when (eq behavior 'complete-then-error)
          (should (= (gptel-runner-get run 'result) 42)))))))

(ert-deftest gptel-runner-elisp-async-retry-cleans-before-new-attempt ()
  (gptel-runner-elisp-test--isolated
    (let (callbacks reasons)
      (let ((run
             (gptel-runner-start
              (gptel-runner-elisp-step
               :async t :retries 1 :save-as 'result
               :function
               (lambda (_run _node resolve reject)
                 (when callbacks (should (equal reasons '(failed))))
                 (push (cons resolve reject) callbacks)
                 (lambda (reason) (push reason reasons))))
              :max-calls 0 :max-requests 0)))
        (let ((first (car callbacks)))
          (funcall (cdr first) '(error "rejected"))
          (should (= (length callbacks) 2))
          (should-not (funcall (car first) 'stale))
          (should-not (funcall (cdr first) 'duplicate))
          (should (eq (gptel-runner-run-state run) 'running))
          (funcall (caar callbacks) 42)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= (gptel-runner-get run 'result) 42))
          (should (equal reasons '(succeeded failed))))))))

(ert-deftest gptel-runner-elisp-async-cancellation-cleans-and-ignores-callbacks ()
  (gptel-runner-elisp-test--isolated
    (dolist (during-start '(nil t))
      (let (resolve reject reasons (completions 0))
        (let* ((run
                (gptel-runner-start
                 (gptel-runner-elisp-step
                  :async t :timeout 3600 :retries 2 :save-as 'result
                  :function
                  (lambda (run _node complete failure)
                    (setq resolve complete reject failure)
                    (when during-start (gptel-runner-abort-run run))
                    (lambda (reason)
                      (push reason reasons)
                      (should-not (funcall complete 'from-cleanup)))))
                 :callback (lambda (_run) (cl-incf completions))))
               (call (car (gptel-runner-run-calls run))))
          (gptel-runner-abort-call call)
          (gptel-runner-abort-run run)
          (should (equal reasons '(cancelled)))
          (should (= completions 1))
          (should (= (length (gptel-runner-run-calls run)) 1))
          (should-not (funcall resolve 'late))
          (should-not (funcall reject 'late))
          (should-not (gptel-runner-call-elisp-timer call))
          (should (eq (gptel-runner-get run 'result 'absent) 'absent))
          (should (zerop (gptel-runner-run-active-count run))))))))

(ert-deftest gptel-runner-elisp-async-pause-restarts-and-invalidates-old-attempt ()
  (gptel-runner-elisp-test--isolated
    (dolist (during-start '(nil t))
      (let (callbacks reasons)
        (gptel-runner-defworkflow async-pause nil
          (gptel-runner-elisp-step
           :id 'work :async t :timeout 3600 :save-as 'result
           :function
           (lambda (run _node resolve reject)
             (push (cons resolve reject) callbacks)
             (when (and during-start (= (length callbacks) 1))
               (gptel-runner-pause-run run))
             (lambda (reason) (push reason reasons)))))
        (let* ((run (gptel-runner-start 'async-pause))
               (old (car callbacks))
               (call (car (gptel-runner-run-calls run))))
          (gptel-runner-pause-run run)
          (should (equal reasons '(paused)))
          (should-not (gptel-runner-call-elisp-timer call))
          (should (zerop (gptel-runner-run-active-count run)))
          (should-not (funcall (car old) 'while-paused))
          (gptel-runner-resume-run run)
          (should-not (funcall (car old) 'after-resume))
          (should-not (funcall (cdr old) 'after-resume))
          (funcall (caar callbacks) 'fresh)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (eq (gptel-runner-get run 'result) 'fresh))
          (should (equal reasons '(succeeded paused))))))))

(ert-deftest gptel-runner-elisp-async-timeout-retries-and-ignores-stale-timer ()
  (gptel-runner-elisp-test--isolated
    (let (callbacks reasons)
      (let* ((run
              (gptel-runner-start
               (gptel-runner-elisp-step
                :async t :timeout 3600 :retries 1 :save-as 'result
                :function
                (lambda (_run _node resolve _reject)
                  (push resolve callbacks)
                  (lambda (reason) (push reason reasons))))))
             (call (car (gptel-runner-run-calls run)))
             (timer (gptel-runner-call-elisp-timer call)))
        ;; Drive the actual timer callback deterministically, without sleeping.
        (apply (timer--function timer) (timer--args timer))
        (should (equal reasons '(timeout)))
        (should (equal (gptel-runner-call-error call)
                       '(:type elisp-timeout :timeout 3600)))
        (should-not (memq timer timer-list))
        (should-not (funcall (cadr callbacks) 'late))
        (apply (timer--function timer) (timer--args timer))
        (should (= (length callbacks) 2))
        (funcall (car callbacks) 'fresh)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal reasons '(succeeded timeout)))))))

(ert-deftest gptel-runner-elisp-timeout-checks-clock-before-accepting-result ()
  (gptel-runner-elisp-test--isolated
    (dolist (async '(nil t))
      (let (resolve reasons)
        (let ((run
               (gptel-runner-start
                (gptel-runner-elisp-step
                 :async async :timeout 3600 :save-as 'result
                 :function
                 (lambda (run _node &optional complete _reject)
                   ;; Simulate CPU work that prevented the timer from firing.
                   (setf (gptel-runner-call-elisp-deadline
                          (car (gptel-runner-run-calls run))) 0)
                   (if async
                       (progn (setq resolve complete)
                              (lambda (reason) (push reason reasons)))
                     'late))))))
          (when async (funcall resolve 'late))
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                      'elisp-timeout))
          (should (eq (gptel-runner-get run 'result 'absent) 'absent))
          (when async (should (equal reasons '(timeout)))))))))

(ert-deftest gptel-runner-elisp-async-cleanup-errors-do-not-interrupt-teardown ()
  (gptel-runner-elisp-test--isolated
    (dolist (condition '(error quit))
      (let (resolve)
        (let ((run
               (gptel-runner-start
                (gptel-runner-elisp-step
                 :async t :timeout 3600
                 :function (lambda (_run _node complete _reject)
                             (setq resolve complete)
                             (lambda (_reason) (signal condition nil)))))))
          (funcall resolve 'ok)
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (zerop (gptel-runner-run-active-count run)))
          (should (= (cl-count 'elisp-cleanup-error
                               (gptel-runner-run-events run)
                               :key #'gptel-runner-event-type) 1)))))))

(ert-deftest gptel-runner-elisp-async-run-duration-cleans-active-work ()
  (gptel-runner-elisp-test--isolated
    (let (resolve reasons)
      (let ((run
             (gptel-runner-start
              (gptel-runner-elisp-step
               :async t :timeout 3600
               :function (lambda (_run _node complete _reject)
                           (setq resolve complete)
                           (lambda (reason) (push reason reasons))))
              :max-duration 3600)))
        (gptel-runner--duration-expired run (gptel-runner-run-generation run))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get (gptel-runner-run-terminal-data run) :kind) 'duration))
        (should (equal reasons '(cancelled)))
        (should-not (funcall resolve 'late))
        (should-not (gptel-runner-call-elisp-timer
                     (car (gptel-runner-run-calls run))))))))

(ert-deftest gptel-runner-elisp-async-timeout-recovery-from-snapshot ()
  (gptel-runner-elisp-test--isolated
    (let (resolve)
      (gptel-runner-defworkflow async-retry
          (:persist t :max-calls 0 :max-requests 0)
        (gptel-runner-elisp-step
         :id 'work :async t :timeout 3600 :save-as 'result
         :function (lambda (_run _node complete _reject)
                     (setq resolve complete) nil)))
      (let* ((run (gptel-runner-start 'async-retry))
             (old resolve)
             (timer (gptel-runner-call-elisp-timer
                     (car (gptel-runner-run-calls run)))))
        (apply (timer--function timer) (timer--args timer))
        (gptel-runner-store-flush-pending)
        (remhash (gptel-runner-run-id run) gptel-runner--runs)
        (let ((restored (gptel-runner-load-run (gptel-runner-run-snapshot-file run))))
          (gptel-runner-retry restored :from-node 'work)
          (should (eq (gptel-runner-run-state restored) 'running))
          (should-not (funcall old 'late))
          (funcall resolve 'fresh)
          (should (eq (gptel-runner-run-state restored) 'succeeded))
          (should (eq (gptel-runner-get restored 'result) 'fresh)))))))

(ert-deftest gptel-runner-elisp-async-timeout-is-catchable ()
  (gptel-runner-elisp-test--isolated
    (let* ((run
            (gptel-runner-start
             (gptel-runner-try
              :error-as 'failure :save-as 'result
              :body (gptel-runner-elisp-step
                     :id 'work :async t :timeout 3600 :function #'ignore)
              :catch (gptel-runner-elisp-step
                      :function (lambda (_run _node) 'recovered)))))
           (timer (gptel-runner-call-elisp-timer
                   (car (gptel-runner-run-calls run)))))
      (apply (timer--function timer) (timer--args timer))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (eq (gptel-runner-get run 'result) 'recovered))
      (should (equal (gptel-runner-get run 'failure)
                     '(:node work :state failed
                       :value (:type elisp-timeout :timeout 3600)))))))

(provide 'gptel-runner-elisp-test)
;;; gptel-runner-elisp-test.el ends here
