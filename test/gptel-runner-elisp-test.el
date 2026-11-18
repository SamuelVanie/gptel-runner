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

(provide 'gptel-runner-elisp-test)
;;; gptel-runner-elisp-test.el ends here
