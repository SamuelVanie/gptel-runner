;;; gptel-runner-cancellation-test.el --- Parallel teardown tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-cancellation-test--isolated (&rest body)
  "Run BODY with private runner registries and snapshot storage."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner-default-driver nil)
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner-snapshot-directory (make-temp-file "runner-cancellation-" t)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run) (gptel-runner-abort-run run))
                gptel-runner--runs)
       (maphash (lambda (run _coordinator) (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(defun gptel-runner-cancellation-test--step (id)
  "Make a fake agent step with ID."
  (gptel-runner-agent-step :id id :agent 'worker :prompt "work"))

(defun gptel-runner-cancellation-test--call (run id)
  "Find the call in RUN whose node has ID."
  (cl-find id (gptel-runner-run-calls run)
           :key (lambda (call) (gptel-runner-node-id
                               (gptel-runner-call-node call)))))

(defun gptel-runner-cancellation-test--starts (driver)
  "Return node IDs started by fake DRIVER in order."
  (mapcar (lambda (call) (gptel-runner-node-id (gptel-runner-call-node call)))
          (gptel-runner-fake-driver-starts driver)))

(ert-deftest gptel-runner-cancellation-fail-fast-prevents-queued-writes ()
  (gptel-runner-cancellation-test--isolated
    (let ((output (expand-file-name "unexpected-write"
                                    gptel-runner-snapshot-directory))
          reject-a started cleanup-reasons)
      (let* ((run
              (gptel-runner-start
               (gptel-runner-parallel
                :id 'group :policy 'fail-fast
                (gptel-runner-elisp-step
                 :id 'a :async t :function
                 (lambda (_run _node _resolve reject)
                   (push 'a started) (setq reject-a reject) nil))
                (gptel-runner-elisp-step
                 :id 'b :async t :function
                 (lambda (_run _node _resolve _reject)
                   (push 'b started)
                   (lambda (reason) (push reason cleanup-reasons))))
                (gptel-runner-elisp-step
                 :id 'c :workspace-mode 'write :function
                 (lambda (_run _node)
                   (push 'c started)
                   (with-temp-file output (insert "should never execute")))))
               :allow-writes t :max-concurrency 2))
             (queued (gptel-runner-cancellation-test--call run 'c)))
        (should (equal (reverse started) '(a b)))
        (should (equal (gptel-runner-run-queue run) (list queued)))
        (funcall reject-a 'failure)
        (should (equal (reverse started) '(a b)))
        (should-not (file-exists-p output))
        (should (equal cleanup-reasons '(cancelled)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (gptel-runner-call-state queued) 'cancelled))
        (should-not (gptel-runner-call-started-at queued))
        (should-not (gptel-runner-run-queue run))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (zerop (gptel-runner-run-writer-active run)))))))

(ert-deftest gptel-runner-cancellation-minimum-impossible-keeps-queued-agents-idle ()
  (gptel-runner-cancellation-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker
                               '(:manual t) '(:manual t) '(:value "too late"))
      (let* ((run (gptel-runner-start
                   (gptel-runner-parallel
                    :id 'group :policy 'minimum-successes :minimum-successes 3
                    (gptel-runner-cancellation-test--step 'a)
                    (gptel-runner-cancellation-test--step 'b)
                    (gptel-runner-cancellation-test--step 'c))
                   :driver driver :max-concurrency 2))
             (a (gptel-runner-cancellation-test--call run 'a))
             (c (gptel-runner-cancellation-test--call run 'c)))
        (should (equal (gptel-runner-cancellation-test--starts driver) '(a b)))
        (gptel-runner-fake-release driver a '(:status permanent :value "failed"))
        (should (equal (gptel-runner-cancellation-test--starts driver) '(a b)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get (gptel-runner-run-terminal-data run) :type)
                    'minimum-successes))
        (should (eq (gptel-runner-call-state c) 'cancelled))
        (should-not (gptel-runner-call-started-at c))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 2))
        (should-not (gptel-runner-run-queue run))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (zerop (gptel-runner-fake-driver-active driver)))))))

(ert-deftest gptel-runner-cancellation-nested-collect-cannot-advance-sequence ()
  (gptel-runner-cancellation-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (policy '(fail-fast minimum-successes))
      (let ((driver (gptel-runner-fake-driver-create)) advanced)
        (gptel-runner-fake-queue driver 'worker
                                 '(:manual t) '(:manual t) '(:manual t))
        (let ((run
               (gptel-runner-start
                (gptel-runner-parallel
                 :id 'group :policy policy :minimum-successes 2
                 (gptel-runner-cancellation-test--step 'a)
                 (gptel-runner-sequence
                  :id 'sibling
                  (gptel-runner-parallel
                   :id 'inner :policy 'collect
                   (gptel-runner-cancellation-test--step 'b)
                   (gptel-runner-cancellation-test--step 'c))
                  ;; Predicates execute outside the call queue: merely
                  ;; deferring queue draining cannot prevent this effect.
                  (gptel-runner-branch
                   :id 'next :predicate (lambda (_run) (setq advanced t))
                   :then (gptel-runner-elisp-step :id 'write :function #'ignore))))
                :driver driver :max-concurrency 3)))
          (gptel-runner-fake-release
           driver (gptel-runner-cancellation-test--call run 'a)
           '(:status permanent :value "failed"))
          (should-not advanced)
          (should-not (gptel-runner-cancellation-test--call run 'write))
          (should (eq (gptel-runner-run-state run) 'failed))
          (dolist (id '(sibling inner b c))
            (should (eq (gptel-runner-node-state run id) 'cancelled)))
          (dolist (id '(next write))
            (should (eq (gptel-runner-node-state run id) 'skipped)))
          (should-not (gptel-runner-run-queue run))
          (should (zerop (gptel-runner-run-active-count run))))))))

(ert-deftest gptel-runner-cancellation-preserves-unrelated-branches-and-queue ()
  (gptel-runner-cancellation-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker
                               '(:manual t) '(:manual t) '(:manual t)
                               '(:manual t) '(:manual t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-parallel
                :id 'outer :policy 'collect
                (gptel-runner-cancellation-test--step 'independent)
                (gptel-runner-parallel
                 :id 'group :policy 'fail-fast
                 (gptel-runner-cancellation-test--step 'a)
                 (gptel-runner-cancellation-test--step 'b)
                 (gptel-runner-cancellation-test--step 'c))
                (gptel-runner-cancellation-test--step 'queued-independent))
               :driver driver :max-concurrency 3))
             (independent (gptel-runner-cancellation-test--call run 'independent))
             (queued (gptel-runner-cancellation-test--call run 'queued-independent)))
        (should (equal (gptel-runner-cancellation-test--starts driver)
                       '(independent a b)))
        (gptel-runner-fake-release
         driver (gptel-runner-cancellation-test--call run 'a)
         '(:status permanent :value "failed"))
        (should (equal (gptel-runner-cancellation-test--starts driver)
                       '(independent a b queued-independent)))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (eq (gptel-runner-call-state independent) 'running))
        (should (eq (gptel-runner-call-state queued) 'running))
        (should (eq (gptel-runner-node-state run 'group) 'failed))
        (should-not (gptel-runner-run-queue run))
        (gptel-runner-fake-release driver independent '(:value "unaffected"))
        (gptel-runner-fake-release driver queued '(:value "progress"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (zerop (gptel-runner-fake-driver-active driver)))))))

(ert-deftest gptel-runner-cancellation-defers-dispatch-through-parent-failure ()
  (gptel-runner-cancellation-test--isolated
    (gptel-runner-register-agent 'worker)
    (dolist (policy '(fail-fast minimum-successes))
      (let ((driver (gptel-runner-fake-driver-create)) executed)
        (gptel-runner-fake-queue driver 'worker '(:manual t) '(:manual t))
        (let ((run
               (gptel-runner-start
                (gptel-runner-parallel
                 :id 'outer :policy policy :minimum-successes 2
                 (gptel-runner-parallel
                  :id 'inner
                  (gptel-runner-cancellation-test--step 'a)
                  (gptel-runner-cancellation-test--step 'b))
                 (gptel-runner-elisp-step
                  :id 'c :function (lambda (_run _node) (setq executed t))))
                :driver driver :max-concurrency 2)))
          (gptel-runner-fake-release
           driver (gptel-runner-cancellation-test--call run 'a)
           '(:status permanent :value "failed"))
          (should-not executed)
          (should (eq (gptel-runner-run-state run) 'failed))
          (should (eq (gptel-runner-node-state run 'inner) 'failed))
          (should (eq (gptel-runner-node-state run 'c) 'cancelled))
          (should-not (gptel-runner-run-queue run))
          (should (zerop (gptel-runner-run-active-count run))))))))

(ert-deftest gptel-runner-cancellation-invalidates-all-callbacks-before-cleanup ()
  (gptel-runner-cancellation-test--isolated
    (let (reject-a resolve-c other-run)
      (let ((run
             (gptel-runner-start
              (gptel-runner-parallel
               :id 'group
               (gptel-runner-elisp-step
                :id 'a :async t :function
                (lambda (_run _node _resolve reject) (setq reject-a reject) nil))
               (gptel-runner-elisp-step
                :id 'b :async t :function
                (lambda (_run _node _resolve _reject)
                  (lambda (_reason)
                    (funcall resolve-c 'obsolete)
                    (setq other-run
                          (gptel-runner-start
                           (gptel-runner-elisp-step :function #'ignore))))))
               (gptel-runner-elisp-step
                :id 'c :save-as 'result :async t :function
                (lambda (_run _node resolve _reject) (setq resolve-c resolve) nil)))
              :max-concurrency 3)))
        (funcall reject-a 'failure)
        (should (eq (gptel-runner-call-state
                     (gptel-runner-cancellation-test--call run 'c)) 'cancelled))
        (should (eq (gptel-runner-get run 'result 'absent) 'absent))
        (should (eq (gptel-runner-node-state run 'c) 'cancelled))
        (should (eq (gptel-runner-run-state other-run) 'succeeded))
        (funcall resolve-c 'late)
        (should (eq (gptel-runner-get run 'result 'absent) 'absent))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (zerop (gptel-runner-run-active-count run)))))))

(provide 'gptel-runner-cancellation-test)
;;; gptel-runner-cancellation-test.el ends here
