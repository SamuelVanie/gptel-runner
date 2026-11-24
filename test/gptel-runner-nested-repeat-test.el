;;; gptel-runner-nested-repeat-test.el --- Nested repeat regressions -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(cl-defun gptel-runner-nested-repeat-test--flow
    (&key (outer-max 2) (inner-max 2) inner-until progress-key)
  "Make two nested loops with OUTER-MAX and INNER-MAX iteration limits.
INNER-UNTIL defaults to two completed inner iterations.  PROGRESS-KEY
optionally tracks inner progress.  Both loops collect the worker's result."
  (gptel-runner-repeat-until
   :id 'outer :max outer-max
   :until (lambda (run) (= (gptel-runner-iteration run 'outer) 2))
   :collect-keys '(result) :save-history-as 'outer-history
   :body (gptel-runner-sequence
          :id 'outer-body
          (gptel-runner-repeat-until
           :id 'inner :max inner-max
           :until (or inner-until
                      (lambda (run) (= (gptel-runner-iteration run 'inner) 2)))
           :progress-key progress-key
           :collect-keys '(result) :save-history-as 'inner-history
           :body (gptel-runner-agent-step
                  :id 'work :agent 'worker :prompt "work" :save-as 'result)))))

(defun gptel-runner-nested-repeat-test--assert-history (run)
  "Assert RUN collected all four results with local iteration numbering."
  (should
   (equal (gptel-runner-get run 'inner-history)
          '((:iteration 1 :values ((result . "one")))
            (:iteration 2 :values ((result . "two")))
            (:iteration 1 :values ((result . "three")))
            (:iteration 2 :values ((result . "four"))))))
  (should
   (equal (gptel-runner-get run 'outer-history)
          '((:iteration 1 :values ((result . "two")))
            (:iteration 2 :values ((result . "four")))))))

(ert-deftest gptel-runner-nested-repeat-runs-two-by-two ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)) observed)
      (gptel-runner-fake-queue
       driver 'worker '(:value "one") '(:value "two")
       '(:value "three") '(:value "four"))
      (let ((run
             (gptel-runner-start
              (gptel-runner-nested-repeat-test--flow
               :inner-until
               (lambda (run)
                 (let ((inner (gptel-runner-iteration run 'inner)))
                   (push (list (gptel-runner-iteration run 'outer) inner) observed)
                   (= inner 2)))
               :progress-key (lambda (_run) 'same))
              :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 4))
        (should (equal (nreverse observed)
                       '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))))
        (should (equal (mapcar #'gptel-runner-call-iteration
                               (gptel-runner-run-calls run))
                       '(1 2 1 2)))
        (should (= (gptel-runner-iteration run 'outer) 2))
        (should (= (gptel-runner-iteration run 'inner) 2))
        (gptel-runner-nested-repeat-test--assert-history run)))))

(ert-deftest gptel-runner-nested-repeat-collects-only-actual-iteration-writes ()
  (gptel-runner-elisp-test--isolated
    (let* ((inner
            (gptel-runner-repeat-until
             :id 'inner :max 2
             :until (lambda (run)
                      (or (= (gptel-runner-iteration run 'outer) 1)
                          (= (gptel-runner-iteration run 'inner) 2)))
             :collect-keys '(answer) :save-history-as 'inner-history
             :body (gptel-runner-branch
                    :predicate (lambda (run) (zerop (gptel-runner-iteration run 'inner)))
                    :then (gptel-runner-elisp-step :function #'ignore :save-as 'answer))))
           (run (gptel-runner-start
                 (gptel-runner-repeat-until
                  :id 'outer :max 2 :body inner
                  :until (lambda (run) (= (gptel-runner-iteration run 'outer) 2))
                  :collect-keys '(answer inner-history) :save-history-as 'outer-history)))
           (inner-history '((:iteration 1 :values ((answer)))
                            (:iteration 2 :values nil))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'inner-history) inner-history))
      (should (equal (gptel-runner-get run 'outer-history)
                     `((:iteration 1 :values ((answer) (inner-history . ,inner-history)))
                       (:iteration 2 :values nil)))))))

(ert-deftest gptel-runner-nested-repeat-clears-progress-between-executions ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let* ((driver (gptel-runner-fake-driver-create))
           (root (gptel-runner-nested-repeat-test--flow
                  :inner-max 4 :progress-key (lambda (_run) 'same)
                  :inner-until (lambda (run)
                                 (member (gptel-runner-get run 'result)
                                         '("two" "four")))))
           (body (plist-get (gptel-runner-node-properties root) :body)))
      ;; The predicate uses current results, independently of iteration counts.
      (push (gptel-runner-elisp-step
             :id 'prepare :save-as 'result :function #'ignore)
            (gptel-runner-node-children body))
      (gptel-runner-fake-queue
       driver 'worker '(:value "one") '(:value "two")
       '(:value "three") '(:value "four"))
      (let ((run (gptel-runner-start root :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 4))
        (gptel-runner-nested-repeat-test--assert-history run)))))

(ert-deftest gptel-runner-nested-repeat-extends-inner-without-losing-progress ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'worker '(:value "one") '(:manual t :value "two")
       '(:value "three") '(:value "four"))
      (let* ((root (gptel-runner-nested-repeat-test--flow :inner-max 1))
             (run (gptel-runner-start root :driver driver)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (equal (gptel-runner-run-terminal-data run)
                       '(:type iteration-budget :max 1)))
        (should (= (gptel-runner-iteration run 'outer) 0))
        (should (= (gptel-runner-iteration run 'inner) 1))
        (gptel-runner-extend-repeat run 'inner 1)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= (gptel-runner-iteration run 'inner) 1))
        (gptel-runner-fake-release
         driver (car (last (gptel-runner-fake-driver-starts driver))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 4))
        (should (= (gethash 'inner (gptel-runner-run-repeat-limits run)) 2))
        (should (= (plist-get (gptel-runner-node-properties
                              (gptel-runner--find-node root 'inner)) :max) 1))
        (gptel-runner-nested-repeat-test--assert-history run)))))

(ert-deftest gptel-runner-nested-repeat-extends-outer-with-fresh-inner ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'worker '(:value "one") '(:value "two")
       '(:manual t :value "three") '(:value "four"))
      (let ((run (gptel-runner-start
                  (gptel-runner-nested-repeat-test--flow :outer-max 1)
                  :driver driver)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (= (gptel-runner-iteration run 'outer) 1))
        (should (= (gptel-runner-iteration run 'inner) 2))
        (gptel-runner-extend-repeat run 'outer 1)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= (gptel-runner-iteration run 'outer) 1))
        (should (= (gptel-runner-iteration run 'inner) 0))
        (gptel-runner-fake-release
         driver (car (last (gptel-runner-fake-driver-starts driver))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 4))
        (gptel-runner-nested-repeat-test--assert-history run)))))

(ert-deftest gptel-runner-nested-repeat-recovers-incomplete-inner-iteration ()
  (dolist (recovery '(resume retry retry-from-node))
    (ert-info ((format "Recovery: %s" recovery))
      (gptel-runner-elisp-test--isolated
        (gptel-runner-register-agent 'worker)
        (gptel-runner-defworkflow nested-repeat-recovery ()
          (gptel-runner-nested-repeat-test--flow
           :progress-key (lambda (_run) 'same)))
        (let ((driver (gptel-runner-fake-driver-create)))
          (gptel-runner-fake-queue
           driver 'worker '(:value "one")
           (if (eq recovery 'resume) '(:manual t)
             '(:status permanent :value "failed"))
           '(:manual t :value "two") '(:value "three") '(:value "four"))
          (let ((run (gptel-runner-start 'nested-repeat-recovery :driver driver)))
            (should (= (gptel-runner-iteration run 'inner) 1))
            (should-not (gptel-runner--subtree-state-p
                         run (gptel-runner-workflow-root
                              (gptel-runner-run-workflow run)) 'succeeded))
            (pcase recovery
              ('resume
               (gptel-runner-pause-run run)
               (gptel-runner-resume-run run))
              ('retry (gptel-runner-retry run))
              ('retry-from-node (gptel-runner-retry run :from-node 'work)))
            (should (eq (gptel-runner-run-state run) 'running))
            (should (= (gptel-runner-iteration run 'inner) 1))
            (should (= (gptel-runner-iteration run 'outer) 0))
            (should (eq (gptel-runner-get run '(gptel-runner-progress inner))
                        'same))
            (gptel-runner-fake-release
             driver (car (last (gptel-runner-fake-driver-starts driver))))
            (should (eq (gptel-runner-run-state run) 'succeeded))
            (should (= (length (gptel-runner-fake-driver-starts driver)) 5))
            (should-not (gptel-runner-get run '(gptel-runner-retry-repeat inner)))
            (should-not (gptel-runner-get run '(gptel-runner-retry-repeat outer)))
            (gptel-runner-nested-repeat-test--assert-history run)))))))

(ert-deftest gptel-runner-nested-repeat-resumes-from-snapshot ()
  (gptel-runner-elisp-test--isolated
    (gptel-runner-register-agent 'worker)
    (gptel-runner-defworkflow nested-repeat-recovery ()
      (gptel-runner-nested-repeat-test--flow
       :progress-key (lambda (_run) 'same)))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "one") '(:manual t))
      (let ((run (gptel-runner-start 'nested-repeat-recovery :driver driver)))
        (gptel-runner-pause-run run)
        (gptel-runner-save-run run)
        (gptel-runner-store-flush-pending)
        (gptel-runner-forget-run run)
        (let* ((file (gptel-runner-run-snapshot-file run))
               (restored-driver (gptel-runner-fake-driver-create))
               (restored (gptel-runner-load-run file nil restored-driver)))
          (gptel-runner-fake-queue
           restored-driver 'worker '(:manual t :value "two")
           '(:value "three") '(:value "four"))
          (gptel-runner-resume-run restored)
          (should (= (gptel-runner-iteration restored 'inner) 1))
          (should (eq (gptel-runner-get restored '(gptel-runner-progress inner))
                      'same))
          (gptel-runner-fake-release
           restored-driver (car (gptel-runner-fake-driver-starts restored-driver)))
          (should (eq (gptel-runner-run-state restored) 'succeeded))
          (should (= (length (gptel-runner-fake-driver-starts restored-driver)) 3))
          (gptel-runner-nested-repeat-test--assert-history restored))))))

(provide 'gptel-runner-nested-repeat-test)
;;; gptel-runner-nested-repeat-test.el ends here
