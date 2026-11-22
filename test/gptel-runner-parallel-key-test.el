;;; gptel-runner-parallel-key-test.el --- Parallel key validation -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(defun gptel-runner-parallel-key-test--step (id key value)
  "Return an Elisp step with ID saving VALUE under KEY."
  (gptel-runner-elisp-step
   :id id :save-as key :function (lambda (_run _node) value)))

(ert-deftest gptel-runner-parallel-key-allows-branch-alternatives ()
  (dolist (choose-then '(t nil))
    (gptel-runner-elisp-test--isolated
      (let ((run
             (gptel-runner-start
              (gptel-runner-parallel
               (gptel-runner-branch
                :predicate (lambda (_run) choose-then)
                :then (gptel-runner-parallel-key-test--step 'left 'selected 1)
                :else (gptel-runner-parallel-key-test--step 'right 'selected 2))
               (gptel-runner-parallel-key-test--step 'other 'other 3)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-get run 'selected) (if choose-then 1 2)))
        (should (= (gptel-runner-get run 'other) 3))
        (should (eq (gptel-runner-node-state run (if choose-then 'right 'left))
                    'skipped))))))

(ert-deftest gptel-runner-parallel-key-allows-sequential-overwrites ()
  (gptel-runner-elisp-test--isolated
    (let ((run
           (gptel-runner-start
            (gptel-runner-parallel
             (gptel-runner-sequence
              (gptel-runner-parallel-key-test--step
               'first (copy-sequence "selected") 1)
              (gptel-runner-parallel-key-test--step
               'second (copy-sequence "selected") 2))
             (gptel-runner-parallel-key-test--step 'other 'other 3)))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (gptel-runner-get run "selected") 2))
      (should (= (gptel-runner-get run 'other) 3))
      (should (eq (gptel-runner-node-state run 'first) 'succeeded))
      (should (eq (gptel-runner-node-state run 'second) 'succeeded)))))

(ert-deftest gptel-runner-parallel-key-allows-recovery-alternatives ()
  (dolist (fail '(nil t))
    (gptel-runner-elisp-test--isolated
      (let ((run
             (gptel-runner-start
              (gptel-runner-parallel
               (gptel-runner-try
                :save-as 'selected :error-as 'failure
                :body (gptel-runner-elisp-step
                       :id 'body :save-as 'selected
                       :function (lambda (_run _node)
                                   (when fail (error "Recover this failure"))
                                   1))
                :catch (gptel-runner-parallel-key-test--step 'catch 'selected 2))
               (gptel-runner-parallel-key-test--step 'other 'other 3)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-get run 'selected) (if fail 2 1)))
        (should (= (gptel-runner-get run 'other) 3))
        (should (eq (gptel-runner-node-state run 'catch)
                    (if fail 'succeeded 'skipped)))))))

(ert-deftest gptel-runner-parallel-key-rejects-cross-child-collisions ()
  (gptel-runner-elisp-test--isolated
    (dolist (reverse-children '(nil t))
      (let* ((children
              (list
               (gptel-runner-branch
                :predicate (lambda (_run) t)
                :then (gptel-runner-parallel-key-test--step 'left 'selected 1)
                :else (gptel-runner-parallel-key-test--step
                       'right (copy-sequence "shared") 2))
               (gptel-runner-parallel-key-test--step
                'other (copy-sequence "shared") 3)))
             (err
              (should-error
               (gptel-runner-start
                (apply #'gptel-runner-parallel :id 'outer
                       (if reverse-children (reverse children) children)))
               :type 'user-error)))
        (should (equal (cadr err)
                       "Parallel children of outer both write blackboard key \"shared\""))
        (should (zerop (hash-table-count gptel-runner--runs)))))))

(ert-deftest gptel-runner-parallel-key-rejects-nested-parallel-collisions ()
  (gptel-runner-elisp-test--isolated
    (let ((err
           (should-error
            (gptel-runner-start
             (gptel-runner-parallel
              :id 'outer
              (gptel-runner-sequence
               (gptel-runner-parallel
                :id 'inner
                (gptel-runner-parallel-key-test--step 'left 'shared 1)
                (gptel-runner-parallel-key-test--step 'right 'shared 2)))
              (gptel-runner-parallel-key-test--step 'other 'other 3)))
            :type 'user-error)))
      (should (equal (cadr err)
                     "Parallel children of inner both write blackboard key shared"))
      (should (zerop (hash-table-count gptel-runner--runs))))))

(provide 'gptel-runner-parallel-key-test)
;;; gptel-runner-parallel-key-test.el ends here
