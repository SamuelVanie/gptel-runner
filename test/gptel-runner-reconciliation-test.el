;;; gptel-runner-reconciliation-test.el --- Combined recovery regressions -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(ert-deftest gptel-runner-reconciliation-nested-branch-checkpoint ()
  "Retain an entered branch on resume, then reset it for fresh inner cycles."
  (dolist (reload '(nil t))
    (ert-info ((format "Reload checkpoint: %S" reload))
      (gptel-runner-elisp-test--isolated
        (let ((predicates 0) (preparations 0) (starts 0) (callbacks 0)
              resolve)
          (gptel-runner-defworkflow reconciliation ()
            (gptel-runner-repeat-until
             :id 'outer :max 2
             :until (lambda (run) (= (gptel-runner-iteration run 'outer) 2))
             :body
             (gptel-runner-repeat-until
              :id 'inner :max 2
              :until (lambda (run) (= (gptel-runner-iteration run 'inner) 2))
              :body
              (gptel-runner-branch
               :id 'choice
               :predicate (lambda (run)
                            (cl-incf predicates)
                            (not (gptel-runner-get run 'prepared)))
               :then
               (gptel-runner-sequence
                (gptel-runner-elisp-step
                 :id 'prepare :save-as 'prepared
                 :function (lambda (_run _node) (cl-incf preparations) t))
                (gptel-runner-elisp-step
                 :id 'wait :async t
                 :function (lambda (_run _node succeed _reject)
                             (cl-incf starts)
                             (setq resolve succeed)
                             nil)))
               :else
               (gptel-runner-elisp-step
                :id 'subsequent
                :function (lambda (run _node)
                            (gptel-runner-put
                             run 'subsequent
                             (1+ (gptel-runner-get run 'subsequent 0)))))))))
          (let* ((callback (lambda (_run) (cl-incf callbacks)))
                 (run (gptel-runner-start 'reconciliation :callback callback))
                 (stale resolve))
            (gptel-runner-pause-run run)
            (when reload
              (gptel-runner-store-flush-pending)
              (gptel-runner-forget-run run)
              (setq run (gptel-runner-load-run
                         (gptel-runner-run-snapshot-file run) callback)))
            (gptel-runner-resume-run run)
            (should (eq (gptel-runner-run-state run) 'running))
            (should (= preparations 1))
            (should (= starts 2))
            (should (= predicates 1))
            (funcall stale 'stale)
            (should (eq (gptel-runner-run-state run) 'running))
            (funcall resolve 'accepted)
            (should (eq (gptel-runner-run-state run) 'succeeded))
            (should (= predicates 4))
            (should (= (gptel-runner-get run 'subsequent) 3))
            (should (= (gptel-runner-iteration run 'outer) 2))
            (should (= (gptel-runner-iteration run 'inner) 2))
            (should (= callbacks 1))))))))

(provide 'gptel-runner-reconciliation-test)
;;; gptel-runner-reconciliation-test.el ends here
