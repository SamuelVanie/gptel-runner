;;; gptel-runner-retry-output-test.el --- Retrying overlapping writers -*- lexical-binding: t; -*-

(require 'gptel-runner-elisp-test)

(defun gptel-runner-retry-output-test--restore (run mode)
  "Restore RUN, optionally emulating an older snapshot according to MODE."
  (let ((file (gptel-runner-save-run run)))
    (gptel-runner-forget-run run)
    (when (memq mode '(legacy unknown))
      (let* ((snapshot (gptel-runner-store--read-file file))
             (data (plist-get snapshot :run)))
        (cl-remf data :output-undo)
        (when (eq mode 'unknown) (cl-remf data :node-outcomes))
        (setf (plist-get snapshot :run) data)
        (with-temp-file file (prin1 snapshot (current-buffer)))))
    (gptel-runner-load-run file)))

(ert-deftest gptel-runner-retry-restores-selected-predecessor-after-overwrite ()
  (gptel-runner-elisp-test--isolated
    (dolist (mode '(live snapshot legacy unknown))
      (ert-info ((format "Checkpoint: %S" mode))
        (let ((fixed nil) (prefix-count 0) observations)
          (gptel-runner-defworkflow revise ()
            (gptel-runner-sequence
             (gptel-runner-elisp-step :id 'first :save-as 'draft
               :function (lambda (&rest _) (cl-incf prefix-count) "first"))
             (gptel-runner-branch :id 'choose :predicate #'always
               :then (gptel-runner-elisp-step :id 'selected :save-as 'draft
                       :function (lambda (&rest _) (cl-incf prefix-count) nil))
               :else (gptel-runner-elisp-step :id 'unused :save-as 'draft
                       :function (lambda (&rest _) (error "unexpected"))))
             (gptel-runner-elisp-step :id 'edit :save-as 'draft
               :function (lambda (run _)
                           (push (gptel-runner-get run 'draft 'absent) observations)
                           "edited"))
             (gptel-runner-elisp-step :id 'second-edit :save-as 'draft
               :function (lambda (&rest _) "twice edited"))
             (gptel-runner-elisp-step :id 'check
               :function (lambda (&rest _) (unless fixed (error "retry"))))))
          (let ((run (gptel-runner-start 'revise)))
            (should (eq (gptel-runner-run-state run) 'failed))
            (unless (eq mode 'live)
              (setq run (gptel-runner-retry-output-test--restore run mode)))
            (setq fixed t)
            (if (eq mode 'unknown)
                (progn
                  (should-error (gptel-runner-retry run :from-node 'edit) :type 'user-error)
                  (should (eq (gptel-runner-run-state run) 'failed))
                  (should (eq (gptel-runner-node-state run 'edit) 'succeeded))
                  (should (equal (gptel-runner-get run 'draft) "twice edited"))
                  (should (equal observations '(nil))))
              (gptel-runner-retry run :from-node 'edit)
              (should (eq (gptel-runner-run-state run) 'succeeded))
              (should (equal observations '(nil nil))))
            (should (= prefix-count 2))))))))

(ert-deftest gptel-runner-retry-older-snapshot-revises-original-draft-once ()
  (gptel-runner-elisp-test--isolated
    (let (fixed)
      (gptel-runner-defworkflow edited-draft ()
        (gptel-runner-sequence
         (gptel-runner-elisp-step :id 'draft :save-as 'text
           :function (lambda (&rest _) "draft"))
         (gptel-runner-elisp-step :id 'editor :save-as 'text
           :function (lambda (run _) (concat (gptel-runner-get run 'text) " improved")))
         (gptel-runner-elisp-step :id 'check
           :function (lambda (&rest _) (unless fixed (error "retry"))))))
      (let* ((old (gptel-runner-start 'edited-draft))
             (run (gptel-runner-retry-output-test--restore old 'legacy)))
        (should (equal (gptel-runner-get run 'text) "draft improved"))
        (setq fixed t)
        (gptel-runner-retry run :from-node 'editor)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'text) "draft improved"))))))

(ert-deftest gptel-runner-retry-failed-writer-retains-draft-after-finalization ()
  (gptel-runner-elisp-test--isolated
    (let* (fixed observed
           (run (gptel-runner-start
                 (gptel-runner-finalize :max-duration 5
                   :body (gptel-runner-sequence
                          (gptel-runner-elisp-step :id 'draft :save-as 'text
                            :function (lambda (&rest _) "first draft"))
                          (gptel-runner-elisp-step :id 'revise :save-as 'text
                            :function (lambda (run _)
                                        (unless fixed (error "retry"))
                                        (setq observed (gptel-runner-get run 'text))
                                        (concat observed " improved"))))
                   :finally (gptel-runner-elisp-step :id 'publish :save-as 'text
                              :function (lambda (&rest _) "finalizer output"))))))
      (should (eq (gptel-runner-run-state run) 'failed))
      (should (equal (gptel-runner-get run 'text) "finalizer output"))
      (setq fixed t)
      (gptel-runner-retry run :from-node 'revise)
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal observed "first draft"))
      (should (equal (gptel-runner-node-result run 'revise) "first draft improved")))))

(ert-deftest gptel-runner-retry-restores-earlier-iteration-without-collecting-it ()
  (gptel-runner-elisp-test--isolated
    (let ((calls 0) fixed seen)
      (gptel-runner-defworkflow retry-loop ()
        (gptel-runner-repeat-until :id 'loop :max 2
          :until (lambda (run) (= 2 (gptel-runner-iteration run 'loop)))
          :collect-keys '(draft) :save-history-as 'history
          :body (gptel-runner-sequence
                 (gptel-runner-try :id 'recover
                   :body (gptel-runner-elisp-step :id 'edit :save-as 'draft
                           :function (lambda (run _)
                                       (push (gptel-runner-get run 'draft 'absent) seen)
                                       (pcase (cl-incf calls)
                                         (1 'first) (2 'second) (_ (error "withdrawn")))))
                   :catch (gptel-runner-elisp-step :id 'withdraw :function #'ignore))
                 (gptel-runner-elisp-step :id 'verify
                   :function (lambda (run _)
                               (when (and (= 1 (gptel-runner-iteration run 'loop))
                                          (not fixed))
                                 (error "retry")))))))
      (let ((run (gptel-runner-start 'retry-loop)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (setq fixed t
              run (gptel-runner-retry-output-test--restore run 'snapshot))
        (gptel-runner-retry run :from-node 'edit)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal seen '(first first absent)))
        (should (eq (gptel-runner-get run 'draft) 'first))
        (should (equal (gptel-runner-get run 'history)
                       '((:iteration 1 :values ((draft . first)))
                         (:iteration 2 :values nil))))))))

(ert-deftest gptel-runner-retry-try-error-publication-keeps-body-prefix ()
  (gptel-runner-elisp-test--isolated
    (dolist (target '(nil revise))
      (let* (fixed observed
             (run (gptel-runner-start
                   (gptel-runner-try :id 'try :error-as 'shared :save-as 'result
                     :body (gptel-runner-sequence
                            (gptel-runner-elisp-step :id 'draft :save-as 'shared
                              :function (lambda (&rest _) "first draft"))
                            (gptel-runner-elisp-step :id 'revise
                              :function (lambda (run _)
                                          (unless fixed (error "retry"))
                                          (setq observed (gptel-runner-get run 'shared)))))
                     :finally (gptel-runner-elisp-step :id 'cleanup :function #'ignore)))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get (gptel-runner-get run 'shared) :state) 'failed))
        (setq fixed t)
        (gptel-runner-retry run :from-node target)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal observed "first draft"))))))

(ert-deftest gptel-runner-retry-distinguishes-inputs-from-an-older-checkpoint ()
  (gptel-runner-elisp-test--isolated
    (dolist (mode '(snapshot legacy))
      (dolist (edit-wrote '(nil t))
        (let (fixed observations)
          (gptel-runner-defworkflow retry-input ()
            (gptel-runner-sequence
             (gptel-runner-elisp-step :id 'revise :save-as 'text
               :function (lambda (run _)
                           (push (gptel-runner-get run 'text) observations)
                           (unless (or edit-wrote fixed) (error "retry"))
                           "edited"))
             (gptel-runner-elisp-step :id 'check
               :function (lambda (&rest _) (unless fixed (error "retry"))))))
          (let* ((old (gptel-runner-start 'retry-input :inputs '((text . "seed"))))
                 (run (gptel-runner-retry-output-test--restore old mode)))
            (setq fixed t)
            (if (and (eq mode 'legacy) edit-wrote)
                (progn
                  (should-error (gptel-runner-retry run :from-node 'revise)
                                :type 'user-error)
                  (should (eq (gptel-runner-run-state run) 'failed))
                  (should (equal (gptel-runner-get run 'text) "edited")))
              (gptel-runner-retry run :from-node 'revise)
              (should (eq (gptel-runner-run-state run) 'succeeded))
              (should (equal observations '("seed" "seed"))))))))))

(provide 'gptel-runner-retry-output-test)
;;; gptel-runner-retry-output-test.el ends here
