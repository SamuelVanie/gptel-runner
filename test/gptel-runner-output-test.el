;;; gptel-runner-output-test.el --- Shared output contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-output-test--isolated (&rest body)
  "Run BODY with private output storage and runtime state."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "runner-output-test-" t))
          (gptel-runner-output-directory (expand-file-name "outputs/" directory))
          (gptel-runner-snapshot-directory (expand-file-name "snapshots/" directory))
          (gptel-runner--runs (make-hash-table :test #'equal))
          (gptel-runner--workflows (make-hash-table :test #'eq))
          (gptel-runner--agents (make-hash-table :test #'eq))
          (gptel-runner-store--coordinators (make-hash-table :test #'eq)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (run _value) (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory directory t))))

(ert-deftest gptel-runner-output-retains-copies-and-text-with-one-descriptor ()
  (gptel-runner-output-test--isolated
    (let* ((run (gptel-runner-start (gptel-runner-sequence) :workspace directory))
           (source (expand-file-name "source.bin" directory))
           (bytes (unibyte-string 0 255 13 10)))
      (should-not (gptel-runner-artifacts run))
      (should-not (file-exists-p gptel-runner-output-directory))
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil source nil 'silent))
      (let* ((copy (gptel-runner-publish-artifact run "binary" "source.bin"))
             (text (gptel-runner-write-artifact run "report.txt" "Résumé\n")))
        (delete-file source)
        (should (equal (cl-loop for (key _value) on copy by #'cddr collect key)
                       (cl-loop for (key _value) on text by #'cddr collect key)))
        (should-not (file-name-absolute-p (plist-get copy :file)))
        (should-not (plist-get copy :epoch))
        (should (equal (gptel-runner-read-artifact run text) "Résumé\n"))
        (should (equal (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally
                          (gptel-runner-artifact-file run copy))
                         (buffer-string)) bytes))
        (should (= (file-modes (gptel-runner-artifact-file run text)) #o600))
        (should (= (file-modes (gptel-runner-run-output-directory run)) #o700))
        (should (equal (gptel-runner-artifacts run) (list copy text)))
        ;; Public descriptor mutation cannot alter the run's registry.
        (setf (plist-get text :name) "changed")
        (should (equal (plist-get (cadr (gptel-runner-artifacts run)) :name)
                       "report.txt"))))))

(ert-deftest gptel-runner-output-versions-and-run-storage-are-independent ()
  (gptel-runner-output-test--isolated
    (let* ((first (gptel-runner-start (gptel-runner-sequence)))
           (second (gptel-runner-start (gptel-runner-sequence)))
           (a (gptel-runner-write-artifact first "../../report.txt" "one"))
           (b (gptel-runner-write-artifact first "../../report.txt" "two"))
           (c (gptel-runner-write-artifact second "report.txt" "three")))
      (should-not (equal (plist-get a :file) (plist-get b :file)))
      (should-not (equal (gptel-runner-run-output-directory first)
                         (gptel-runner-run-output-directory second)))
      (should (equal (gptel-runner-read-artifact first a) "one"))
      (should (equal (gptel-runner-read-artifact first b) "two"))
      (should-error (gptel-runner-artifact-file first c) :type 'user-error)
      (gptel-runner-forget-run first)
      (should (file-readable-p (gptel-runner-artifact-file first a))))))

(ert-deftest gptel-runner-output-rejects-missing-and-escaping-files ()
  (gptel-runner-output-test--isolated
    (let* ((run (gptel-runner-start (gptel-runner-sequence)))
           (artifact (gptel-runner-write-artifact run "report" "text"))
           (file (gptel-runner-artifact-file run artifact))
           (outside (expand-file-name "outside.txt" directory)))
      (should-error (gptel-runner-publish-artifact run "missing" "/absent/file"))
      (should-error (gptel-runner-write-artifact run "" "text"))
      (should-error (gptel-runner-write-artifact run "name" 42))
      (with-temp-file outside (insert "outside"))
      (delete-file file)
      (make-symbolic-link outside file)
      (should-error (gptel-runner-artifact-file run artifact) :type 'user-error)
      (delete-file file)
      (should-error (gptel-runner-read-artifact run artifact) :type 'user-error))))

(ert-deftest gptel-runner-output-failed-write-does-not-register-or-leak ()
  (gptel-runner-output-test--isolated
    (let ((run (gptel-runner-start (gptel-runner-sequence))))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (error "injected storage failure"))))
        (should-error (gptel-runner-write-artifact run "report" "text")))
      (should-not (gptel-runner-artifacts run))
      (should-not
       (directory-files (expand-file-name "artifacts" (gptel-runner-run-output-directory run))
                        nil directory-files-no-dot-files-regexp)))))

(ert-deftest gptel-runner-output-context-consumes-preceding-publication ()
  (gptel-runner-output-test--isolated
    (gptel-runner-register-agent 'reader)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'reader '(:value "accepted"))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-sequence
                (gptel-runner-elisp-step
                 :id 'publish :save-as 'report
                 :function (lambda (run _node)
                             (gptel-runner-write-artifact run "plan.txt" "Shared plan")))
                (gptel-runner-agent-step
                 :id 'read :agent 'reader :prompt "Review"
                 :context (gptel-runner-context-artifact 'report)))
               :driver driver))
             (call (cadr (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (string-match-p "Shared plan" (gptel-runner-call-prompt call)))
        (should (equal (gptel-runner-node-workspace run 'read)
                       (gptel-runner-node-workspace run (gptel-runner-find-node run 'read))))
        (should-error (gptel-runner-node-workspace run 'missing) :type 'user-error))
      (let ((run (gptel-runner-start
                  (gptel-runner-agent-step
                   :id 'missing-context :agent 'reader :prompt "Review"
                   :context (gptel-runner-context-artifact 'missing))
                  :driver driver)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-not (gptel-runner-run-calls run))))))

(ert-deftest gptel-runner-output-snapshot-restores-storage-and-registry ()
  (gptel-runner-output-test--isolated
    (gptel-runner-defworkflow output-durable ()
      (gptel-runner-elisp-step
       :id 'publish :save-as 'report
       :function (lambda (run _node)
                   (gptel-runner-write-artifact run "report.txt" "retained"))))
    (let* ((run (gptel-runner-start 'output-durable))
           (artifact (gptel-runner-get run 'report)))
      (gptel-runner-save-run run)
      (gptel-runner-store-flush-pending)
      (let ((file (gptel-runner-run-snapshot-file run)))
        (gptel-runner-forget-run run)
        (let ((restored (gptel-runner-load-run file)))
          (should (equal (gptel-runner-artifacts restored) (list artifact)))
          (should (equal (gptel-runner-read-artifact restored artifact) "retained"))
          (should (equal (gptel-runner-get restored 'report)
                         (gptel-runner-node-result restored 'publish)))
          (gptel-runner-continue restored "second publication")
          (should (= (length (gptel-runner-artifacts restored)) 2))
          (should (equal (gptel-runner-read-artifact restored artifact) "retained")))))))

(ert-deftest gptel-runner-output-reads-preserve-nil-and-isolate-alist-structure ()
  (gptel-runner-output-test--isolated
    (let* ((run (gptel-runner-start (gptel-runner-sequence)
                                   :inputs '(("key" . nil))))
           (outputs (gptel-runner-outputs run)))
      (should-not (gptel-runner-get run "key" 'missing))
      (should (eq (gptel-runner-get run "absent" 'missing) 'missing))
      (setcdr (car outputs) 'modified)
      (should-not (gptel-runner-get run "key" 'missing)))))

(provide 'gptel-runner-output-test)
;;; gptel-runner-output-test.el ends here
