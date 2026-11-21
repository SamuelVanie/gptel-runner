;;; gptel-runner-artifact-picker-test.el --- Artifact selection tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-jobs-ui)

(defmacro gptel-runner-artifact-picker-test--with-files (&rest body)
  "Run BODY with three retained files in a temporary directory."
  (declare (indent 0))
  `(let ((directory (make-temp-file "runner-artifact-picker-" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "artifacts" directory))
           (dolist (name '("first.md" "second.md" "third.md"))
             (with-temp-file (expand-file-name (concat "artifacts/" name) directory)
               (insert name)))
           ,@body)
       (delete-directory directory t))))

(defun gptel-runner-artifact-picker-test--select (directory artifacts index)
  "Select candidate INDEX from ARTIFACTS retained under DIRECTORY.
Return its resolved path and the offered labels, using the real path resolver."
  (let* ((run (list :format 'runner-invocation :id "picker-test"
                    :directory directory :artifacts artifacts))
         (target '(background-run "picker-test"))
         labels opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'gptel-runner-jobs-ui--selection)
                 (lambda () (list run nil target)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate require-match &rest _)
                   (should require-match)
                   (setq labels (mapcar #'car collection))
                   (nth index labels)))
                ((symbol-function 'find-file-noselect)
                 (lambda (path &rest _)
                   (should-not enable-local-variables)
                   (should-not enable-local-eval)
                   (setq opened path)
                   (current-buffer)))
                ((symbol-function 'pop-to-buffer) #'identity))
        (gptel-runner-background-artifact)
        (should buffer-read-only)
        (should (equal gptel-runner-background--target target))))
    (list :path opened :labels labels)))

(ert-deftest gptel-runner-artifact-picker-selects-newer-version ()
  (gptel-runner-artifact-picker-test--with-files
    (let* ((artifacts '((:name "report.md" :file "artifacts/first.md" :epoch 1)
                        (:name "report.md" :file "artifacts/second.md" :epoch 2)))
           (result (gptel-runner-artifact-picker-test--select directory artifacts 1))
           (labels (plist-get result :labels)))
      (should (equal (plist-get result :path)
                     (expand-file-name "artifacts/second.md" directory)))
      (should-not (equal (car labels) (cadr labels)))
      (should (string-match-p "epoch 1" (car labels)))
      (should (string-match-p "epoch 2" (cadr labels))))))

(ert-deftest gptel-runner-artifact-picker-distinguishes-same-epoch-publications ()
  (gptel-runner-artifact-picker-test--with-files
    (let ((artifacts '((:name "report.md" :file "artifacts/first.md"
                       :epoch 2 :published-at 1000.0)
                      (:name "report.md" :file "artifacts/second.md"
                       :epoch 2 :published-at 1000.0)
                      (:name "report.md" :file "artifacts/third.md"
                       :epoch 2 :published-at 1000.0)))
          labels)
      (dotimes (index (length artifacts))
        (let* ((result (gptel-runner-artifact-picker-test--select
                        directory artifacts index))
               (file (plist-get (nth index artifacts) :file)))
          (should (equal (plist-get result :path) (expand-file-name file directory)))
          (should (string-match-p (regexp-quote file)
                                  (nth index (plist-get result :labels))))
          (when labels (should (equal labels (plist-get result :labels))))
          (setq labels (plist-get result :labels))))
      (should (= (length (delete-dups (copy-sequence labels))) 3)))))

(ert-deftest gptel-runner-artifact-picker-allows-missing-optional-metadata ()
  (gptel-runner-artifact-picker-test--with-files
    (let* ((artifacts '((:name "report.md" :file "artifacts/first.md")
                        (:name "report.md" :file "artifacts/second.md")
                        (:file "artifacts/third.md")))
           (result (gptel-runner-artifact-picker-test--select directory artifacts 1))
           (labels (plist-get result :labels)))
      (should (equal (plist-get result :path)
                     (expand-file-name "artifacts/second.md" directory)))
      (should-not (equal (car labels) (cadr labels)))
      (should (equal (nth 2 labels) "artifacts/third.md"))
      (should (equal (plist-get (gptel-runner-artifact-picker-test--select
                                directory artifacts 2) :path)
                     (expand-file-name "artifacts/third.md" directory))))))

(ert-deftest gptel-runner-artifact-picker-preserves-unique-names ()
  (gptel-runner-artifact-picker-test--with-files
    (let ((result (gptel-runner-artifact-picker-test--select
                   directory '((:name "report.md" :file "artifacts/first.md" :epoch 1)
                               (:name "notes.md" :file "artifacts/second.md" :epoch 2))
                   1)))
      (should (equal (plist-get result :labels) '("report.md" "notes.md")))
      (should (equal (plist-get result :path)
                     (expand-file-name "artifacts/second.md" directory))))))

(ert-deftest gptel-runner-artifact-picker-reserves-literal-names ()
  (gptel-runner-artifact-picker-test--with-files
    (let* ((versions '((:name "report.md" :file "artifacts/first.md" :epoch 1)
                       (:name "report.md" :file "artifacts/second.md" :epoch 2)))
           (version-label
            (cadr (plist-get (gptel-runner-artifact-picker-test--select
                             directory versions 1) :labels)))
           (artifacts (append versions
                              (list (list :name version-label
                                          :file "artifacts/third.md"))))
           (result (gptel-runner-artifact-picker-test--select directory artifacts 2))
           (labels (plist-get result :labels)))
      (should (equal (nth 2 labels) version-label))
      (should (= (length (delete-dups (copy-sequence labels))) 3))
      (should (equal (plist-get result :path)
                     (expand-file-name "artifacts/third.md" directory)))
      (should (equal (plist-get (gptel-runner-artifact-picker-test--select
                                directory artifacts 1) :path)
                     (expand-file-name "artifacts/second.md" directory))))))

(ert-deftest gptel-runner-artifact-picker-disambiguates-identical-descriptors ()
  (gptel-runner-artifact-picker-test--with-files
    (let* ((artifact '(:name "report.md" :file "artifacts/first.md"))
           (artifacts (list artifact (copy-tree artifact) (copy-tree artifact)))
           (result (gptel-runner-artifact-picker-test--select directory artifacts 2))
           (labels (plist-get result :labels)))
      (should (= (length (delete-dups (copy-sequence labels))) 3))
      (should (equal labels
                     (plist-get (gptel-runner-artifact-picker-test--select
                                 directory artifacts 1) :labels)))
      (should (equal (plist-get result :path)
                     (expand-file-name "artifacts/first.md" directory))))))

(ert-deftest gptel-runner-artifact-picker-validates-selected-version-path ()
  (gptel-runner-artifact-picker-test--with-files
    (let ((outside (make-temp-file "runner-artifact-outside-")))
      (unwind-protect
          (progn
            (make-symbolic-link outside (expand-file-name "artifacts/escape" directory))
            (dolist (file (list outside (file-relative-name outside directory)
                                "artifacts/escape" "artifacts/missing"))
              (should-error
               (gptel-runner-artifact-picker-test--select
                directory (list '(:name "report.md" :file "artifacts/first.md")
                                (list :name "report.md" :file file))
                1)
               :type 'user-error)))
        (delete-file outside)))))

(provide 'gptel-runner-artifact-picker-test)
;;; gptel-runner-artifact-picker-test.el ends here
