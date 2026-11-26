;;; gptel-runner-workspace-diff-test.el --- Git progress regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-review)

(defun gptel-runner-workspace-diff-test--git (&rest arguments)
  "Run Git with ARGUMENTS in the current fixture directory."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil '(t t) nil
                          "-c" "user.name=Runner Test"
                          "-c" "user.email=runner@example.invalid" arguments))
      (ert-fail (buffer-string)))))

(defmacro gptel-runner-workspace-diff-test--with-directory (&rest body)
  "Run BODY with an empty temporary directory as `default-directory'."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "runner-git-progress-" t))
          (default-directory (file-name-as-directory directory))
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn
           (setenv "GIT_CONFIG_NOSYSTEM" "1")
           (setenv "GIT_CONFIG_GLOBAL" null-device)
           (setenv "GIT_CEILING_DIRECTORIES" (directory-file-name
                                             (file-name-directory directory)))
           ,@body)
       (delete-directory directory t))))

(defun gptel-runner-workspace-diff-test--init ()
  "Create a temporary repository with one commit in the current directory."
  (unless (executable-find "git") (ert-skip "Git is unavailable"))
  (gptel-runner-workspace-diff-test--git "init" "--quiet")
  (with-temp-file "tracked" (insert "original\n"))
  (with-temp-file ".gitignore" (insert "ignored\n"))
  (gptel-runner-workspace-diff-test--git "add" ".")
  (gptel-runner-workspace-diff-test--git "commit" "--quiet" "-m" "initial"))

(ert-deftest gptel-runner-workspace-diff-tracks-working-tree-and-index ()
  (gptel-runner-workspace-diff-test--with-directory
    (gptel-runner-workspace-diff-test--init)
    (let* ((run (gptel-runner-run-create :workspace default-directory))
           (clean (gptel-runner-workspace-diff-key run)))
      (should (stringp clean))
      (should (equal clean (gptel-runner-workspace-diff-key run)))
      (with-temp-file "tracked" (insert "revision A\n"))
      (let ((unstaged (gptel-runner-workspace-diff-key run)))
        (should-not (equal clean unstaged))
        (with-temp-file "tracked" (insert "revision B\n"))
        (should-not (equal unstaged (gptel-runner-workspace-diff-key run))))
      (gptel-runner-workspace-diff-test--git "add" "tracked")
      (let ((staged (gptel-runner-workspace-diff-key run)))
        (should-not (equal clean staged))
        (with-temp-file "tracked" (insert "revision C\n"))
        (gptel-runner-workspace-diff-test--git "add" "tracked")
        (should-not (equal staged (gptel-runner-workspace-diff-key run))))
      ;; The working-tree bytes now match HEAD, but the index does not.
      (with-temp-file "tracked" (insert "original\n"))
      (should-not (equal clean (gptel-runner-workspace-diff-key run)))
      (gptel-runner-workspace-diff-test--git "reset" "--quiet" "HEAD" "--" "tracked")
      (should (equal clean (gptel-runner-workspace-diff-key run))))))

(ert-deftest gptel-runner-workspace-diff-tracks-untracked-contents-and-paths ()
  (gptel-runner-workspace-diff-test--with-directory
    (gptel-runner-workspace-diff-test--init)
    (let* ((run (gptel-runner-run-create :workspace default-directory))
           (clean (gptel-runner-workspace-diff-key run))
           (file (concat "nested/-a 'draft'"
                         (if (eq system-type 'windows-nt) " " "\n")
                         "$(echo surprise)")))
      (make-directory "nested")
      (with-temp-file "ignored" (insert "ignored output"))
      (should (equal clean (gptel-runner-workspace-diff-key run)))
      (with-temp-file file (insert "one\0two"))
      (let ((added (gptel-runner-workspace-diff-key run)))
        (should-not (equal clean added))
        (with-temp-file file (insert "one\0six"))
        (let ((edited (gptel-runner-workspace-diff-key run)))
          (should-not (equal added edited))
          (should (equal edited (gptel-runner-workspace-diff-key run)))
          (rename-file file "renamed")
          (should-not (equal edited (gptel-runner-workspace-diff-key run)))))
      (delete-file "renamed")
      (should (equal clean (gptel-runner-workspace-diff-key run))))))

(ert-deftest gptel-runner-workspace-diff-discovers-linked-worktree-from-subdir ()
  (gptel-runner-workspace-diff-test--with-directory
    (make-directory "main")
    (let ((default-directory (expand-file-name "main/")))
      (gptel-runner-workspace-diff-test--init)
      (gptel-runner-workspace-diff-test--git "worktree" "add" "--detach" "../linked"))
    (let* ((default-directory (expand-file-name "linked/"))
           (root (gptel-runner-run-create :workspace default-directory))
           (sub (gptel-runner-run-create :workspace (expand-file-name "sub/"))))
      (make-directory "sub")
      (let ((clean (gptel-runner-workspace-diff-key sub)))
        (should (stringp clean))
        (with-temp-file "tracked" (insert "edit outside run workspace\n"))
        (let ((edited (gptel-runner-workspace-diff-key sub)))
          (should-not (equal clean edited))
          (should (equal edited (gptel-runner-workspace-diff-key root))))))))

(ert-deftest gptel-runner-workspace-diff-observes-committed-work ()
  (gptel-runner-workspace-diff-test--with-directory
    (gptel-runner-workspace-diff-test--init)
    (let* ((run (gptel-runner-run-create :workspace default-directory))
           (before (gptel-runner-workspace-diff-key run)))
      (with-temp-file "tracked" (insert "committed between iterations"))
      (gptel-runner-workspace-diff-test--git "commit" "-qam" "work")
      (let ((after (gptel-runner-workspace-diff-key run)))
        (should-not (equal before after))
        (should (equal after (gptel-runner-workspace-diff-key run)))))))

(ert-deftest gptel-runner-workspace-diff-hashes-link-target-names ()
  (gptel-runner-workspace-diff-test--with-directory
    (gptel-runner-workspace-diff-test--init)
    (let ((run (gptel-runner-run-create :workspace default-directory)))
      (make-directory "link")
      (with-temp-file "link/file" (insert "tracked before replacement"))
      (gptel-runner-workspace-diff-test--git "add" "link/file")
      (rename-file "link" "ignored")
      (condition-case err
          (make-symbolic-link "ignored" "link")
        (file-error
         (if (eq system-type 'windows-nt)
             (ert-skip (error-message-string err))
           (signal (car err) (cdr err)))))
      (let ((linked (gptel-runner-workspace-diff-key run)))
        ;; Git sees link/file as deleted, even though following the symlink
        ;; would find it.  Editing the ignored target must not count.
        (with-temp-file "ignored/file" (insert "outside the fingerprint"))
        (should (equal linked (gptel-runner-workspace-diff-key run)))
        (make-symbolic-link "another target" "link" t)
        (should-not (equal linked (gptel-runner-workspace-diff-key run)))))))

(ert-deftest gptel-runner-workspace-diff-tracks-submodule-checkout ()
  (gptel-runner-workspace-diff-test--with-directory
    (make-directory "source")
    (let ((default-directory (expand-file-name "source/")))
      (gptel-runner-workspace-diff-test--init))
    (make-directory "parent")
    (let* ((default-directory (expand-file-name "parent/"))
           (run (gptel-runner-run-create :workspace default-directory)))
      (gptel-runner-workspace-diff-test--init)
      (gptel-runner-workspace-diff-test--git
       "-c" "protocol.file.allow=always" "submodule" "add" "../source" "child")
      (gptel-runner-workspace-diff-test--git "commit" "--quiet" "-m" "Record A")
      (let ((original (gptel-runner-workspace-diff-key run))
            keys)
        ;; Leave the parent index at A while moving the child to B, then C.
        (dolist (revision '("B" "C"))
          (with-temp-file "child/tracked" (insert revision))
          (gptel-runner-workspace-diff-test--git
           "-C" "child" "commit" "-qam" revision)
          (let ((key (gptel-runner-workspace-diff-key run)))
            (should (stringp key))
            (should (equal key (gptel-runner-workspace-diff-key run)))
            (push key keys)))
        (should-not (equal original (cadr keys)))
        (should-not (equal (car keys) (cadr keys)))
        (gptel-runner-workspace-diff-test--git "-C" "child" "checkout" "--detach" "HEAD~2")
        (should (equal original (gptel-runner-workspace-diff-key run)))))))

(ert-deftest gptel-runner-workspace-diff-outside-git ()
  (gptel-runner-workspace-diff-test--with-directory
    (let ((run (gptel-runner-run-create :workspace default-directory)))
      (should-not (gptel-runner-workspace-diff-key run))
      (with-temp-file "scratch" (insert "scratch text"))
      (should-not (gptel-runner-workspace-diff-key run)))))

(provide 'gptel-runner-workspace-diff-test)
;;; gptel-runner-workspace-diff-test.el ends here
