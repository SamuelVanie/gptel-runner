;;; gptel-runner-jobs-lock-test.el --- Process lock tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-jobs)

(defmacro gptel-runner-jobs-lock-test--directory (&rest body)
  "Run BODY with fresh temporary directory ROOT and lock path LOCK."
  (declare (indent 0))
  `(let* ((root (make-temp-file "runner-lock-test-" t))
          (lock (expand-file-name "job" root)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun gptel-runner-jobs-lock-test--dead-owner ()
  "Return an exact local identity that cannot denote this live process."
  (plist-put (gptel-runner-jobs--owner "dead-owner" nil) :start '(0 0 0 0)))

(ert-deftest gptel-runner-jobs-lock-live-owner-is-never-expired ()
  (gptel-runner-jobs-lock-test--directory
    (let ((first (plist-put (gptel-runner-jobs--owner "first" nil)
                            :created-at 0))
          (second (gptel-runner-jobs--owner "second" nil)))
      (should (gptel-runner-jobs--lock lock first))
      (should-not (gptel-runner-jobs--lock lock second))
      (should-not (gptel-runner-jobs--unlock lock "second"))
      (should (equal (gptel-runner-jobs--read (expand-file-name "owner.el" lock))
                     first))
      (should (gptel-runner-jobs--unlock lock "first"))
      (should (gptel-runner-jobs--lock lock second)))))

(ert-deftest gptel-runner-jobs-lock-unavailable-live-process-attributes-are-unknown ()
  (let ((owner (gptel-runner-jobs--owner "owner" nil)))
    (cl-letf (((symbol-function 'process-attributes) (lambda (_) nil))
              ((symbol-function 'list-system-processes) (lambda () (list (emacs-pid)))))
      (should (eq (gptel-runner-jobs--lock-owner-state owner) 'unknown)))
    (cl-letf (((symbol-function 'process-attributes) (lambda (_) nil))
              ((symbol-function 'list-system-processes) (lambda () nil)))
      (should (eq (gptel-runner-jobs--lock-owner-state owner) 'dead)))))

(ert-deftest gptel-runner-jobs-lock-reclaim-serializes-competing-inspection ()
  (gptel-runner-jobs-lock-test--directory
    (let ((first (gptel-runner-jobs--owner "first" nil))
          (second (gptel-runner-jobs--owner "second" nil))
          (original (symbol-function 'gptel-runner-jobs--lock-owner-state))
          attempted acquired)
      (gptel-runner-jobs--write (expand-file-name "owner.el" lock)
                               (gptel-runner-jobs-lock-test--dead-owner))
      (cl-letf (((symbol-function 'gptel-runner-jobs--lock-owner-state)
                 (lambda (owner)
                   (when (and (not attempted)
                              (equal (plist-get owner :token) "dead-owner"))
                     (setq attempted t
                           acquired (gptel-runner-jobs--lock lock second)))
                   (funcall original owner))))
        (should (gptel-runner-jobs--lock lock first)))
      (should attempted)
      (should-not acquired)
      (should (equal (plist-get (gptel-runner-jobs--read
                                 (expand-file-name "owner.el" lock)) :token)
                     "first"))
      (should-not (gptel-runner-jobs--lock lock second)))))

(ert-deftest gptel-runner-jobs-lock-unlock-cannot-delete-replacement-owner ()
  (gptel-runner-jobs-lock-test--directory
    (let ((next (gptel-runner-jobs--owner "next" nil))
          (file (expand-file-name "owner.el" lock))
          (original (symbol-function 'gptel-runner-jobs--read))
          attempted acquired)
      (gptel-runner-jobs--write file (gptel-runner-jobs-lock-test--dead-owner))
      (cl-letf (((symbol-function 'gptel-runner-jobs--read)
                 (lambda (path)
                   (let ((value (funcall original path)))
                     (when (and (equal path file) (not attempted))
                       (setq attempted t acquired (gptel-runner-jobs--lock lock next)))
                     value))))
        (should (gptel-runner-jobs--unlock lock "dead-owner")))
      (should attempted)
      (should-not acquired)
      (should (gptel-runner-jobs--lock lock next))
      (should-not (gptel-runner-jobs--unlock lock "dead-owner"))
      (should (equal (gptel-runner-jobs--read file) next)))))

(ert-deftest gptel-runner-jobs-lock-incomplete-owner-stays-busy ()
  (gptel-runner-jobs-lock-test--directory
    (let ((owner (gptel-runner-jobs--owner "new" nil))
          (file (expand-file-name "owner.el" lock)))
      (make-directory lock)
      (should-not (gptel-runner-jobs--lock lock owner))
      (write-region "(:pid" nil file nil 'silent)
      (should-not (gptel-runner-jobs--lock lock owner))
      (gptel-runner-jobs--write file (list :pid (emacs-pid) :host (system-name)))
      (should-not (gptel-runner-jobs--lock lock owner))
      (gptel-runner-jobs--write file
                               (plist-put (copy-sequence owner) :host "another-host"))
      (should-not (gptel-runner-jobs--lock lock owner)))))

(ert-deftest gptel-runner-jobs-lock-guard-survives-a-dead-contender ()
  (gptel-runner-jobs-lock-test--directory
    (let* ((guard (concat lock ".guard"))
           (dead-file (expand-file-name "dead.claim.el" guard))
           (owner (gptel-runner-jobs--owner "new" nil)))
      (gptel-runner-jobs--write
       dead-file (list :owner (gptel-runner-jobs-lock-test--dead-owner) :ticket nil))
      (should (gptel-runner-jobs--lock lock owner))
      (should-not (file-exists-p dead-file))
      (should (gptel-runner-jobs--unlock lock "new"))
      (gptel-runner-jobs--write dead-file '(:owner nil :ticket nil))
      (should-not (gptel-runner-jobs--lock lock owner)))))

(ert-deftest gptel-runner-jobs-lock-real-process-reclaim-has-one-winner ()
  (gptel-runner-jobs-lock-test--directory
    (let* ((emacs (expand-file-name invocation-name invocation-directory))
           (barrier (expand-file-name "start" root))
           (winner (expand-file-name "winner" root))
           (release (expand-file-name "release" root))
           processes buffers result-files)
      (gptel-runner-jobs--write (expand-file-name "owner.el" lock)
                               (gptel-runner-jobs-lock-test--dead-owner))
      (unwind-protect
          (progn
            (dotimes (index 3)
              (let* ((token (format "contender-%d" index))
                     (result (expand-file-name (concat token ".el") root))
                     (buffer (generate-new-buffer " *runner-lock-contender*"))
                     (form
                      `(let* ((owner (gptel-runner-jobs--owner ,token nil))
                              (deadline (+ (float-time) 8)) acquired)
                         (while (and (not (file-exists-p ,barrier))
                                     (< (float-time) deadline))
                           (sleep-for 0.005))
                         (while (and (not acquired) (not (file-exists-p ,winner))
                                     (< (float-time) deadline))
                           (setq acquired (gptel-runner-jobs--lock ,lock owner))
                           (unless acquired (sleep-for 0.005)))
                         (when acquired (gptel-runner-jobs--write ,winner ,token))
                         (gptel-runner-jobs--write ,result
                                                  (list :token ,token :acquired acquired))
                         (when acquired
                           (while (and (not (file-exists-p ,release))
                                       (< (float-time) deadline))
                             (sleep-for 0.005))
                           (gptel-runner-jobs--unlock ,lock ,token)))))
                (push result result-files)
                (push buffer buffers)
                (push (make-process
                       :name token :buffer buffer :noquery t
                       :command (list emacs "-Q" "--batch" "-L"
                                      gptel-runner-jobs--package-directory
                                      "-l" "gptel-runner-jobs"
                                      "--eval" (prin1-to-string form)))
                      processes)))
            (write-region "go" nil barrier nil 'silent)
            (let ((deadline (+ (float-time) 6)))
              (while (and (not (cl-every #'file-exists-p result-files))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (cl-every #'file-exists-p result-files))
            (let* ((results (mapcar #'gptel-runner-jobs--read result-files))
                   (winners (cl-remove-if-not
                             (lambda (item) (plist-get item :acquired)) results)))
              (should (= (length winners) 1))
              (should (equal (plist-get (car winners) :token)
                             (plist-get (gptel-runner-jobs--read
                                         (expand-file-name "owner.el" lock)) :token))))
            (write-region "done" nil release nil 'silent)
            (let ((deadline (+ (float-time) 3)))
              (while (and (cl-some #'process-live-p processes)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (dolist (process processes)
              (should-not (process-live-p process))
              (should (= (process-exit-status process) 0))))
        (dolist (process processes)
          (when (process-live-p process) (delete-process process)))
        (mapc #'kill-buffer buffers)))))

(provide 'gptel-runner-jobs-lock-test)
;;; gptel-runner-jobs-lock-test.el ends here
