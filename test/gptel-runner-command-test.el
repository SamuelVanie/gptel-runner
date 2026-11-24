;;; gptel-runner-command-test.el --- Configurable scheduler tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-command)

(defmacro gptel-runner-command-test--job (&rest body)
  "Run BODY with a fresh job, custom scheduler and private job directory."
  (declare (indent 0) (debug t))
  `(let* ((gptel-runner-job-directory (make-temp-file "runner command $HOME " t))
          (init (expand-file-name "workflow.el" gptel-runner-job-directory))
          (gptel-runner-job-scheduler-program gptel-runner-job-emacs-program)
          (gptel-runner-job-scheduler-arguments '("space and \"quote\"" "$literal"))
          action calls job
          (gptel-runner-job-scheduler-function
           (lambda (operation definition batch)
             (when (eq operation 'install) (setq action batch))
             (list (symbol-name operation) (plist-get definition :id)))))
     (unwind-protect
         (progn
           (gptel-runner-jobs--write
            init '(gptel-runner-defworkflow custom-scheduled ()
                    (gptel-runner-elisp-step
                     :id 'done :save-as 'value :function (lambda (_run _node) 42))))
           (setq job (gptel-runner-register-job
                      "custom" :workflow 'custom-scheduled :init-file init
                      :preload-features nil :schedule '(:interval 60)))
           ,@body)
       (delete-directory gptel-runner-job-directory t))))

(ert-deftest gptel-runner-command-pins-commands-and-dispatches-in-a-clean-worker ()
  (gptel-runner-command-test--job
    (let ((process (symbol-function 'gptel-runner-job-call-process)))
      (cl-letf (((symbol-function 'gptel-runner-job-call-process)
                 (lambda (program &rest args)
                   (push (cons program args) calls)
                   '(:exit 0 :output "scheduler output"))))
        (let ((info (gptel-runner-command-diagnostics job)))
          (should (equal (plist-get info :output) "scheduler output")))
        (let* ((installed (gptel-runner-job-schedule job))
               (status (plist-get installed :scheduler)))
          (should (eq (plist-get status :backend) 'command))
          (should (eq (plist-get status :state) 'installed))
          (should (equal (cdar calls) '("space and \"quote\"" "$literal" "install" "custom")))
          (should (equal (last action 4)
                         (list "scheduled-run" "custom" "--root"
                               (expand-file-name (plist-get job :root)))))
          (let ((gptel-runner-job-scheduler-function nil)
                (gptel-runner-job-scheduler-program nil)
                (gptel-runner-job-scheduler-arguments nil))
            (let ((result (apply process action)))
              (ert-info ((plist-get result :output)) (should (eq 0 (plist-get result :exit)))))
            (let ((run (car (gptel-runner-invocation-list))))
              (should (eq (plist-get run :state) 'succeeded))
              (should (eq (plist-get run :trigger) 'schedule))
              (should (equal (alist-get 'value (gptel-runner-invocation-outputs run)) 42)))
            (gptel-runner-register-job "custom" :name "Updated")
            (gptel-runner-command-diagnostics (gptel-runner-job-get "custom"))
            (should (equal (car calls) (plist-get status :diagnostics-command)))
            (let ((removed (gptel-runner-job-unschedule job)))
              (should (equal (plist-get removed :name) "Updated"))
              (should-not (plist-get removed :enabled))
              (should (equal (car calls) (plist-get status :remove-command))))
            (let ((result (apply process action)))
              (ert-info ((plist-get result :output)) (should (eq 0 (plist-get result :exit)))))
            (should (= (length (gptel-runner-invocation-list)) 1))))))))

(ert-deftest gptel-runner-command-scheduled-run-preserves-a-dash-job-id ()
  (gptel-runner-command-test--job
    (let* ((dash-job (gptel-runner-register-job
                      "--" :workflow 'custom-scheduled :init-file init :preload-features nil))
           (result (apply #'gptel-runner-job-call-process
                          (gptel-runner-job-scheduled-command dash-job))))
      (ert-info ((plist-get result :output)) (should (eq 0 (plist-get result :exit))))
      (should (equal "--" (plist-get (car (gptel-runner-invocation-list)) :job-id))))))

(ert-deftest gptel-runner-command-validates-before-running-or-changing-a-job ()
  (gptel-runner-command-test--job
    (cl-letf (((symbol-function 'gptel-runner-job-call-process)
               (lambda (&rest _) (ert-fail "Ran an invalid command"))))
      (let ((gptel-runner-job-scheduler-program
             (expand-file-name "missing" gptel-runner-job-directory)))
        (should-error (gptel-runner-job-schedule job) :type 'user-error))
      (dolist (invalid '("one argument" ("valid" . "tail") (flag) ("a\0b")))
        (let ((gptel-runner-job-scheduler-arguments invalid))
          (should-error (gptel-runner-job-schedule job) :type 'user-error)))
      (let ((gptel-runner-job-scheduler-function
             (lambda (op _job _action) (if (eq op 'remove) 42 '("ok")))))
        (should-error (gptel-runner-job-schedule job) :type 'user-error))
      (should (equal job (gptel-runner-job-get "custom")))
      (gptel-runner-job-delete "custom")
      (should-not (gptel-runner-job-list)))))

(ert-deftest gptel-runner-command-keeps-ownership-across-errors-and-deletion ()
  (gptel-runner-command-test--job
    (let ((fail "install"))
      (cl-letf (((symbol-function 'gptel-runner-job-call-process)
                 (lambda (_program &rest args)
                   (let ((op (car (last args 2))))
                     (push op calls)
                     (when (equal op "install")
                       (let ((status (plist-get (gptel-runner-job-get "custom") :scheduler)))
                         (should (plist-get status :remove-command))))
                     (if (equal op fail) '(:exit 6 :output "task operation failed")
                       '(:exit 0 :output ""))))))
        (should-error (gptel-runner-job-schedule job))
        (let ((status (plist-get (gptel-runner-job-get "custom") :scheduler)))
          (should (eq (plist-get status :state) 'error))
          (should (string-search "task operation failed" (plist-get status :error))))
        (let ((gptel-runner-job-scheduler-arguments '("--another-account")))
          (setq action nil)
          (should-error (gptel-runner-job-schedule job) :type 'user-error)
          (should-not action))
        (should (equal calls '("install")))
        (setq fail "remove")
        (gptel-runner-job-schedule job '(:interval 120))
        (should-error (gptel-runner-job-unschedule job))
        (delete-file init)
        (should-error (gptel-runner-job-delete job))
        (should (gptel-runner-job-get "custom"))
        (setq fail nil)
        (let ((gptel-runner-job-scheduler-function nil)
              (gptel-runner-job-scheduler-program nil))
          (let ((removed (gptel-runner-job-unschedule job)))
            (should (eq (plist-get (plist-get removed :scheduler) :state) 'removed))
            (should (eq (plist-get (plist-get removed :scheduler) :backend) 'command)))
          (gptel-runner-job-delete job))
        (should (equal calls '("remove" "remove" "remove" "install" "install")))
        (should-not (gptel-runner-job-list))))))

(ert-deftest gptel-runner-command-preserves-previously-installed-native-schedules ()
  (gptel-runner-command-test--job
    (cl-letf (((symbol-function 'gptel-runner-jobs--scheduler) (lambda () 'launchd))
              ((symbol-function 'gptel-runner-job-call-process)
               (lambda (&rest _) (ert-fail "Replaced an installed schedule"))))
      (dolist (status '((:state installed) (:backend launchd :state installed)))
        (let ((native-job (plist-put (copy-sequence job) :scheduler status)))
          (gptel-runner-jobs--write (gptel-runner-jobs--definition-file "custom") native-job)
          (should-error (gptel-runner-job-schedule job) :type 'user-error)
          (should (equal native-job (gptel-runner-job-get "custom"))))))))

(provide 'gptel-runner-command-test)
;;; gptel-runner-command-test.el ends here
