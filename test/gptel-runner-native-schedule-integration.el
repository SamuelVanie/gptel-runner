;;; gptel-runner-native-schedule-integration.el --- Live scheduler test -*- lexical-binding: t; -*-

;; Run explicitly with plain gptel on the load path in a user login session.

(require 'ert)
(require 'gptel-runner)
(defvar gptel-runner-launchd-directory)

(ert-deftest gptel-runner-native-schedule-runs-a-plain-gptel-worker ()
  (let* ((gptel-runner-job-directory (make-temp-file "runner $HOME %u " t))
         (gptel-runner-launchd-directory (expand-file-name "agents" gptel-runner-job-directory))
         (init (expand-file-name "workflow.el" gptel-runner-job-directory))
         (value "literal $HOME %u \"quoted\" \\ path")
         job finished)
    (unwind-protect
        (progn
          (gptel-runner-jobs--write
           init
           '(gptel-runner-defworkflow timer-smoke ()
              (gptel-runner-elisp-step
               :id 'worker :save-as 'result
               :function (lambda (_run _node)
                           (list (featurep 'gptel) (featurep 'gptel-agent)
                                 (getenv "RUNNER_TIMER_TEST"))))))
          (setq job (gptel-runner-register-job
                     "timer-smoke" :workflow 'timer-smoke :init-file init
                     :environment (list (cons "RUNNER_TIMER_TEST" value))
                     :schedule '(:interval 5)))
          (should (equal (plist-get job :preload-features) '(gptel)))
          (gptel-runner-job-schedule job)
          (let ((deadline (+ (float-time) 60)))
            (while (and (not finished) (< (float-time) deadline))
              (sleep-for 0.2)
              (setq finished
                    (cl-find-if (lambda (run)
                                  (and (eq (plist-get run :phase) 'closed)
                                       (not (eq (plist-get run :state) 'skipped))))
                                (gptel-runner-invocation-list)))))
          (unless finished
            (ert-fail (list "No completed invocation after 60 seconds"
                            (gptel-runner-jobs--scheduler-call 'diagnostics job))))
          (ert-info ((format "Worker error: %S" (plist-get finished :error)))
            (should (eq (plist-get finished :state) 'succeeded)))
          (should (eq (plist-get finished :trigger) 'schedule))
          (should (equal (gptel-runner-get finished 'result) (list t nil value)))
          (let ((removed (gptel-runner-job-unschedule job)))
            (should (eq (plist-get (plist-get removed :scheduler) :state) 'removed))))
      (when job
        (gptel-runner-job-delete job)
        (gptel-runner-jobs--scheduler-call 'reap))
      (delete-directory gptel-runner-job-directory t))))

;;; gptel-runner-native-schedule-integration.el ends here
