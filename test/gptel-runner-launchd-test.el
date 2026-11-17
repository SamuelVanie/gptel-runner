;;; gptel-runner-launchd-test.el --- User scheduler tests -*- lexical-binding: t; -*-

;;; Commentary:
;; All launchctl calls are mocked.  No test installs a user service.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-runner-jobs)
(require 'gptel-runner-launchd)

(defmacro gptel-runner-launchd-test--fixture (&rest body)
  "Execute BODY with private files and mocked scheduler operations."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "runner-launchd-test-" t))
          (system-type 'darwin)
          (gptel-runner-job-directory (expand-file-name "store/" directory))
          (gptel-runner-launchd-directory (expand-file-name "agents/" directory))
          (gptel-runner-jobs--package-directory default-directory)
          (job (list :id "job-test" :name "Example & <test>"
                     :workflow 'example :root gptel-runner-job-directory
                     :emacs (expand-file-name invocation-name invocation-directory)
                     :schedule '(:interval 300)
                     :environment '(("RUNNER_LABEL" . "A & <B> \"C\""))))
          invocations commands saved fail-command loaded)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-runner-job-save)
                    (lambda (record) (setq saved record)))
                   ((symbol-function 'gptel-runner-job-get) (lambda (_id) job))
                   ((symbol-function 'gptel-runner-invocation-list)
                    (lambda () invocations))
                   ((symbol-function 'gptel-runner-launchd--await-worker)
                    (lambda (invocation) invocation))
                   ((symbol-function 'gptel-runner-launchd--invoke)
                    (lambda (&rest args)
                      (push args commands)
                      (list :exit (cond ((equal (car args) fail-command) 5)
                                        ((equal (car args) "print")
                                         (if loaded 0 113))
                                        (t 0))
                            :output "mock launchctl output" :arguments args))))
           ,@body)
       (delete-directory directory t))))

(ert-deftest gptel-runner-launchd-plist-preserves-arguments-and-escapes-values ()
  (gptel-runner-launchd-test--fixture
    (let* ((args (gptel-runner-launchd--arguments job "worker" "/tmp/a & b/"))
           (xml (gptel-runner-launchd--plist
                 (gptel-runner-launchd--base job "label" args directory))))
      (should (equal (aref args (1- (length args))) "/tmp/a & b/"))
      (should (string-match-p "/tmp/a &amp; b/" xml))
      (should (string-match-p "A &amp; &lt;B&gt; &quot;C&quot;" xml))
      (require 'xml)
      (with-temp-buffer
        (insert xml)
        (should (xml-parse-region (point-min) (point-max))))
      (should-not (string-match-p "<key>KeepAlive</key>" xml)))))

(ert-deftest gptel-runner-launchd-validates-calendar-and-interval-schedules ()
  (should (equal (gptel-runner-launchd--schedule '(:interval 30))
                 '("StartInterval" . 30)))
  (should (equal
           (gptel-runner-launchd--schedule
            '(:calendar ((:hour 9 :minute 15) (:weekday 1 :hour 12))))
           '("StartCalendarInterval" .
             [( ("Hour" . 9) ("Minute" . 15))
              (("Weekday" . 1) ("Hour" . 12))])))
  (dolist (schedule '((:interval 0) (:interval 1.5) (:interval 30 :extra t)
                      (:calendar nil) (:calendar ((:minute 60)))
                      (:calendar ((:hour 9 :hour 10)))
                      (:calendar ((:year 2027))) (:calendar ((:hour)))))
    (should-error (gptel-runner-launchd--schedule schedule))))

(ert-deftest gptel-runner-launchd-install-does-not-run-immediately ()
  (gptel-runner-launchd-test--fixture
    (let ((record (gptel-runner-launchd-install job)))
      (should (plist-get record :enabled))
      (should (eq (plist-get (plist-get saved :scheduler) :state) 'installed))
      (let ((file (gptel-runner-launchd--schedule-file job)))
        (should (= #o600 (logand #o777 (file-modes file))))
        (with-temp-buffer
          (insert-file-contents file)
          (should (search-forward "<string>trigger</string>" nil t))
          (should (search-forward "<key>StartInterval</key>" nil t))
          (goto-char (point-min))
          (should-not (search-forward "<key>RunAtLoad</key>" nil t))))
      (should (cl-find "bootstrap" commands :key #'car :test #'equal))
      (should-not (cl-find "kickstart" commands :key #'car :test #'equal)))))

(ert-deftest gptel-runner-launchd-install-records-failure ()
  (gptel-runner-launchd-test--fixture
    (setq fail-command "bootstrap")
    (should-error (gptel-runner-launchd-install job))
    (should (eq (plist-get (plist-get saved :scheduler) :state) 'error))
    (should (string-match-p "bootstrap failed"
                            (plist-get (plist-get saved :scheduler) :error)))))

(ert-deftest gptel-runner-launchd-disable-only-touches-scheduled-trigger ()
  (gptel-runner-launchd-test--fixture
    (setq loaded t)
    (gptel-runner-launchd-disable job)
    (should-not (plist-get saved :enabled))
    (should (cl-find "bootout" commands :key #'car :test #'equal))
    (dolist (command commands)
      (should (string-match-p "schedule.job-test" (cadr command)))
      (should-not (string-match-p "worker" (cadr command))))))

(ert-deftest gptel-runner-launchd-worker-is-independent-of-trigger ()
  (gptel-runner-launchd-test--fixture
    (let* ((worker-directory (expand-file-name "invocation/" directory))
           (invocation (list :id "inv-test" :epoch 2 :job job
                             :directory worker-directory :root gptel-runner-job-directory))
           (label (gptel-runner-launchd-start-worker invocation)))
      (should (string-suffix-p "worker.inv-test.2" label))
      (should (file-exists-p (expand-file-name "worker-registration-2.el" worker-directory)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "worker.plist" worker-directory))
        (should (search-forward "<string>worker</string>" nil t))
        (should (search-forward "<key>RunAtLoad</key><true/>" nil t))
        (goto-char (point-min))
        (should-not (search-forward "StartInterval" nil t)))
      (should (= 1 (length commands)))
      (should (equal (caar commands) "bootstrap")))))

(ert-deftest gptel-runner-launchd-reap-waits-for-worker-exit ()
  (gptel-runner-launchd-test--fixture
    (let* ((worker-directory (expand-file-name "invocation/" directory))
           (invocation (list :id "inv-test" :epoch 1 :job job
                             :directory worker-directory :state 'succeeded)))
      (gptel-runner-launchd-start-worker invocation)
      (setq commands nil invocations (list invocation) loaded t)
      (gptel-runner-launchd-reap)
      (should-not commands)
      (setq invocations (list (plist-put invocation :worker-state 'exited)))
      (gptel-runner-launchd-reap)
      (should (cl-find "bootout" commands :key #'car :test #'equal))
      (should-not (file-exists-p (expand-file-name "worker-registration-1.el" worker-directory))))))

(ert-deftest gptel-runner-launchd-reap-cleans-previous-epoch-after-restart ()
  (gptel-runner-launchd-test--fixture
    (let* ((worker-directory (expand-file-name "invocation/" directory))
           (invocation (list :id "inv-test" :epoch 1 :job job
                             :directory worker-directory :state 'succeeded)))
      (gptel-runner-launchd-start-worker invocation)
      (setq invocations (list (plist-put (copy-sequence invocation) :epoch 2))
            loaded t commands nil)
      (gptel-runner-launchd-reap)
      (should (cl-find "bootout" commands :key #'car :test #'equal))
      (should-not (file-exists-p (expand-file-name "worker-registration-1.el" worker-directory))))))

(ert-deftest gptel-runner-launchd-refuses-unrelated-service-and-non-macos ()
  (should-error (gptel-runner-launchd--target "com.other.service"))
  (let ((system-type 'gnu/linux))
    (should-error (gptel-runner-launchd-install nil) :type 'user-error)))

(ert-deftest gptel-runner-launchd-startup-timeout-stops-exact-worker ()
  (gptel-runner-launchd-test--fixture
    (cl-letf (((symbol-function 'gptel-runner-launchd--await-worker)
               (lambda (_invocation) (error "Worker startup timed out"))))
      (let* ((worker-directory (expand-file-name "invocation/" directory))
             (invocation (list :id "inv-test" :epoch 1 :job job
                               :directory worker-directory :root gptel-runner-job-directory)))
        (should-error (gptel-runner-launchd-start-worker invocation))
        (should (equal (car commands)
                       (list "bootout" (concat (gptel-runner-launchd--domain)
                                               "/org.gnu.emacs.gptel-runner.worker.inv-test.1"))))
        (should-not (file-exists-p
                     (expand-file-name "worker-registration-1.el" worker-directory)))))))

(ert-deftest gptel-runner-launchd-failed-timeout-cleanup-is-distinct ()
  (gptel-runner-launchd-test--fixture
    (setq fail-command "bootout" loaded t)
    (cl-letf (((symbol-function 'gptel-runner-launchd--await-worker)
               (lambda (_invocation) (error "Worker startup timed out"))))
      (let* ((worker-directory (expand-file-name "invocation/" directory))
             (invocation (list :id "inv-test" :epoch 1 :job job
                               :directory worker-directory :root gptel-runner-job-directory)))
        (should-error (gptel-runner-launchd-start-worker invocation)
                      :type 'gptel-runner-launchd-cleanup-error)
        (should (file-exists-p
                 (expand-file-name "worker-registration-1.el" worker-directory)))))))

(ert-deftest gptel-runner-launchd-readiness-requires-ownership-handoff ()
  (let ((gptel-runner-launchd-startup-timeout 0.5)
        (invocation '(:id "inv-test" :epoch 1 :root "/tmp/"))
        (reads 0) (waits 0))
    (cl-letf (((symbol-function 'gptel-runner-invocation-get)
               (lambda (_id)
                 (cl-incf reads)
                 (list :epoch 1 :phase (if (= reads 1) 'starting 'open))))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _args) (cl-incf waits))))
      (should (eq (plist-get (gptel-runner-launchd--await-worker invocation) :phase) 'open))
      (should (= waits 1))
      (should (= reads 2)))))

(ert-deftest gptel-runner-launchd-readiness-accepts-fast-completion-and-reports-startup-error ()
  (let ((invocation '(:id "inv-test" :epoch 1 :root "/tmp/")))
    (cl-letf (((symbol-function 'gptel-runner-invocation-get)
               (lambda (_id) '(:epoch 1 :phase closed :state succeeded))))
      (should (eq (plist-get (gptel-runner-launchd--await-worker invocation) :state)
                  'succeeded)))
    (cl-letf (((symbol-function 'gptel-runner-invocation-get)
               (lambda (_id) '(:epoch 1 :phase closed :state startup-error :error "Missing init"))))
      (should-error (gptel-runner-launchd--await-worker invocation)))))

(ert-deftest gptel-runner-launchd-uncertain-reaping-requires-confirmed-absence ()
  (gptel-runner-launchd-test--fixture
    (let* ((worker-directory (expand-file-name "invocation/" directory))
           (invocation (list :id "inv-test" :epoch 1 :job job
                             :directory worker-directory :root gptel-runner-job-directory))
           confirmed)
      (gptel-runner-launchd-start-worker invocation)
      (setq invocation (plist-put invocation :worker-state 'uncertain)
            invocations (list invocation) loaded t)
      (cl-letf (((symbol-function 'gptel-runner-jobs--confirm-stopped)
                 (lambda (_record) (setq confirmed t))))
        (gptel-runner-launchd-reap)
        (should-not confirmed)
        (setq fail-command "print")
        (gptel-runner-launchd-reap)
        (should-not confirmed)
        (setq fail-command nil loaded nil)
        (gptel-runner-launchd-reap)
        (should confirmed)
        (should-not (file-exists-p
                     (expand-file-name "worker-registration-1.el" worker-directory)))))))

(provide 'gptel-runner-launchd-test)
;;; gptel-runner-launchd-test.el ends here
