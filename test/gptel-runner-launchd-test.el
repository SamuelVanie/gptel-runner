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

(defun gptel-runner-launchd-test--save (invocation)
  "Persist INVOCATION's manifest and return it."
  (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation))

(defun gptel-runner-launchd-test--register (invocation)
  "Write INVOCATION's exact service registration and return its filename."
  (let* ((epoch (plist-get invocation :epoch))
         (file (expand-file-name (format "worker-registration-%d.el" epoch)
                                 (plist-get invocation :directory))))
    (gptel-runner-jobs--write
     file (list :epoch epoch :label (gptel-runner-launchd--label
                                     "worker" (plist-get invocation :id) epoch)))
    file))

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
        (should (search-forward "<string>--epoch</string>\n<string>2</string>" nil t))
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
      (setq commands nil invocations (list (gptel-runner-launchd-test--save invocation)) loaded t)
      (gptel-runner-launchd-reap)
      (should-not commands)
      (setq invocations (list (gptel-runner-launchd-test--save
                               (plist-put invocation :worker-state 'exited))))
      (gptel-runner-launchd-reap)
      (should (cl-find "bootout" commands :key #'car :test #'equal))
      (should-not (file-exists-p (expand-file-name "worker-registration-1.el" worker-directory))))))

(ert-deftest gptel-runner-launchd-reap-cleans-previous-epoch-after-restart ()
  (gptel-runner-launchd-test--fixture
    (let* ((worker-directory (expand-file-name "invocation/" directory))
           (invocation (list :id "inv-test" :epoch 1 :job job
                             :directory worker-directory :state 'succeeded)))
      (gptel-runner-launchd-start-worker invocation)
      (setq invocations (list (gptel-runner-launchd-test--save
                               (plist-put (copy-sequence invocation) :epoch 2)))
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
            invocation (plist-put invocation :phase 'starting)
            invocations (list (gptel-runner-launchd-test--save invocation)) loaded t)
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

(ert-deftest gptel-runner-launchd-reap-refreshes-generation-under-control-lock ()
  (dolist (boundary '(snapshot control))
    (dolist (operation '(retry continue rerun))
      (gptel-runner-launchd-test--fixture
        (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
               (id (plist-get invocation :id))
               (control-lock (symbol-function 'gptel-runner-jobs--with-control-lock))
               old-file new-file restart-pending)
          (gptel-runner-jobs--launch-failed invocation '(error "Finished generation"))
          (setq invocation (gptel-runner-invocation-get id)
                old-file (gptel-runner-launchd-test--register invocation)
                invocations (list invocation) loaded t)
          (gptel-runner-jobs--write
           (expand-file-name "checkpoint.el" (plist-get invocation :directory)) nil)
          (let ((gptel-runner-job-launch-function
                 (lambda (fresh)
                   (setq new-file (gptel-runner-launchd-test--register fresh))
                   ;; Adopt the new reservation under the real control lock,
                   ;; with this process as the exact live worker identity.
                   (funcall control-lock fresh
                            (lambda ()
                              (setq fresh (plist-put fresh :phase 'open)
                                    fresh (plist-put fresh :worker-state 'running))
                              (gptel-runner-launchd-test--save fresh))))))
            (cl-labels ((restart ()
                          (setq restart-pending nil)
                          (gptel-runner-job-command
                           invocation operation
                           (when (eq operation 'continue) '(:observation "Continue")))))
              (cl-letf (((symbol-function 'gptel-runner-invocation-list)
                         (lambda ()
                           (setq restart-pending t)
                           (when (eq boundary 'snapshot) (restart))
                           invocations))
                        ((symbol-function 'gptel-runner-jobs--with-control-lock)
                         (lambda (record function)
                           ;; A controller can win the lock after discovery.
                           (when (and restart-pending (eq boundary 'control))
                             (restart))
                           (funcall control-lock record function))))
                (gptel-runner-launchd-reap))))
          (let ((fresh (gptel-runner-invocation-get id)))
            (should (= (plist-get fresh :epoch) 2))
            (should (gptel-runner-invocation-live-p fresh)))
          (should-not (file-exists-p old-file))
          (should (file-exists-p new-file))
          (should (equal commands
                         (list (list "bootout" (gptel-runner-launchd--target
                                                (gptel-runner-launchd--label "worker" id 1)))
                               (list "print" (gptel-runner-launchd--target
                                              (gptel-runner-launchd--label "worker" id 1)))))))))))

(ert-deftest gptel-runner-launchd-reap-keeps-future-or-mismatched-registration ()
  (dolist (kind '(future label invocation filename))
    (gptel-runner-launchd-test--fixture
      (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
             (id (plist-get invocation :id))
             (epoch (if (eq kind 'future) 2 1))
             (file (expand-file-name
                    (format "worker-registration-%d.el" (if (eq kind 'filename) 2 epoch))
                    (plist-get invocation :directory)))
             (label (gptel-runner-launchd--label
                     "worker" (if (eq kind 'invocation) "another-invocation" id)
                     (if (eq kind 'label) 2 epoch))))
        (gptel-runner-jobs--launch-failed invocation '(error "Finished generation"))
        (setq invocations (list (gptel-runner-invocation-get id)) loaded t)
        (gptel-runner-jobs--write file (list :epoch epoch :label label))
        (gptel-runner-launchd-reap)
        (should-not commands)
        (should (file-exists-p file))))))

(ert-deftest gptel-runner-launchd-reap-holds-control-through-stop-and-deletion ()
  (gptel-runner-launchd-test--fixture
    (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
           (id (plist-get invocation :id))
           (manifest (gptel-runner-jobs--manifest-file invocation))
           (lock (expand-file-name "control.lock" (plist-get invocation :directory)))
           (read-record (symbol-function 'gptel-runner-jobs--read))
           (invoke (symbol-function 'gptel-runner-launchd--invoke))
           (delete (symbol-function 'delete-file))
           checked file)
      (gptel-runner-jobs--launch-failed invocation '(error "Finished generation"))
      (setq invocations (list (gptel-runner-invocation-get id)) loaded t
            file (gptel-runner-launchd-test--register invocation))
      (cl-labels ((check-lock (stage)
                    (let* ((owner (gptel-runner-jobs--owner "contender" id))
                           (acquired (gptel-runner-jobs--lock lock owner)))
                      (when acquired (gptel-runner-jobs--unlock lock "contender"))
                      (should-not acquired)
                      (push stage checked))))
        (cl-letf (((symbol-function 'gptel-runner-jobs--read)
                   (lambda (path)
                     (when (equal path manifest) (check-lock 'manifest))
                     (funcall read-record path)))
                  ((symbol-function 'gptel-runner-launchd--invoke)
                   (lambda (&rest args)
                     (check-lock (car args))
                     (apply invoke args)))
                  ((symbol-function 'delete-file)
                   (lambda (path &optional trash)
                     (when (equal path file) (check-lock 'delete))
                     (funcall delete path trash))))
          (gptel-runner-launchd-reap)))
      (should (equal (nreverse checked) '(manifest "print" "bootout" delete)))
      (should-not (file-exists-p file)))))

(ert-deftest gptel-runner-launchd-reap-retains-registration-on-stop-failure ()
  (gptel-runner-launchd-test--fixture
    (let* ((invocation (gptel-runner-jobs--new-invocation job 'test))
           (file (gptel-runner-launchd-test--register invocation)))
      (gptel-runner-jobs--launch-failed invocation '(error "Finished generation"))
      (setq invocations (list (gptel-runner-invocation-get (plist-get invocation :id)))
            loaded t fail-command "bootout")
      (gptel-runner-launchd-reap)
      (should (cl-find "bootout" commands :key #'car :test #'equal))
      (should (file-exists-p file))
      (setq fail-command nil commands nil)
      (gptel-runner-launchd-reap)
      (should (cl-find "bootout" commands :key #'car :test #'equal))
      (should-not (file-exists-p file)))))

(ert-deftest gptel-runner-launchd-reap-is-inert-on-non-macos ()
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'gptel-runner-invocation-list)
               (lambda () (ert-fail "Reaping inspected the store on non-macOS"))))
      (gptel-runner-launchd-reap))))

(provide 'gptel-runner-launchd-test)
;;; gptel-runner-launchd-test.el ends here
