;;; gptel-runner-systemd.el --- User systemd timers and workers -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Each recurring timer activates a short-lived trigger service.  Each
;; invocation has a separate, transient worker service, collected by systemd
;; after exit.  Stopping a timer leaves its workers running.

;;; Code:

(require 'gptel-runner-jobs)
(require 'xdg)

(defgroup gptel-runner-systemd nil
  "User-owned GNU/Linux schedules for background pipelines."
  :group 'gptel-runner)

(defcustom gptel-runner-systemd-directory
  (expand-file-name "systemd/user/" (xdg-config-home))
  "Directory for trigger services and timers in the user unit search path."
  :type 'directory)

(defcustom gptel-runner-systemctl-command '("systemctl" "--user")
  "Executable and leading arguments for managing systemd units.
The first string names systemctl or a compatible wrapper.  Each remaining
string is one literal argument, passed before the operation and unit name."
  :type '(cons (string :tag "Executable") (repeat (string :tag "Argument"))))

(defcustom gptel-runner-systemd-run-command '("systemd-run" "--user")
  "Executable and leading arguments for launching transient systemd workers.
The first string names systemd-run or a compatible wrapper.  Each remaining
string is one literal argument, passed before the worker service options."
  :type '(cons (string :tag "Executable") (repeat (string :tag "Argument"))))

(defcustom gptel-runner-systemd-startup-timeout 15.0
  "Maximum seconds to wait for a worker to adopt its launch reservation."
  :type 'number)

(defun gptel-runner-systemd--invoke (program &rest arguments)
  "Run the configured PROGRAM command with ARGUMENTS and return its result."
  (unless (eq system-type 'gnu/linux)
    (user-error "User systemd services require GNU/Linux"))
  (let ((command (pcase program
                   ("systemctl" gptel-runner-systemctl-command)
                   ("systemd-run" gptel-runner-systemd-run-command)
                   (_ (error "Unknown systemd program: %s" program)))))
    (unless (and (consp command) (proper-list-p command))
      (user-error "Configure %s with a list containing its executable and arguments" program))
    (apply #'gptel-runner-job-call-process (car command) (append (cdr command) arguments))))

(defun gptel-runner-systemd--checked (program &rest arguments)
  "Run PROGRAM with ARGUMENTS and signal failure with its diagnostic."
  (let ((result (apply #'gptel-runner-systemd--invoke program arguments)))
    (unless (eq (plist-get result :exit) 0)
      (error "%s %s failed (%s): %s" program (car arguments) (plist-get result :exit)
             (string-trim (plist-get result :output))))
    result))

(defun gptel-runner-systemd--schedule-name (job)
  "Return the unit basename for JOB's trigger service and timer."
  (concat "gptel-runner-schedule-" (gptel-runner-job-schedule-id job)))

(defun gptel-runner-systemd--worker-unit (invocation)
  "Return the exact transient service name for INVOCATION's generation."
  (let ((epoch (plist-get invocation :epoch)))
    (unless (and (integerp epoch) (> epoch 0)) (error "Invalid worker epoch: %S" epoch))
    (format "gptel-runner-worker-%s-%d.service"
            (gptel-runner-jobs--valid-id (plist-get invocation :id)) epoch)))

(defun gptel-runner-systemd--quote (word)
  "Encode WORD as one literal argument in a systemd unit file."
  ;; Unit-file specifiers are expanded even inside quotes.  Control
  ;; characters must not introduce new directives or line continuations.
  (concat "\"" (mapconcat
                (lambda (c)
                  (cond ((eq c ?%) "%%")
                        ((memq c '(?\" ?\\)) (string ?\\ c))
                        ((zerop c) (error "A unit argument contains a NUL character"))
                        ((or (< c 32) (= c 127)) (format "\\x%02x" c))
                        (t (char-to-string c))))
                word "") "\""))

(defun gptel-runner-systemd--command (job &rest operation)
  "Build the literal Emacs command for JOB's batch OPERATION."
  (let* ((args (apply #'gptel-runner-jobs--batch-arguments job operation))
         (command (mapcar (lambda (arg) (string-replace "$" "$$" arg)) args)))
    ;; systemd looks up the executable before expanding dollars in argv.
    ;; Pass an executable containing a dollar sign as an argument to env.
    (if (string-search "$" (aref args 0))
        (cons "/usr/bin/env" command)
      command)))

(defun gptel-runner-systemd--calendar (rule)
  "Return OnCalendar lines implementing a validated calendar RULE."
  (let* ((day (plist-get rule :day))
         (weekday (plist-get rule :weekday))
         (month (or (plist-get rule :month) "*"))
         (clock (format "%s:%s:00" (or (plist-get rule :hour) "*")
                        (or (plist-get rule :minute) "*"))))
    ;; A single systemd expression ANDs weekday and date.  The runner rule
    ;; matches either field when both are present, so give each its own line.
    (concat (when (or day (null weekday))
              (format "OnCalendar=*-%s-%s %s\n" month (or day "*") clock))
            (when weekday
              (format "OnCalendar=%s *-%s-* %s\n"
                      (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"] weekday)
                      month clock)))))

(defun gptel-runner-systemd--timer (schedule)
  "Render a timer unit for SCHEDULE."
  (gptel-runner-jobs--validate-schedule schedule)
  (concat "[Unit]\nDescription=gptel-runner scheduled trigger\n\n"
          "[Timer]\nAccuracySec=1s\n"
          (pcase schedule
            (`(:interval ,seconds)
             (format "OnActiveSec=%ds\nOnUnitActiveSec=%ds\n" seconds seconds))
            (`(:calendar ,rules) (mapconcat #'gptel-runner-systemd--calendar rules "")))
          "\n[Install]\nWantedBy=timers.target\n"))

(defun gptel-runner-systemd--service (job)
  "Render the short-lived service which triggers JOB."
  (let ((args (gptel-runner-systemd--command
               job "trigger" (gptel-runner-jobs--valid-id (plist-get job :id))
               "--root" (expand-file-name
                         (or (plist-get job :root) gptel-runner-job-directory)))))
    (concat "[Unit]\nDescription=gptel-runner schedule dispatcher\nStartLimitIntervalSec=0\n\n"
            "[Service]\nType=oneshot\nUMask=0077\nExecStart="
            (mapconcat #'gptel-runner-systemd--quote args " ") "\n")))

(defun gptel-runner-systemd-install (job)
  "Install or update JOB's user timer and return the saved definition."
  ;; A Linux host without a user manager can still run and delete CLI jobs.
  ;; Check the manager before creating unit files or changing the definition.
  (gptel-runner-systemd--checked "systemctl" "show" "--property=Version" "--value")
  (let* ((name (gptel-runner-systemd--schedule-name job))
         (timer (concat name ".timer")))
    (condition-case err
        (let ((rule (gptel-runner-systemd--timer (plist-get job :schedule)))
              (service (gptel-runner-systemd--service job)))
          (gptel-runner-jobs--write-scheduler-file
           (expand-file-name (concat name ".service") gptel-runner-systemd-directory) service)
          (gptel-runner-jobs--write-scheduler-file
           (expand-file-name timer gptel-runner-systemd-directory) rule)
          (gptel-runner-systemd--checked "systemctl" "daemon-reload")
          (gptel-runner-systemd--checked "systemctl" "enable" timer)
          (gptel-runner-systemd--checked "systemctl" "restart" timer)
          (gptel-runner-jobs--scheduler-status (plist-put job :enabled t) 'installed 'install))
      (error
       (gptel-runner-jobs--scheduler-status job 'error 'install (error-message-string err))
       (signal (car err) (cdr err))))))

(defun gptel-runner-systemd--absent-p (unit)
  "Return non-nil only if the user manager confirms UNIT is missing."
  (let ((result (gptel-runner-systemd--invoke
                 "systemctl" "show" "--property=LoadState" "--value" unit)))
    (and (memq (plist-get result :exit) '(0 4))
         (equal (string-trim (plist-get result :output)) "not-found"))))

(defun gptel-runner-systemd-delete-schedule (job)
  "Remove JOB's installed schedule, including a partial installation."
  (let* ((name (gptel-runner-systemd--schedule-name job))
         (timer (concat name ".timer"))
         (file (expand-file-name timer gptel-runner-systemd-directory))
         (service (expand-file-name (concat name ".service") gptel-runner-systemd-directory)))
    (when (or (file-exists-p file) (file-exists-p service)
              (not (memq (plist-get (plist-get job :scheduler) :state) '(nil removed))))
      (cond ((file-exists-p file)
             (gptel-runner-systemd--checked "systemctl" "disable" "--now" timer))
            ((not (gptel-runner-systemd--absent-p timer))
             (gptel-runner-systemd--checked "systemctl" "stop" timer)))
      (dolist (unit-file (list file service))
        (when (file-exists-p unit-file) (delete-file unit-file)))
      (gptel-runner-systemd--checked "systemctl" "daemon-reload"))))

(defun gptel-runner-systemd-remove (job)
  "Remove JOB's timer, retaining its saved rule, workers, and history."
  (condition-case err
      (progn
        (gptel-runner-systemd-delete-schedule job)
        (gptel-runner-jobs--scheduler-status (plist-put job :enabled nil) 'removed 'remove))
    (error
     (gptel-runner-jobs--scheduler-status job 'error 'remove (error-message-string err))
     (signal (car err) (cdr err)))))

(defun gptel-runner-systemd-diagnostics (job)
  "Return the user manager's timer diagnostics for JOB."
  (let ((unit (concat (gptel-runner-systemd--schedule-name job) ".timer")))
    (append (list :label unit :file (expand-file-name unit gptel-runner-systemd-directory)
                  :saved-status (plist-get job :scheduler))
            (gptel-runner-systemd--invoke "systemctl" "status" "--no-pager" unit))))

(defun gptel-runner-systemd-reap ()
  "Reconcile abandoned launches after confirming the exact service is absent.
Systemd collects exited transient units automatically, including failed ones."
  (dolist (invocation (gptel-runner-invocation-list))
    (when (and (memq (plist-get invocation :launcher) '(systemd direct))
               (gptel-runner-jobs--abandoned-start-p invocation))
      (condition-case err
          (when (or (eq (plist-get invocation :launcher) 'direct)
                    (gptel-runner-systemd--absent-p
                     (gptel-runner-systemd--worker-unit invocation)))
            (gptel-runner-jobs--confirm-stopped invocation))
        (error (message "Could not reconcile background startup: %s"
                        (error-message-string err)))))))

(defun gptel-runner-systemd--stop-unconfirmed (unit original-error)
  "Stop exact worker UNIT after ORIGINAL-ERROR or retain its reservation."
  (condition-case err
      (gptel-runner-systemd--checked "systemctl" "stop" unit)
    (error
     (unless (ignore-errors (gptel-runner-systemd--absent-p unit))
       (signal 'gptel-runner-job-cleanup-error
               (list (error-message-string original-error) (error-message-string err)))))))

(defun gptel-runner-systemd-start-worker (invocation)
  "Start INVOCATION in a transient service and wait for worker adoption."
  (gptel-runner-systemd-reap)
  (let* ((job (plist-get invocation :job))
         (unit (gptel-runner-systemd--worker-unit invocation))
         (command (gptel-runner-systemd--command
                   job "worker" (plist-get invocation :directory)
                   "--epoch" (number-to-string (plist-get invocation :epoch)))))
    ;; The worker reads :environment from its private manifest before init.
    ;; Keep those values out of process listings and the unit's description.
    (apply #'gptel-runner-systemd--checked
           "systemd-run" "--collect" "--service-type=exec" "--property=UMask=0077"
           (concat "--unit=" unit) "--" command)
    (condition-case err
        (gptel-runner-jobs--await-worker invocation gptel-runner-systemd-startup-timeout)
      (error
       (gptel-runner-systemd--stop-unconfirmed unit err)
       (signal (car err) (cdr err))))
    unit))

(provide 'gptel-runner-systemd)
;;; gptel-runner-systemd.el ends here
