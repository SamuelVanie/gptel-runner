;;; gptel-runner-windows.el --- Windows scheduled tasks and workers -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Windows Task Scheduler runs recurring jobs as the logged-in user.  Each
;; installation writes the task XML in the job store.  For manual runs,
;; Windows starts a worker independently of the editor through ShellExecute.

;;; Code:

(require 'gptel-runner-jobs)
(require 'seq)
(require 'xml)

(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag))

(defun gptel-runner-windows--quote (arg)
  "Quote one ARG for the native Windows executable's argument parser."
  ;; Backslashes before a literal quote are doubled, with one extra slash
  ;; escaping the quote.  A closing delimiter also doubles trailing slashes.
  (let ((body (replace-regexp-in-string
               "\\\\*\"" (lambda (part) (concat (substring part 0 -1) "\\" part))
               arg t t)))
    (concat "\"" (replace-regexp-in-string
                  "\\\\+\\'" (lambda (slashes) (concat slashes slashes)) body t t)
            "\"")))

(defun gptel-runner-windows--triggers (schedule)
  "Build Task Scheduler XML trigger nodes for SCHEDULE."
  (pcase schedule
    (`(:interval ,seconds)
     (unless (and (integerp seconds) (<= 60 seconds (* 31 86400)) (zerop (% seconds 60)))
       (user-error "Windows Task Scheduler intervals must be whole minutes from 1 minute to 31 days"))
     (list `(TimeTrigger nil
             (StartBoundary nil ,(format-time-string "%FT%TZ" (time-add nil seconds) t))
             (Repetition nil (Interval nil ,(format "PT%dM" (/ seconds 60)))))))
    (`(:calendar ,rules)
     (when (> (length rules) 48)
       (user-error "Windows Task Scheduler accepts up to 48 triggers per task"))
     (mapcar #'gptel-runner-windows--calendar-trigger rules))
    (_ (user-error "Unsupported Windows schedule: %S" schedule))))

(defun gptel-runner-windows--calendar-trigger (rule)
  "Build a daily or weekly Task Scheduler trigger for a validated calendar RULE."
  (let ((hour (plist-get rule :hour))
        (minute (plist-get rule :minute))
        (weekday (plist-get rule :weekday)))
    (unless (and hour minute (= (length rule) (if weekday 6 4)))
      (user-error "Windows calendar rules support :hour and :minute with an optional :weekday"))
    `(CalendarTrigger nil
      (StartBoundary nil ,(format "%sT%02d:%02d:00" (format-time-string "%F") hour minute))
      ,(if weekday
           `(ScheduleByWeek nil (WeeksInterval nil "1")
             (DaysOfWeek nil (,(aref [Sunday Monday Tuesday Wednesday
                                      Thursday Friday Saturday Sunday] weekday) nil)))
         '(ScheduleByDay nil (DaysInterval nil "1"))))))

(defun gptel-runner-windows--task-file (job name command)
  "Write JOB's task NAME with execution COMMAND and return its XML path."
  ;; Task Scheduler expands %VARIABLE% in action paths and arguments.
  (when (seq-some (lambda (arg) (string-match-p "%[^%]+%" arg)) command)
    (user-error "Windows Task Scheduler would expand a percent-delimited variable in the command"))
  (let* ((file (expand-file-name (concat name ".xml") (plist-get job :root)))
         (triggers (gptel-runner-windows--triggers (plist-get job :schedule)))
         (task `(Task ((version . "1.3") (xmlns . "http://schemas.microsoft.com/windows/2004/02/mit/task"))
                 (Triggers nil ,@triggers)
                 (Principals nil (Principal ((id . "Runner"))
                                            (LogonType nil "InteractiveToken")))
                 (Settings nil (MultipleInstancesPolicy nil "Parallel")
                           (DisallowStartIfOnBatteries nil "false")
                           (StopIfGoingOnBatteries nil "false")
                           (ExecutionTimeLimit nil "PT0S"))
                 (Actions ((Context . "Runner"))
                          (Exec nil (Command nil ,(car command))
                                (Arguments nil ,(mapconcat #'gptel-runner-windows--quote
                                                           (cdr command) " ")))))))
    (let ((coding-system-for-write 'utf-16le-with-signature)
          (temporary (make-temp-file (expand-file-name ".task-" (file-name-directory file)))))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert "<?xml version=\"1.0\" encoding=\"UTF-16\"?>\n")
              (xml-print (list task)))
            (rename-file temporary file t))
        (when (file-exists-p temporary) (delete-file temporary))))
    ;; schtasks.exe also accepts UNC paths.  Use native separators in its
    ;; /XML argument so a network path cannot be mistaken for a switch.
    (if (eq system-type 'windows-nt) (subst-char-in-string ?/ ?\\ file) file)))

;;;###autoload
(defun gptel-runner-windows-scheduler-arguments (operation job command)
  "Return Windows Task Scheduler arguments for OPERATION on JOB.
Use COMMAND as the executable and arguments registered by `install'."
  (let ((name (concat "gptel-runner-" (gptel-runner-job-schedule-id job))))
    (pcase operation
      ('install (list "/Create" "/TN" name "/XML"
                      (gptel-runner-windows--task-file job name command) "/F"))
      ('remove (list "/Delete" "/TN" name "/F"))
      ('diagnostics (list "/Query" "/TN" name "/XML"))
      (_ (error "Unknown scheduler operation: %S" operation)))))

;;;###autoload
(defun gptel-runner-windows-start-worker (invocation)
  "Launch INVOCATION as a detached Windows process and wait for adoption."
  (let ((command (gptel-runner-job-worker-command invocation)))
    (w32-shell-execute "open" (car command)
                       (mapconcat #'gptel-runner-windows--quote (cdr command) " ") 0)
    ;; ShellExecute returns no process handle.  A late worker may still
    ;; adopt its reservation after a timeout.
    (condition-case err
        (gptel-runner-job-await-worker invocation 15)
      (error (signal 'gptel-runner-job-cleanup-error (list (error-message-string err)))))))

(provide 'gptel-runner-windows)
;;; gptel-runner-windows.el ends here
