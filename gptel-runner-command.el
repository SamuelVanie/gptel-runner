;;; gptel-runner-command.el --- User-configured scheduling commands -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Build scheduler arguments in the configured Emacs session.  Save the exact
;; removal and inspection commands with the job so they continue to refer to
;; the installed schedule after a restart or a change in configuration.

;;; Code:

(require 'gptel-runner-jobs)

(defun gptel-runner-command--arguments (arguments)
  "Validate and return a list of literal command ARGUMENTS."
  (unless (and (proper-list-p arguments)
               (cl-every (lambda (arg) (and (stringp arg) (not (string-search "\0" arg))))
                         arguments))
    (user-error "Scheduler arguments must be a list of strings without NUL characters"))
  arguments)

(defun gptel-runner-command--prefix ()
  "Return the resolved scheduler executable and its leading arguments."
  (let ((program (or gptel-runner-job-scheduler-program
                     (gptel-runner-jobs--default-scheduler-program))))
    (unless (and (stringp program) (not (string-empty-p program)))
      (user-error "Configure gptel-runner-job-scheduler-program with an executable"))
    (cons (expand-file-name
           (or (executable-find program)
               (user-error "Cannot find scheduler executable %s" program)))
          (gptel-runner-command--arguments gptel-runner-job-scheduler-arguments))))

(defun gptel-runner-command--build (prefix operation job action)
  "Append arguments for OPERATION on JOB with scheduled ACTION to PREFIX."
  (let ((builder (or gptel-runner-job-scheduler-function
                     (gptel-runner-jobs--default-scheduler-function))))
    (unless (functionp builder)
      (user-error "Configure gptel-runner-job-scheduler-function to build scheduler arguments"))
    (append prefix
            (gptel-runner-command--arguments (funcall builder operation job action)))))

(defun gptel-runner-command--checked (command)
  "Run scheduler COMMAND and report its diagnostic on failure."
  (let ((result (apply #'gptel-runner-job-call-process command)))
    (unless (eq (plist-get result :exit) 0)
      (error "%s failed (%s): %s" (car command) (plist-get result :exit)
             (string-trim (plist-get result :output))))
    result))

(defun gptel-runner-command-install (job)
  "Install JOB's configured schedule and return its saved definition."
  (let* ((prefix (gptel-runner-command--prefix))
         (action (gptel-runner-job-scheduled-command job))
         (remove (gptel-runner-command--build prefix 'remove job action))
         (previous (plist-get (plist-get job :scheduler) :remove-command)))
    (when (and previous (not (equal previous remove)))
      (user-error "Unschedule the installed job before changing its removal command"))
    (let* ((diagnostics (gptel-runner-command--build prefix 'diagnostics job action))
           (details (list :backend 'command :remove-command remove
                          :diagnostics-command diagnostics))
           (install (gptel-runner-command--build prefix 'install job action)))
      ;; A scheduler may create a task before it reports an error or Emacs
      ;; exits.  Persist the commands needed to clean it up first.
      (setq job (gptel-runner-jobs--scheduler-status job 'installing 'install nil details))
      (condition-case err
          (gptel-runner-command--checked install)
        (error
         (gptel-runner-jobs--scheduler-status
          job 'error 'install (error-message-string err) details)
         (signal (car err) (cdr err))))
      (gptel-runner-jobs--scheduler-status
       (plist-put job :enabled t) 'installed 'install nil details))))

(defun gptel-runner-command-delete-schedule (job)
  "Remove JOB's installed external task using its saved command."
  (let ((status (plist-get job :scheduler)))
    (when (and (eq (plist-get status :backend) 'command)
               (not (eq (plist-get status :state) 'removed)))
      (gptel-runner-command-remove job))))

(defun gptel-runner-command-remove (job)
  "Remove JOB's external schedule, preserving its rule and history."
  (let ((status (plist-get job :scheduler)))
    (when (and (eq (plist-get status :backend) 'command)
               (not (eq (plist-get status :state) 'removed)))
      (let ((remove (or (plist-get status :remove-command)
                        (user-error "Missing saved removal command for %s" (plist-get job :id)))))
        (condition-case err
            (gptel-runner-command--checked remove)
          (error
           (gptel-runner-jobs--scheduler-status
            job 'error 'remove (error-message-string err)
            (list :backend 'command :remove-command remove
                  :diagnostics-command (plist-get status :diagnostics-command)))
           (signal (car err) (cdr err)))))))
  (gptel-runner-jobs--scheduler-status
   (plist-put job :enabled nil) 'removed 'remove nil '(:backend command)))

(defun gptel-runner-command-diagnostics (job)
  "Return the external scheduler's diagnostics and saved status for JOB."
  (let* ((status (plist-get job :scheduler))
         (command (or (plist-get status :diagnostics-command)
                      (gptel-runner-command--build
                       (gptel-runner-command--prefix) 'diagnostics job
                       (gptel-runner-job-scheduled-command job)))))
    (append (list :saved-status status) (apply #'gptel-runner-job-call-process command))))

(provide 'gptel-runner-command)
;;; gptel-runner-command.el ends here
