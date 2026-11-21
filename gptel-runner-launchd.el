;;; gptel-runner-launchd.el --- User launchd jobs and workers -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; macOS scheduling is an adapter around the generic jobs entry point.
;; Scheduled triggers are short-lived; each invocation is a separate launchd
;; service, so disabling a schedule does not stop an invocation or tie its
;; lifetime to the editor.  Only this package's own services are managed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-runner-core)

(declare-function gptel-runner-job-get "gptel-runner-jobs")
(declare-function gptel-runner-job-save "gptel-runner-jobs")
(declare-function gptel-runner-jobs--validate-schedule "gptel-runner-jobs")
(declare-function gptel-runner-invocation-list "gptel-runner-jobs")
(declare-function gptel-runner-invocation-live-p "gptel-runner-jobs")
(declare-function gptel-runner-invocation-get "gptel-runner-jobs")
(declare-function gptel-runner-jobs--confirm-stopped "gptel-runner-jobs")
(declare-function gptel-runner-jobs--abandoned-start-p "gptel-runner-jobs")
(declare-function gptel-runner-jobs--read "gptel-runner-jobs")
(declare-function gptel-runner-jobs--manifest-file "gptel-runner-jobs")
(declare-function gptel-runner-jobs--with-control-lock "gptel-runner-jobs")
(defvar gptel-runner-job-directory)
(defvar gptel-runner-jobs--package-directory)

(defgroup gptel-runner-launchd nil
  "User-owned macOS schedules for background pipelines."
  :group 'gptel-runner)

(defcustom gptel-runner-launchd-directory
  (expand-file-name "~/Library/LaunchAgents/")
  "Directory for the current user's scheduled trigger property lists."
  :type 'directory)

(defcustom gptel-runner-launchctl-program "/bin/launchctl"
  "Absolute path to launchctl."
  :type 'file)

(defcustom gptel-runner-launchd-startup-timeout 15.0
  "Maximum seconds to wait for a worker to adopt its launch reservation."
  :type 'number)

(define-error 'gptel-runner-launchd-cleanup-error
  "Could not stop an unconfirmed launchd worker")

(defconst gptel-runner-launchd--prefix "org.gnu.emacs.gptel-runner."
  "Namespace reserved for this package's launchd services.")

(defun gptel-runner-launchd--require-macos ()
  "Reject schedule mutations on systems without user launchd services."
  (unless (eq system-type 'darwin)
    (user-error "Schedule management requires macOS; use the generic batch entry point")))

(defun gptel-runner-launchd--job (job)
  "Resolve JOB, either a job record or its identifier."
  (require 'gptel-runner-jobs)
  (if (stringp job) (gptel-runner-job-get job) job))

(defun gptel-runner-launchd--domain ()
  "Return the current user's graphical login domain."
  (format "gui/%d" (user-uid)))

(defun gptel-runner-launchd--label (kind id &optional epoch)
  "Return a package-owned KIND label for ID and optional EPOCH."
  (unless (and (stringp id) (string-match-p "\\`[[:alnum:]_-]+\\'" id))
    (error "Invalid launchd record identifier: %S" id))
  (when (and epoch (not (and (integerp epoch) (> epoch 0))))
    (error "Invalid worker epoch: %S" epoch))
  (concat gptel-runner-launchd--prefix kind "." id
          (if epoch (format ".%s" epoch) "")))

(defun gptel-runner-launchd--target (label)
  "Return this user's service target for package-owned LABEL."
  (unless (string-prefix-p gptel-runner-launchd--prefix label)
    (error "Refusing to manage unrelated launchd service: %s" label))
  (concat (gptel-runner-launchd--domain) "/" label))

(defun gptel-runner-launchd--invoke (&rest arguments)
  "Invoke launchctl with ARGUMENTS and return its exit code and output.
Arguments are passed directly to the executable, without a shell."
  (gptel-runner-launchd--require-macos)
  (unless (file-name-absolute-p gptel-runner-launchctl-program)
    (error "Launchctl must have an absolute path"))
  (with-temp-buffer
    (let ((status (apply #'call-process gptel-runner-launchctl-program
                         nil '(t t) nil arguments)))
      (list :exit status :output (buffer-string) :arguments arguments))))

(defun gptel-runner-launchd--checked (&rest arguments)
  "Invoke launchctl with ARGUMENTS, signaling an error on failure."
  (let ((result (apply #'gptel-runner-launchd--invoke arguments)))
    (unless (eq (plist-get result :exit) 0)
      (error "Launchctl %s failed (%s): %s"
             (car arguments) (plist-get result :exit)
             (string-trim (plist-get result :output))))
    result))

(defun gptel-runner-launchd--loaded-p (label)
  "Return non-nil when package-owned LABEL is registered.
Only the command exit status is used; launchctl's display is not a data API."
  (eq 0 (plist-get
         (gptel-runner-launchd--invoke
          "print" (gptel-runner-launchd--target label)) :exit)))

(defun gptel-runner-launchd--unload (label)
  "Unload package-owned LABEL if it is registered."
  (let ((probe (gptel-runner-launchd--invoke
                "print" (gptel-runner-launchd--target label))))
    (pcase (plist-get probe :exit)
      (0 (gptel-runner-launchd--checked
          "bootout" (gptel-runner-launchd--target label)))
      (113 nil)
      (_ (error "Cannot determine whether service %s is loaded: %s"
                label (plist-get probe :output))))))

(defun gptel-runner-launchd--escape (string)
  "Escape STRING for an XML text node."
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
                  ("\"" . "&quot;") ("'" . "&apos;")) string)
    (setq string (replace-regexp-in-string
                  (regexp-quote (car pair)) (cdr pair) string t t))))

(defun gptel-runner-launchd--xml (value)
  "Encode property-list VALUE as XML.
Alists represent dictionaries and vectors represent arrays."
  (cond
   ((stringp value) (format "<string>%s</string>"
                            (gptel-runner-launchd--escape value)))
   ((integerp value) (format "<integer>%d</integer>" value))
   ((eq value t) "<true/>")
   ((null value) "<false/>")
   ((vectorp value)
    (concat "<array>\n"
            (mapconcat #'gptel-runner-launchd--xml value "\n") "\n</array>"))
   ((listp value)
    (concat "<dict>\n"
            (mapconcat
             (lambda (entry)
               (format "<key>%s</key>%s"
                       (gptel-runner-launchd--escape (car entry))
                       (gptel-runner-launchd--xml (cdr entry)))) value "\n")
            "\n</dict>"))
   (t (error "Invalid property-list value: %S" value))))

(defun gptel-runner-launchd--plist (dictionary)
  "Render property-list DICTIONARY as a complete XML document."
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
          "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\">\n"
          (gptel-runner-launchd--xml dictionary) "\n</plist>\n"))

(defun gptel-runner-launchd--write (file text)
  "Atomically write private TEXT to FILE."
  (let* ((directory (file-name-directory file)) temporary)
    (make-directory directory t)
    (setq temporary (make-temp-file (expand-file-name ".runner-" directory)))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((coding-system-for-write 'utf-8-unix)) (insert text)))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(defun gptel-runner-launchd--schedule (schedule)
  "Validate SCHEDULE and return its launchd dictionary entry."
  (require 'gptel-runner-jobs)
  (gptel-runner-jobs--validate-schedule schedule)
  (pcase (car schedule)
    (:interval (cons "StartInterval" (cadr schedule)))
    (:calendar
     (cons
      "StartCalendarInterval"
      (vconcat
       (mapcar
        (lambda (rule)
          (cl-loop for (key value) on rule by #'cddr
                   collect (cons (capitalize (substring (symbol-name key) 1)) value)))
        (cadr schedule)))))))

(defun gptel-runner-launchd--arguments (job &rest operation)
  "Build Emacs arguments for JOB and batch OPERATION."
  (let ((emacs (plist-get job :emacs)))
    (unless (and (stringp emacs) (file-name-absolute-p emacs)
                 (file-executable-p emacs))
      (error "Job must specify an executable absolute :emacs path"))
    (vconcat
     (list emacs "-Q" "--batch" "-L" gptel-runner-jobs--package-directory
           "-l" "gptel-runner-jobs" "-f" "gptel-runner-jobs-batch-main" "--")
     operation)))

(defun gptel-runner-launchd--environment (job)
  "Validate and return JOB's explicit environment dictionary."
  (let ((environment (plist-get job :environment)))
    (unless (and (proper-list-p environment)
                 (cl-every (lambda (entry)
                             (and (consp entry) (stringp (car entry))
                                  (stringp (cdr entry)))) environment))
      (error "Job :environment must be a string-to-string alist"))
    environment))

(defun gptel-runner-launchd--base (job label arguments directory)
  "Build common JOB properties for LABEL, ARGUMENTS, and log DIRECTORY."
  (append
   (list (cons "Label" label) (cons "ProgramArguments" arguments)
         (cons "ProcessType" "Background") (cons "Umask" "077")
         (cons "StandardOutPath" (expand-file-name "stdout.log" directory))
         (cons "StandardErrorPath" (expand-file-name "stderr.log" directory)))
   (when-let* ((environment (gptel-runner-launchd--environment job)))
     (list (cons "EnvironmentVariables" environment)))))

(defun gptel-runner-launchd--schedule-label (job)
  "Return the user-wide schedule label for JOB, including older definitions."
  (gptel-runner-launchd--label "schedule" (or (plist-get job :launchd-id)
                                              (plist-get job :id))))

(defun gptel-runner-launchd--schedule-file (job)
  "Return the scheduled trigger property-list filename for JOB."
  (expand-file-name
   (concat (gptel-runner-launchd--schedule-label job) ".plist")
   gptel-runner-launchd-directory))

(defun gptel-runner-launchd--status (job state operation &optional error-data)
  "Save scheduler STATE for JOB after OPERATION with optional ERROR-DATA."
  (setq job (copy-sequence job))
  (setq job (plist-put job :scheduler
                       (list :state state :operation operation
                             :updated-at (float-time) :error error-data)))
  (gptel-runner-job-save job))

(defun gptel-runner-launchd-install (job)
  "Install or update JOB's recurring schedule for the current user.
This does not start the pipeline immediately.  Return the saved job record."
  (gptel-runner-launchd--require-macos)
  (setq job (gptel-runner-launchd--job job))
  (condition-case err
      (let* ((id (plist-get job :id))
             (root (or (plist-get job :root) gptel-runner-job-directory))
             (label (gptel-runner-launchd--schedule-label job))
             (logs (expand-file-name (concat "scheduler/" id "/") root))
             (file (gptel-runner-launchd--schedule-file job))
             (dictionary
              (append
               (gptel-runner-launchd--base
                job label (gptel-runner-launchd--arguments
                           job "trigger" id "--root" (expand-file-name root)) logs)
               (list (gptel-runner-launchd--schedule (plist-get job :schedule))))))
        (make-directory logs t)
        (set-file-modes logs #o700)
        (gptel-runner-launchd--write file (gptel-runner-launchd--plist dictionary))
        (gptel-runner-launchd--unload label)
        (gptel-runner-launchd--checked "enable" (gptel-runner-launchd--target label))
        (gptel-runner-launchd--checked "bootstrap" (gptel-runner-launchd--domain) file)
        (gptel-runner-launchd--status (plist-put job :enabled t) 'installed 'install))
    (error
     (gptel-runner-launchd--status job 'error 'install (error-message-string err))
     (signal (car err) (cdr err)))))

(defun gptel-runner-launchd-disable (job)
  "Disable JOB's future scheduled triggers without stopping its workers."
  (gptel-runner-launchd--require-macos)
  (setq job (gptel-runner-launchd--job job))
  (condition-case err
      (let ((label (gptel-runner-launchd--schedule-label job)))
        (gptel-runner-launchd--checked "disable" (gptel-runner-launchd--target label))
        (gptel-runner-launchd--unload label)
        (gptel-runner-launchd--status (plist-put job :enabled nil) 'disabled 'disable))
    (error
     (gptel-runner-launchd--status job 'error 'disable (error-message-string err))
     (signal (car err) (cdr err)))))

(defun gptel-runner-launchd-remove (job)
  "Remove JOB's installed schedule, retaining its configuration and history."
  (setq job (gptel-runner-launchd-disable job))
  (let ((file (gptel-runner-launchd--schedule-file job)))
    (when (file-exists-p file) (delete-file file)))
  (gptel-runner-launchd--status job 'removed 'remove))

(defun gptel-runner-launchd-delete-schedule (job)
  "Unload and remove the recurring trigger for the JOB being deleted.
The definition can be retired even if its init file or workspace is gone."
  (let ((file (gptel-runner-launchd--schedule-file job))
        (state (plist-get (plist-get job :scheduler) :state)))
    ;; An interrupted installation can leave a plist before status is saved.
    (when (or (file-exists-p file) (not (memq state '(nil removed))))
      (gptel-runner-launchd--unload (gptel-runner-launchd--schedule-label job))
      (when (file-exists-p file) (delete-file file)))))

(defun gptel-runner-launchd-diagnostics (job)
  "Return schedule diagnostics for JOB without changing its state.
The raw launchctl display is intended for people, not parsed as an API."
  (setq job (gptel-runner-launchd--job job))
  (let* ((label (gptel-runner-launchd--schedule-label job))
         (result (gptel-runner-launchd--invoke
                  "print" (gptel-runner-launchd--target label))))
    (append (list :label label :file (gptel-runner-launchd--schedule-file job)
                  :saved-status (plist-get job :scheduler)) result)))

(defun gptel-runner-launchd--await-worker (invocation)
  "Wait until INVOCATION's worker adopts its reservation or exits.
The launching process stays alive until ownership is handed over, preventing
a subsequent trigger from reclaiming a dead launcher's reservation."
  (unless (and (numberp gptel-runner-launchd-startup-timeout)
               (> gptel-runner-launchd-startup-timeout 0))
    (error "Worker startup timeout must be positive"))
  (let ((gptel-runner-job-directory (plist-get invocation :root))
        (deadline (+ (float-time) gptel-runner-launchd-startup-timeout))
        adopted)
    (while (not adopted)
      (let ((current (gptel-runner-invocation-get (plist-get invocation :id))))
        (unless (equal (plist-get current :epoch) (plist-get invocation :epoch))
          (error "Worker ownership changed during launch"))
        (when (memq (plist-get current :phase) '(open closing closed))
          (when (memq (plist-get current :state) '(startup-error storage-error))
            (error "Worker startup failed: %s" (plist-get current :error)))
          (setq adopted current)))
      (unless adopted
        (when (>= (float-time) deadline)
          (error "Worker did not adopt its launch reservation within %.1f seconds"
                 gptel-runner-launchd-startup-timeout))
        (accept-process-output nil 0.05)))
    adopted))

(defun gptel-runner-launchd--stop-unconfirmed (label original-error)
  "Stop exact worker LABEL after ORIGINAL-ERROR before releasing its claim.
Signal a distinct condition if stopping cannot be confirmed, so the caller
can retain the launch reservation instead of permitting a duplicate worker."
  (condition-case cleanup-error
      (gptel-runner-launchd--checked "bootout" (gptel-runner-launchd--target label))
    (error
     (unless (eq 113 (plist-get
                     (condition-case nil
                         (gptel-runner-launchd--invoke
                          "print" (gptel-runner-launchd--target label))
                       (error nil)) :exit))
       (signal 'gptel-runner-launchd-cleanup-error
               (list (error-message-string original-error)
                     (error-message-string cleanup-error)))))))

(defun gptel-runner-launchd-start-worker (invocation)
  "Start isolated INVOCATION under the current user's launchd domain.
The worker is independent of the editor or short-lived schedule trigger."
  (gptel-runner-launchd--require-macos)
  (require 'gptel-runner-jobs)
  (gptel-runner-launchd-reap)
  (let* ((job (plist-get invocation :job))
         (directory (file-name-as-directory (plist-get invocation :directory)))
         (label (gptel-runner-launchd--label
                 "worker" (plist-get invocation :id) (plist-get invocation :epoch)))
         (file (expand-file-name "worker.plist" directory))
         (registration (expand-file-name (format "worker-registration-%d.el"
                                                 (plist-get invocation :epoch))
                                        directory))
         (dictionary
          (append
           (gptel-runner-launchd--base
            job label (gptel-runner-launchd--arguments
                       job "worker" directory "--epoch"
                       (number-to-string (plist-get invocation :epoch))) directory)
           '(("RunAtLoad" . t)))))
    (gptel-runner-launchd--write file (gptel-runner-launchd--plist dictionary))
    (gptel-runner-launchd--checked "bootstrap" (gptel-runner-launchd--domain) file)
    (condition-case err
        (progn
          (gptel-runner-launchd--write
           registration
           (concat (prin1-to-string (list :label label :file file
                                         :epoch (plist-get invocation :epoch))) "\n"))
          (gptel-runner-launchd--await-worker invocation))
      (error
       (gptel-runner-launchd--stop-unconfirmed label err)
       (when (file-exists-p registration) (delete-file registration))
       (signal (car err) (cdr err))))
    label))

(defun gptel-runner-launchd--startup-managed-p (invocation label)
  "Return non-nil if INVOCATION's startup is covered by service LABEL.
Legacy records need current-generation launchd metadata; a custom launcher
may have a detached child that probing launchd cannot account for."
  (if (plist-member invocation :launcher)
      (memq (plist-get invocation :launcher) '(launchd direct))
    (let* ((directory (plist-get invocation :directory))
           (registration (ignore-errors
                           (gptel-runner-jobs--read
                            (expand-file-name
                             (format "worker-registration-%d.el"
                                     (plist-get invocation :epoch)) directory)))))
      (or (and (proper-list-p registration)
               (equal (plist-get registration :label) label)
               (equal (plist-get registration :epoch) (plist-get invocation :epoch)))
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents (expand-file-name "worker.plist" directory))
              (search-forward (format "<key>Label</key><string>%s</string>" label) nil t)))))))

(defun gptel-runner-launchd--reap-registration (invocation file)
  "Reap registration FILE if INVOCATION's current generation permits it."
  (gptel-runner-jobs--with-control-lock
   invocation
   (lambda ()
     ;; Restart and adoption use this lock too.  The list snapshot can predate
     ;; either transition, so keep the fresh read and stop in one critical section.
     (let* ((fresh (gptel-runner-jobs--read
                    (gptel-runner-jobs--manifest-file invocation)))
            (record (gptel-runner-jobs--read file))
            (epoch (plist-get record :epoch)))
       (when (and (integerp epoch) (> epoch 0)
                  (equal (file-name-nondirectory file)
                         (format "worker-registration-%d.el" epoch))
                  (equal (plist-get record :label)
                         (gptel-runner-launchd--label "worker" (plist-get fresh :id) epoch))
                  (or
                   ;; Epochs advance only after the former owner has exited or
                   ;; relinquished ownership.  Newer registrations stay protected.
                   (< epoch (plist-get fresh :epoch))
                   (and (= epoch (plist-get fresh :epoch))
                        (or (eq (plist-get fresh :worker-state) 'exited)
                            (and (memq (plist-get fresh :phase) '(open closing))
                                 (not (gptel-runner-invocation-live-p fresh)))))))
         (gptel-runner-launchd--unload (plist-get record :label))
         (delete-file file))))))

(defun gptel-runner-launchd-reap ()
  "Unload completed invocation services, preserving their result history.
Only exited, superseded, or confirmed dead workers are reaped; terminal
pipeline state alone is insufficient while the worker may flush results.
Abandoned launch reservations close only after the exact service is absent."
  (when (eq system-type 'darwin)
    (require 'gptel-runner-jobs)
    (dolist (invocation (gptel-runner-invocation-list))
      (let ((directory (plist-get invocation :directory)))
        ;; Bootstrap precedes the registration write.  Its absence alone is
        ;; never proof that launchd cannot still start this generation.
        (when (gptel-runner-jobs--abandoned-start-p invocation)
          (condition-case err
              (let* ((label (gptel-runner-launchd--label
                             "worker" (plist-get invocation :id)
                             (plist-get invocation :epoch)))
                     (registration (expand-file-name
                                    (format "worker-registration-%d.el"
                                            (plist-get invocation :epoch)) directory)))
                (when (and (gptel-runner-launchd--startup-managed-p invocation label)
                           (eq 113 (plist-get
                                   (gptel-runner-launchd--invoke
                                    "print" (gptel-runner-launchd--target label)) :exit))
                           (gptel-runner-jobs--confirm-stopped invocation))
                  (when (file-exists-p registration) (delete-file registration))))
            (error (message "Could not reconcile background startup: %s"
                            (error-message-string err)))))
        (dolist (file (directory-files directory t
                                      "\\`worker-registration-[0-9]+\\.el\\'"))
          (condition-case err
              (gptel-runner-launchd--reap-registration invocation file)
            (error (message "Could not reap background worker: %s"
                            (error-message-string err)))))))))

(provide 'gptel-runner-launchd)
;;; gptel-runner-launchd.el ends here
