;;; gptel-runner-jobs.el --- Isolated durable workflow jobs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Jobs are reusable launch configurations.  An invocation has one owning
;; batch Emacs, an execution checkpoint, an inspection history, and a typed
;; command inbox.  Reading history never creates executable runner objects.
;; The command line entry point also works with external schedulers such as cron.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-runner-core)
(require 'gptel-runner-flow)
(require 'gptel-runner-store)

(declare-function gptel-runner-gptel-answer-interaction "gptel-runner-gptel")
(declare-function gptel-runner-gptel-answer-questions "gptel-runner-gptel")
(declare-function gptel-runner-gptel-check-interaction "gptel-runner-gptel")
(declare-function gptel-runner-gptel-approve-interactions "gptel-runner-gptel")
(declare-function gptel-runner-gptel-deny-interactions "gptel-runner-gptel")
(declare-function gptel-runner-gptel-revoke-outside-access "gptel-runner-gptel")
(declare-function gptel-runner-gptel-interaction-pending-p "gptel-runner-gptel")
(declare-function gptel-runner-gptel-feedback "gptel-runner-gptel")
(declare-function gptel-runner-complete-call-from-buffer "gptel-runner-gptel")
(declare-function gptel-agent-update "gptel-agent")

(autoload 'gptel-runner-windows-scheduler-arguments "gptel-runner-windows")
(autoload 'gptel-runner-windows-start-worker "gptel-runner-windows")

(defgroup gptel-runner-jobs nil
  "Durable background workflow jobs."
  :group 'gptel-runner)

(defcustom gptel-runner-job-directory
  (expand-file-name "gptel-runner/jobs/" user-emacs-directory)
  "Root directory for job definitions, invocation history, and locks."
  :type 'directory)

(defcustom gptel-runner-job-emacs-program
  (expand-file-name invocation-name invocation-directory)
  "Absolute Emacs executable used for background jobs."
  :type 'file)

(defcustom gptel-runner-job-preload-features 'auto
  "Packages loaded in new job workers before their initialization file.
With `auto', saving a new job selects gptel and includes gptel-agent when
its library is on the worker's search path.  A list selects the required
features explicitly, including nil for standalone jobs.  The job captures
the selected list as :preload-features.  An explicit job value overrides it.
When gptel-agent is included, its definitions are refreshed before the
initialization file, so that file has final authority over presets."
  :type '(choice (const :tag "Detect gptel-agent when saving a job" auto)
                 (repeat symbol)))

(defcustom gptel-runner-job-load-path nil
  "Extra Lisp library directories captured when saving a new job.
These precede the editor's current `load-path'.  The resulting absolute
paths are saved as :load-path, excluding Emacs's bundled Lisp directories.
Set this once for packages in local checkouts that are not on `load-path'.
An explicit job :load-path overrides automatic capture.  Re-saving a job
preserves its saved paths unless a replacement :load-path is supplied."
  :type '(repeat directory))

(defcustom gptel-runner-job-launch-function #'gptel-runner-job-launch-native-worker
  "Function accepting an invocation record and starting its worker.
The default uses launchd on macOS, systemd user services on GNU/Linux,
and a detached Emacs process on Windows.
Custom launchers can use `gptel-runner-job-worker-command' and
`gptel-runner-job-await-worker'.  The batch `run' command executes directly."
  :type 'function)

(defun gptel-runner-jobs--default-scheduler-program ()
  "Return the OS default executable for the configurable command scheduler."
  (when (eq system-type 'windows-nt) "schtasks.exe"))

(defun gptel-runner-jobs--default-scheduler-function ()
  "Return the OS default argument builder for the configurable scheduler."
  (when (eq system-type 'windows-nt) #'gptel-runner-windows-scheduler-arguments))

(defcustom gptel-runner-job-scheduler-program (gptel-runner-jobs--default-scheduler-program)
  "Executable for an external recurring scheduler, or nil for the OS default.
Windows defaults to schtasks.exe.  GNU/Linux uses systemd user timers
and macOS uses launchd.  To change the scheduler's command syntax,
set `gptel-runner-job-scheduler-function'."
  :type '(choice (const :tag "Native scheduler" nil) string))

(defcustom gptel-runner-job-scheduler-arguments nil
  "Literal arguments passed to the external scheduler before each operation.
Each string is a single argument, including when it contains spaces."
  :type '(repeat string))

(defcustom gptel-runner-job-scheduler-function (gptel-runner-jobs--default-scheduler-function)
  "Argument builder for `gptel-runner-job-scheduler-program'.
The function receives OPERATION, JOB and COMMAND.  OPERATION is one of
`install', `remove' or `diagnostics'.  JOB is the saved job definition.
COMMAND is a list containing the Emacs executable and its arguments for
running JOB when the schedule fires.  Return a list of argument strings.
Runner calls the builder at installation to capture the commands it will
need later.  Use `gptel-runner-job-schedule-id' to name the task.
On Windows the default is `gptel-runner-windows-scheduler-arguments'."
  :type '(choice (const :tag "Native scheduler" nil) function))

(defcustom gptel-runner-job-poll-interval 0.2
  "Seconds between worker command and external lock checks."
  :type 'number)

(defconst gptel-runner-jobs--package-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the installed runner Lisp files.")

(defconst gptel-runner-jobs--version 1)
(define-error 'gptel-runner-job-cleanup-error
  "Could not confirm that a background worker stopped")
(define-error 'gptel-runner-job-storage-error "Background job storage failed")
(defconst gptel-runner-jobs--operations
  '(pause resume cancel retry rerun continue extend extend-repeat decision
	  pause-call cancel-call feedback accept approve approve-all deny reject answer cancel-questions
          revoke-outside))

(defvar gptel-runner-headless nil)
(defvar gptel-runner-interaction-hook nil)
(defvar gptel-runner-call-admission-functions nil)
(defvar gptel-runner-call-release-hook nil)
(defvar native-comp-enable-subr-trampolines)
(defvar native-comp-jit-compilation)
(defvar gptel-runner-jobs--invocation nil)
(defvar gptel-runner-jobs--run nil)
(defvar gptel-runner-jobs--dirty nil)
(defvar gptel-runner-jobs--pending-events nil)
(defvar gptel-runner-jobs--interactions nil)
(defvar gptel-runner-jobs--writer-locks nil)
(defvar gptel-runner-jobs--exports nil)
(defvar gptel-runner-jobs--command-started nil)

(defun gptel-runner-jobs--id ()
  "Return a new opaque identifier suitable for a filename."
  (substring (secure-hash 'sha256
                          (format "%s:%s:%s:%s:%s" (system-name) (emacs-pid)
                                  (float-time) (random) (current-time))) 0 32))

(defun gptel-runner-jobs--valid-id (id)
  "Validate opaque string ID before using it as a path component."
  (unless (and (stringp id) (string-match-p "\\`[[:alnum:]_-]+\\'" id))
    (error "Invalid job or invocation identifier: %S" id))
  id)

(defun gptel-runner-jobs--mkdir (directory)
  "Create private DIRECTORY and return its absolute name."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (make-directory directory t)
  (set-file-modes directory #o700)
  directory)

(defun gptel-runner-jobs--root ()
  "Return the absolute configured job root without creating it."
  (file-name-as-directory (expand-file-name gptel-runner-job-directory)))

(defun gptel-runner-jobs--data-string (data)
  "Return a readable representation of DATA, or signal an error."
  (let* ((print-length nil) (print-level nil) (print-circle t)
         (print-escape-newlines t) (print-escape-control-characters t)
         (text (prin1-to-string data)))
    ;; Reading validates that opaque objects and closures containing them do
    ;; not silently turn a durable record into an unreadable file.
    (condition-case err
        (unless (= (cdr (read-from-string text)) (length text))
          (error "Serialized value has trailing unreadable data"))
      (error (error "Job data is not serializable: %s"
                    (error-message-string err))))
    (concat text "\n")))

(defun gptel-runner-jobs--write (file data)
  "Atomically replace private FILE with DATA read as Lisp data."
  (let* ((text (gptel-runner-jobs--data-string data))
         (directory (gptel-runner-jobs--mkdir (file-name-directory file)))
         (temporary (make-temp-file (expand-file-name ".write-" directory))))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix)
              (write-region-inhibit-fsync nil))
          (set-file-modes temporary #o600)
          (write-region text nil temporary nil 'silent)
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary))))
  data)

(defun gptel-runner-jobs--read (file)
  "Read exactly one data form from FILE without loading or evaluating it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (skip-chars-forward " \t\r\n")
      (unless (eobp) (error "Trailing data in %s" file))
      data)))

(defun gptel-runner-jobs--record (file kind)
  "Read versioned record of KIND from FILE."
  (let ((record (gptel-runner-jobs--read file)))
    (unless (and (eq (plist-get record :format) kind)
                 (equal (plist-get record :version) gptel-runner-jobs--version))
      (error "Unsupported %s record: %s" kind file))
    record))

(defun gptel-runner-jobs--definition-file (id)
  "Return the definition path for job ID."
  (expand-file-name (concat "definitions/" (gptel-runner-jobs--valid-id id) ".el")
                    (gptel-runner-jobs--root)))

(defun gptel-runner-job-get (id)
  "Read the saved job identified by ID."
  (gptel-runner-jobs--record (gptel-runner-jobs--definition-file id) 'runner-job))

(defun gptel-runner-job-list ()
  "Return saved job definitions, ordered by name."
  (let ((directory (expand-file-name "definitions" (gptel-runner-jobs--root))))
    (when (file-directory-p directory)
      (sort (mapcar (lambda (file) (gptel-runner-jobs--record file 'runner-job))
                    (directory-files directory t "\\.el\\'"))
            (lambda (a b) (string-lessp (plist-get a :name)
					(plist-get b :name)))))))

(defun gptel-runner-jobs--validate-schedule (schedule)
  "Validate a recurring job SCHEDULE."
  (pcase schedule
    (`(:interval ,seconds)
     (unless (and (integerp seconds) (> seconds 0))
       (error "Schedule interval must be positive seconds")))
    (`(:calendar ,rules)
     (unless (and rules (proper-list-p rules))
       (error "Calendar schedule needs a list of rules"))
     (dolist (rule rules)
       (unless (and rule (proper-list-p rule) (cl-evenp (length rule)))
         (error "Invalid calendar rule: %S" rule))
       (let (seen)
         (cl-loop for (key value) on rule by #'cddr
                  for range = (assq key '((:minute 0 59) (:hour 0 23)
                                          (:day 1 31) (:weekday 0 7) (:month 1 12)))
                  do (unless (and range (not (memq key seen)) (integerp value)
                                  (<= (cadr range) value (caddr range)))
                       (error "Invalid calendar field: %S %S" key value))
                  do (push key seen)))))
    (_ (error "Expected (:interval SECONDS) or (:calendar (RULE ...))"))))

(defun gptel-runner-jobs--library-paths (paths &optional base)
  "Normalize library PATHS against BASE, excluding bundled Emacs Lisp.
Nil entries, which normally mean the current directory, are omitted so
workers do not search a different workspace for packages."
  (unless (and (proper-list-p paths)
               (cl-every (lambda (path) (or (null path) (stringp path))) paths))
    (error "Job :load-path must be a list of library directories"))
  (let ((bundled (file-name-as-directory (file-truename lisp-directory)))
        result)
    (dolist (path paths (nreverse result))
      (when path
        (let ((absolute (file-name-as-directory (expand-file-name path base))))
          (when (file-remote-p absolute)
            (error "Job library directories must be local: %s" path))
          (unless (or (string-prefix-p bundled
                                       (file-name-as-directory (file-truename absolute)))
                      (member absolute result))
            (push absolute result)))))))

(defun gptel-runner-jobs--default-preloads (paths)
  "Return new job preloads for the external library PATHS saved for a worker."
  (cond ((not (eq gptel-runner-job-preload-features 'auto))
         (copy-tree gptel-runner-job-preload-features))
        ((locate-library "gptel-agent" nil
                         (append paths (list gptel-runner-jobs--package-directory)))
         (list 'gptel 'gptel-agent))
        (t (list 'gptel))))

(defun gptel-runner-job-save (job)
  "Validate and save JOB, a plist, and return its saved definition.
Required keys are :name, :workflow, and :init-file.  Optional keys are
:id, :arguments (runner start arguments), :schedule, :environment,
:emacs, :base-directory, :enabled, :load-path, and :preload-features.
New jobs capture external library paths and default preload features;
updates preserve them unless explicitly replaced.  Saving captures the
base directory; the worker resolves the workflow's workspace default after
loading the initialization file."
  (setq job (copy-tree job))
  (gptel-runner-jobs--mkdir (gptel-runner-jobs--root))
  (unless (and (stringp (plist-get job :name))
               (not (string-empty-p (plist-get job :name)))
               (symbolp (plist-get job :workflow)) (plist-get job :workflow))
    (error "A job needs a name and a named workflow"))
  (let* ((id (or (plist-get job :id) (gptel-runner-jobs--id)))
         (old (when (file-exists-p (gptel-runner-jobs--definition-file id))
                (gptel-runner-job-get id)))
         ;; Native scheduler names are per user, while named job IDs are per store.
         ;; Old definitions already have a service identity equal to their ID.
         (launchd-id (cond (old (or (plist-get old :launchd-id) id))
                           ((plist-get job :id) (gptel-runner-jobs--id))
                           (t id)))
         (init (plist-get job :init-file))
         (arguments (copy-tree (plist-get job :arguments)))
         (base (file-name-as-directory
                (file-truename (or (plist-get job :base-directory)
                                   (plist-get old :base-directory)
                                   default-directory))))
         (emacs (or (plist-get job :emacs) gptel-runner-job-emacs-program))
         ;; Explicit bootstrap fields take precedence over saved ones,
         ;; including when their value is nil.
         (bootstrap (append job old))
         (paths (if-let* ((entry (plist-member bootstrap :load-path)))
                    (gptel-runner-jobs--library-paths (cadr entry) base)
                  (gptel-runner-jobs--library-paths
                   (append gptel-runner-job-load-path load-path))))
         (preloads (if-let* ((entry (plist-member bootstrap :preload-features)))
                       (copy-tree (cadr entry))
                     (gptel-runner-jobs--default-preloads paths))))
    (unless (and (stringp init) (file-readable-p init)
                 (not (file-directory-p init)))
      (error "Job initialization file is not readable: %S" init))
    (unless (and (proper-list-p arguments) (cl-evenp (length arguments)))
      (error "Job :arguments must be a keyword plist"))
    (unless (and (proper-list-p preloads)
                 (cl-every (lambda (feature)
                             (and feature (symbolp feature) (not (keywordp feature))))
                           preloads))
      (error "Job :preload-features must be a list of feature symbols"))
    (cl-loop for (key _value) on arguments by #'cddr
             unless (memq key '(:goal :workspace :inputs :max-requests :max-calls
				      :max-concurrency :max-duration :allow-writes
				      :allow-unconfirmed-tools :decision-memory :context :completion))
             do (error "Unsupported durable job start argument: %S" key))
    (unless (and (stringp emacs) (file-name-absolute-p emacs)
                 (file-executable-p emacs))
      (error "Job Emacs must be an absolute executable: %S" emacs))
    (dolist (entry (plist-get job :environment))
      (unless (and (consp entry) (stringp (car entry)) (stringp (cdr entry))
                   (string-match-p "\\`[A-Za-z_][A-Za-z_0-9]*\\'" (car entry)))
        (error "Job environment must contain string name/value pairs")))
    (when-let* ((schedule (plist-get job :schedule)))
      (gptel-runner-jobs--validate-schedule schedule))
    (unless (file-directory-p base)
      (error "Job base directory is not a directory: %s" base))
    (when (plist-member arguments :workspace)
      (let ((workspace (file-name-as-directory
                        (file-truename
                         (expand-file-name (or (plist-get arguments :workspace) base) base)))))
        (unless (file-directory-p workspace)
          (error "Job workspace is not a directory: %s" workspace))
        (setq arguments (plist-put arguments :workspace workspace))))
    (setq job (plist-put job :arguments arguments)
          job (plist-put job :load-path paths)
          job (plist-put job :preload-features preloads)
          job (plist-put job :base-directory base)
          job (plist-put job :id id)
          job (plist-put job :launchd-id launchd-id)
          job (plist-put job :init-file (expand-file-name init))
          job (plist-put job :emacs emacs)
          job (plist-put job :root (gptel-runner-jobs--root))
          job (plist-put job :revision (1+ (or (plist-get old :revision) 0)))
          job (plist-put job :format 'runner-job)
          job (plist-put job :version gptel-runner-jobs--version))
    (unless (plist-member job :enabled) (setq job (plist-put job :enabled t)))
    (gptel-runner-jobs--write (gptel-runner-jobs--definition-file id) job)))

(defun gptel-runner-jobs--update-definition (job function &optional create)
  "Call FUNCTION on the latest definition of JOB under its store lock.
JOB is a string ID or saved record.  With CREATE, supply a new definition
when the ID is unused."
  (let* ((id (if (stringp job) job (plist-get job :id)))
         (gptel-runner-job-directory
          (or (and (listp job) (plist-get job :root)) gptel-runner-job-directory))
         (file (gptel-runner-jobs--definition-file id))
         (lock (expand-file-name (concat "locks/definitions/" id)
                                 (gptel-runner-jobs--root)))
         result)
    (unless (gptel-runner-jobs--with-lock-guard
             lock
             (lambda ()
               (let ((definition (if (and create (not (file-exists-p file)))
                                     (list :id id :name id)
                                   (gptel-runner-job-get id))))
                 (setq result (funcall function definition))
                 t)))
      (user-error "Job %s is being updated; retry shortly" id))
    result))

;;;###autoload
(defun gptel-runner-register-job (id &rest properties)
  "Define or update the background job with stable string ID.
Use letters, digits, hyphens, and underscores for ID.
Supply :workflow and :init-file for a new job.  Its :name defaults to ID.
PROPERTIES are the job options documented by `gptel-runner-job-save'.
Only supplied fields are replaced, including the entire :arguments plist
when present.  Return the saved definition.  Use `gptel-runner-job-run'
to launch it or `gptel-runner-job-schedule' to install its schedule."
  (gptel-runner-jobs--valid-id id)
  (unless (cl-evenp (length properties))
    (error "Job properties must be keyword/value pairs"))
  (gptel-runner-jobs--update-definition
   id (lambda (job)
        (let (seen)
          (cl-loop for (key value) on properties by #'cddr
                   do (unless (memq key '(:name :workflow :init-file :arguments
                                          :schedule :environment :emacs :base-directory
                                          :enabled :load-path :preload-features))
                        (error "Unknown job property: %S" key))
                   do (when (memq key seen) (error "Duplicate job property: %S" key))
                   do (push key seen)
                   do (setq job (plist-put job key value))))
        (gptel-runner-job-save job))
   t))

(defun gptel-runner-jobs--scheduler ()
  "Return the native user scheduler for this operating system."
  (pcase system-type
    ('darwin 'launchd)
    ('gnu/linux 'systemd)
    ('windows-nt 'windows)))

(defun gptel-runner-job-call-process (program &rest arguments)
  "Run PROGRAM with literal ARGUMENTS and collect its exit status and output.
Return a plist with :exit, :output (stdout and stderr), and :arguments.
PROGRAM can be an absolute filename or a command found via variable `exec-path'."
  (unless (and (stringp program) (not (string-empty-p program))
               (cl-every #'stringp arguments))
    (user-error "A scheduler command needs an executable and string arguments"))
  (let ((executable (or (executable-find program)
                        (user-error "Cannot find scheduler executable %s" program))))
    (with-temp-buffer
      (let ((status (apply #'call-process executable nil '(t t) nil arguments)))
        (list :exit status :output (buffer-string) :arguments arguments)))))

(defun gptel-runner-job-schedule-id (job)
  "Return JOB's stable identity for naming a task in a per-user scheduler."
  (gptel-runner-jobs--valid-id (or (plist-get job :launchd-id) (plist-get job :id))))

(defun gptel-runner-jobs--schedule-backend (operation job)
  "Select the backend for schedule OPERATION on JOB."
  (let* (;; Windows uses the same persisted commands as a user-supplied CLI.
         (native (pcase (gptel-runner-jobs--scheduler) ('windows 'command) (other other)))
         (configured (if (or gptel-runner-job-scheduler-program
                             gptel-runner-job-scheduler-function)
                         'command native))
         (status (plist-get job :scheduler))
         (installed (unless (memq (plist-get status :state) '(nil removed))
                      ;; Definitions written before backend selection used
                      ;; the native scheduler on the machine that saved them.
                      (or (plist-get status :backend) native))))
    (when (and (eq operation 'install) installed (not (eq configured installed)))
      (user-error "Unschedule job %s from %s before installing with %s"
                  (plist-get job :id) installed (or configured system-type)))
    (or installed configured)))

(defun gptel-runner-jobs--scheduler-call (operation &rest arguments)
  "Call the selected scheduler's OPERATION with ARGUMENTS."
  (let ((backend (if (memq operation '(start-worker reap))
                     (gptel-runner-jobs--scheduler)
                   (gptel-runner-jobs--schedule-backend operation (car arguments)))))
    (cond ((memq backend '(command launchd systemd))
           (require (intern (format "gptel-runner-%s" backend)))
           (apply (intern (format "gptel-runner-%s-%s" backend operation)) arguments))
          ((and (eq operation 'start-worker) (eq backend 'windows))
           (gptel-runner-windows-start-worker (car arguments)))
          ((and (null backend) (eq operation 'delete-schedule)
                (memq (plist-get (plist-get (car arguments) :scheduler) :state)
                      '(nil removed))) nil)
          ((eq operation 'start-worker)
           (user-error "Configure gptel-runner-job-launch-function on %s" system-type))
          ((eq operation 'reap)
           ;; A scheduled-run owns its process directly.  A custom launcher
           ;; may still have a pending child after its launching process exits.
           (dolist (run (gptel-runner-invocation-list))
             (when (and (eq (plist-get run :launcher) 'direct)
                        (gptel-runner-jobs--abandoned-start-p run))
               (gptel-runner-jobs--confirm-stopped run))))
          (t (user-error "Configure gptel-runner-job-scheduler-program and its argument builder for %s"
                         (or backend system-type))))))

(defun gptel-runner-jobs--write-scheduler-file (file text)
  "Atomically write private TEXT to FILE."
  (let* ((directory (file-name-directory file)) temporary)
    (make-directory directory t)
    (setq temporary (make-temp-file (expand-file-name ".runner-" directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary (insert text)))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(defun gptel-runner-jobs--native-settings-arguments ()
  "Return batch arguments restoring customized native scheduler settings."
  (let* ((options
          (pcase system-type
            ('darwin '(gptel-runner-launchctl-program gptel-runner-launchctl-arguments
                       gptel-runner-launchd-startup-timeout))
            ('gnu/linux '(gptel-runner-systemctl-command gptel-runner-systemd-run-command
                          gptel-runner-systemd-startup-timeout))))
         (settings
          (cl-loop for option in options
                   for standard = (get option 'standard-value)
                   when (and (boundp option)
                             (not (and standard
                                       (equal (symbol-value option) (eval (car standard) t)))))
                   append (list option (list 'quote (symbol-value option))))))
    (when settings
      (list "--eval" (string-trim-right
                      (gptel-runner-jobs--data-string (cons 'setq settings)))))))

(defun gptel-runner-jobs--batch-arguments (job &rest operation)
  "Build Emacs arguments for JOB and batch OPERATION."
  (let ((emacs (plist-get job :emacs)))
    (unless (and (stringp emacs) (file-name-absolute-p emacs)
                 (file-executable-p emacs))
      (error "Job must specify an executable absolute :emacs path"))
    (vconcat
     (list emacs "-Q" "--batch" "-L" gptel-runner-jobs--package-directory
           "-l" "gptel-runner-jobs")
     (when (equal (car operation) "trigger")
       (gptel-runner-jobs--native-settings-arguments))
     '("-f" "gptel-runner-jobs-batch-main" "--")
     operation)))

(defun gptel-runner-job-scheduled-command (job)
  "Return executable and argument strings for running JOB on a scheduler tick.
The fresh batch Emacs checks the latest saved definition and executes the
workflow directly, including when the editor has exited."
  (append (gptel-runner-jobs--batch-arguments
           job "scheduled-run" (gptel-runner-jobs--valid-id (plist-get job :id))
           "--root" (expand-file-name (or (plist-get job :root) gptel-runner-job-directory)))
          nil))

(defun gptel-runner-job-worker-command (invocation)
  "Return the executable and arguments to adopt INVOCATION's worker reservation."
  (append (gptel-runner-jobs--batch-arguments
           (plist-get invocation :job) "worker" (plist-get invocation :directory)
           "--epoch" (number-to-string (plist-get invocation :epoch))) nil))

(defun gptel-runner-jobs--scheduler-status (job state operation &optional error-data details)
  "Save scheduler STATE for JOB after OPERATION with ERROR-DATA and DETAILS.
DETAILS can supply a plist of backend-specific metadata."
  (setq job (copy-sequence job))
  (setq job (plist-put job :scheduler
                       (append (list :state state :operation operation
                                     :updated-at (float-time) :error error-data)
                               (or details (list :backend (gptel-runner-jobs--scheduler))))))
  (if (and (memq operation '(remove disable)) (eq (plist-get job :format) 'runner-job))
      ;; Cleanup is permitted after the original workspace or init file is
      ;; gone.  Change only scheduling metadata in the saved definition.
      (let* ((id (plist-get job :id))
             (saved (gptel-runner-job-get id)))
        (setq saved (plist-put saved :scheduler (plist-get job :scheduler))
              saved (plist-put saved :enabled (plist-get job :enabled))
              saved (plist-put saved :revision (1+ (or (plist-get saved :revision) 0))))
        (gptel-runner-jobs--write (gptel-runner-jobs--definition-file id) saved))
    (gptel-runner-job-save job)))

(defun gptel-runner-job-await-worker (invocation timeout)
  "Wait up to TIMEOUT seconds for INVOCATION's worker to adopt or exit.
The launching process stays alive until ownership is handed over, preventing
a subsequent trigger from reclaiming a dead launcher's reservation."
  (unless (and (numberp timeout)
               (> timeout 0))
    (error "Worker startup timeout must be positive"))
  (let ((gptel-runner-job-directory (plist-get invocation :root))
        (deadline (+ (float-time) timeout))
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
                 timeout))
        (accept-process-output nil 0.05)))
    adopted))

(defalias 'gptel-runner-jobs--await-worker #'gptel-runner-job-await-worker)

;;;###autoload
(defun gptel-runner-job-schedule (job &optional schedule)
  "Install or update JOB's recurring schedule and return the saved job.
JOB is its string ID or a saved record.  Use its latest saved schedule
unless SCHEDULE supplies a new (:interval SECONDS) or (:calendar (RULE ...))
value.  Installing a schedule does not run the job immediately."
  (gptel-runner-jobs--update-definition
   job (lambda (definition)
         (setq schedule (or schedule (plist-get definition :schedule)))
         (gptel-runner-jobs--validate-schedule schedule)
         (gptel-runner-jobs--scheduler-call
          'install (plist-put definition :schedule schedule)))))

;;;###autoload
(defun gptel-runner-job-unschedule (job)
  "Remove JOB's recurring trigger and return the updated saved job.
JOB is its string ID or a saved record.  Keep its schedule rule, invocation
history, and running workers."
  (gptel-runner-jobs--update-definition
   job (lambda (definition) (gptel-runner-jobs--scheduler-call 'remove definition))))

(defun gptel-runner-job-delete (id)
  "Remove job ID and its schedule, retaining all invocation history."
  (gptel-runner-jobs--update-definition
   id (lambda (job)
        (gptel-runner-jobs--scheduler-call 'delete-schedule job)
        (delete-file (gptel-runner-jobs--definition-file (plist-get job :id))))))

(defun gptel-runner-jobs--manifest-file (invocation)
  "Return manifest filename for INVOCATION."
  (expand-file-name "manifest.el" (plist-get invocation :directory)))

(defun gptel-runner-invocation-get (id)
  "Read invocation ID without creating executable runner state."
  (gptel-runner-jobs--record
   (expand-file-name (concat "runs/" (gptel-runner-jobs--valid-id id) "/manifest.el")
                     (gptel-runner-jobs--root)) 'runner-invocation))

(defun gptel-runner-invocation-list ()
  "Return saved invocation manifests, newest first.
An interrupted or corrupt manifest does not hide other invocations."
  (let ((directory (expand-file-name "runs" (gptel-runner-jobs--root))) records)
    (when (file-directory-p directory)
      (dolist (entry (directory-files directory t "\\`[^.]"))
        (when (file-exists-p (expand-file-name "manifest.el" entry))
          (condition-case err
              (let ((record (gptel-runner-jobs--record
                             (expand-file-name "manifest.el" entry)
                             'runner-invocation)))
                (when (or (and (memq (plist-get record :phase) '(open closing))
                               (not (gptel-runner-invocation-live-p record)))
                          (and (eq (plist-get record :phase) 'starting)
                               (eq (gptel-runner-jobs--lock-owner-state
                                    (plist-get record :worker)) 'dead)))
                  ;; This is an inspection projection only; do not rewrite a
                  ;; possibly live worker's files from the dashboard.
                  (setq record (plist-put record :state 'interrupted)))
                (push record records))
            (error (message "Cannot read job history %s: %s"
                            entry (error-message-string err)))))))
    (sort records (lambda (a b) (> (plist-get a :created-at)
                                   (plist-get b :created-at))))))

(defun gptel-runner-jobs--artifact-path (invocation relative)
  "Resolve RELATIVE inspection file within INVOCATION's directory."
  (let* ((directory (file-name-as-directory
                     (file-truename (plist-get invocation :directory))))
         (file (file-truename (expand-file-name relative directory))))
    (unless (and (not (file-name-absolute-p relative))
                 (string-prefix-p directory file))
      (error "Inspection artifact escapes invocation directory"))
    file))

(defun gptel-runner-invocation-outputs (invocation)
  "Read all named outputs for INVOCATION on demand."
  (when-let* ((file (plist-get invocation :outputs-file)))
    (gptel-runner-jobs--read (gptel-runner-jobs--artifact-path invocation file))))

(defun gptel-runner-jobs--inspection-record (source)
  "Validate SOURCE as a background invocation inspection record."
  (unless (eq (plist-get source :format) 'runner-invocation)
    (user-error "Not a runner invocation record: %S" source))
  source)

(cl-defmethod gptel-runner-outputs ((source list))
  "Read named values from background invocation SOURCE."
  (gptel-runner-invocation-outputs
   (gptel-runner-jobs--inspection-record source)))

(cl-defmethod gptel-runner-artifacts ((source list))
  "Return the descriptors published by background invocation SOURCE."
  (copy-tree (plist-get (gptel-runner-jobs--inspection-record source) :artifacts)))

(cl-defmethod gptel-runner--artifact-root ((source list))
  "Return the storage directory of background invocation SOURCE."
  (plist-get (gptel-runner-jobs--inspection-record source) :directory))

(cl-defmethod gptel-runner-node-outcome ((run list) node)
  "Read NODE's exported outcome from invocation RUN without restoring it."
  (gptel-runner-jobs--inspection-record run)
  (let* ((id (if (gptel-runner-node-p node) (gptel-runner-node-id node) node))
         (file (plist-get run :nodes-file))
         (outcomes (when file
                     (gptel-runner-jobs--read
                      (gptel-runner-jobs--artifact-path run file)))))
    (unless file
      (user-error "This invocation has no exported node outcomes"))
    (or (cl-find id outcomes :key (lambda (entry) (plist-get entry :node))
                 :test #'equal)
        (user-error "Unknown node %S in invocation %s" id (plist-get run :id)))))

(defun gptel-runner-invocation-call (invocation call)
  "Read the complete CALL inspection record for INVOCATION on demand."
  (if-let* ((file (plist-get call :record)))
      (gptel-runner-jobs--read (gptel-runner-jobs--artifact-path invocation file))
    call))

(defconst gptel-runner-jobs--linux-boot-id
  (when (eq system-type 'gnu/linux)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents-literally "/proc/sys/kernel/random/boot_id")
          (let ((id (string-trim (buffer-string))))
            (if (string-empty-p id) 'unknown id)))
      (file-error 'unknown)))
  "Linux boot ID, `unknown' if unavailable, or nil on another OS.")

(defun gptel-runner-jobs--process-attributes (pid &optional legacy)
  "Return state and stable start identity for PID, or nil if unavailable.
LEGACY requests the wall-clock start time used by older records."
  (if (or legacy (null gptel-runner-jobs--linux-boot-id))
      (process-attributes pid)
    ;; On Linux, Emacs estimates the absolute start time on each query from
    ;; the wall clock and /proc/uptime.  Use the kernel's raw start ticks.
    (when (stringp gptel-runner-jobs--linux-boot-id)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents-literally (format "/proc/%d/stat" pid))
            (goto-char (point-max))
            ;; The command in field 2 can contain spaces, newlines and ')'.
            (when (search-backward ") " nil t)
              (forward-char 2)
              (let* ((fields (split-string (buffer-substring (point) (point-max))))
                     (ticks (nth 19 fields))) ; Field 22, counting from state.
                (when (and ticks (string-match-p "\\`[0-9]+\\'" ticks))
                  (list (cons 'state (car fields))
                        (list 'start 'linux gptel-runner-jobs--linux-boot-id
                              (string-to-number ticks)))))))
        (file-error nil)))))

(defun gptel-runner-jobs--owner (token invocation)
  "Return this process's ownership identity with TOKEN and INVOCATION ID."
  (list :pid (emacs-pid)
        :start (alist-get 'start (gptel-runner-jobs--process-attributes (emacs-pid)))
        :host (system-name) :token token :invocation invocation
        :created-at (float-time)))

(defun gptel-runner-jobs--owner-live-p (owner)
  "Return non-nil if OWNER's exact process identity still exists."
  (eq (gptel-runner-jobs--lock-owner-state owner t) 'live))

(defun gptel-runner-invocation-live-p (invocation)
  "Return non-nil when INVOCATION has a live worker or launch reservation."
  (and (not (eq (plist-get invocation :phase) 'closed))
       (gptel-runner-jobs--owner-live-p (plist-get invocation :worker))))

(defun gptel-runner-invocation-forgettable-p (invocation)
  "Return non-nil when INVOCATION's worker has stopped safely.
Paused workers and unconfirmed launch reservations remain protected."
  (or (eq (plist-get invocation :phase) 'closed)
      (and (memq (plist-get invocation :phase) '(open closing))
           (eq (gptel-runner-jobs--lock-owner-state
                (plist-get invocation :worker)) 'dead))))

(defun gptel-runner-invocation-forgotten-p (invocation)
  "Return non-nil when INVOCATION's current generation was forgotten."
  (and (plist-member invocation :forgotten-epoch)
       (equal (plist-get invocation :forgotten-epoch)
              (plist-get invocation :epoch))))

(defun gptel-runner-invocation-forget (invocation &optional delete-checkpoint)
  "Hide stopped INVOCATION or its ID from the dashboard persistently.
Keep saved history accessible through `gptel-runner-invocation-list' and
other inspection APIs.  A new worker generation becomes visible again.
With DELETE-CHECKPOINT, also delete its recovery checkpoint; results,
transcripts, artifacts, job definitions, and schedules are preserved."
  (when (stringp invocation)
    (setq invocation (gptel-runner-invocation-get invocation)))
  (let ((gptel-runner-job-directory (plist-get invocation :root)))
    (gptel-runner-jobs--with-control-lock
     invocation
     (lambda ()
       (let ((fresh (gptel-runner-invocation-get (plist-get invocation :id))))
         (unless (and (equal (plist-get fresh :epoch) (plist-get invocation :epoch))
                      (gptel-runner-invocation-forgettable-p fresh))
           (user-error "Background run changed or still has a worker; refresh after it stops"))
         (when delete-checkpoint
           (let ((file (expand-file-name "checkpoint.el" (plist-get fresh :directory))))
             (when (file-exists-p file) (delete-file file))))
         (setq fresh (plist-put fresh :forgotten-epoch (plist-get fresh :epoch)))
         (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file fresh) fresh))))))

(defun gptel-runner-jobs--lock-owner-state (owner &optional process-only)
  "Classify OWNER as `live', `dead', or `unknown' by exact process identity.
Incomplete identities and owners on another host are never declared dead.
PROCESS-ONLY checks the process even when its launch outcome is uncertain."
  (let* ((pid (and (proper-list-p owner) (plist-get owner :pid)))
         (start (and pid (plist-get owner :start)))
         (linux (pcase start
                  (`(linux ,(pred stringp) ,(pred natnump)) t))))
    (if (not (and (integerp pid) (> pid 0)
                  (or process-only (not (plist-get owner :uncertain)))
                  (equal (plist-get owner :host) (system-name))
                  (if linux (stringp gptel-runner-jobs--linux-boot-id)
                    (and start (ignore-errors (time-convert start 'integer))))))
        'unknown
      (condition-case nil
          (let* ((attributes (gptel-runner-jobs--process-attributes pid (not linux)))
                 (observed (alist-get 'start attributes)))
            (cond ((null attributes)
                   (if (memq pid (list-system-processes)) 'unknown 'dead))
                  ((null observed) 'unknown)
                  ((equal (alist-get 'state attributes) "Z") 'dead)
                  ((equal start observed) 'live)
                  ;; A legacy Linux wall-clock mismatch cannot prove PID reuse.
                  ((and (not linux) (eq system-type 'gnu/linux)) 'unknown)
                  (t 'dead)))
        (error 'unknown)))))

(defun gptel-runner-jobs--guard-claims (directory)
  "Read contender claims in stable guard DIRECTORY.
Each entry is (FILE STATE TICKET).  Unknown or incomplete claims stay busy."
  (mapcar
   (lambda (file)
     (let* ((claim (ignore-errors (gptel-runner-jobs--read file)))
            (owner (and (proper-list-p claim) (plist-get claim :owner)))
            (ticket (and (proper-list-p claim) (plist-get claim :ticket))))
       (list file (gptel-runner-jobs--lock-owner-state owner)
             (if (or (null ticket) (and (integerp ticket) (> ticket 0)))
                 ticket 'invalid))))
   (directory-files directory t "\\.claim\\.el\\'")))

(defun gptel-runner-jobs--with-lock-guard (directory function)
  "Call FUNCTION while exclusively guarding DIRECTORY, or return nil if busy.
Acquisition is nonblocking.  Each contender publishes its own choosing flag,
then a Lamport ticket.  It enters only if no live contender is choosing or
holds an earlier ticket.  A failed attempt removes only its own unique claim.
Dead claims can be ignored without unlinking a shared acquisition pathname.
All owner replacement, reclamation, and release must use this guard."
  (let* ((guard (gptel-runner-jobs--mkdir
                 (concat (directory-file-name directory) ".guard")))
         (token (gptel-runner-jobs--id))
         (file (expand-file-name (concat token ".claim.el") guard))
         (owner (gptel-runner-jobs--owner token nil))
         (claim (list :owner owner :ticket nil)))
    (unless (eq (gptel-runner-jobs--lock-owner-state owner) 'live)
      (error "Cannot establish an exact process identity for locking"))
    (unwind-protect
        (progn
          ;; Only complete records become visible; a crash before publication
          ;; leaves an ignored .write-* file rather than an ownerless claim.
          (gptel-runner-jobs--write file claim)
          (let ((ticket 1) unknown)
            (dolist (entry (gptel-runner-jobs--guard-claims guard))
              (unless (eq (nth 1 entry) 'dead)
                (cond ((or (eq (nth 1 entry) 'unknown)
                           (eq (nth 2 entry) 'invalid))
                       (setq unknown t))
                      ((integerp (nth 2 entry))
                       (setq ticket (max ticket (1+ (nth 2 entry))))))))
            (unless unknown
              (gptel-runner-jobs--write file (plist-put claim :ticket ticket))
              (let ((entries (gptel-runner-jobs--guard-claims guard)))
                (unless
                    (cl-some
                     (lambda (entry)
                       (and (not (equal (car entry) file))
                            (not (eq (nth 1 entry) 'dead))
                            (or (eq (nth 1 entry) 'unknown)
                                (not (integerp (nth 2 entry)))
                                (< (nth 2 entry) ticket)
                                (and (= (nth 2 entry) ticket)
                                     (string-lessp (car entry) file)))))
                     entries)
                  ;; These names are never reused by a different contender.
                  ;; The stable guard directory itself is never reclaimed.
                  (dolist (entry entries)
                    (when (eq (nth 1 entry) 'dead)
                      (ignore-errors (delete-file (car entry)))))
                  (funcall function))))))
      (when (file-exists-p file) (delete-file file)))))

(defun gptel-runner-jobs--lock (directory owner)
  "Try to acquire lock DIRECTORY for OWNER; return non-nil on success.
Only a provably dead local owner is reclaimed.  Heartbeats are not leases.
Missing or unreadable owner records remain busy, including initialization
interrupted before owner publication."
  (setq directory (directory-file-name (expand-file-name directory)))
  (gptel-runner-jobs--with-lock-guard
   directory
   (lambda ()
     (unless (file-symlink-p directory)
       (let* ((file (expand-file-name "owner.el" directory))
              (exists (file-exists-p directory))
              (previous (and exists
                             (ignore-errors (gptel-runner-jobs--read file)))))
         (when (or (not exists)
                   (and (file-directory-p directory)
                        (eq (gptel-runner-jobs--lock-owner-state previous) 'dead)))
           (when exists (delete-directory directory t))
           (make-directory directory)
           (condition-case err
               (progn
                 (set-file-modes directory #o700)
                 (gptel-runner-jobs--write file owner)
                 t)
             (error
              ;; Still under the guard: no worker could have acquired this
              ;; unpublished reservation.  Roll back a caught write failure.
              (ignore-errors (delete-directory directory t))
              (signal (car err) (cdr err))))))))))

(defun gptel-runner-jobs--unlock (directory token)
  "Release lock DIRECTORY only when it still belongs to TOKEN.
The owner check and removal are serialized with acquisition and reclamation.
Brief guard contention is retried for at most a quarter second; persistent
contention signals a file error instead of silently abandoning the lock."
  (setq directory (directory-file-name (expand-file-name directory)))
  (let ((deadline (+ (float-time) 0.25)) result)
    (while
        (and
         (not
          (setq result
                (gptel-runner-jobs--with-lock-guard
                 directory
                 (lambda ()
                   (let* ((file (expand-file-name "owner.el" directory))
                          (owner
                           (and (not (file-symlink-p directory))
                                (ignore-errors
                                  (gptel-runner-jobs--read file)))))
                     (cons
                      t (when (and (stringp token)
                                   (equal token (plist-get owner :token)))
                          (delete-directory directory t)
                          t)))))))
         (< (float-time) deadline))
      (sleep-for 0.005))
    (unless result
      (signal 'file-error (list "Lock control is busy; retry release" directory)))
    (cdr result)))

(defun gptel-runner-jobs--job-lock (job-id)
  "Return the lock directory for JOB-ID."
  (expand-file-name (concat "locks/jobs/" (gptel-runner-jobs--valid-id job-id))
                    (gptel-runner-jobs--root)))

(defun gptel-runner-jobs--with-control-lock (invocation function)
  "Call FUNCTION under INVOCATION's short control lock."
  (let* ((directory (expand-file-name "control.lock" (plist-get invocation :directory)))
         (token (gptel-runner-jobs--id))
         (owner (gptel-runner-jobs--owner token (plist-get invocation :id)))
         (deadline (+ (float-time) 5)) acquired)
    (while (and (not (setq acquired (gptel-runner-jobs--lock directory owner)))
                (< (float-time) deadline))
      (sleep-for 0.02))
    (unless acquired (error "Invocation control is busy; retry shortly"))
    (unwind-protect (funcall function)
      (condition-case err
          (gptel-runner-jobs--unlock directory token)
        (error
         ;; A failed release may still own the lock.  The exact token makes
         ;; a second attempt safe even if a successor already acquired it.
         (ignore-errors (gptel-runner-jobs--unlock directory token))
         (signal (car err) (cdr err)))))))

(defun gptel-runner-job-launch-native-worker (invocation)
  "Start INVOCATION in a detached worker for the current operating system."
  (gptel-runner-jobs--scheduler-call 'start-worker invocation))

(defalias 'gptel-runner-jobs--launch #'gptel-runner-job-launch-native-worker)

(defun gptel-runner-jobs--launcher ()
  "Identify the configured launcher for later startup reconciliation."
  (pcase gptel-runner-job-launch-function
    ((or 'gptel-runner-job-launch-native-worker 'gptel-runner-jobs--launch)
     (or (gptel-runner-jobs--scheduler) 'custom))
    ('gptel-runner-launchd-start-worker 'launchd)
    ('gptel-runner-systemd-start-worker 'systemd)
    ('gptel-runner-windows-start-worker 'windows)
    (_ 'custom)))

(defun gptel-runner-jobs--new-invocation (job trigger &optional launcher)
  "Create a durable invocation of JOB for TRIGGER, claiming its job lock.
LAUNCHER defaults to the configured detached launcher; the CLI uses `direct'."
  (let* ((id (gptel-runner-jobs--id))
         (token (gptel-runner-jobs--id))
         (directory (gptel-runner-jobs--mkdir
                     (expand-file-name (concat "runs/" id) (gptel-runner-jobs--root))))
         (owner (gptel-runner-jobs--owner token id))
         (claimed (gptel-runner-jobs--lock
                   (gptel-runner-jobs--job-lock (plist-get job :id)) owner))
         (record (list :format 'runner-invocation :version gptel-runner-jobs--version
                       :id id :root (gptel-runner-jobs--root) :directory directory
                       :job-id (plist-get job :id) :job job :epoch 1
                       :workflow (plist-get job :workflow) :trigger trigger
                       :launcher (or launcher (gptel-runner-jobs--launcher))
                       :created-at (float-time) :worker owner
                       :worker-state (if claimed 'starting 'exited)
                       :phase (if claimed 'starting 'closed)
                       :state (if claimed 'starting 'skipped)
                       :reason (unless claimed 'overlap))))
    (condition-case err
        (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file record) record)
      (error
       (when claimed (gptel-runner-jobs--unlock
                      (gptel-runner-jobs--job-lock (plist-get job :id)) token))
       (signal (car err) (cdr err))))))

(defun gptel-runner-jobs--owned-manifest (invocation)
  "Return the durable manifest matching INVOCATION's epoch and token.
Call under the invocation control lock, and retain it while writing."
  (let ((fresh (gptel-runner-jobs--record
                (gptel-runner-jobs--manifest-file invocation) 'runner-invocation))
        (token (plist-get (plist-get invocation :worker) :token)))
    (and (natnump (plist-get fresh :epoch))
         (stringp token)
         (equal token (plist-get (plist-get fresh :worker) :token))
         (equal (plist-get invocation :epoch) (plist-get fresh :epoch))
         fresh)))

(defun gptel-runner-jobs--finish-pending-receipt (file receipt reason)
  "Finalize pending RECEIPT in FILE with REASON.
A processing command may have taken effect before its worker stopped."
  (setq receipt (plist-put receipt :status
                           (if (eq (plist-get receipt :status) 'processing)
                               'indeterminate 'rejected))
        receipt (plist-put receipt :error reason)
        receipt (plist-put receipt :finished-at (float-time)))
  (gptel-runner-jobs--write file receipt))

(defun gptel-runner-jobs--fail-receipt (file invocation reason)
  "Finalize pending receipt FILE if it belongs to INVOCATION, using REASON."
  (let ((receipt (gptel-runner-jobs--read file)))
    (when (and (equal (plist-get receipt :epoch) (plist-get invocation :epoch))
               (or (not (plist-member receipt :invocation))
                   (equal (plist-get receipt :invocation) (plist-get invocation :id)))
               (memq (plist-get receipt :status) '(queued processing)))
      (gptel-runner-jobs--finish-pending-receipt file receipt reason))))

(defun gptel-runner-jobs--close-failed (invocation state reason)
  "Resolve INVOCATION's pending commands and publish failure STATE and REASON.
The caller must hold the control lock and have verified current ownership."
  (let ((directory (expand-file-name "receipts" (plist-get invocation :directory)))
        retry errors)
    (condition-case err
        (when (file-directory-p directory)
          (dolist (file (directory-files directory t "\\.el\\'"))
            (condition-case nil
                (gptel-runner-jobs--fail-receipt file invocation reason)
              (error (push file retry)))))
      (file-error (push (error-message-string err) errors)))
    ;; Retry individual I/O failures.  An unreadable historical receipt must
    ;; not strand the worker or the other commands in this generation.
    (dolist (file retry)
      (condition-case err
          (gptel-runner-jobs--fail-receipt file invocation reason)
        (error (push (format "%s (%s)" (file-name-nondirectory file)
                             (error-message-string err)) errors))))
    (when errors
      (setq reason (format "%s\nCould not finalize command receipts: %s"
                           reason (string-join errors ", ")))))
  (setq invocation (plist-put invocation :state state)
        invocation (plist-put invocation :error reason)
        invocation (plist-put invocation :phase 'closed)
        invocation (plist-put invocation :worker-state 'exited))
  (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation))

(defun gptel-runner-jobs--launch-failed (invocation err)
  "Record INVOCATION launch failure ERR without abandoning a live worker."
  (gptel-runner-jobs--with-control-lock
   invocation
   (lambda ()
     (when-let* ((fresh (gptel-runner-jobs--owned-manifest invocation))
                 (owner (plist-get fresh :worker))
                 (token (plist-get owner :token))
                 (lock (gptel-runner-jobs--job-lock (plist-get fresh :job-id))))
       (let ((uncertain (memq 'gptel-runner-job-cleanup-error
                              (get (car err) 'error-conditions))))
         (when (or (eq (plist-get fresh :phase) 'starting)
                   (and (not uncertain) (memq (plist-get fresh :phase) '(open closing))
                        (eq (gptel-runner-jobs--lock-owner-state owner) 'dead)))
           (if uncertain
               ;; The service could still adopt this generation and run its commands.
               (let ((file (expand-file-name "owner.el" lock)))
                 (setq owner (plist-put owner :uncertain t))
                 (unless (gptel-runner-jobs--with-lock-guard
                          lock
                          (lambda ()
                            (unless (equal token (plist-get (gptel-runner-jobs--read file) :token))
                              (error "Launch reservation changed during failure handling"))
                            (gptel-runner-jobs--write file owner)))
                   (error "Could not retain uncertain launch reservation"))
                 (setq fresh (plist-put fresh :worker owner)
                       fresh (plist-put fresh :state 'launch-uncertain)
                       fresh (plist-put fresh :worker-state 'uncertain)
                       fresh (plist-put fresh :error (error-message-string err)))
                 (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file fresh) fresh))
             (gptel-runner-jobs--unlock lock token)
             (gptel-runner-jobs--close-failed fresh 'startup-error (error-message-string err)))))))))

(defun gptel-runner-jobs--abandoned-start-p (invocation)
  "Return non-nil if INVOCATION needs its startup reconciled.
A dead launcher alone does not establish that its detached worker is absent."
  (and (eq (plist-get invocation :phase) 'starting)
       (or (eq (plist-get invocation :worker-state) 'uncertain)
           (and (eq (plist-get invocation :worker-state) 'starting)
                (eq (gptel-runner-jobs--lock-owner-state
                     (plist-get invocation :worker)) 'dead)))))

(defun gptel-runner-jobs--confirm-stopped (invocation)
  "Close abandoned INVOCATION after its scheduler confirms it absent.
The caller must have verified the exact worker service no longer exists.
Recheck both the manifest and reservation under their ownership locks, since
adoption writes the job lock before publishing the new manifest owner."
  (let ((gptel-runner-job-directory (plist-get invocation :root)))
    (gptel-runner-jobs--with-control-lock
     invocation
     (lambda ()
       (when-let* ((fresh (gptel-runner-jobs--owned-manifest invocation))
                   ((gptel-runner-jobs--abandoned-start-p fresh))
                   (owner (plist-get fresh :worker))
                   ((equal owner (plist-get invocation :worker)))
                   (token (plist-get owner :token))
                   (lock (gptel-runner-jobs--job-lock (plist-get fresh :job-id))))
         (gptel-runner-jobs--with-lock-guard
          lock
          (lambda ()
            (let* ((exists (file-exists-p lock))
                   (reservation (and exists
                                     (ignore-errors
                                       (gptel-runner-jobs--read
                                        (expand-file-name "owner.el" lock)))))
                   (reserved-token (and (proper-list-p reservation)
                                        (plist-get reservation :token))))
              (when (and
                     (not (file-symlink-p lock))
                     (or (not exists)
                         (and (stringp reserved-token)
                              (or (not (equal token reserved-token))
                                  (eq (gptel-runner-jobs--lock-owner-state reservation) 'dead)
                                  (and (eq (plist-get fresh :worker-state) 'uncertain)
                                       (equal reservation owner))))))
                ;; Revoke admission before publishing closed.  A late child
                ;; must pass this guard and the control lock to adopt.
                (when (equal token reserved-token) (delete-directory lock t))
                (gptel-runner-jobs--close-failed
                 fresh 'startup-error
                 (or (plist-get fresh :error)
                     "Launcher exited before worker adoption; service is absent")))))))))))

(defun gptel-runner-job-run (job &optional trigger)
  "Launch JOB in an isolated worker and return its invocation record.
JOB is a saved job plist or ID.  TRIGGER defaults to `manual'.  A duplicate
invocation is recorded as skipped without starting another worker."
  (when (stringp job) (setq job (gptel-runner-job-get job)))
  (let* ((gptel-runner-job-directory (or (plist-get job :root)
                                         gptel-runner-job-directory))
         (invocation (gptel-runner-jobs--new-invocation job (or trigger 'manual))))
    (unless (eq (plist-get invocation :state) 'skipped)
      (condition-case err
          (funcall gptel-runner-job-launch-function invocation)
        (error (gptel-runner-jobs--launch-failed invocation err)
               (signal (car err) (cdr err)))))
    invocation))

(defun gptel-runner-jobs--receipt-file (invocation id)
  "Return receipt path for command ID in INVOCATION."
  (expand-file-name (concat "receipts/" (gptel-runner-jobs--valid-id id) ".el")
                    (plist-get invocation :directory)))

(defun gptel-runner-jobs--command-file (invocation id)
  "Return the worker inbox path for command ID in INVOCATION."
  (expand-file-name (concat "commands/" (gptel-runner-jobs--valid-id id) ".el")
                    (plist-get invocation :directory)))

(defun gptel-runner-job-command-status (invocation command-id)
  "Read the receipt for COMMAND-ID sent to INVOCATION or its ID."
  (when (stringp invocation) (setq invocation (gptel-runner-invocation-get invocation)))
  (gptel-runner-jobs--read (gptel-runner-jobs--receipt-file invocation command-id)))

(defun gptel-runner-invocation-commands (invocation)
  "Return durable command receipts for INVOCATION, newest first."
  (when (stringp invocation) (setq invocation (gptel-runner-invocation-get invocation)))
  (let ((directory (expand-file-name "receipts" (plist-get invocation :directory))))
    (when (file-directory-p directory)
      (sort (mapcar #'gptel-runner-jobs--read
                    (directory-files directory t "\\.el\\'"))
            (lambda (a b) (> (or (plist-get a :sequence) 0)
                             (or (plist-get b :sequence) 0)))))))

(defun gptel-runner-jobs--validate-command-arguments (operation arguments)
  "Validate the data-only argument schema for OPERATION and ARGUMENTS."
  (unless (and (memq operation gptel-runner-jobs--operations)
               (proper-list-p arguments) (cl-evenp (length arguments)))
    (error "Invalid background command or arguments"))
  (let* ((budgets '(:additional-requests :additional-calls :additional-duration))
         (call-keys '(:call-id :call-epoch))
         (allowed
          (pcase operation
            ((or 'pause 'cancel) nil)
            ('resume '(:feedback))
            ('retry '(:from-node :feedback))
            ('rerun '(:reset-budget))
            ('continue (append '(:observation :reset-budget) budgets))
            ('extend budgets)
            ('extend-repeat (append '(:node :additional-iterations) budgets))
            ('decision '(:text :reason))
            ((or 'pause-call 'cancel-call) call-keys)
            ('feedback (cons :feedback call-keys))
            ('accept (cons :result call-keys))
            ('answer (append '(:interaction-id :generation :answers) call-keys))
            ('cancel-questions (append '(:interaction-id :generation) call-keys))
            ('approve (append '(:interaction-id :generation :allow-outside) call-keys))
            ('approve-all (append '(:interaction-ids :generation :allow-outside) call-keys))
            ('deny (append '(:interaction-ids :generation) call-keys))
            ('revoke-outside (cons :generation call-keys))
            ('reject (append '(:interaction-id :generation) call-keys))))
         seen)
    (cl-loop for (key value) on arguments by #'cddr do
             (unless (and (memq key allowed) (not (memq key seen)))
               (error "Unsupported or duplicate %s argument: %S" operation key))
             (push key seen)
             (unless
                 (pcase key
                   ((or :feedback :result :reason) (or (null value) (stringp value)))
                   ((or :text :observation :call-id :interaction-id)
                    (and (stringp value) (not (string-empty-p value))))
                   ((or :node :from-node)
                    (or (symbolp value) (stringp value) (integerp value)))
                   ((or :reset-budget :allow-outside) (memq value '(nil t)))
                   (:answers
                    (and (consp value) (proper-list-p value)
                         (cl-every (lambda (s) (and (stringp s) (not (string-blank-p s))))
                                   value)))
                   (:interaction-ids
                    (and (consp value) (proper-list-p value)
                         (cl-every (lambda (id) (and (stringp id)
                                                     (not (string-empty-p id)))) value)
                         (equal value (delete-dups (copy-sequence value)))))
                   (:additional-duration (and (numberp value) (> value 0)))
                   (:generation (and (integerp value) (>= value 0)))
                   (_ (and (integerp value) (> value 0))))
               (error "Invalid %s argument %S" operation key)))
    (dolist (key (pcase operation
                   ('continue '(:observation)) ('decision '(:text))
                   ('extend-repeat '(:node :additional-iterations))
                   ((or 'pause-call 'cancel-call 'accept) call-keys)
                   ('revoke-outside (cons :generation call-keys))
                   ((or 'approve-all 'deny) (append '(:interaction-ids :generation) call-keys))
                   ('feedback (cons :feedback call-keys))
                   ('answer (append '(:interaction-id :generation :answers) call-keys))
                   ('cancel-questions (append '(:interaction-id :generation) call-keys))
                   ((or 'approve 'reject) (cons :interaction-id call-keys))))
      (unless (plist-get arguments key)
        (error "%s requires %S" operation key)))))

(defun gptel-runner-jobs--submit-command (invocation command receipt)
  "Publish COMMAND under INVOCATION's control lock and return its RECEIPT."
  (let* ((id (plist-get command :id))
         (receipt-file (gptel-runner-jobs--receipt-file invocation id))
         (command-file (gptel-runner-jobs--command-file invocation id)))
    (condition-case err
        (progn
          (gptel-runner-jobs--write receipt-file receipt)
          (gptel-runner-jobs--write command-file command))
      (error
       ;; The worker polls without this lock and retains the immutable
       ;; command file.  A write error after rename can race acknowledgment.
       (unless (and (file-exists-p command-file)
                    (equal command (gptel-runner-jobs--read command-file)))
         (when (file-exists-p receipt-file)
           (gptel-runner-jobs--fail-receipt
            receipt-file invocation
            (format "Command submission failed: %s" (error-message-string err))))
         (signal (car err) (cdr err))))))
  receipt)

(defun gptel-runner-job-command (invocation operation &optional arguments)
  "Send typed OPERATION with ARGUMENTS to INVOCATION or its ID.
Return a queued receipt; inspect completion with
`gptel-runner-job-command-status'.  A stopped invocation is restored only
inside a newly launched worker, never inside the caller's Emacs."
  (gptel-runner-jobs--validate-command-arguments operation arguments)
  (when (stringp invocation) (setq invocation (gptel-runner-invocation-get invocation)))
  (let ((gptel-runner-job-directory (plist-get invocation :root))
        (id (gptel-runner-jobs--id)) restart receipt)
    (gptel-runner-jobs--data-string arguments)
    (condition-case err
        (gptel-runner-jobs--with-control-lock
         invocation
         (lambda ()
	   (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
	   (when (and (plist-get arguments :call-epoch)
                      (/= (plist-get arguments :call-epoch) (plist-get invocation :epoch)))
             (error "This call belongs to an earlier worker generation"))
	   (unless (gptel-runner-invocation-live-p invocation)
             (unless (memq operation '(resume retry rerun continue extend extend-repeat cancel))
               (error "This operation requires the original live worker"))
             (unless (file-exists-p (expand-file-name "checkpoint.el"
                                                      (plist-get invocation :directory)))
               (error "Invocation has no recoverable checkpoint"))
             (let* ((token (gptel-runner-jobs--id))
                    (owner (gptel-runner-jobs--owner token (plist-get invocation :id)))
                    (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id))))
               (unless (gptel-runner-jobs--lock lock owner)
                 (error "Another invocation of this job is active"))
               (setq invocation (plist-put invocation :worker owner)
                     invocation (plist-put invocation :epoch (1+ (plist-get invocation :epoch)))
                     invocation (plist-put invocation :state 'starting)
                     invocation (plist-put invocation :phase 'starting)
                     invocation (plist-put invocation :worker-state 'starting)
                     invocation (plist-put invocation :launcher (gptel-runner-jobs--launcher))
                     invocation (plist-put invocation :restore t))
               (cl-remf invocation :error)
               ;; Publish ownership before creating any receipt for this epoch.
               ;; Until publication succeeds, only the new reservation is ours.
               (unwind-protect
                   (progn
                     (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)
                     (setq restart t))
                 (unless restart (gptel-runner-jobs--unlock lock token)))))
	   (let* ((sequence-file (expand-file-name "command-sequence.el"
						   (plist-get invocation :directory)))
		  (sequence (1+ (if (file-exists-p sequence-file)
                                    (gptel-runner-jobs--read sequence-file) 0)))
		  (command (list :id id :invocation (plist-get invocation :id)
				 :epoch (plist-get invocation :epoch)
				 :sequence sequence
				 :operation operation :arguments arguments
				 :created-at (float-time))))
             (gptel-runner-jobs--write sequence-file sequence)
             (setq receipt
                   (gptel-runner-jobs--submit-command
                    invocation command
                    (list :id id :status 'queued :operation operation
                          :sequence sequence :invocation (plist-get invocation :id)
                          :epoch (plist-get invocation :epoch)))))))
      (error
       (if receipt
           (message "Submitted command %s. Control lock cleanup reported %s"
                    id (error-message-string err))
         (when restart (gptel-runner-jobs--launch-failed invocation err))
         (signal (car err) (cdr err)))))
    (when restart
      (condition-case err
          (funcall gptel-runner-job-launch-function invocation)
        (error (gptel-runner-jobs--launch-failed invocation err)
               (signal (car err) (cdr err)))))
    receipt))

(defun gptel-runner-jobs--safe-value (value)
  "Return inspection data for VALUE without serializing live callbacks.
Opaque runtime objects and circular references are represented by type."
  (let ((seen (make-hash-table :test #'eq)))
    (cl-labels
        ((walk (item)
           (cond
            ((or (null item) (numberp item) (symbolp item)) item)
            ((stringp item) (substring-no-properties item))
            ((gethash item seen) '(:unavailable circular-reference))
            ((or (functionp item) (recordp item) (bufferp item) (processp item))
             (list :unavailable (type-of item)))
            ((consp item)
             ;; Walk list spines iteratively.  Visit each car before checking
             ;; its cdr so a shared tail gets the usual reference marker.
             (let (elements)
               (while (and (consp item) (not (gethash item seen))
                           (not (functionp item)))
                 (puthash item t seen)
                 (push (walk (car item)) elements)
                 (setq item (cdr item)))
               (nconc (nreverse elements) (walk item))))
            ((vectorp item)
             (puthash item t seen)
             (vconcat (mapcar #'walk item)))
            (t (list :unavailable (type-of item))))))
      (walk value))))

(defun gptel-runner-jobs--event (event)
  "Queue EVENT for durable inspection and request a manifest refresh."
  (when (and (null gptel-runner-jobs--run)
             (memq (gptel-runner-event-type event) '(run-started snapshot-loaded)))
    ;; Capture synchronously dispatched runs as soon as they are registered,
    ;; even if initialization of a later node throws before start returns.
    (setq gptel-runner-jobs--run
          (gptel-runner-find-run (gptel-runner-event-run-id event)))
    (when gptel-runner-jobs--run
      (setf (gptel-runner-run-snapshot-file gptel-runner-jobs--run)
            (expand-file-name "checkpoint.el"
                              (plist-get gptel-runner-jobs--invocation :directory))
            (gptel-runner-run-output-directory gptel-runner-jobs--run)
            (plist-get gptel-runner-jobs--invocation :directory)
            (gptel-runner-run-output-epoch gptel-runner-jobs--run)
            (plist-get gptel-runner-jobs--invocation :epoch)
            (gptel-runner-run-artifacts gptel-runner-jobs--run)
            ;; Old checkpoints did not retain the artifact registry.  Merge
            ;; history as well as any newer checkpoint committed before exit.
            (cl-remove-duplicates
             (append (plist-get gptel-runner-jobs--invocation :artifacts)
                     (gptel-runner-run-artifacts gptel-runner-jobs--run))
             :key (lambda (item) (plist-get item :file)) :test #'equal))))
  (push (list :time (gptel-runner-event-time event)
              :epoch (plist-get gptel-runner-jobs--invocation :epoch)
              :run (gptel-runner-event-run-id event)
              :node (gptel-runner-event-node-id event)
              :call (gptel-runner-event-call-id event)
              :type (gptel-runner-event-type event)
              :data (gptel-runner-jobs--safe-value (gptel-runner-event-data event)))
        gptel-runner-jobs--pending-events)
  (setq gptel-runner-jobs--dirty t))

(defun gptel-runner-jobs--interaction (call interaction)
  "Register CALL's serializable INTERACTION for the observing dashboard."
  (setq interaction (copy-tree interaction))
  (setq interaction (plist-put interaction :call-id (gptel-runner-call-id call))
        interaction (plist-put interaction :epoch (plist-get gptel-runner-jobs--invocation :epoch)))
  (push interaction gptel-runner-jobs--interactions)
  (setq gptel-runner-jobs--dirty t))

(defun gptel-runner-jobs--call (id)
  "Find live call ID in the current worker or signal an error."
  (or (cl-find id (gptel-runner-run-calls gptel-runner-jobs--run)
               :key #'gptel-runner-call-id :test #'equal)
      (error "Call is unavailable in this worker: %s" id)))

(defun gptel-runner-jobs--interaction-current-p (interaction)
  "Return non-nil if INTERACTION still targets a live pending call."
  (let* ((call (ignore-errors (gptel-runner-jobs--call (plist-get interaction :call-id))))
         (pending (and call
                       (gptel-runner-gptel-interaction-pending-p
                        call (plist-get interaction :id)))))
    ;; A sibling may be executing another tool while this request waits.
    (and pending
         (equal (plist-get interaction :epoch)
                (plist-get gptel-runner-jobs--invocation :epoch))
         (= (or (plist-get interaction :generation) -1)
            (gptel-runner-call-generation call)))))

(defun gptel-runner-jobs--export-file (relative data)
  "Write changed DATA to invocation RELATIVE file."
  (let* ((text (gptel-runner-jobs--data-string data))
         (hash (secure-hash 'sha256 text)))
    (unless (equal hash (gethash relative gptel-runner-jobs--exports))
      (gptel-runner-jobs--write
       (expand-file-name relative (plist-get gptel-runner-jobs--invocation :directory)) data)
      (puthash relative hash gptel-runner-jobs--exports))))

(defun gptel-runner-jobs--export-chat (call epoch &optional child)
  "Return published chat fields for CALL in EPOCH, or for its CHILD record."
  (let* ((node (gptel-runner-node-id (gptel-runner-call-node call)))
         (chat (if child (plist-get child :chat)
                 (gethash node (gptel-runner-run-chats (gptel-runner-call-run call))))))
    (when chat
      (let* ((key (if child (plist-get child :id) node))
             (relative (format "%s/%d-%s.chat" (if child "child-chats" "chats") epoch
                               (secure-hash 'sha256 (prin1-to-string key))))
             (file (expand-file-name relative (plist-get gptel-runner-jobs--invocation :directory)))
             (stamp (if (buffer-live-p chat)
                        (with-current-buffer chat
                          (list chat (buffer-modified-tick) (point-min) (point-max)
                                (point) (buffer-name)))
                      chat))
             (cached (gethash relative gptel-runner-jobs--exports)))
        (unless (equal stamp (car cached))
          (gptel-runner-jobs--mkdir (file-name-directory file))
          (setq cached (cons stamp (gptel-runner-store-export-chat
                                    (if child (gptel-runner-store--child-archive child) chat) file)))
          (puthash relative cached gptel-runner-jobs--exports))
        (list :chat relative :chat-format (cdr cached))))))

(defun gptel-runner-jobs--publish ()
  "Publish current worker inspection data and its compact manifest."
  (when gptel-runner-jobs--run
    (let* ((run gptel-runner-jobs--run)
           (epoch (plist-get gptel-runner-jobs--invocation :epoch))
           (directory (plist-get gptel-runner-jobs--invocation :directory))
           (previous (cl-remove-if
                      (lambda (call) (= (plist-get call :epoch) epoch))
                      (plist-get gptel-runner-jobs--invocation :calls)))
           (children-by-call (make-hash-table :test #'equal))
           calls outputs)
      (maphash (lambda (_id child)
                 (push child (gethash (plist-get child :call) children-by-call)))
               (gptel-runner-run-child-chats run))
      ;; Preserve earlier rows under their owning epoch.  A worker might
      ;; also have checkpointed a chat just before its first publication.
      (dolist (call (cl-remove-if
                     (lambda (c)
                       (and (plist-get (gptel-runner-call-driver-data c) :restored)
                            (cl-find (gptel-runner-call-id c) previous
                                     :key (lambda (row) (plist-get row :id)) :test #'equal)))
                     (gptel-runner-run-calls run)))
        (let* ((base (format "calls/%d-%s" epoch (gptel-runner-call-id call)))
               (record (list :id (gptel-runner-call-id call) :epoch epoch
                             :node (gptel-runner-node-id (gptel-runner-call-node call))
                             :kind (gptel-runner-node-kind (gptel-runner-call-node call))
                             :agent (when-let* ((agent (gptel-runner-call-agent call)))
                                      (gptel-runner-agent-name agent))
                             :state (gptel-runner-call-state call)
                             :generation (gptel-runner-call-generation call)
                             :outside-access
                             (and (plist-get (gptel-runner-call-driver-data call)
                                             :outside-access-buffers) t)
                             :started-at (gptel-runner-call-started-at call)
                             :finished-at (gptel-runner-call-finished-at call)
                             :attempts (gptel-runner-call-request-attempt call)
                             :iteration (gptel-runner-call-iteration call)
                             :record (concat base ".el"))))
          (setq record (append record (gptel-runner-jobs--export-chat call epoch)))
          (when-let* ((children (gethash (gptel-runner-call-id call) children-by-call)))
            (setq record
                  (plist-put record :children
                             (mapcar
                              (lambda (child)
                                (let ((entry (copy-sequence child)))
                                  (cl-remf entry :chat)
                                  (append entry (gptel-runner-jobs--export-chat call epoch child))))
                              children))))
          (gptel-runner-jobs--export-file
           (concat base ".el")
           (append record (list :prompt (gptel-runner-call-prompt call)
                                :workspace (gptel-runner-call-workspace call)
                                :result (gptel-runner-jobs--safe-value (gptel-runner-call-result call))
                                :error (gptel-runner-jobs--safe-value (gptel-runner-call-error call)))))
          (push record calls)))
      (setq outputs (gptel-runner-outputs run))
      (gptel-runner-jobs--export-file "outputs.el" outputs)
      (gptel-runner-jobs--export-file
       "nodes.el"
       (gptel-runner-map-nodes
        (lambda (node) (gptel-runner-node-outcome run node))
        run))
      (when gptel-runner-jobs--pending-events
        (let ((file (expand-file-name "events.el" directory))
              (coding-system-for-write 'utf-8-unix))
          (dolist (event (reverse gptel-runner-jobs--pending-events))
            (write-region (gptel-runner-jobs--data-string event) nil file t 'silent)
            (setq gptel-runner-jobs--pending-events
                  (delq event gptel-runner-jobs--pending-events)))
          (set-file-modes file #o600))
        (setq gptel-runner-jobs--pending-events nil))
      (setq gptel-runner-jobs--interactions
            (cl-remove-if-not #'gptel-runner-jobs--interaction-current-p
                              gptel-runner-jobs--interactions))
      (dolist (pair (list (cons :state (gptel-runner-run-state run))
                          (cons :run-id (gptel-runner-run-id run))
                          (cons :started-at (gptel-runner-run-started-at run))
                          (cons :finished-at (gptel-runner-run-finished-at run))
                          (cons :heartbeat (float-time))
                          (cons :calls (append previous (nreverse calls)))
                          (cons :outputs (mapcar #'car outputs))
                          (cons :outputs-file "outputs.el")
                          (cons :nodes-file "nodes.el")
                          (cons :artifacts (gptel-runner-artifacts run))
                          (cons :interactions gptel-runner-jobs--interactions)))
        (setq gptel-runner-jobs--invocation
              (plist-put gptel-runner-jobs--invocation (car pair) (cdr pair))))
      (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)
				gptel-runner-jobs--invocation)
      (setq gptel-runner-jobs--dirty nil))))

(defun gptel-runner-jobs--admit (call)
  "Acquire CALL's external writer lock without blocking other work."
  (or (not (gptel-runner--writer-p call))
      (gethash call gptel-runner-jobs--writer-locks)
      (let* ((directory (expand-file-name
                         (concat "locks/workspaces/"
                                 (secure-hash 'sha256 (gptel-runner-call-workspace call)))
                         (gptel-runner-jobs--root)))
             (token (gptel-runner-jobs--id))
             (owner (gptel-runner-jobs--owner token (plist-get gptel-runner-jobs--invocation :id))))
        (when (gptel-runner-jobs--lock directory owner)
          (puthash call (cons directory token) gptel-runner-jobs--writer-locks)
          t))))

(defun gptel-runner-jobs--release (call)
  "Release CALL's external writer lock, if any."
  (when-let* ((lock (gethash call gptel-runner-jobs--writer-locks)))
    (gptel-runner-jobs--unlock (car lock) (cdr lock))
    (remhash call gptel-runner-jobs--writer-locks)))

(defun gptel-runner-jobs--flush ()
  "Commit all queued execution and inspection data, failing visibly on error."
  (when gptel-runner-jobs--run
    (gptel-runner-save-run gptel-runner-jobs--run)
    (gptel-runner-store-flush-pending)
    (unless (eq (gptel-runner-store-save-status gptel-runner-jobs--run) 'clean)
      (signal 'gptel-runner-job-storage-error '("Worker checkpoint could not be committed")))
    (gptel-runner-jobs--publish)))

(defun gptel-runner-jobs--cancel-snapshot ()
  "Detach the worker's snapshot coordinator before relinquishing ownership."
  (when gptel-runner-jobs--run
    ;; Even a failed temporary-file cleanup must not retain save work that
    ;; could overwrite a successor's checkpoint during process shutdown.
    (ignore-errors (gptel-runner-store-cancel-save gptel-runner-jobs--run))))

(defun gptel-runner-jobs--apply-command (command)
  "Execute validated COMMAND in its owning worker."
  (gptel-runner-jobs--validate-command-arguments
   (plist-get command :operation) (plist-get command :arguments))
  (let* ((operation (plist-get command :operation))
         (args (plist-get command :arguments))
         (run gptel-runner-jobs--run)
         (call (when (plist-get args :call-id)
                 (unless (= (or (plist-get args :call-epoch) -1)
                            (plist-get gptel-runner-jobs--invocation :epoch))
                   (error "Stale call generation"))
                 (gptel-runner-jobs--call (plist-get args :call-id)))))
    (when (and call (plist-member args :generation)
               (/= (plist-get args :generation) (gptel-runner-call-generation call)))
      (error "Stale call generation"))
    (unless (and (equal (plist-get command :invocation)
                        (plist-get gptel-runner-jobs--invocation :id))
                 (= (plist-get command :epoch) (plist-get gptel-runner-jobs--invocation :epoch)))
      (error "Stale invocation ownership generation"))
    (unless (memq operation gptel-runner-jobs--operations)
      (error "Unsupported worker operation: %S" operation))
    (unless (and (proper-list-p args) (cl-evenp (length args)))
      (error "Command arguments must be a property list"))
    ;; A stale selection can be rejected cleanly before the first decision.
    ;; After a tool callback runs, a failure may indicate partial application.
    (dolist (id (or (plist-get args :interaction-ids)
                    (when-let* ((id (plist-get args :interaction-id))) (list id))))
      (gptel-runner-gptel-check-interaction
       call id (if (memq operation '(answer cancel-questions)) 'question 'tool-approval)
       (plist-get args :answers)))
    (setq gptel-runner-jobs--command-started t)
    (pcase operation
      ('pause (gptel-runner-pause-run run))
      ('resume (gptel-runner-resume-run run (plist-get args :feedback)))
      ('cancel (gptel-runner-abort-run run 'background-command))
      ('retry (apply #'gptel-runner-retry run args))
      ('rerun (apply #'gptel-runner-rerun run args))
      ('continue (let ((rest (copy-sequence args)))
                   (cl-remf rest :observation)
                   (apply #'gptel-runner-continue run (plist-get args :observation) rest)))
      ('extend (apply #'gptel-runner-extend run args))
      ('extend-repeat
       (let ((rest (copy-sequence args)))
         (cl-remf rest :node) (cl-remf rest :additional-iterations)
         (apply #'gptel-runner-extend-repeat run (plist-get args :node)
                (plist-get args :additional-iterations) rest)))
      ('decision (gptel-runner-record-decision run (plist-get args :text) (plist-get args :reason)))
      ('pause-call (gptel-runner-pause-call call))
      ('cancel-call (gptel-runner-abort-call call 'background-command))
      ('feedback (gptel-runner-gptel-feedback call (plist-get args :feedback)))
      ('accept (gptel-runner-complete-call-from-buffer (plist-get args :result) call))
      ((or 'answer 'cancel-questions)
       (gptel-runner-gptel-answer-questions call (plist-get args :interaction-id)
                                            (plist-get args :answers)))
      ((or 'approve 'reject)
       (gptel-runner-gptel-answer-interaction call (plist-get args :interaction-id)
                                              (eq operation 'approve)
                                              (plist-get args :allow-outside)))
      ('approve-all
       (gptel-runner-gptel-approve-interactions
        call (plist-get args :interaction-ids) (plist-get args :allow-outside)))
      ('deny (gptel-runner-gptel-deny-interactions call (plist-get args :interaction-ids)))
      ('revoke-outside (gptel-runner-gptel-revoke-outside-access call))
      (_ (error "Unsupported worker operation: %S" operation)))))

(defun gptel-runner-jobs--commands ()
  "Process queued commands, leaving durable acknowledgments."
  (let ((directory (expand-file-name "commands" (plist-get gptel-runner-jobs--invocation :directory))))
    (when (file-directory-p directory)
      (dolist (command
               (sort (mapcar #'gptel-runner-jobs--read
                             (directory-files directory t "\\.el\\'"))
                     (lambda (a b) (< (or (plist-get a :sequence) 0)
                                      (or (plist-get b :sequence) 0)))))
        (let* ((id (plist-get command :id))
               (receipt-file (gptel-runner-jobs--receipt-file gptel-runner-jobs--invocation id))
               (receipt (when (file-exists-p receipt-file) (gptel-runner-jobs--read receipt-file)))
               (gptel-runner-jobs--command-started nil))
          (when (or (null receipt) (eq (plist-get receipt :status) 'queued))
            (setq receipt (list :id id :status 'processing :operation (plist-get command :operation)
                                :sequence (plist-get command :sequence)
                                :invocation (plist-get command :invocation)
                                :epoch (plist-get command :epoch)))
            (gptel-runner-jobs--write receipt-file receipt)
            (condition-case err
                (progn
                  (gptel-runner-jobs--apply-command command)
                  (setq receipt (plist-put receipt :status 'applied)))
              (error
               (setq receipt (plist-put receipt :status
                                        (if gptel-runner-jobs--command-started
                                            'indeterminate 'rejected))
                     receipt (plist-put receipt :error (error-message-string err)))))
            ;; A storage error after applying an operation is indeterminate,
            ;; not a rejected operation that a client may safely retry.
            ;; Leave the on-disk processing receipt and fail the worker.
            (when (memq (plist-get receipt :status) '(applied indeterminate))
              (gptel-runner-jobs--flush))
            (setq receipt (plist-put receipt :finished-at (float-time)))
            (gptel-runner-jobs--write receipt-file receipt)))))))

(defun gptel-runner-jobs--abandoned-receipts ()
  "Settle interrupted submissions and acknowledgments through this epoch.
Call under the control lock while the worker is idle, so a submitter cannot
still be publishing a queued receipt's command."
  (let* ((invocation gptel-runner-jobs--invocation)
         (directory (expand-file-name "receipts" (plist-get invocation :directory))))
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.el\\'"))
        (let* ((receipt (gptel-runner-jobs--read file))
               (epoch (plist-get receipt :epoch)))
          (when (and (natnump epoch) (<= epoch (plist-get invocation :epoch))
                     (or (not (plist-member receipt :invocation))
                         (equal (plist-get receipt :invocation) (plist-get invocation :id))))
            (pcase (plist-get receipt :status)
              ('processing
               (gptel-runner-jobs--finish-pending-receipt
                file receipt "Worker stopped before acknowledging the command"))
              ('queued
               (unless (file-exists-p (gptel-runner-jobs--command-file
                                       invocation (plist-get receipt :id)))
                 (gptel-runner-jobs--finish-pending-receipt
                  file receipt "Submission interrupted before command publication"))))))))))

(defun gptel-runner-jobs--unsupported-input (&rest _arguments)
  "Fail synchronous input attempts instead of reading worker stdin."
  (error "Background jobs require an asynchronous input adapter; synchronous input is unsupported"))

(defun gptel-runner-jobs--adopt (lock owner)
  "Adopt reserved job LOCK for OWNER under the shared ownership guard."
  (let ((deadline (+ (float-time) 5)) adopted)
    (while
        (and
         (not
          (setq adopted
                (gptel-runner-jobs--with-lock-guard
                 lock
                 (lambda ()
                   (let ((reservation (gptel-runner-jobs--read
                                       (expand-file-name "owner.el" lock))))
                     (unless (equal (plist-get owner :token)
                                    (plist-get reservation :token))
                       (error "Worker launch reservation was replaced")))
                   (gptel-runner-jobs--write (expand-file-name "owner.el" lock) owner)
                   t))))
         (< (float-time) deadline))
      (sleep-for 0.02))
    (unless adopted (error "Worker ownership is busy; launch cannot proceed"))))

(defun gptel-runner-jobs--preload (features)
  "Load worker FEATURES, reporting failures with bootstrap context."
  (dolist (feature features)
    (condition-case err
        (require feature)
      (error
       (error (concat "Could not preload %S in the job worker: %s. "
                      "Check its dependencies and replace the job's saved "
                      ":load-path if necessary")
              feature (error-message-string err))))))

(defun gptel-runner-jobs--worker-failed (err lock token may-flush)
  "Close this worker after ERR if it still owns the persisted generation.
Release job LOCK with TOKEN.  MAY-FLUSH allows saving the checkpoint
only if the worker has not already attempted to relinquish it."
  (gptel-runner-jobs--with-control-lock
   gptel-runner-jobs--invocation
   (lambda ()
     (when-let* ((fresh (gptel-runner-jobs--owned-manifest gptel-runner-jobs--invocation))
                 ((not (eq (plist-get fresh :phase) 'closed))))
       (setq gptel-runner-jobs--invocation fresh)
       (when (and may-flush gptel-runner-jobs--run)
         (ignore-errors (gptel-runner-abort-run gptel-runner-jobs--run 'worker-error))
         (ignore-errors (gptel-runner-jobs--flush)))
       (gptel-runner-jobs--cancel-snapshot)
       (maphash (lambda (call _lock) (gptel-runner-jobs--release call))
                gptel-runner-jobs--writer-locks)
       (gptel-runner-jobs--unlock lock token)
       (gptel-runner-jobs--close-failed
        gptel-runner-jobs--invocation
        (cond ((null gptel-runner-jobs--run) 'startup-error)
              ((or (eq (car err) 'gptel-runner-job-storage-error)
                   (memq 'file-error (get (car err) 'error-conditions)))
               'storage-error)
              (t 'worker-error))
        (error-message-string err))))))

(defun gptel-runner-jobs-worker (directory &optional epoch)
  "Execute or restore the invocation in DIRECTORY and return its exit code.
When EPOCH is non-nil, refuse admission to any other worker generation.
Zero means success, one a pipeline outcome other than success, and two an
initialization or persistence error.  This function does not call `kill-emacs'."
  (let* ((gptel-runner-jobs--invocation
          (gptel-runner-jobs--record (expand-file-name "manifest.el" directory) 'runner-invocation))
         (gptel-runner-job-directory (plist-get gptel-runner-jobs--invocation :root))
         (job (plist-get gptel-runner-jobs--invocation :job))
         ;; Older invocations embed jobs without bootstrap fields.  Preserve
         ;; their init-controlled loading, including when restoring a run.
         (preloads (plist-get job :preload-features))
         (old-owner (plist-get gptel-runner-jobs--invocation :worker))
         (token (plist-get old-owner :token))
         (lock (gptel-runner-jobs--job-lock (plist-get job :id)))
         (owner (gptel-runner-jobs--owner token (plist-get gptel-runner-jobs--invocation :id)))
         (gptel-runner-jobs--run nil)
         (gptel-runner-jobs--dirty t)
         (gptel-runner-jobs--pending-events nil)
         (gptel-runner-jobs--interactions nil)
         (gptel-runner-jobs--writer-locks (make-hash-table :test #'eq))
         (gptel-runner-jobs--exports (make-hash-table :test #'equal))
         (gptel-runner-headless t)
         (gptel-runner-retain-worker-buffers t)
         (gptel-runner-event-hook (cons #'gptel-runner-jobs--event gptel-runner-event-hook))
         (gptel-runner-interaction-hook (cons #'gptel-runner-jobs--interaction gptel-runner-interaction-hook))
         (gptel-runner-call-admission-functions (cons #'gptel-runner-jobs--admit gptel-runner-call-admission-functions))
         (gptel-runner-call-release-hook (cons #'gptel-runner-jobs--release gptel-runner-call-release-hook))
         (gptel-runner-snapshot-directory (file-name-as-directory directory))
         (default-directory (or (plist-get (plist-get job :arguments) :workspace)
                                (plist-get job :base-directory)))
         (process-environment (copy-sequence process-environment))
         (exec-path (copy-sequence exec-path))
         (load-path (append (plist-get job :load-path) load-path))
         ;; Local package checkouts may have stale bytecode after an update.
         ;; Bootstrap the current source rather than silently using old tools.
         (load-prefer-newer t)
         ;; Scheduled workers must not need a compiler/toolchain just to
         ;; install input guards, or write native caches into the editor's
         ;; configuration.  Already installed native code remains usable.
         (native-comp-enable-subr-trampolines nil)
         (native-comp-jit-compilation nil)
         (last-publish 0) (exit-code 2) finished ownership-released)
    (unless (and (or (null epoch)
                     (equal epoch (plist-get gptel-runner-jobs--invocation :epoch)))
                 (eq (plist-get gptel-runner-jobs--invocation :phase) 'starting)
                 (file-exists-p (expand-file-name "owner.el" lock))
                 (equal token (plist-get (gptel-runner-jobs--read (expand-file-name "owner.el" lock)) :token)))
      (error "Worker launch reservation is no longer valid"))
    ;; The control lock prevents two accidentally launched workers from both
    ;; adopting the same reservation.
    (gptel-runner-jobs--with-control-lock
     gptel-runner-jobs--invocation
     (lambda ()
       (let ((fresh (gptel-runner-jobs--owned-manifest gptel-runner-jobs--invocation)))
         (unless (eq (plist-get fresh :phase) 'starting)
           (error "Invocation ownership changed before worker adoption"))
         (setq gptel-runner-jobs--invocation fresh))
       (gptel-runner-jobs--adopt lock owner)
       (setq gptel-runner-jobs--invocation (plist-put gptel-runner-jobs--invocation :worker owner)
             gptel-runner-jobs--invocation (plist-put gptel-runner-jobs--invocation :phase 'open)
             gptel-runner-jobs--invocation (plist-put gptel-runner-jobs--invocation :worker-state 'running))
       (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)
				 gptel-runner-jobs--invocation)))
    (unwind-protect
        (condition-case err
            (cl-letf (((symbol-function 'read-from-minibuffer) #'gptel-runner-jobs--unsupported-input)
                      ((symbol-function 'read-string) #'gptel-runner-jobs--unsupported-input)
                      ((symbol-function 'read-char) #'gptel-runner-jobs--unsupported-input)
                      ((symbol-function 'read-event) #'gptel-runner-jobs--unsupported-input)
                      ((symbol-function 'yes-or-no-p) #'gptel-runner-jobs--unsupported-input)
                      ((symbol-function 'y-or-n-p) #'gptel-runner-jobs--unsupported-input))
              (dolist (entry (plist-get job :environment)) (setenv (car entry) (cdr entry)))
              (when (assoc "PATH" (plist-get job :environment))
                (setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory))))
              (gptel-runner-jobs--preload preloads)
              (when (memq 'gptel-agent preloads)
                (gptel-agent-update))
              ;; `load' expands dollar expressions even in absolute filenames.
              (load (string-replace "$" "$$" (plist-get job :init-file)) nil t)
              (setq gptel-runner-jobs--run
                    (if (plist-get gptel-runner-jobs--invocation :restore)
                        (gptel-runner-load-run (expand-file-name "checkpoint.el" directory))
                      (apply #'gptel-runner-start (plist-get job :workflow)
                             :persist t (plist-get job :arguments))))
              ;; The first synchronous node may finish before start returns.
              ;; Redirect its already queued default snapshot before flushing.
              (gptel-runner-store-cancel-save gptel-runner-jobs--run)
              (setf (gptel-runner-run-snapshot-file gptel-runner-jobs--run)
                    (expand-file-name "checkpoint.el" directory))
              (gptel-runner-jobs--flush)
              (gptel-runner-jobs--with-control-lock
               gptel-runner-jobs--invocation #'gptel-runner-jobs--abandoned-receipts)
              (while (not finished)
                (gptel-runner-jobs--commands)
                (gptel-runner--drain-queue gptel-runner-jobs--run)
                (when (or gptel-runner-jobs--dirty (> (- (float-time) last-publish) 1.0))
                  (gptel-runner-jobs--publish)
                  (setq last-publish (float-time)))
                (when (gptel-runner--run-terminal-p gptel-runner-jobs--run)
                  (gptel-runner-jobs--with-control-lock
                   gptel-runner-jobs--invocation
                   (lambda ()
                     ;; Drain again under the lock so commands submitted just
                     ;; before close either apply here or launch a new owner.
                     (gptel-runner-jobs--commands)
                     (when (gptel-runner--run-terminal-p gptel-runner-jobs--run)
                       (gptel-runner-jobs--abandoned-receipts)
                       (setq gptel-runner-jobs--invocation
                             (plist-put gptel-runner-jobs--invocation :phase 'closing))
                       (gptel-runner-jobs--flush)
                       (setq exit-code (if (eq (gptel-runner-run-state gptel-runner-jobs--run) 'succeeded) 0 1)
                             finished t)
                       (gptel-runner-jobs--cancel-snapshot)
                       ;; Unlock may signal after removing ownership, while
                       ;; cleaning its guard.  Never save after attempting
                       ;; this handoff of an already committed checkpoint.
                       (setq ownership-released t)
                       (gptel-runner-jobs--unlock lock token)
                       (setq gptel-runner-jobs--invocation
                             (plist-put gptel-runner-jobs--invocation :phase 'closed)
                             gptel-runner-jobs--invocation
                             (plist-put gptel-runner-jobs--invocation :worker-state 'exited))
                       (gptel-runner-jobs--write
                        (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)
                        gptel-runner-jobs--invocation)))))
                (unless finished (accept-process-output nil gptel-runner-job-poll-interval))))
          (error
           (setq exit-code 2)
           (let ((may-flush (not ownership-released)))
             ;; Even if revalidation fails, unwind cleanup must not flush.
             (setq ownership-released t)
             (condition-case cleanup-error
                 (gptel-runner-jobs--worker-failed err lock token may-flush)
               (error (message "Could not record background worker failure: %s"
                               (error-message-string cleanup-error)))))
           (message "Background worker failed: %s" (error-message-string err))))
      (unwind-protect
          ;; Preserve queued progress on quit or another nonlocal exit while
          ;; this worker still owns its checkpoint.
          (when (and gptel-runner-jobs--run (not ownership-released))
            (ignore-errors (gptel-runner-store-flush-pending)))
        (gptel-runner-jobs--cancel-snapshot)
        (setq ownership-released t)
        (maphash (lambda (call _lock) (gptel-runner-jobs--release call))
                 gptel-runner-jobs--writer-locks)
        ;; Cleanup can run after a different generation has taken ownership.
        (when (file-exists-p lock)
          (gptel-runner-jobs--with-control-lock
           gptel-runner-jobs--invocation
           (lambda ()
             (when (gptel-runner-jobs--owned-manifest gptel-runner-jobs--invocation)
               (gptel-runner-jobs--unlock lock token)))))))
    exit-code))

(defun gptel-runner-jobs-batch-main ()
  "Run the background CLI and exit with a meaningful process status.
Use worker DIRECTORY [--epoch EPOCH], or run, scheduled-run or trigger
with JOB-ID [--root DIRECTORY].  Scheduled operations check JOB's enabled
flag.  Only trigger dispatches a separate worker."
  (let* ((arguments (if (equal (car command-line-args-left) "--")
                        (cdr command-line-args-left) command-line-args-left))
         (operation (pop arguments)) (target (pop arguments))
         (epoch-text (cadr (member "--epoch" arguments)))
         (root (cadr (member "--root" arguments)))
         (gptel-runner-job-directory (or root gptel-runner-job-directory)))
    (setq command-line-args-left nil)
    (condition-case err
        (kill-emacs
         (pcase operation
           ("worker"
            (when (and (member "--epoch" arguments)
                       (not (and epoch-text
                                 (string-match-p "\\`[0-9]+\\'" epoch-text)
                                 (> (string-to-number epoch-text) 0))))
              (error "Worker epoch must be a positive integer"))
            (gptel-runner-jobs-worker target (and epoch-text (string-to-number epoch-text))))
           ((or "run" "scheduled-run")
            (let ((job (gptel-runner-job-get target))
                  (scheduled (equal operation "scheduled-run")))
              (if (and scheduled (not (plist-get job :enabled))) 0
                (let ((invocation (gptel-runner-jobs--new-invocation
                                   job (if scheduled 'schedule 'cli) 'direct)))
                  (if (eq (plist-get invocation :state) 'skipped) 0
                    (gptel-runner-jobs-worker (plist-get invocation :directory)))))))
           ("trigger"
            (let ((job (gptel-runner-job-get target)))
              (when (plist-get job :enabled) (gptel-runner-job-run job 'schedule))
              0))
           (_ (error "Usage: worker DIRECTORY | {run,scheduled-run,trigger} JOB-ID [--root DIRECTORY]"))))
      (error (message "Job launch failed: %s" (error-message-string err))
             (kill-emacs 2)))))

(provide 'gptel-runner-jobs)
;;; gptel-runner-jobs.el ends here
