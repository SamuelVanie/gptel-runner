;;; gptel-runner-jobs.el --- Isolated durable workflow jobs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Jobs are reusable launch configurations.  An invocation has one owning
;; batch Emacs, an execution checkpoint, an inspection history, and a typed
;; command inbox.  Reading history never creates executable runner objects.
;; The command line entry point works with cron as well as the launchd adapter.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel-runner-core)
(require 'gptel-runner-flow)
(require 'gptel-runner-store)

(declare-function gptel-runner-launchd-start-worker "gptel-runner-launchd")
(declare-function gptel-runner-launchd-remove "gptel-runner-launchd")
(declare-function gptel-runner-gptel-answer-interaction "gptel-runner-gptel")
(declare-function gptel-runner-gptel-approve-interactions "gptel-runner-gptel")
(declare-function gptel-runner-gptel-revoke-outside-access "gptel-runner-gptel")
(declare-function gptel-runner-gptel-interaction-pending-p "gptel-runner-gptel")
(declare-function gptel-runner-gptel-feedback "gptel-runner-gptel")
(declare-function gptel-runner-complete-call-from-buffer "gptel-runner-gptel")
(declare-function gptel-agent-update "gptel-agent")

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

(defcustom gptel-runner-job-preload-features '(gptel gptel-agent)
  "Packages loaded in new job workers before their initialization file.
Saving a job captures this value as :preload-features.  An explicit job
value overrides it, including nil for fake-driver or other standalone jobs.
When gptel-agent is included, its definitions are refreshed before the
initialization file, so that file has final authority over presets."
  :type '(repeat symbol))

(defcustom gptel-runner-job-load-path nil
  "Extra Lisp library directories captured when saving a new job.
These precede the editor's current `load-path'.  The resulting absolute
paths are saved as :load-path, excluding Emacs's bundled Lisp directories.
Set this once for packages in local checkouts that are not on `load-path'.
An explicit job :load-path overrides automatic capture.  Re-saving a job
preserves its saved paths unless a replacement :load-path is supplied."
  :type '(repeat directory))

(defcustom gptel-runner-job-launch-function #'gptel-runner-jobs--launch
  "Function accepting an invocation record and starting its worker.
The default uses launchd on macOS.  The command line `run' entry point
executes directly and does not need this launcher."
  :type 'function)

(defcustom gptel-runner-job-poll-interval 0.2
  "Seconds between worker command and external lock checks."
  :type 'number)

(defconst gptel-runner-jobs--package-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the installed runner Lisp files.")

(defconst gptel-runner-jobs--version 1)
(define-error 'gptel-runner-job-storage-error "Background job storage failed")
(defconst gptel-runner-jobs--operations
  '(pause resume cancel retry continue extend extend-repeat decision
	  pause-call cancel-call feedback accept approve approve-all reject
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
        (read-from-string text)
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
  "Validate SCHEDULE's portable launchd subset."
  (when schedule
    (pcase (car-safe schedule)
      (:interval
       (unless (and (= (length schedule) 2)
                    (integerp (cadr schedule)) (> (cadr schedule) 0))
         (error "Schedule interval must be positive seconds")))
      (:calendar
       (unless (and (= (length schedule) 2) (consp (cadr schedule)))
         (error "Calendar schedule needs a list of rules"))
       (dolist (rule (cadr schedule))
         (unless (and (proper-list-p rule) (cl-evenp (length rule)))
           (error "Invalid calendar rule: %S" rule))
         (cl-loop for (key value) on rule by #'cddr
                  for range = (assq key '((:minute 0 59) (:hour 0 23)
                                          (:day 1 31) (:weekday 0 7) (:month 1 12)))
                  unless (and range (integerp value)
                              (<= (cadr range) value (caddr range)))
                  do (error "Invalid calendar field: %S %S" key value))))
      (_ (error "Unknown job schedule: %S" schedule)))))

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

(defun gptel-runner-jobs--bootstrap-value (job old key default)
  "Resolve bootstrap KEY from JOB, OLD, or DEFAULT, preserving explicit nil."
  (copy-tree (cond ((plist-member job key) (plist-get job key))
                   ((plist-member old key) (plist-get old key))
                   (t default))))

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
         (init (plist-get job :init-file))
         (arguments (copy-tree (plist-get job :arguments)))
         (base (file-name-as-directory
                (file-truename (or (plist-get job :base-directory)
                                   (plist-get old :base-directory)
                                   default-directory))))
         (emacs (or (plist-get job :emacs) gptel-runner-job-emacs-program))
         (preloads (gptel-runner-jobs--bootstrap-value
                    job old :preload-features gptel-runner-job-preload-features))
         (paths (if (or (plist-member job :load-path)
                        (plist-member old :load-path))
                    (gptel-runner-jobs--library-paths
                     (gptel-runner-jobs--bootstrap-value job old :load-path nil) base)
                  (gptel-runner-jobs--library-paths
                   (append gptel-runner-job-load-path load-path)))))
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
				      :allow-unconfirmed-tools :decision-memory :context))
             do (error "Unsupported durable job start argument: %S" key))
    (unless (and (stringp emacs) (file-name-absolute-p emacs)
                 (file-executable-p emacs))
      (error "Job Emacs must be an absolute executable: %S" emacs))
    (dolist (entry (plist-get job :environment))
      (unless (and (consp entry) (stringp (car entry)) (stringp (cdr entry))
                   (string-match-p "\\`[A-Za-z_][A-Za-z_0-9]*\\'" (car entry)))
        (error "Job environment must contain string name/value pairs")))
    (gptel-runner-jobs--validate-schedule (plist-get job :schedule))
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
          job (plist-put job :init-file (expand-file-name init))
          job (plist-put job :emacs emacs)
          job (plist-put job :root (gptel-runner-jobs--root))
          job (plist-put job :revision (1+ (or (plist-get old :revision) 0)))
          job (plist-put job :format 'runner-job)
          job (plist-put job :version gptel-runner-jobs--version))
    (unless (plist-member job :enabled) (setq job (plist-put job :enabled t)))
    (gptel-runner-jobs--write (gptel-runner-jobs--definition-file id) job)))

(defun gptel-runner-job-delete (id)
  "Remove job ID and its schedule, retaining all invocation history."
  (let ((job (gptel-runner-job-get id)))
    (when (eq system-type 'darwin)
      (require 'gptel-runner-launchd)
      (gptel-runner-launchd-remove job))
    (delete-file (gptel-runner-jobs--definition-file id))))

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
                (when (and (memq (plist-get record :phase) '(open closing))
                           (not (gptel-runner-invocation-live-p record)))
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
  (let* ((directory (file-name-as-directory (plist-get invocation :directory)))
         (file (expand-file-name relative directory)))
    (unless (and (not (file-name-absolute-p relative))
                 (file-in-directory-p file directory))
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

(defun gptel-runner-jobs--owner (token invocation)
  "Return this process's ownership identity with TOKEN and INVOCATION ID."
  (list :pid (emacs-pid) :start (alist-get 'start (process-attributes (emacs-pid)))
        :host (system-name) :token token :invocation invocation
        :created-at (float-time)))

(defun gptel-runner-jobs--owner-live-p (owner)
  "Return non-nil if OWNER's exact process identity still exists."
  (and (equal (plist-get owner :host) (system-name))
       (integerp (plist-get owner :pid))
       (let ((attributes (process-attributes (plist-get owner :pid))))
         (and attributes
              (not (equal (alist-get 'state attributes) "Z"))
              (equal (plist-get owner :start) (alist-get 'start attributes))))))

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

(defun gptel-runner-jobs--lock-owner-state (owner)
  "Classify OWNER as `live', `dead', or `unknown' by exact process identity.
Incomplete identities and owners on another host are never declared dead."
  (if (not (and (proper-list-p owner)
                (not (plist-get owner :uncertain))
                (equal (plist-get owner :host) (system-name))
                (integerp (plist-get owner :pid))
                (> (plist-get owner :pid) 0)
                (plist-get owner :start)
                (ignore-errors (time-convert (plist-get owner :start) 'integer))))
      'unknown
    (condition-case nil
        (let ((attributes (process-attributes (plist-get owner :pid))))
          (cond ((null attributes)
                 (if (memq (plist-get owner :pid) (list-system-processes))
                     'unknown 'dead))
                ((null (alist-get 'start attributes)) 'unknown)
                ((or (equal (alist-get 'state attributes) "Z")
                     (not (equal (plist-get owner :start)
                                 (alist-get 'start attributes))))
                 'dead)
                (t 'live)))
      (error 'unknown))))

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
      (gptel-runner-jobs--unlock directory token))))

(defun gptel-runner-jobs--launch (invocation)
  "Start INVOCATION independently of the dashboard on macOS."
  (unless (eq system-type 'darwin)
    (error "Configure a job launcher on this OS, or use the CLI run command"))
  (require 'gptel-runner-launchd)
  (gptel-runner-launchd-start-worker invocation))

(defun gptel-runner-jobs--new-invocation (job trigger)
  "Create a durable invocation of JOB for TRIGGER, claiming its job lock."
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

(defun gptel-runner-jobs--launch-failed (invocation err)
  "Record INVOCATION launch failure ERR without abandoning a live worker."
  (gptel-runner-jobs--with-control-lock
   invocation
   (lambda ()
     (let* ((fresh (ignore-errors (gptel-runner-jobs--read
				   (gptel-runner-jobs--manifest-file invocation))))
            (same (equal (plist-get fresh :epoch) (plist-get invocation :epoch)))
            (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id)))
            (token (plist-get (plist-get invocation :worker) :token)))
       (cond
	;; Another process already produced the authoritative outcome, or adopted
	;; the reservation.  Do not replace its history with the launcher's copy.
	((and same (or (eq (plist-get fresh :phase) 'closed)
                       (and (eq (plist-get fresh :phase) 'open)
                            (gptel-runner-invocation-live-p fresh)))) nil)
	((eq (car err) 'gptel-runner-launchd-cleanup-error)
	 ;; An unconfirmed launchd cancellation can still start a worker later.
	 ;; Keep the token adoptable, but never reclaim it based on a dead trigger.
	 (when same (setq invocation fresh))
	 (let ((owner (plist-put (copy-sequence (plist-get invocation :worker)) :uncertain t)))
           (unless
               (gptel-runner-jobs--with-lock-guard
		lock
		(lambda ()
		  (unless (equal token (plist-get (gptel-runner-jobs--read
                                                   (expand-file-name "owner.el" lock)) :token))
                    (error "Launch reservation changed during failure handling"))
		  (gptel-runner-jobs--write (expand-file-name "owner.el" lock) owner)
		  t))
             (error "Could not retain uncertain launch reservation"))
           (setq invocation (plist-put invocation :worker owner)
		 invocation (plist-put invocation :state 'launch-uncertain)
		 invocation (plist-put invocation :worker-state 'uncertain)
		 invocation (plist-put invocation :error (error-message-string err)))
           (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)))
	(t
	 (setq invocation (plist-put invocation :state 'startup-error)
               invocation (plist-put invocation :error (error-message-string err))
               invocation (plist-put invocation :phase 'closed)
               invocation (plist-put invocation :worker-state 'exited))
	 (unwind-protect
             (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)
           (gptel-runner-jobs--unlock lock token))))))))

(defun gptel-runner-jobs--confirm-stopped (invocation)
  "Release an uncertain INVOCATION after its scheduler confirms it absent.
The caller must have verified the exact worker service no longer exists."
  (let ((gptel-runner-job-directory (plist-get invocation :root)))
    (gptel-runner-jobs--with-control-lock
     invocation
     (lambda ()
       (let ((fresh (gptel-runner-invocation-get (plist-get invocation :id))))
         (when (and (eq (plist-get fresh :worker-state) 'uncertain)
                    (equal (plist-get fresh :epoch) (plist-get invocation :epoch)))
           (gptel-runner-jobs--unlock
            (gptel-runner-jobs--job-lock (plist-get fresh :job-id))
            (plist-get (plist-get fresh :worker) :token))
           (setq fresh (plist-put fresh :state 'startup-error)
                 fresh (plist-put fresh :phase 'closed)
                 fresh (plist-put fresh :worker-state 'exited))
           (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file fresh) fresh)))))))

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
            ('continue (append '(:observation :reset-budget) budgets))
            ('extend budgets)
            ('extend-repeat (append '(:node :additional-iterations) budgets))
            ('decision '(:text :reason))
            ((or 'pause-call 'cancel-call) call-keys)
            ('feedback (cons :feedback call-keys))
            ('accept (cons :result call-keys))
            ('approve (append '(:interaction-id :generation :allow-outside) call-keys))
            ('approve-all (append '(:interaction-ids :generation :allow-outside) call-keys))
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
                   ('approve-all (append '(:interaction-ids :generation) call-keys))
                   ('feedback (cons :feedback call-keys))
                   ((or 'approve 'reject) (cons :interaction-id call-keys))))
      (unless (plist-get arguments key)
        (error "%s requires %S" operation key)))))

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
             (unless (memq operation '(resume retry continue extend extend-repeat cancel))
               (error "This operation requires the original live worker"))
             (unless (file-exists-p (expand-file-name "checkpoint.el"
                                                      (plist-get invocation :directory)))
               (error "Invocation has no recoverable checkpoint"))
             (let* ((token (gptel-runner-jobs--id))
                    (owner (gptel-runner-jobs--owner token (plist-get invocation :id))))
               (unless (gptel-runner-jobs--lock
			(gptel-runner-jobs--job-lock (plist-get invocation :job-id)) owner)
		 (error "Another invocation of this job is active"))
               (setq invocation (plist-put invocation :worker owner)
                     invocation (plist-put invocation :epoch (1+ (plist-get invocation :epoch)))
                     invocation (plist-put invocation :phase 'starting)
                     invocation (plist-put invocation :worker-state 'starting)
                     invocation (plist-put invocation :restore t)
                     restart t)))
	   (let* ((sequence-file (expand-file-name "command-sequence.el"
						   (plist-get invocation :directory)))
		  (sequence (1+ (if (file-exists-p sequence-file)
                                    (gptel-runner-jobs--read sequence-file) 0)))
		  (command (list :id id :invocation (plist-get invocation :id)
				 :epoch (plist-get invocation :epoch)
				 :sequence sequence
				 :operation operation :arguments arguments
				 :created-at (float-time))))
             (setq receipt (list :id id :status 'queued :operation operation
				 :sequence sequence
				 :invocation (plist-get invocation :id)
				 :epoch (plist-get invocation :epoch)))
             (gptel-runner-jobs--write sequence-file sequence)
             (gptel-runner-jobs--write (gptel-runner-jobs--receipt-file invocation id) receipt)
             (gptel-runner-jobs--write
              (expand-file-name (concat "commands/" id ".el")
				(plist-get invocation :directory)) command)
             (when restart
               (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)))))
      (error
       (when restart (gptel-runner-jobs--launch-failed invocation err))
       (signal (car err) (cdr err))))
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
             (puthash item t seen)
             (cons (walk (car item)) (walk (cdr item))))
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
    (and call (eq (gptel-runner-call-state call) 'waiting-confirmation)
         pending
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

(defun gptel-runner-jobs--export-transcript (call relative)
  "Export CALL's transcript to RELATIVE when its text changes."
  (let ((buffer (gptel-runner-call-buffer call)))
    (when (buffer-live-p buffer)
      (let* ((text (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))
             (hash (secure-hash 'sha256 text))
             (file (expand-file-name relative (plist-get gptel-runner-jobs--invocation :directory))))
        (unless (equal hash (gethash relative gptel-runner-jobs--exports))
          (gptel-runner-jobs--mkdir (file-name-directory file))
          (let ((temporary (make-temp-file (concat file ".tmp-"))))
            (unwind-protect
                (progn (set-file-modes temporary #o600)
                       (write-region text nil temporary nil 'silent)
                       (rename-file temporary file t))
              (when (file-exists-p temporary) (delete-file temporary))))
          (puthash relative hash gptel-runner-jobs--exports))))))

(defun gptel-runner-jobs--publish ()
  "Publish current worker inspection data and its compact manifest."
  (when gptel-runner-jobs--run
    (let* ((run gptel-runner-jobs--run)
           (epoch (plist-get gptel-runner-jobs--invocation :epoch))
           (directory (plist-get gptel-runner-jobs--invocation :directory))
           (previous (cl-remove-if
                      (lambda (call) (= (plist-get call :epoch) epoch))
                      (plist-get gptel-runner-jobs--invocation :calls)))
           calls outputs)
      (dolist (call (gptel-runner-run-calls run))
        (let* ((base (format "calls/%d-%s" epoch (gptel-runner-call-id call)))
               (transcript (concat base ".txt"))
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
                             :record (concat base ".el")
                             :transcript (when (buffer-live-p (gptel-runner-call-buffer call)) transcript))))
          (gptel-runner-jobs--export-file
           (concat base ".el")
           (append record (list :prompt (gptel-runner-call-prompt call)
                                :workspace (gptel-runner-call-workspace call)
                                :result (gptel-runner-jobs--safe-value (gptel-runner-call-result call))
                                :error (gptel-runner-jobs--safe-value (gptel-runner-call-error call)))))
          (gptel-runner-jobs--export-transcript call transcript)
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
    (setq gptel-runner-jobs--command-started t)
    (pcase operation
      ('pause (gptel-runner-pause-run run))
      ('resume (gptel-runner-resume-run run (plist-get args :feedback)))
      ('cancel (gptel-runner-abort-run run 'background-command))
      ('retry (apply #'gptel-runner-retry run args))
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
      ((or 'approve 'reject)
       (gptel-runner-gptel-answer-interaction call (plist-get args :interaction-id)
                                              (eq operation 'approve)
                                              (plist-get args :allow-outside)))
      ('approve-all
       (gptel-runner-gptel-approve-interactions
        call (plist-get args :interaction-ids) (plist-get args :allow-outside)))
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
  "Mark commands interrupted by an earlier worker as indeterminate."
  (let ((directory (expand-file-name "receipts" (plist-get gptel-runner-jobs--invocation :directory))))
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "\\.el\\'"))
        (let ((receipt (gptel-runner-jobs--read file)))
          (when (eq (plist-get receipt :status) 'processing)
            (gptel-runner-jobs--write
             file (plist-put receipt :status 'indeterminate))))))))

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

(defun gptel-runner-jobs-worker (directory)
  "Execute or restore the invocation in DIRECTORY and return its exit code.
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
         (last-publish 0) (exit-code 2) finished)
    (unless (and (eq (plist-get gptel-runner-jobs--invocation :phase) 'starting)
                 (file-exists-p (expand-file-name "owner.el" lock))
                 (equal token (plist-get (gptel-runner-jobs--read (expand-file-name "owner.el" lock)) :token)))
      (error "Worker launch reservation is no longer valid"))
    ;; The control lock prevents two accidentally launched workers from both
    ;; adopting the same reservation.
    (gptel-runner-jobs--with-control-lock
     gptel-runner-jobs--invocation
     (lambda ()
       (let ((fresh (gptel-runner-jobs--read (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation))))
         (unless (eq (plist-get fresh :phase) 'starting)
           (error "Invocation already has an owner")))
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
              (load (plist-get job :init-file) nil t)
              (gptel-runner-jobs--abandoned-receipts)
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
                       (setq gptel-runner-jobs--invocation
                             (plist-put gptel-runner-jobs--invocation :phase 'closing))
                       (gptel-runner-jobs--flush)
                       (setq exit-code (if (eq (gptel-runner-run-state gptel-runner-jobs--run) 'succeeded) 0 1)
                             finished t)
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
           (when gptel-runner-jobs--run
             (ignore-errors (gptel-runner-abort-run gptel-runner-jobs--run 'worker-error))
             (ignore-errors (gptel-runner-jobs--flush)))
           ;; Closed/exited is a public handoff boundary on failures too:
           ;; relinquish every resource before publishing that boundary.
           (maphash (lambda (call _lock) (gptel-runner-jobs--release call))
                    gptel-runner-jobs--writer-locks)
           (gptel-runner-jobs--unlock lock token)
           (setq gptel-runner-jobs--invocation
                 (plist-put gptel-runner-jobs--invocation :state
                            (cond ((null gptel-runner-jobs--run) 'startup-error)
                                  ((or (eq (car err) 'gptel-runner-job-storage-error)
                                       (memq 'file-error (get (car err) 'error-conditions)))
                                   'storage-error)
                                  (t 'worker-error)))
                 gptel-runner-jobs--invocation
                 (plist-put gptel-runner-jobs--invocation :error (error-message-string err))
                 gptel-runner-jobs--invocation
                 (plist-put gptel-runner-jobs--invocation :phase 'closed)
                 gptel-runner-jobs--invocation
                 (plist-put gptel-runner-jobs--invocation :worker-state 'exited))
           (ignore-errors
             (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)
                                       gptel-runner-jobs--invocation))
           (message "Background worker failed: %s" (error-message-string err))))
      (maphash (lambda (call _lock) (gptel-runner-jobs--release call)) gptel-runner-jobs--writer-locks)
      (gptel-runner-jobs--unlock lock token))
    exit-code))

(defun gptel-runner-jobs-batch-main ()
  "Run the background CLI and exit with a meaningful process status.
Arguments: worker DIRECTORY; run JOB-ID [--root DIRECTORY]; or trigger
JOB-ID [--root DIRECTORY].  `run' executes in the current batch process;
`trigger' uses the configured detached launcher."
  (let* ((arguments (delete "--" (copy-sequence command-line-args-left)))
         (operation (pop arguments)) (target (pop arguments))
         (root (cadr (member "--root" arguments)))
         (gptel-runner-job-directory (or root gptel-runner-job-directory)))
    (setq command-line-args-left nil)
    (condition-case err
        (kill-emacs
         (pcase operation
           ("worker" (gptel-runner-jobs-worker target))
           ("run"
            (let ((invocation (gptel-runner-jobs--new-invocation
                               (gptel-runner-job-get target) 'cli)))
              (if (eq (plist-get invocation :state) 'skipped) 0
                (gptel-runner-jobs-worker (plist-get invocation :directory)))))
           ("trigger"
            (let ((job (gptel-runner-job-get target)))
              (when (plist-get job :enabled) (gptel-runner-job-run job 'schedule))
              0))
           (_ (error "Usage: worker DIRECTORY | run JOB-ID | trigger JOB-ID [--root DIRECTORY]"))))
      (error (message "Job launch failed: %s" (error-message-string err))
             (kill-emacs 2)))))

(provide 'gptel-runner-jobs)
;;; gptel-runner-jobs.el ends here
