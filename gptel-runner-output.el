;;; gptel-runner-output.el --- Managed pipeline artifacts -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Pipeline-owned files with the same descriptor and lifetime in local runs
;; and background workers.  The job adapter selects storage before dispatch;
;; pipeline functions never need to inspect their launch environment.

;;; Code:

(require 'gptel-runner-core)

(defcustom gptel-runner-output-directory
  (expand-file-name "gptel-runner/outputs/" user-emacs-directory)
  "Parent directory for local runs' retained artifacts.
Each publishing run gets a private, unique subdirectory.  Background workers
use their invocation directory instead.  Files survive run completion,
forgetting a run, and snapshot deletion; remove retained directories explicitly
when no longer needed.  Snapshots reference these files, not their contents."
  :type 'directory
  :group 'gptel-runner)

(cl-defgeneric gptel-runner-artifacts (source)
  "Return SOURCE's artifact descriptors in publication order.
SOURCE is a run or background invocation record.  Return fresh list/plist
structure.  Repeated names denote distinct retained versions.")

(cl-defmethod gptel-runner-artifacts ((source gptel-runner-run))
  "Return live SOURCE's retained artifact descriptors."
  (copy-tree (gptel-runner-run-artifacts source)))

(cl-defgeneric gptel-runner--artifact-root (source)
  "Return SOURCE's retained output directory, or nil before publication.")

(cl-defmethod gptel-runner--artifact-root ((source gptel-runner-run))
  "Return live SOURCE's retained output directory."
  (gptel-runner-run-output-directory source))

(defun gptel-runner-artifact-file (source artifact)
  "Return the absolute readable filename of ARTIFACT belonging to SOURCE.
SOURCE is a run or background invocation record.  ARTIFACT is a descriptor
returned by publication or `gptel-runner-artifacts'.  Resolve its relative
`:file' against managed storage, never against a workspace or current buffer.
Signal a user error for unregistered, missing, or escaping files."
  (let ((root (gptel-runner--artifact-root source))
        (relative (plist-get artifact :file)))
    (unless (and root (stringp relative)
                 (not (file-name-absolute-p relative))
                 (member artifact (gptel-runner-artifacts source)))
      (user-error "Artifact does not belong to this run or invocation"))
    (let ((file (expand-file-name relative root)))
      (unless (and (file-in-directory-p file root)
                   (file-regular-p file) (file-readable-p file))
        (user-error "Artifact is unavailable or escapes storage: %s" relative))
      file)))

(defun gptel-runner-read-artifact (source artifact)
  "Read ARTIFACT from SOURCE as text using normal Emacs decoding.
For binary files, use `gptel-runner-artifact-file' instead."
  (let ((file (gptel-runner-artifact-file source artifact)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun gptel-runner-output--directory (run)
  "Return or allocate RUN's private retained output directory."
  (or (gptel-runner-run-output-directory run)
      (let ((root (expand-file-name gptel-runner-output-directory)))
        (make-directory root t)
        (set-file-modes root #o700)
        ;; Session run IDs are not unique across Emacs processes.
        (setf (gptel-runner-run-output-directory run)
              (file-name-as-directory
               (make-temp-file (expand-file-name "run-" root) t))))))

(defun gptel-runner-output--publish (run name filename media-type writer)
  "Retain RUN's named output using FILENAME, MEDIA-TYPE, and WRITER.
WRITER receives a private temporary path.  Register NAME only after its
completed file has been atomically moved into place."
  (unless (and (gptel-runner-run-p run)
               (stringp name) (not (string-empty-p name))
               (or (null media-type) (stringp media-type)))
    (user-error "Publication needs a run, non-empty name, and optional media type"))
  (let* ((root (gptel-runner-output--directory run))
         (directory (expand-file-name "artifacts/" root))
         (basename (replace-regexp-in-string
                    "[^[:alnum:]_.-]" "_" (file-name-nondirectory filename)))
         temporary target descriptor)
    (make-directory directory t)
    (unless (file-in-directory-p directory root)
      (user-error "Artifact directory escapes output storage"))
    (set-file-modes directory #o700)
    (setq temporary (make-temp-file (expand-file-name ".write-" directory))
          target (expand-file-name
                  (concat "artifact-" (substring (file-name-nondirectory temporary) 7)
                          "-" basename)
                  directory))
    (unwind-protect
        (progn
          (funcall writer temporary)
          (set-file-modes temporary #o600)
          (rename-file temporary target)
          (setq descriptor
                (list :name name :file (file-relative-name target root)
                      :media-type media-type
                      :epoch (gptel-runner-run-output-epoch run)
                      :published-at (float-time)))
          (setf (gptel-runner-run-artifacts run)
                (append (gptel-runner-run-artifacts run) (list descriptor))))
      (when (file-exists-p temporary) (delete-file temporary)))
    (gptel-runner--emit run 'artifact-published nil nil (copy-tree descriptor))
    (gptel-runner--checkpoint run)
    (copy-tree descriptor)))

(defun gptel-runner-publish-artifact (run name file &optional media-type)
  "Copy FILE into RUN's managed storage as NAME and return its descriptor.
The descriptor always has `:name', relative `:file', `:media-type', `:epoch'
and `:published-at'.  Epoch is nil locally and a worker generation in jobs.
FILE must be readable and regular; relative paths use RUN's workspace.
The source remains caller-owned.  Later source edits do not affect the copy.
Publication appends to `gptel-runner-artifacts'; use `:save-as' or
`gptel-runner-put' to also give the descriptor a blackboard key."
  (unless (and (gptel-runner-run-p run) (stringp file))
    (user-error "Publication needs a run and a filename"))
  (let ((source (expand-file-name file (gptel-runner-run-workspace run))))
    (unless (and (file-regular-p source) (file-readable-p source))
      (user-error "Artifact source is not a readable regular file: %s" source))
    (gptel-runner-output--publish
     run name source media-type
     (lambda (target) (copy-file source target t)))))

(defun gptel-runner-write-artifact (run name text &optional media-type)
  "Write TEXT directly to RUN's managed storage as NAME.
Return the same descriptor as `gptel-runner-publish-artifact'.  No temporary
file or launch-mode branch is needed in pipeline code.  TEXT is encoded as
UTF-8 with Unix line endings.  MEDIA-TYPE defaults to text/plain.  NAME is a
display name and filename hint, not a path into the workspace."
  (unless (stringp text) (user-error "Artifact text must be a string"))
  (gptel-runner-output--publish
   run name name (or media-type "text/plain")
   (lambda (target)
     (let ((coding-system-for-write 'utf-8-unix))
       (write-region text nil target nil 'silent)))))

(provide 'gptel-runner-output)
;;; gptel-runner-output.el ends here
