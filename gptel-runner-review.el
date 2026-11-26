;;; gptel-runner-review.el --- Structured review helpers -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Provider schema hint, authoritative JSON parser, semantic validation, and
;; stable progress keys for implementation/review workflows.

;;; Code:

(require 'json)
(require 'seq)
(require 'gptel-runner-core)

(defconst gptel-runner-review-schema
  '(:type "object"
    :required ["verdict" "summary" "issues"]
    :properties
    (:verdict (:type "string" :enum ["pass" "revise" "blocked"])
     :summary (:type "string")
     :issues (:type "array"
              :items (:type "object"
                      :required ["severity" "message"]
                      :properties
                      (:severity (:type "string")
                       :file (:type ["string" "null"])
                       :line (:type ["integer" "null"])
                       :message (:type "string")
                       :suggested_fix (:type ["string" "null"]))))))
  "JSON schema hint for review agents.")

(defun gptel-runner--plist-symbolize-verdict (review)
  "Normalize REVIEW's verdict to a symbol and return REVIEW."
  (let ((verdict (plist-get review :verdict)))
    (when (stringp verdict)
      (setq review (plist-put review :verdict (intern (downcase verdict))))))
  review)

(defun gptel-runner-valid-review-p (review)
  "Return non-nil when REVIEW is a safe, semantically valid review plist."
  (and (listp review)
       (memq (plist-get review :verdict) '(pass revise blocked))
       (stringp (plist-get review :summary))
       (consp (cdr (plist-member review :issues)))
       (proper-list-p (plist-get review :issues))
       (cl-every
        (lambda (issue)
          (and (listp issue)
               (stringp (plist-get issue :severity))
               (stringp (plist-get issue :message))
               (let ((line (plist-get issue :line)))
                 (or (null line) (and (integerp line) (> line 0))))))
        (plist-get review :issues))))

(defun gptel-runner--review-json-value (value)
  "Convert arrays and nulls in decoded JSON VALUE to the public Lisp form."
  (cond ((or (vectorp value) (consp value))
         (mapcar #'gptel-runner--review-json-value value))
        ((eq value 'null) nil)
        (t value)))

(defun gptel-runner--parse-review-json (text)
  "Parse review TEXT, checking JSON types before normalizing arrays and nulls."
  (let* ((object (json-parse-string text :object-type 'plist :array-type 'array
                                    :null-object 'null :false-object :false))
         (issues (and (listp object) (plist-get object :issues))))
    (unless (vectorp issues)
      (error "Review JSON requires an issues array"))
    (cl-loop for issue across issues do
             (unless (consp issue)
               (error "Expected a review issue object with severity and message"))
             (dolist (key '(:file :line :suggested_fix))
               (when-let* ((field (plist-member issue key)))
                 (let ((value (cadr field)))
                   (unless (or (eq value 'null)
                               (if (eq key :line) (integerp value) (stringp value)))
                     (error "Invalid review issue %s: %S" key value))))))
    (gptel-runner--review-json-value object)))

(defun gptel-runner-parse-review (value)
  "Parse VALUE into a validated review plist.
JSON must contain an issues array.  A Lisp plist supplies its issues
as a list, with nil representing an empty array.
Signal an error for malformed data or an unknown verdict."
  (let* ((parsed
          (cond
           ((stringp value) (gptel-runner--parse-review-json value))
           ((listp value) value)
           (t (error "Review result is neither JSON nor a plist"))))
         (review (gptel-runner--plist-symbolize-verdict parsed)))
    (unless (gptel-runner-valid-review-p review)
      (error "Invalid review result: %S" review))
    review))

(defun gptel-runner-normalize-review (review)
  "Return stable progress material for REVIEW, ignoring prose ordering noise."
  (let ((issues
         (mapcar
          (lambda (issue)
            (list (downcase (plist-get issue :severity))
                  (or (plist-get issue :file) "")
                  (or (plist-get issue :line) 0)
                  (string-trim (downcase (plist-get issue :message)))))
          (plist-get review :issues))))
    (list (plist-get review :verdict)
          (sort issues (lambda (a b)
                         (string< (prin1-to-string a)
                                  (prin1-to-string b)))))))

(defun gptel-runner-review-progress-key (run &optional review-key)
  "Hash normalized review in RUN at REVIEW-KEY, defaulting to `review'."
  (when-let* ((review (gptel-runner-get run (or review-key 'review))))
    (secure-hash 'sha256
                 (prin1-to-string (gptel-runner-normalize-review review)))))

(defun gptel-runner--git-output (directory &rest arguments)
  "Return Git's stdout in DIRECTORY for ARGUMENTS, or nil on failure."
  (when directory
    (let* ((default-directory (file-name-as-directory directory))
           (coding-system-for-read
            (coding-system-change-eol-conversion
             (or file-name-coding-system default-file-name-coding-system 'utf-8) 0))
           (coding-system-for-write coding-system-for-read))
      (with-temp-buffer
        (when (eql 0 (ignore-error file-error
                       (apply #'process-file "git" nil '(t nil) nil
                              "--no-optional-locks" arguments)))
          (buffer-string))))))

(defun gptel-runner--git-dirty-path (record)
  "Extract a path needing a content hash from Git porcelain v2 RECORD."
  ;; Ordinary records have eight space-terminated fields, unmerged ones ten.
  ;; With renames disabled, each record (including its path) is NUL-delimited.
  (pcase (aref record 0)
    (?? (substring record 2))
    (?1 (unless (memq (aref record 3) '(?. ?D)) ; unchanged or deleted
          (when (string-match "\\`\\(?:[^ ]+ \\)\\{8\\}" record)
            (substring record (match-end 0)))))
    (?u (when (string-match "\\`\\(?:[^ ]+ \\)\\{10\\}" record)
          (substring record (match-end 0))))))

(defun gptel-runner--git-worktree-key (root)
  "Hash the HEAD, index and visible working tree changes at ROOT."
  (when-let* ((status (gptel-runner--git-output
                       root "status" "--porcelain=v2" "-z" "--branch"
                       "--no-ahead-behind" "--no-renames" "--untracked-files=all"
                       "--ignore-submodules=none")))
    (with-temp-buffer
      (let ((default-directory root)
            files)
        (dolist (record (sort (split-string status "\0" t) #'string<))
          ;; HEAD also records progress when a worker commits between reviews.
          (unless (and (eq (aref record 0) ?#)
                       (not (string-prefix-p "# branch.oid " record)))
            (insert record "\0")
            (when-let* ((path (gptel-runner--git-dirty-path record))
                        (file (concat "./" path))
                        (attrs (file-attributes file)))
              (let ((type (file-attribute-type attrs))
                    (mode (file-attribute-modes attrs)))
                (cond
                 ((stringp type) (insert "link " type "\0"))
                 ((eq (aref mode 0) ?-)
                  (insert (if (memq (aref mode 3) '(?x ?s)) "+x\0" "-x\0"))
                  (push file files)))))))
        ;; Let Git read regular files directly.  Only fixed-size object IDs
        ;; enter Emacs, even for large binary files.  Never read FIFOs/devices.
        (catch 'incomplete
          (dolist (batch (seq-partition (nreverse files) 32))
            (let ((oids (apply #'gptel-runner--git-output root
                               "hash-object" "--no-filters" "--" batch)))
              (unless oids (throw 'incomplete nil))
              (insert oids)))
          (secure-hash 'sha256 (current-buffer)))))))

(defun gptel-runner-workspace-diff-key (run)
  "Hash Git worktrees used by RUN and its explicit node workspace overrides.
Each distinct worktree contributes its HEAD, staged changes, and unstaged
and non-ignored untracked changes.  A workspace inside a repository covers
the whole worktree.  Return nil if no worktree can be fingerprinted."
  (let* ((workspace (gptel-runner-run-workspace run))
         (directories (list workspace))
         roots keys)
    (when (gptel-runner-run-workflow run)
      (dolist (node (gptel-runner-list-nodes run))
        (when (and (memq (gptel-runner-node-kind node) '(agent elisp))
                   (plist-get (gptel-runner-node-properties node) :workspace))
          (push (gptel-runner--node-workspace node workspace t) directories))))
    (dolist (directory (delete-dups directories))
      (when-let* ((top (gptel-runner--git-output directory "rev-parse" "--show-toplevel")))
        (push (file-name-as-directory
               (concat (file-remote-p directory) (string-remove-suffix "\n" top)))
              roots)))
    (dolist (root (sort (delete-dups roots) #'string<))
      (when-let* ((key (gptel-runner--git-worktree-key root)))
        (push key keys)))
    (when keys (secure-hash 'sha256 (apply #'concat keys)))))

(defun gptel-runner-review-diff-progress-key (run &optional review-key)
  "Hash the review at REVIEW-KEY and the Git worktree progress of RUN."
  (when-let* ((review (gptel-runner-get run (or review-key 'review))))
    (secure-hash
     'sha256
     (prin1-to-string
      (list (gptel-runner-normalize-review review)
            (gptel-runner-workspace-diff-key run))))))

(provide 'gptel-runner-review)
;;; gptel-runner-review.el ends here
