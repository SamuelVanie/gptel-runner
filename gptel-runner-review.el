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

(defun gptel-runner-workspace-diff-key (run)
  "Hash the unstaged Git diff from RUN's workspace.
Git discovers the worktree from the workspace directory.  Return nil
when the workspace has no worktree or Git cannot produce a diff."
  (when-let* ((workspace (gptel-runner-run-workspace run))
              ((file-directory-p workspace)))
    (with-temp-buffer
      (let ((default-directory workspace))
        (when (and (executable-find "git" t)
                   (equal 0 (process-file "git" nil '(t nil) nil
                                          "diff" "--no-ext-diff")))
          (secure-hash 'sha256 (buffer-string)))))))

(defun gptel-runner-review-diff-progress-key (run &optional review-key)
  "Hash review and optional Git diff progress for RUN at REVIEW-KEY."
  (when-let* ((review (gptel-runner-get run (or review-key 'review))))
    (secure-hash
     'sha256
     (prin1-to-string
      (list (gptel-runner-normalize-review review)
            (gptel-runner-workspace-diff-key run))))))

(provide 'gptel-runner-review)
;;; gptel-runner-review.el ends here
