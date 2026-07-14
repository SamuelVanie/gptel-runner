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
  '(:type object
    :required ["verdict" "summary" "issues"]
    :properties
    (:verdict (:type string :enum ["pass" "revise" "blocked"])
     :summary (:type string)
     :issues (:type array
              :items (:type object
                      :required ["severity" "message"]
                      :properties
                      (:severity (:type string)
                       :file (:type [string null])
                       :line (:type [integer null])
                       :message (:type string)
                       :suggested_fix (:type [string null]))))))
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
       (listp (plist-get review :issues))
       (cl-every
        (lambda (issue)
          (and (listp issue)
               (stringp (plist-get issue :severity))
               (stringp (plist-get issue :message))
               (let ((line (plist-get issue :line)))
                 (or (null line) (and (integerp line) (> line 0))))))
        (plist-get review :issues))))

(defun gptel-runner-parse-review (value)
  "Parse VALUE into a validated review plist.
Malformed data, missing approval, or an unknown verdict signals an error;
therefore it can never be interpreted as `pass'."
  (let* ((parsed
          (cond
           ((stringp value)
            (json-parse-string value :object-type 'plist :array-type 'list
                               :null-object nil :false-object :false))
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
  (when-let ((review (gptel-runner-get run (or review-key 'review))))
    (secure-hash 'sha256
                 (prin1-to-string (gptel-runner-normalize-review review)))))

(defun gptel-runner-workspace-diff-key (run)
  "Return a Git diff hash for RUN, or nil when Git is unavailable/inapplicable."
  (when (and (executable-find "git")
             (file-directory-p
              (expand-file-name ".git" (gptel-runner-run-workspace run))))
    (with-temp-buffer
      (let ((default-directory (gptel-runner-run-workspace run)))
        (when (zerop (process-file "git" nil t nil "diff" "--no-ext-diff"))
          (secure-hash 'sha256 (buffer-string)))))))

(defun gptel-runner-review-diff-progress-key (run &optional review-key)
  "Hash review and optional Git diff progress for RUN at REVIEW-KEY."
  (when-let ((review (gptel-runner-get run (or review-key 'review))))
    (secure-hash
     'sha256
     (prin1-to-string
      (list (gptel-runner-normalize-review review)
            (gptel-runner-workspace-diff-key run))))))

(provide 'gptel-runner-review)
;;; gptel-runner-review.el ends here
