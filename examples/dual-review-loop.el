;;; dual-review-loop.el --- Parallel reviewers with output repair -*- lexical-binding: t; -*-

;;; Commentary:

;; This example implements a bounded workflow of the form:
;;
;;   writer -> (reviewer A || reviewer B) -> writer -> ...
;;
;; Both independent reviewers must report PASS in the same iteration.  Every
;; iteration uses fresh stateless calls, and the writer receives the previous
;; reviews explicitly through the run blackboard.
;;
;; Reviewers must begin their response with exactly one of:
;;
;;   VERDICT: PASS
;;   VERDICT: REVISE
;;
;; The strict parser signals an error when this protocol is missing.  Each
;; reviewer step enables `:repair-invalid t', so the runner makes one
;; stateless format-repair call.  If that response is malformed too, the
;; workflow fails safely with `invalid-output'.

;;; Code:

(require 'subr-x)
(require 'gptel-runner)

(defun example/dual-review-parse (text)
  "Parse strict verdict header and preserve reviewer TEXT."
  (let ((first-line (string-trim (or (car (string-lines text)) ""))))
    (cond
     ((string= first-line "VERDICT: PASS")
      (list :verdict 'pass :feedback text))
     ((string= first-line "VERDICT: REVISE")
      (list :verdict 'revise :feedback text))
     (t
      (error
       "First line must be exactly VERDICT: PASS or VERDICT: REVISE")))))

(defun example/dual-review-valid-p (review)
  "Return non-nil when REVIEW has the required parsed shape."
  (and (listp review)
       (memq (plist-get review :verdict) '(pass revise))
       (stringp (plist-get review :feedback))))

(gptel-runner-register-agent
 'dual-review-writer
 :preset 'dual-review-writer
 :workspace-mode 'write)

(gptel-runner-register-agent
 'dual-reviewer-a
 :preset 'dual-reviewer-a
 :workspace-mode 'read
 :parser #'example/dual-review-parse
 :validator #'example/dual-review-valid-p)

(gptel-runner-register-agent
 'dual-reviewer-b
 :preset 'dual-reviewer-b
 :workspace-mode 'read
 :parser #'example/dual-review-parse
 :validator #'example/dual-review-valid-p)

(defun example/dual-review-feedback (run key)
  "Return natural-language feedback stored under KEY in RUN."
  (or (plist-get (gptel-runner-get run key) :feedback) "None yet"))

(defun example/dual-review-writer-prompt (run _node)
  "Build a writer prompt from RUN and both previous reviews."
  (format
   (concat
    "Goal:\n%s\n\nWorkspace: %s\nIteration: %d\n\n"
    "Reviewer A's previous feedback:\n%s\n\n"
    "Reviewer B's previous feedback:\n%s\n\n"
    "Reinspect the current workspace.  Implement the goal or correct every "
    "applicable issue, then run the relevant tests.  Do not assume access to "
    "any earlier conversation.  Return a concise implementation report.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (1+ (gptel-runner-iteration run 'dual-review-cycle))
   (example/dual-review-feedback run 'dual-review-a)
   (example/dual-review-feedback run 'dual-review-b)))

(defun example/dual-review-reviewer-prompt (run node)
  "Build an independent review prompt for NODE in RUN."
  (format
   (concat
    "Goal:\n%s\n\nWorkspace: %s\n\n"
    "You are independent reviewer %s.  Reinspect the implementation and run "
    "the relevant tests yourself.  Do not trust the writer's report as "
    "evidence and do not modify the workspace.\n\n"
    "Your first line must be exactly VERDICT: PASS when all requirements are "
    "satisfied, or VERDICT: REVISE when any correction remains.  After that "
    "line, give concrete natural-language findings for the next writer.\n\n"
    "Writer report (informational only):\n%s")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (if (eq (gptel-runner-node-id node) 'dual-review-a) "A" "B")
   (or (gptel-runner-get run 'dual-review-implementation-report) "No report")))

(defun example/dual-review-both-pass-p (run)
  "Return non-nil when both reviews in RUN have a PASS verdict."
  (and (eq (plist-get (gptel-runner-get run 'dual-review-a) :verdict)
           'pass)
       (eq (plist-get (gptel-runner-get run 'dual-review-b) :verdict)
           'pass)))

(defun example/dual-review-progress-key (run)
  "Return normalized combined feedback from RUN for stall detection."
  (list
   (string-trim
    (downcase (example/dual-review-feedback run 'dual-review-a)))
   (string-trim
    (downcase (example/dual-review-feedback run 'dual-review-b)))))

(gptel-runner-defworkflow dual-review-loop
    (:max-requests 50 :max-calls 25 :max-concurrency 2 :max-duration 3600)
  (gptel-runner-repeat-until
   :id 'dual-review-cycle
   :max 5
   :until #'example/dual-review-both-pass-p
   :progress-key #'example/dual-review-progress-key
   :body
   (gptel-runner-sequence
    :id 'dual-review-iteration

    (gptel-runner-agent-step
     :id 'dual-review-implementation
     :agent 'dual-review-writer
     :prompt #'example/dual-review-writer-prompt
     :save-as 'dual-review-implementation-report)

    (gptel-runner-parallel
     :id 'dual-review-parallel
     :policy 'fail-fast

     (gptel-runner-agent-step
      :id 'dual-review-a
      :agent 'dual-reviewer-a
      :prompt #'example/dual-review-reviewer-prompt
      :repair-invalid t
      :save-as 'dual-review-a)

     (gptel-runner-agent-step
      :id 'dual-review-b
      :agent 'dual-reviewer-b
      :prompt #'example/dual-review-reviewer-prompt
      :repair-invalid t
      :save-as 'dual-review-b)))))

;; Define the `dual-review-writer', `dual-reviewer-a', and `dual-reviewer-b'
;; gptel presets, then start the workflow with:
;;
;; (gptel-runner-start
;;  'dual-review-loop
;;  :goal "Implement the requested feature"
;;  :workspace (project-root (project-current t))
;;  :allow-writes t
;;  :callback
;;  (lambda (run)
;;    (message "Dual-review workflow finished: %s"
;;             (gptel-runner-run-state run))))

(provide 'dual-review-loop)
;;; dual-review-loop.el ends here
