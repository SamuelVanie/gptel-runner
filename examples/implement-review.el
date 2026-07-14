;;; implement-review.el --- Bounded implementation/review example -*- lexical-binding: t; -*-

(require 'gptel-runner)
(require 'gptel-runner-review)

;; Define runner-implementer and runner-reviewer with `gptel-make-preset'
;; before registering these agents.
(gptel-runner-register-agent
 'implementer :preset 'runner-implementer :workspace-mode 'write)
(gptel-runner-register-agent
 'reviewer :preset 'runner-reviewer :workspace-mode 'read
 :schema gptel-runner-review-schema :parser #'gptel-runner-parse-review
 :validator #'gptel-runner-valid-review-p)

(defun example/implementation-prompt (run _node)
  "Build an implementation prompt for RUN."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\nRevision iteration: %d\n"
           "Prior review: %S\nInspect actual files, make the change, run tests, "
           "and return a concise report.")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-iteration run 'review-cycle)
   (gptel-runner-get run 'review)))

(defun example/review-prompt (run _node)
  "Build an independent review prompt for RUN."
  (format
   (concat "Review the current workspace for this goal:\n%s\n\nWorkspace: %s\n"
           "Implementation report: %S\nReturn only the required review JSON. "
           "Do not modify files.")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-get run 'implementation)))

(gptel-runner-defworkflow implement-review
    (:max-requests 30 :max-calls 12 :max-concurrency 2 :max-duration 3600)
  (gptel-runner-repeat-until
   :id 'review-cycle :max 5
   :until (lambda (run)
            (eq (plist-get (gptel-runner-get run 'review) :verdict) 'pass))
   :stop-when (lambda (run)
                (eq (plist-get (gptel-runner-get run 'review) :verdict)
                    'blocked))
   :progress-key #'gptel-runner-review-progress-key
   :body
   (gptel-runner-sequence
    (gptel-runner-agent-step
     :id 'implement :agent 'implementer
     :prompt #'example/implementation-prompt :save-as 'implementation)
    (gptel-runner-agent-step
     :id 'review :agent 'reviewer :prompt #'example/review-prompt
     :save-as 'review :repair-invalid t))))

;; Starting this workflow intentionally requires explicit write opt-in:
;; (gptel-runner-start 'implement-review :goal "..." :workspace "..."
;;                     :allow-writes t)

(provide 'implement-review)
;;; implement-review.el ends here
