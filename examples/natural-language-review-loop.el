;;; natural-language-review-loop.el --- File-based review loop example -*- lexical-binding: t; -*-

;;; Commentary:

;; This example builds an implementation/review loop using only the generic
;; runner primitives.  It does not require `gptel-runner-review'.
;;
;; Every invocation is a fresh, stateless call:
;;
;;   fresh implementer -> fresh reviewer -> fresh implementer -> ...
;;
;; The calls communicate through `.gptel-runner/handoff.md' in the workspace.
;; That file contains only the goal, the latest status, and natural-language
;; feedback.  Agents must still reinspect the actual workspace and run tests.
;;
;; Free-form prose is not a deterministic stopping condition, so the reviewer
;; writes one small protocol line before its natural-language observations:
;;
;;   STATUS: PASS
;;   STATUS: REVISE
;;   STATUS: BLOCKED
;;
;; The workflow predicates below interpret only that line.  Everything after
;; it is ordinary natural-language feedback with no package-specific schema.

;;; Code:

(require 'subr-x)
(require 'gptel-runner)

(defconst example/natural-review-state-file ".gptel-runner/handoff.md"
  "Workspace-relative handoff file used by the natural-language example.")

(gptel-runner-register-agent
 'natural-review-implementer
 :preset 'natural-review-implementer
 :workspace-mode 'write)

;; This reviewer is marked writable only because it replaces the handoff
;; file.  Its preset should otherwise contain read/test tools and instructions
;; forbidding changes to the implementation.
(gptel-runner-register-agent
 'natural-review-reviewer
 :preset 'natural-review-reviewer
 :workspace-mode 'write)

(defun example/natural-review-state-path (run)
  "Return the absolute handoff-file path for RUN."
  (expand-file-name example/natural-review-state-file
                    (gptel-runner-run-workspace run)))

(defun example/natural-review-status (run)
  "Return the reviewer status symbol found in RUN's latest raw response."
  (let ((response (gptel-runner-get run 'latest-review-response)))
    (when (and (stringp response)
               (string-match
                "\\`[[:space:]\n]*STATUS:[[:space:]]*\\([A-Za-z]+\\)"
                response))
      (intern (downcase (match-string 1 response))))))

(defun example/natural-review-passed-p (run)
  "Return non-nil when RUN's fresh reviewer reported PASS."
  (eq (example/natural-review-status run) 'pass))

(defun example/natural-review-blocked-p (run)
  "Return non-nil when RUN's fresh reviewer reported BLOCKED."
  (eq (example/natural-review-status run) 'blocked))

(defun example/natural-review-progress-key (run)
  "Hash RUN's latest natural-language reviewer response for stall detection."
  (when-let ((response (gptel-runner-get run 'latest-review-response)))
    (secure-hash 'sha256
                 (string-trim (downcase response)))))

(defun example/natural-review-implement-prompt (run _node)
  "Build a fresh implementation prompt from RUN and its handoff file."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\nIteration: %d\n\n"
           "Read %s.  It contains the goal and the latest natural-language "
           "feedback from an independent reviewer.  Reinspect the actual "
           "workspace, address every applicable observation, and run relevant "
           "tests.  Do not assume any previous conversation or agent state.\n\n"
           "Return a concise natural-language report of changes and checks.  "
           "Do not delete or replace the handoff file; the reviewer owns it.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (gptel-runner-iteration run 'natural-review-cycle)
   (example/natural-review-state-path run)))

(defun example/natural-review-review-prompt (run _node)
  "Build an independent review prompt for RUN without prior review context."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n\n"
           "Act as a fresh independent reviewer.  Reinspect the implementation "
           "and run the relevant tests yourself.  Do not trust the implementer "
           "report as evidence and do not modify implementation files.\n\n"
           "Replace %s with only the goal, one status line, and the feedback "
           "needed by the next fresh implementer.  Use STATUS: PASS when no "
           "work remains, STATUS: REVISE when corrections remain, or "
           "STATUS: BLOCKED when external input is required.\n\n"
           "Return exactly the same text that you wrote to the handoff file.\n\n"
           "Implementer report (informational only):\n%s")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (example/natural-review-state-path run)
   (or (gptel-runner-get run 'latest-implementation-report) "No report")))

(gptel-runner-defworkflow natural-language-review-loop
    (:max-requests 24 :max-calls 12 :max-concurrency 1 :max-duration 3600)
  (gptel-runner-repeat-until
   :id 'natural-review-cycle
   :max 6
   :until #'example/natural-review-passed-p
   :stop-when #'example/natural-review-blocked-p
   :progress-key #'example/natural-review-progress-key
   :body
   (gptel-runner-sequence
    (gptel-runner-agent-step
     :id 'natural-implementation
     :agent 'natural-review-implementer
     :prompt #'example/natural-review-implement-prompt
     :save-as 'latest-implementation-report)

    (gptel-runner-agent-step
     :id 'natural-review
     :agent 'natural-review-reviewer
     :prompt #'example/natural-review-review-prompt
     :save-as 'latest-review-response))))

(defun example/start-natural-language-review (goal workspace)
  "Start a natural-language review for GOAL in WORKSPACE.
Create the minimal handoff file before starting the workflow."
  (let* ((workspace (file-name-as-directory (file-truename workspace)))
         (state-file (expand-file-name example/natural-review-state-file
                                       workspace)))
    (make-directory (file-name-directory state-file) t)
    (write-region
     (format "# Workflow handoff\n\nGoal: %s\n\nSTATUS: REVISE\n\nNo review has run yet.\n"
             goal)
     nil state-file nil 'silent)
    (gptel-runner-start
     'natural-language-review-loop
     :goal goal
     :workspace workspace
     :allow-writes t)))

;; Define the `natural-review-implementer' and `natural-review-reviewer' gptel
;; presets, then start the workflow with:
;;
;; (example/start-natural-language-review
;;  "Add a health-check command to this package"
;;  (project-root (project-current t)))
;;
;; Inspect or delete `.gptel-runner/handoff.md' after the run.  Projects that
;; retain it during development should add the path to their local ignore file.

(provide 'natural-language-review-loop)
;;; natural-language-review-loop.el ends here

