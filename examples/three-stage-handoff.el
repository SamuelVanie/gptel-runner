;;; three-stage-handoff.el --- Sequential agent handoff example -*- lexical-binding: t; -*-

;;; Commentary:

;; This example demonstrates an explicit, stateless handoff:
;;
;;   researcher -> planner -> implementer
;;
;; Each step stores its result on the run blackboard with `:save-as'.  The
;; next step receives that result because its prompt function reads it with
;; `gptel-runner-get'.  No conversational transcript is shared implicitly.
;;
;; Define the gptel presets `handoff-researcher', `handoff-planner', and
;; `handoff-implementer' before running this workflow.  Their tool lists are
;; the actual capability boundary; the runner workspace modes are metadata.

;;; Code:

(require 'gptel-runner)

(gptel-runner-register-agent
 'handoff-researcher
 :preset 'handoff-researcher
 :workspace-mode 'read)

(gptel-runner-register-agent
 'handoff-planner
 :preset 'handoff-planner
 :workspace-mode 'read)

(gptel-runner-register-agent
 'handoff-implementer
 :preset 'handoff-implementer
 :workspace-mode 'write)

(defun example/handoff-research-prompt (run _node)
  "Ask the first agent to investigate the goal in RUN."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n\n"
           "Inspect the workspace and gather the facts needed to accomplish "
           "the goal.  Do not modify files.  Return a concise research report "
           "for the planning agent.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)))

(defun example/handoff-plan-prompt (run _node)
  "Pass the research report in RUN to the planning agent."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n\n"
           "Research report from the previous agent:\n%S\n\n"
           "Reinspect relevant files, then produce a concrete implementation "
           "plan for the final agent.  Do not modify files.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (gptel-runner-get run 'research-report)))

(defun example/handoff-implement-prompt (run _node)
  "Pass prior results in RUN to the final implementation agent."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n\n"
           "Research report:\n%S\n\nImplementation plan:\n%S\n\n"
           "Inspect the current workspace, implement the goal, and run the "
           "relevant verification.  Return a concise completion report.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)
   (gptel-runner-get run 'research-report)
   (gptel-runner-get run 'implementation-plan)))

(gptel-runner-defworkflow three-stage-handoff
    (:max-requests 12 :max-calls 3 :max-concurrency 1 :max-duration 1800)
  (gptel-runner-sequence
   :id 'three-stage-handoff-sequence
   (gptel-runner-agent-step
    :id 'research
    :agent 'handoff-researcher
    :prompt #'example/handoff-research-prompt
    :save-as 'research-report)

   (gptel-runner-agent-step
    :id 'plan
    :agent 'handoff-planner
    :prompt #'example/handoff-plan-prompt
    :save-as 'implementation-plan)

   (gptel-runner-agent-step
    :id 'implement
    :agent 'handoff-implementer
    :prompt #'example/handoff-implement-prompt
    :save-as 'completion-report)))

;; The final agent is writable, so the run requires explicit write opt-in:
;;
;; (gptel-runner-start
;;  'three-stage-handoff
;;  :goal "Add a health-check command to this package"
;;  :workspace (project-root (project-current t))
;;  :persist t
;;  :allow-writes t
;;  :callback
;;  (lambda (run)
;;    (message "State: %s, report: %S"
;;             (gptel-runner-run-state run)
;;             (gptel-runner-get run 'completion-report))))

(provide 'three-stage-handoff)
;;; three-stage-handoff.el ends here
