;;; three-stage-handoff.el --- Sequential agent handoff example -*- lexical-binding: t; -*-

;;; Commentary:

;; The researcher sends findings to the planner.  The planner may ask the
;; researcher for evidence before handing its plan to the implementer.
;; The implementer can ask the planner questions while doing the work.
;; Each agent answers in its own chat.  The plan requires human acceptance.
;; After the last handoff, read the report with
;; (gptel-runner-get run 'completion-report).
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
           "the goal.  Call RunnerHandoff with the findings and file locations "
           "the planner will need.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)))

(defun example/handoff-plan-prompt (run _node)
  "Ask the planner to turn its incoming research into a plan for RUN."
  (format
   (concat "Plan the implementation of this goal:\n%s\n\n"
           "Check the researcher's findings against the workspace.  "
           "If evidence is missing, use RunnerAskPrevious to ask for it.  "
           "Use RunnerHandoff to submit the plan for approval.  Include "
           "the files to change and the tests the implementer should run.")
   (gptel-runner-run-goal run)))

(defun example/handoff-implement-prompt (run _node)
  "Ask the implementer to carry out the accepted plan for RUN."
  (format
   (concat "Goal:\n%s\n\nWorkspace: %s\n\n"
           "Carry out the approved plan.  Use RunnerAskPrevious if you need "
           "the planner to clarify a decision.  Run the relevant tests, then "
           "use RunnerHandoff to report the changes and test results.")
   (gptel-runner-run-goal run)
   (gptel-runner-run-workspace run)))

(gptel-runner-defworkflow three-stage-handoff
    (:max-requests 30 :max-calls 12 :max-concurrency 1 :max-duration 1800)
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
    :save-as 'implementation-plan
    :pause-after t)

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
