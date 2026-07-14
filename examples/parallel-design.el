;;; parallel-design.el --- Fan-out and synthesis example -*- lexical-binding: t; -*-

(require 'gptel-runner)

(dolist (name '(architect-a architect-b architect-c arbiter))
  (gptel-runner-register-agent name :preset name :workspace-mode 'read))

(defun example/design-prompt (run node)
  "Build a proposal prompt for RUN and NODE."
  (format "Goal: %s\nWorkspace: %s\nProduce proposal %s after inspecting files."
          (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
          (gptel-runner-node-id node)))

(defun example/synthesis-prompt (run _node)
  "Build an arbiter prompt from RUN's three proposals."
  (format "Synthesize one decision for goal %s from:\nA=%S\nB=%S\nC=%S"
          (gptel-runner-run-goal run)
          (gptel-runner-get run 'proposal-a)
          (gptel-runner-get run 'proposal-b)
          (gptel-runner-get run 'proposal-c)))

(gptel-runner-defworkflow parallel-design
    (:max-requests 20 :max-calls 10 :max-concurrency 3)
  (gptel-runner-sequence
   (gptel-runner-parallel
    :id 'proposals :policy 'fail-fast
    (gptel-runner-agent-step :id 'proposal-a :agent 'architect-a
                             :prompt #'example/design-prompt
                             :save-as 'proposal-a)
    (gptel-runner-agent-step :id 'proposal-b :agent 'architect-b
                             :prompt #'example/design-prompt
                             :save-as 'proposal-b)
    (gptel-runner-agent-step :id 'proposal-c :agent 'architect-c
                             :prompt #'example/design-prompt
                             :save-as 'proposal-c))
   (gptel-runner-agent-step :id 'synthesis :agent 'arbiter
                            :prompt #'example/synthesis-prompt
                            :save-as 'decision)))

(provide 'parallel-design)
;;; parallel-design.el ends here
