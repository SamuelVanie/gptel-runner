;;; gptel-runner session · report · paused · 2026-11-24 16:00
(:format gptel-runner-snapshot
 :version 4
 :saved-at 1795532426.966379
 :run (:id "run-832ada5e3d1e-1"
  :workflow report
  :goal nil
  :workspace "/"
  :output-directory nil
  :artifacts nil
  :output-epoch nil
  :state paused
  :blackboard ()
  :node-states (
   (draft . running))
  :node-outcomes ()
  :discussions ()
  :messages nil
  :chats (
   (draft gptel-agent :version 1 :name "*gptel:report/draft*" :title nil :buffer-name "*gptel:report/draft*" :family-id nil :sent-end 35 :directory "/" :mode text-mode :point 19 :narrow nil :updated 1795532426.966494 :text "### Draft a report

Which dataset?

### " :bounds ((response (21 35))) :backend "runner-test" :tools (("misc" "runner_read")) :settings ((gptel-agent--todos) (gptel-agent--enabled-skills) (gptel-agent--enabled-agents) (gptel-agent--current-agent) (gptel-agent-use-project-instructions . t) (gptel-response-separator . "

") (gptel-response-prefix-alist (markdown-mode . "") (org-mode . "") (text-mode . "")) (gptel-prompt-prefix-alist (markdown-mode . "### ") (org-mode . "*** ") (text-mode . "### ")) (gptel--request-params) (gptel-track-media) (gptel-org-convert-response . t) (gptel-confirm-tool-calls) (gptel-use-curl . t) (gptel-cache) (gptel-include-reasoning . ignore) (gptel--num-messages-to-send) (gptel-max-tokens) (gptel-temperature) (gptel-use-tools . t) (gptel-stream) (gptel-model . runner-test) (gptel--preset . report-style)) :draft nil))
  :child-chats ()
  :chat-calls (
   (:id "call-832ada5e3d1e-2" :node draft :state waiting-feedback :discussion-owner nil :discussion-wait t :submitted-end 19 :rendered-end 35 :prompt "Draft a report" :result "Which dataset?" :error nil :started-at 1795532426.9465 :finished-at nil :attempt 1 :generation 1 :repair nil))
  :iterations ()
  :repeat-limits ()
  :budget (:max-requests nil :max-calls nil :max-duration nil :requests 1 :calls 1)
  :started-at 1795532426.946078
  :finished-at nil
  :terminal-data nil
  :extension-context nil
  :finalization nil
  :paused-at 1795532426.966125
  :duration-remaining nil
  :generation 1
  :options (:max-requests nil :max-calls nil :max-concurrency 1 :max-duration nil :allow-writes nil :allow-unconfirmed-tools nil :completion handoff :decision-memory t :context nil :persist t)))
