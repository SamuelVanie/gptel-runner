# Changelog

## 0.1.0 - unreleased

- Move the optional Atelier chat presentation and its tests into the separate
  `gptel-ui` project. Add that repository to `load-path` and enable
  `gptel-ui-global-mode`; compatibility loaders there preserve the previous
  `gptel-runner-chat-*` public interface. Jobs, pipelines, and their dashboards
  remain in `gptel-runner`.

- Agent nodes now hand results to the next agent in a sequence with
  `RunnerHandoff`. A recipient can use `RunnerAskPrevious` to get a reply
  from its predecessor's existing chat. The final agent submits its result
  with the same handoff tool. Select `:completion 'response` on a node to
  keep completion on an ordinary assistant response.

- Each agent node now uses an ordinary gptel conversation across turns.
  Open it with `? v` in the dashboard. Save and reopen a named run with
  `gptel-runner-save-session` and `gptel-runner-open-session`. Version 4
  checkpoints include the node chats, child transcripts, and pending feedback.
  Runner-owned chats stay under their run even with global gptel-agent session
  saving enabled. Open child transcripts with `? j` on a node or run.
  Background agents publish editable copies that link to the worker controls.
  Existing version 2 and 3 checkpoints and background text transcripts still open.
- Unfinished runs keep their node chats alive when you close a window or try
  to kill the buffer. Feedback follows interrupted tool output and reasoning,
  preserving unsent drafts. Press `RET` on a dashboard node with pending tools
  to review and approve the requests for that node.
- Rerun a finished workflow from its first node with the same goal, chats,
  and fresh budget accounting via `gptel-runner-rerun` or `? R` on the
  local or background run. Previous outputs remain in continuation history.

- Add a compact, collapsible dashboard tree and live background node panels
  with readable requests, direct controls, node navigation, and inline command
  status. Approve all displayed requests for one node in a single action, or
  approve individually. Offer revocable gptel-agent outside-file grants while
  retaining other confirmations, reviews, and stale-request checks.
- Add background dashboard cleanup keys matching the classic run menu: forget
  a run, forget workflow history, or clear finished runs. Preserve saved history
  by default, optionally delete recovery checkpoints, and protect active or
  uncertain workers. Forgotten history stays hidden until a new generation.
- Unify local and background output APIs: retain files or generated text with
  the same artifact descriptors, persist their registry across recovery, and
  resolve artifacts through public readers and agent context sources. Share
  named-value and node-outcome inspection across runs and invocation history;
  accept node IDs in workspace lookup and document the common pipeline API.
- Add explicit root-level run finalization with separate bounded budgets,
  selectable cancellation behavior, partial-result access, and durable phase
  resume. Retain body and finalizer outcomes separately and defer terminal
  callbacks until finalization completes; keep `try :finally` unchanged.
- Add public node traversal, lookup, paths, states, results, and error helpers.
  Preserve current outcomes for agent, Elisp, and container nodes in checkpoints,
  distinguish successful nil from unavailable values, and reuse accepted values
  when resuming containers without historical calls or `:save-as` entries.
- Add native asynchronous Elisp steps with completion/rejection callbacks,
  once-only cleanup on completion, cancellation, pause, and timeout, per-attempt
  timeouts, and protection against duplicate or late results.
- New background jobs preload gptel and capture external library paths for
  fresh workers. Include gptel-agent in `:preload-features` to discover its
  definitions before job init. Existing jobs keep their saved preloads.
- Add generic isolated background jobs, durable invocation/agent history,
  published file artifacts, command receipts, and remote lifecycle controls.
- Add start-time `:inputs` and workflow-level default workspaces.
- Register background jobs by stable ID from Emacs Lisp with
  `gptel-runner-register-job`. Install or remove recurring macOS LaunchAgent
  schedules with `gptel-runner-job-schedule` and `gptel-runner-job-unschedule`.
  Each invocation runs in a separate service. The jobs view supports inspection,
  manual runs, and diagnostics.
- Add headless feedback and exact-interaction tool approval, cross-process
  writer coordination, and a network-free parallel background example.
- Add per-agent-step `:workspace` overrides with run-workspace fallback,
  node-relative context files, and writer serialization per directory.
- Honor gptel's streaming setting instead of forcing non-streaming requests,
  render streamed text and reasoning correctly, and validate incomplete tool
  responses after the full payload arrives.
- Retry incomplete tool-call responses instead of accepting introductory text
  as a completed node, and save only the final model response for downstream
  steps while retaining intermediate commentary in transcripts and events.
- Add deterministic workflow AST and asynchronous scheduler.
- Add fake and gptel drivers, retries, budgets, cancellation, and events.
- Add structured review cycles, stall detection, and parallel joins.
- Add a session dashboard, examples, and ERT coverage.
- Add atomic persistent snapshots and cross-session workflow resume.
- Replace synchronous full-history snapshots with debounced, time-sliced v2
  execution checkpoints that keep Emacs responsive while saving.
- Add pausable agent calls that can be guided and completed from their normal
  gptel transcript buffers.
- Add `:pause-after t` agent-step approval points that hold successful output
  for human refinement before saving it or dispatching downstream nodes.
- Add repeat `:collect-keys` and `:save-history-as` options for preserving
  ordered per-iteration blackboard values for downstream summary nodes.
- Add default-on, run-scoped decision memory with provenance, automatic
  downstream prompt propagation, snapshot persistence, and an interactive
  command for recording decisions from worker buffers or the dashboard.
- Add `gptel-runner-extend-repeat` to continue iteration-budget failures while
  preserving saved history and using a run-local, durable repeat limit.
- Add `gptel-runner-extend` for increasing request, call, or duration budgets
  on running and paused runs without restarting work, as well as continuing
  persisted budget failures, and retain terminal failure details in snapshots.
- Add `gptel-runner-continue` and dashboard follow-ups for rerunning a finished
  pipeline with a human observation as its new goal, archived prior results,
  and optional budget reset or growth.
- Make continuation of unsuccessful runs retry the failed or unfinished safe
  checkpoint before dispatching later nodes, while successful runs still begin
  a fresh full workflow cycle.
- Add `gptel-runner-retry` and a dashboard command for retrying unsuccessful
  work from its safe checkpoint without changing the run goal.
- Add retry `:from-node` and `:feedback` options, plus a dashboard action to
  rerun a selected reviewer after fixing a blocker while keeping earlier work.
- Preserve completed repeat prefixes when retrying an agent call that blocked
  before its repeat body completed.
- Make retry reuse the previous budget or repeat increment when an extended
  run fails after consuming that newly added capacity.
- Fix confirmed tool calls so raw tool-result rendering returns to gptel's FSM
  and the runner leaves `waiting-confirmation`.
- Show active tool calls in the dashboard as `Calling TOOL-NAME...` using a
  distinct `waiting-tool` call state.
- Group dashboard rows by workflow and add safe run/workflow cleanup commands.
- Add configurable dashboard column visibility, a compact non-wrapping layout,
  state faces, row highlighting, and an interactive column toggle.
- Keep dashboard columns aligned by ellipsizing long workflow and node values
  while exposing their complete text as hover help.
- Refresh open dashboards automatically at a user-configurable interval and
  clean up their timers when the dashboard is closed.
- Preserve each dashboard window's cursor and scroll position across automatic
  and manual refreshes.
- Add a state-aware transient dashboard action menu, make `RET` open the
  natural inspection target for the selected row, and keep only the menu,
  refresh, and normal dashboard navigation keys permanently bound.
- Fix the built-in reviewer schema so nullable issue fields serialize as valid
  JSON instead of failing reviewer calls with `json-value-p`.
- Reject empty final agent responses, attempt one stateless repair, and fail
  the node before handing an empty value to downstream workflow steps.
- Add composable `:context` sources at workflow, run, registered-agent, and
  node scopes, including inherited subtree context, workspace-relative files,
  and functions evaluated for each agent call.
