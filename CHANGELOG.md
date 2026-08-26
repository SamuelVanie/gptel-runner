# Changelog

## 0.1.0 - unreleased

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
- Add `gptel-runner-extend` for continuing persisted request, call, or duration
  budget failures, and retain terminal failure details in snapshots.
- Add `gptel-runner-continue` and dashboard follow-ups for rerunning a finished
  pipeline with a human observation as its new goal, archived prior results,
  and optional budget reset or growth.
- Make continuation of unsuccessful runs retry the failed or unfinished safe
  checkpoint before dispatching later nodes, while successful runs still begin
  a fresh full workflow cycle.
- Add `gptel-runner-retry` and a dashboard command for retrying unsuccessful
  work from its safe checkpoint without changing the run goal.
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
- Fix the built-in reviewer schema so nullable issue fields serialize as valid
  JSON instead of failing reviewer calls with `json-value-p`.
- Reject empty final agent responses, attempt one stateless repair, and fail
  the node before handing an empty value to downstream workflow steps.
