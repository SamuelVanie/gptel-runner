# Changelog

## 0.1.0 - unreleased

- Add deterministic workflow AST and asynchronous scheduler.
- Add fake and gptel drivers, retries, budgets, cancellation, and events.
- Add structured review cycles, stall detection, and parallel joins.
- Add a session dashboard, examples, and ERT coverage.
- Add atomic persistent snapshots and cross-session workflow resume.
- Add pausable agent calls that can be guided and completed from their normal
  gptel transcript buffers.
- Fix confirmed tool calls so raw tool-result rendering returns to gptel's FSM
  and the runner leaves `waiting-confirmation`.
- Group dashboard rows by workflow and add safe run/workflow cleanup commands.
- Add configurable dashboard column visibility, a compact non-wrapping layout,
  state faces, row highlighting, and an interactive column toggle.
