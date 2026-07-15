# gptel-runner v0.1 plan

This is the durable implementation checklist for the approved v0.1 scope.  A
checked item means it is implemented in this repository and covered by the
normal verification commands where applicable.

## Bootstrap and release hygiene

- [x] Initialize a Git repository and add package/documentation scaffolding.
- [x] Declare Emacs 29.1 and gptel 0.9.9.4 as minimum versions.
- [x] Configure byte compilation, ERT, checkdoc, package-lint, and CI jobs for
  pinned gptel and current gptel master.
- [ ] Select a license and add final SPDX/copyright metadata.  This remains a
  blocking release task; the package must not be published before resolution.
- [ ] Add the final repository URL and submit a MELPA recipe.

## Runtime and drivers

- [x] Public structs, registries, IDs, terminal guards, blackboard, iterations,
  event journal, budgets, duration timer, and callback-once behavior.
- [x] Driver protocol and deterministic fake driver, including delayed/manual
  outcomes, transient errors, duplicate completions, and late callbacks.
- [x] Generation-based retry/cancellation protection and request backoff.
- [x] gptel compatibility adapter with private worker buffers, local presets,
  non-streaming stateless requests, event bridging, abort integration, and
  early API compatibility checks.
- [ ] Validate the adapter against a live provider (opt-in; never part of CI).

## Workflow interpreter

- [x] Preflight validation, agent steps, sequence, branch, bounded repeat, and
  parallel nodes.
- [x] Global concurrency, shared-workspace writer serialization, fail-fast,
  collect, and minimum-successes joins.
- [x] Parsers, validation, save keys, one output-repair call, semantic retries,
  iteration counts, and two-key stall detection.
- [x] Review schema/parser and progress-key helpers.

## User experience and hardening

- [x] Workflow-grouped session dashboard with inspect, visit, lifecycle,
  cleanup, configurable columns, and abort actions.
- [x] Versioned atomic snapshots, pause/resume across Emacs sessions, restored
  transcript buffers, in-buffer human feedback, and active-duration accounting.
- [x] Implement/review and fan-out/synthesis examples.
- [x] Automated tests for core workflow behavior, budgets, retries,
  cancellation, output repair, stalls, parallel joins, and writer safety.
- [x] Installation, API, safety, compatibility, and troubleshooting docs.
- [ ] Dedicated human gate nodes, worktrees, enforced tool wrappers,
  gptel-agent files, bounded dialogue, and streaming (post-v0.1).
- [ ] Add a start-time custom-input API, such as `:inputs', that initializes
  the run blackboard before the first node can dispatch.  Prompt functions
  will read these values with `gptel-runner-get`; arbitrary inputs will not
  create dynamic `gptel-runner-run-*' struct accessors.
- [ ] Reject unknown `gptel-runner-start' keywords unless they are explicitly
  supported, instead of silently accepting and discarding user data.

## Release gate

Before tagging v0.1: resolve the license, run `make release-check` on Emacs
29.1 and the current stable Emacs, validate gptel v0.9.9.4, inspect the allowed
to fail gptel-master job, and confirm the worktree remains clean.
