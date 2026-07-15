# gptel-runner

`gptel-runner` is a deterministic workflow engine for stateless Emacs AI
agent calls, with durable checkpoints and inspectable agent transcripts.  It
composes registered gptel presets into sequences,
branches, bounded review loops, and parallel fan-out/synthesis workflows while
keeping budgets, retries, cancellation, and terminal decisions in Emacs Lisp.

The package requires Emacs 29.1 or newer and, for live calls, gptel 0.9.9.4 or
newer.  Tests use the included fake driver and make no network requests.

> **Release status:** the implementation is pre-release.  A project license
> has not yet been selected, so this repository is not ready for publication or
> MELPA submission.

## Installation

Put the repository on `load-path`, install gptel, and require the package:

```elisp
(add-to-list 'load-path "/path/to/gptel-runner")
(require 'gptel-runner)
```

Opinionated structured-review helpers are separate and optional:

```elisp
(require 'gptel-runner-review)
```

Register agents after defining their gptel presets:

```elisp
(gptel-runner-register-agent
 'implementer :preset 'runner-implementer :workspace-mode 'write)
(gptel-runner-register-agent
 'reviewer :preset 'runner-reviewer :workspace-mode 'read
 :parser #'gptel-runner-parse-review :schema gptel-runner-review-schema)
```

See [examples/three-stage-handoff.el](examples/three-stage-handoff.el) for a
linear researcher-to-planner-to-implementer handoff.  The
[natural-language review loop](examples/natural-language-review-loop.el)
shows fresh implementer/reviewer calls communicating through a minimal file
without `gptel-runner-review`.  The
[dual-review loop](examples/dual-review-loop.el) demonstrates a writer followed
by two parallel independent reviewers, strict verdict parsing, automatic
one-shot output repair, bounded revisions, and stall detection.  The
[structured implementation/review](examples/implement-review.el) and
[parallel design](examples/parallel-design.el) examples demonstrate the
optional schema helpers and fan-out/synthesis workflows.

## Core API

Workflows are data, built with `gptel-runner-agent-step`,
`gptel-runner-sequence`, `gptel-runner-branch`,
`gptel-runner-repeat-until`, and `gptel-runner-parallel`.  Define one with
`gptel-runner-defworkflow`, then start it:

```elisp
(gptel-runner-start 'implement-review
 :goal "Add status-aware retries"
 :workspace (project-root (project-current t))
 :allow-writes t
 :callback (lambda (run) (message "Run: %s" (gptel-runner-run-state run))))
```

`gptel-runner-start` returns immediately.  Its callback runs exactly once.
Use `gptel-runner-get` and `gptel-runner-put` for structured run-local values,
`gptel-runner-iteration` for repeat counts, and
`gptel-runner-show-dashboard` to inspect live and completed runs.

The dashboard uses `g` to refresh, `RET` to inspect a run journal, `v` to
visit an agent transcript, `p` to pause a call for feedback, `x` to accept its
latest response, `P` to pause and snapshot a run, `r` to resume it, `s` to
save a snapshot, `l` to load one, `c` to abort a call, and `a` to abort a run.
Workflow headers contain their run summary rows, and each run contains its
agent-call rows; registered workflows with no runs remain visible.

Use `d` on a run or call row to forget that run, `D` on any row in a workflow
group to unregister the workflow and forget its retained runs, and `C` to
clear all terminal runs.  Cleanup kills retained worker and event buffers but
preserves durable snapshots by default.  Use `C-u d`, `C-u D`, or `C-u C` to
delete the corresponding snapshots as well.  Active runs are protected: pause
or abort them before cleanup.  Paused runs may be forgotten because their
snapshot remains recoverable unless snapshot deletion was explicitly chosen.
Programmatically use `gptel-runner-abort-call` and
`gptel-runner-abort-run`; cleanup is available through
`gptel-runner-forget-run`, `gptel-runner-forget-workflow`, and
`gptel-runner-unregister-workflow`.  Every appended `gptel-runner-event` is
also passed to `gptel-runner-event-hook`.

Each gptel call has a visible buffer named
`*gptel-runner:RUN:NODE:CALL*`.  It displays the exact prompt, reasoning when
enabled by gptel, proposed tool calls and confirmation controls, tool results,
and the final response.  These buffers are retained by default for human
inspection but are not reused as agent memory.  Set
`gptel-runner-retain-worker-buffers` to nil to remove them automatically when
calls finish.  Events and runtime state remain authoritative; editing a
transcript does not alter the run.

### Human feedback in an agent buffer

Use `M-x gptel-runner-pause-call` in an active worker buffer, or press `p` on
its dashboard row.  The provider request is stopped without failing the
workflow node and the buffer becomes an ordinary gptel conversation.  Add
your correction, use the normal gptel commands to obtain a new response, then
run `M-x gptel-runner-complete-call-from-buffer`.  An active region is used
when present; otherwise the latest gptel response is returned to the workflow.
The workflow then continues from that node's original continuation.

This is deliberately different from `gptel-runner-abort-call`: aborting is a
terminal cancellation, while pausing enters the `waiting-feedback` state.

### Durable snapshots and overnight resume

Persistent runs must use a named workflow and stable explicit node IDs.  Add
`:persist t` to the workflow defaults or start call:

```elisp
(gptel-runner-defworkflow durable-handoff (:persist t)
  (gptel-runner-sequence
   :id 'durable-handoff-sequence
   (gptel-runner-agent-step
    :id 'investigate :agent 'researcher :prompt #'research-prompt
    :save-as 'findings)
   (gptel-runner-agent-step
    :id 'implement :agent 'implementer :prompt #'implement-prompt
    :save-as 'report)))

(gptel-runner-start 'durable-handoff
 :goal "Implement the requested change"
 :workspace (project-root (project-current t))
 :allow-writes t)
```

Safe checkpoints are written atomically beneath
`gptel-runner-snapshot-directory` (by default
`~/.emacs.d/gptel-runner/snapshots/`).  Pause the entire process with
`M-x gptel-runner-pause-run` from a worker or `P` in the dashboard.  The
active-duration clock stops while paused.

After restarting Emacs, load the same workflow and agent definitions, then:

```elisp
(setq my-run
      (gptel-runner-load-run
       "/path/to/run-12.snapshot.el"
       (lambda (run)
         (message "Restored run: %s" (gptel-runner-run-state run)))))

(gptel-runner-resume-run my-run "Please prefer the smaller public API")
```

Alternatively, use `l` and `r` in the dashboard.  If the snapshot contains a
call paused for feedback, visit its restored transcript with `v`, continue it
as a normal gptel conversation, and accept it with
`gptel-runner-complete-call-from-buffer`; the reconstructed workflow resumes
after that node.

Snapshots preserve the goal, canonical workspace, blackboard, completed node
states, repeat iterations, budgets, event journal, calls, and retained
transcripts.  Completion callbacks and live provider processes are not
serializable: supply a new callback to `gptel-runner-load-run`, and expect an
unfinished call to restart statelessly unless you complete its restored
feedback buffer.  Calls consumed before pausing remain charged to call and
request budgets.  Snapshot files have mode `0600`, but they contain prompts,
outputs, tool results, and possibly secrets; protect and delete them as you
would other sensitive local state.

Run options include `:driver`, `:max-requests`, `:max-calls`,
`:max-concurrency`, `:max-duration`, `:allow-writes`, and
`:allow-unconfirmed-tools`, and `:persist`.  Workflow defaults are used when
an option is not provided.

## Semantics and safety

- Calls are stateless.  Prompts must explicitly include the goal, workspace,
  iteration, selected blackboard inputs, and prior findings.  Agents should
  reinspect files instead of trusting earlier reports.
- A writable workflow is rejected unless the start call includes
  `:allow-writes t`.  Writers sharing a canonical workspace are serialized.
- `workspace-mode` is orchestration metadata, **not a security sandbox**.  Tool
  lists and gptel confirmation policy remain the real capability boundary.
  Parallel writable isolation and enforced read-only access require future
  worktree/tool-wrapper support.
- Tool confirmation remains enabled unless callers also explicitly request
  `:allow-unconfirmed-tools t`.  Side-effecting tools should be idempotent:
  transport retries have at-least-once semantics, and arbitrary asynchronous
  tool effects cannot be rolled back generically.
- Request retries reuse one logical call and consume request attempts.  An
  agent-step retry creates a new call.  An implementation revision is a new
  workflow iteration.  These are intentionally separate counters.
- Events and runtime state are authoritative.  Snapshots are versioned
  projections written at safe checkpoints; they do not serialize Lisp
  continuations, provider connections, timers, or external tool processes.

## Extension points

Custom agents combine any gptel preset with a runner parser, validator,
schema, retry policy, and metadata.  Custom drivers implement
`gptel-runner-driver-start`, `gptel-runner-driver-cancel`, and optionally
`gptel-runner-driver-pause` (which defaults to stopping external work without
terminalizing the runner call).  Prompt,
branch, repeat-stop, and progress-key functions receive the live run and can
read explicit blackboard values.  Parsers and validators remain runner-owned,
so provider schema support is a reliability hint rather than an approval
authority.

## Compatibility

All private gptel references live in `gptel-runner-gptel.el`.  The adapter
checks required entry points before dispatch and reports a `compatibility`
failure with actionable details.  gptel's private preset and request machinery
can change; CI checks v0.9.9.4 and uses current master as an early-warning job.

| Symptom | Action |
| --- | --- |
| `compatibility` before a call | Install supported gptel or inspect adapter/API changes. |
| Writable workflow rejected | Pass `:allow-writes t` after reviewing its tools. |
| Run is `stalled` | Inspect repeated review issues/diff and adjust the workflow or agent prompt. |
| `invalid-output` | The original and one repair response both failed validation. |
| Cancellation cannot undo a tool | Stop/undo the external tool action separately. |
| Snapshot cannot find a node | Reload the same named workflow with stable explicit node IDs. |
| Snapshot cannot be saved | Keep blackboard keys/values and the run goal readable as Emacs Lisp data. |

## Development

```sh
make test
make compile
make checkdoc
make package-lint
make release-check
```

Live provider smoke tests are opt-in and excluded from ordinary CI.
