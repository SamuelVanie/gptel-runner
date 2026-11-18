# gptel-runner

`gptel-runner` is a deterministic workflow engine for stateless Emacs AI
agent calls and local Elisp steps, with durable checkpoints and inspectable
results and agent transcripts.  It
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

Workflows are data, built with `gptel-runner-agent-step`, `gptel-runner-elisp-step`,
`gptel-runner-sequence`, `gptel-runner-branch`,
`gptel-runner-repeat-until`, `gptel-runner-parallel`, and `gptel-runner-try`.
Define one with
`gptel-runner-defworkflow`, then start it:

```elisp
(gptel-runner-start 'implement-review
 :goal "Add status-aware retries"
 :workspace (project-root (project-current t))
 :allow-writes t
 :callback (lambda (run) (message "Run: %s" (gptel-runner-run-state run))))
```

Agent calls execute asynchronously; Elisp steps can run synchronously or use
the native async contract below.  The run's callback runs exactly once.
Use `gptel-runner-get` and `gptel-runner-put` for structured run-local values,
`gptel-runner-iteration` for repeat counts, and
`gptel-runner-show-dashboard` to inspect live and completed runs.

### Finding nodes and inspecting outcomes

Inspect a run without searching its call history or requiring `:save-as`:

```elisp
(gptel-runner-find-node run 'review)       ; node object, or nil
(gptel-runner-list-nodes run)              ; root, then descendants
(gptel-runner-node-path run 'review)       ; root-to-review node objects
(gptel-runner-node-state run 'review)      ; pending, running, succeeded, ...
(gptel-runner-node-outcome run 'review)    ; state and available value
(gptel-runner-node-result run 'review 'unavailable)
(gptel-runner-node-error run 'review 'no-error)

;; A report for every node, including containers and unused branches.
(gptel-runner-map-nodes
 (lambda (node) (gptel-runner-node-outcome run node))
 run)

;; Select just the failed nodes, in workflow definition order.
(gptel-runner-list-nodes
 run (lambda (node) (eq (gptel-runner-node-state run node) 'failed)))
```

Traversal helpers accept a run, workflow object, registered workflow name,
or node used as a subtree root.  They visit each node before its children,
in definition order, including unused branch arms and try handlers.  Repeat
bodies appear once.  An optional predicate on `gptel-runner-list-nodes`
filters nodes without pruning their descendants.

State, outcome, result, and error helpers take a run object and either a node
object or its ID.  Unknown IDs signal an error; `find-node` and `node-path`
return nil when absent.  Nodes and outcome values are shared with the run;
treat them as read-only.

Outcomes use the same shape for agent, Elisp, and container nodes:

```elisp
(:node check :state pending)                  ; not reached yet
(:node check :state running)                  ; no accepted result yet
(:node check :state succeeded :value nil)      ; successfully returned nil
(:node check :state failed :value (:type ...)) ; failed with these details
(:node check :state skipped :value branch-not-selected)
```

The `:value` key is present for every known terminal outcome, even when its
value is nil.  `gptel-runner-node-result` returns only successful values and
uses its optional default for failures or unavailable results.  Thus a
non-nil default distinguishes successful nil from missing output.
`gptel-runner-node-error` returns data for failed, blocked, stalled, or
cancelled nodes, with its optional default for other states or missing data.
Node state describes workflow progress; individual call states such as
`retry-wait` and `waiting-feedback` remain available on calls.

Agent outcomes contain parsed, validated outputs; existing empty-response
and parser validation rules still apply.  Elisp nodes accept nil.  Sequence
success yields nil; branches propagate the selected child's value; repeats
propagate the finishing body's value (or nil if already satisfied); parallel
success yields its collected child outcomes; try nodes preserve the body or
handler outcome unless cleanup fails.

Outcomes survive checkpoint recovery even when historical calls are absent.
They describe the current execution: repeat iterations, retries, and new
continuation cycles clear outcomes only for nodes being reset.  Use repeat
history options when you need earlier iterations.  Older v2 snapshots still
load, but completed nodes without stored outcomes omit `:value`; they are
not reported as successful nil.  Check `plist-member` when reading that
distinction directly.

### Mixing Elisp and agent steps

Use `gptel-runner-elisp-step` for local operations such as reading files,
transforming agent output, checking results, or publishing artifacts.  Its
`:function` receives `(run node)` and returns the node's output.  `:save-as`
makes that value available to later steps through `gptel-runner-get`:

```elisp
(gptel-runner-defworkflow mixed-pipeline (:max-calls 1)
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-elisp-step
    :id 'prepare :save-as 'source
    :function (lambda (_run _node)
                (with-temp-buffer
                  (insert-file-contents "README.md")
                  (buffer-string))))
   (gptel-runner-agent-step
    :id 'summarize :agent 'summarizer ; Register this agent first.
    :prompt (lambda (run _node)
              (concat "Summarize this project:\n"
                      (gptel-runner-get run 'source)))
    :save-as 'summary)
   (gptel-runner-elisp-step
    :id 'format :save-as 'report
    :function (lambda (run _node)
                (list :summary (gptel-runner-get run 'summary)
                      :characters (length (gptel-runner-get run 'summary)))))))

(setq my-run (gptel-runner-start 'mixed-pipeline :workspace "/path/to/project/"))
;; Once the run succeeds:
(gptel-runner-get my-run 'report)
```

The [mixed Elisp example](examples/mixed-elisp.el) includes a fake-driver
invocation you can try without a model connection.  Entirely local workflows
can load just `gptel-runner-flow` and need no driver or registered agents.

Elisp steps work inside sequences, branches, parallel groups, and repeat
bodies, including repeat history collection.  Any return value is valid,
including `nil`, empty strings, lists, and hash tables.  Outputs and errors
appear in the event journal and background history; the dashboard labels
these calls `Elisp`.  Opening a local Elisp row shows the event journal.

Each function runs in a temporary buffer with `default-directory` set to its
effective `:workspace`.  Set `:workspace-mode 'write` for functions that modify
the workspace and start the run with `:allow-writes t`.  The default mode is
`read`; these declarations are scheduling metadata, not access restrictions.
Elisp steps share concurrency limits and writer locks with agent calls,
including background worker locks, and consume neither `:max-calls` nor
`:max-requests`.  They execute trusted Lisp directly, without gptel tool
confirmation.

By default, functions run synchronously in the owning Emacs process, even
inside a parallel group.  Keep them short; long computations block that
process, and timers cannot preempt Lisp that does not yield.  The runner
checks elapsed duration again before accepting a function's return value.
Elisp steps have no agent conversation; use `gptel-runner-pause-run` to pause
their workflow.

An error fails the node with `:type 'elisp-error`; quitting cancels it.
`:retries` defaults to zero and opts into additional attempts after errors.
Agent-only options such as `:prompt`, `:parser`, `:validator`, `:context`, and
`:pause-after` are rejected on Elisp steps; inherited context still applies
to agent steps.  Put transformations and validation in the function itself.

Checkpoint recovery retains completed Elisp nodes and their outcomes.
Unfinished nodes restart, and `gptel-runner-retry :from-node` accepts Elisp
step IDs.  Use stable explicit IDs and reload the workflow's function
definitions before restoring a snapshot.  Outcome values must be readable Lisp
data.  Functions with side effects should tolerate repeated execution after
retries or recovery; pausing or cancellation cannot undo effects already made.

### Asynchronous Elisp steps

Set `:async t` to start asynchronous work.  The function receives
`(run node resolve reject)` and returns promptly.  Call `(funcall resolve value)`
to succeed, including with `nil`, or `(funcall reject error-data)` to fail with
`(:type elisp-error :error ERROR-DATA)`.  The first callback wins; duplicates
and callbacks from cancelled, paused, timed-out, or superseded attempts are
ignored.  Inline callbacks are supported, with completion deferred until
startup returns.  Errors or quits before completion fail or cancel the step.

The return value must be `nil` or a cleanup function accepting one reason:
`succeeded`, `failed`, `cancelled`, `paused`, or `timeout`.  Once startup has
returned, cleanup runs once before releasing concurrency and writer locks or
starting a retry.  Use it to stop processes, cancel timers, and release buffers.
Cleanup may trigger process sentinels safely; their old callbacks are already invalid.
Cleanup errors and quits produce an `elisp-cleanup-error` event and do not
replace the step's outcome.  Startup must release any resources it acquired
if it signals before returning its cleanup function.

```elisp
;; This snippet needs lexical-binding, as do closures used by process sentinels.
(gptel-runner-elisp-step
 :id 'wait :async t :timeout 5 :save-as 'result
 :function
 (lambda (_run _node resolve _reject)
   (let ((timer (run-at-time 0.1 nil (lambda () (funcall resolve 'ready)))))
     (lambda (_reason) (cancel-timer timer)))))
```

Optional `:timeout` is a positive finite number of seconds from admission,
excluding time spent queued.  Expiry fails the attempt with
`(:type elisp-timeout :timeout SECONDS)` and runs cleanup with `timeout`.
`:retries` also applies to rejections and timeouts; every new attempt gets a
fresh timeout.  `:timeout` works for synchronous Elisp steps too, subject to
Emacs yielding.  A run's `:max-duration` still bounds the whole workflow.

Startup and cleanup run in temporary buffers in the step's workspace.
Asynchronous handlers should capture their own buffers and other resources;
the startup buffer is gone when the function returns.  An unfinished async
step keeps its concurrency slot and writer lock until completion or cleanup.
Pausing cleans up the attempt; resume or snapshot recovery restarts it with
fresh callbacks and timeout.  Runtime callbacks, timers, and cleanup functions
are never serialized.  If pause or cancellation occurs while startup yields,
the returned cleanup runs as soon as startup returns.

The [async Elisp example](examples/async-elisp.el) runs `git status` in a
subprocess, resolves or rejects from its sentinel, and cleans up on every exit.

### Failure recovery and cleanup

Use `gptel-runner-try` to recover from a failed operation and continue the
workflow.  Its `:body`, `:catch`, and `:finally` are ordinary workflow nodes,
including sequences, parallel groups, agent calls, and Elisp steps:

```elisp
(gptel-runner-try
 :id 'fetch-report
 :error-as 'report-failure
 :save-as 'report
 :body (gptel-runner-agent-step
        :id 'fetch :agent 'researcher :prompt "Prepare the report.")
 :catch (gptel-runner-elisp-step
         :id 'fallback
         :function (lambda (run _node)
                     (let ((failure (gptel-runner-get run 'report-failure)))
                       (list :status 'unavailable
                             :reason (plist-get failure :value)))))
 :finally (gptel-runner-elisp-step
           :id 'finish-report
           :function (lambda (run _node)
                       (gptel-runner-put run 'report-attempt-finished t))))
```

The body is required, along with at least one of `:catch` and `:finally`.
By default, the handler catches `failed` outcomes after the body's configured
retries and output repairs have finished.  Use
`:catch-states '(failed blocked stalled)` to also recover from blocked or
stalled work, or a subset to handle only those outcomes.  Errors in branch
and repeat predicates become `failed` outcomes with `:type 'flow-error`.

`:error-as` receives `(:node BODY-ID :state STATE :value VALUE)` before the
handler or cleanup starts, or nil on body success.  The value is the original
failure data; the ID identifies the body subtree.  The error key is cleared
when a new body attempt starts.  It must differ from `:save-as`.

A successful handler makes the try succeed and allows the enclosing sequence
to continue.  `:save-as` receives the successful body or handler value,
including nil.  Without a matching handler, the body's failure propagates.
Cleanup runs after the body and any handler, even if the handler fails.
Successful cleanup preserves their outcome; failed cleanup becomes the try's
failure.  A handler or cleanup failure can be caught by an enclosing try.
The original child failures remain visible in node states and the event log.

Recovery and cleanup obey ordinary concurrency, workspace permissions, and
budgets.  An Elisp handler can still finish local work after an agent call or
request budget is exhausted.  Cancellation (including a cancelled parallel
sibling) and run duration expiry stop immediately without starting recovery
or cleanup; `:finally` is workflow cleanup, not an Emacs `unwind-protect`.

Pausing or restoring a snapshot resumes the current phase without rerunning
an already failed body while handling it.  Retrying a failed try resumes its
failed phase; `gptel-runner-retry :from-node` can explicitly revisit an earlier
step.  A new repeat iteration or `gptel-runner-continue` starts a fresh try.
Use stable node IDs and serializable outputs for durable checkpoints.

### Per-node workspaces

Set `:workspace` on an agent or Elisp step to run it in a different directory.
An omitted or nil value inherits the workspace passed to `gptel-runner-start`.
Relative paths are resolved from that run workspace; absolute paths are used
directly.  Overrides must name existing directories and are validated before
any calls start.

For example, with a registered `researcher` agent, three nodes can research
three repositories concurrently:

```elisp
(defun example/research-prompt (run node)
  (format "Research this goal: %s\nWorkspace: %s\nReturn your findings."
          (gptel-runner-run-goal run)
          (gptel-runner-node-workspace run node)))

(gptel-runner-defworkflow multi-workspace-research (:max-concurrency 3)
  (gptel-runner-parallel
   (gptel-runner-agent-step
    :id 'research-a :agent 'researcher :workspace "/projects/repo-a"
    :prompt #'example/research-prompt :save-as 'findings-a)
   (gptel-runner-agent-step
    :id 'research-b :agent 'researcher :workspace "/projects/repo-b"
    :prompt #'example/research-prompt :save-as 'findings-b)
   (gptel-runner-agent-step
    :id 'research-c :agent 'researcher
    :prompt #'example/research-prompt :save-as 'findings-c)))

(gptel-runner-start 'multi-workspace-research
 :goal "Compare the authentication implementations"
 :workspace "/projects/repo-c")
```

Use `gptel-runner-node-workspace` in prompt and context functions to get the
selected node directory.  `gptel-runner-run-workspace` continues to return the
run's default.  Each worker buffer uses its node directory as
`default-directory`.  Workspace overrides belong to agent and Elisp steps;
sequence, parallel, branch, and repeat nodes do not set a subtree workspace.

Within a run, writers targeting the same canonical directory (including
symlink aliases) are serialized.  Writers in different directories can run
concurrently up to `:max-concurrency`.

### Starting context and artifacts

Add `:context` at any workflow level to prepend stable project knowledge to
the stateless prompt of each affected agent call.  A context value is one
source or a list of sources.  Each source can be an inline string, a function
of `(run node)` that returns a string (or nil to omit itself), or a file
descriptor produced by `gptel-runner-context-file`.  `node` is the target
agent step even when the function belongs to a broader scope:

```elisp
(defun example/current-plan-context (run _node)
  (when-let ((plan (gptel-runner-get run 'plan)))
    (format "The accepted plan is:\n%s" plan)))

(gptel-runner-register-agent
 'implementer :preset 'runner-implementer :workspace-mode 'write
 :context (list "Follow the repository conventions exactly."
                (gptel-runner-context-file "docs/architecture.md")))

(gptel-runner-defworkflow context-aware
    ;; Workflow options are quoted by the macro, so write file descriptors
    ;; as data here.  This context reaches every agent in the workflow.
    (:context ((:file "AGENTS.md")))
  (gptel-runner-sequence
   :id 'delivery
   :context "Keep all changes within the requested feature."
   (gptel-runner-agent-step
    :id 'plan :agent 'planner :prompt #'example/plan-prompt
    :save-as 'plan)
   (gptel-runner-repeat-until
    :id 'implementation-cycle :max 3
    :context #'example/current-plan-context
    :until #'example/review-passed-p
    :body
    (gptel-runner-agent-step
     :id 'implement :agent 'implementer
     :context "Run the focused tests before returning."
     :prompt #'example/implementation-prompt))))
```

Context is additive and appears from broadest to narrowest: workflow, optional
`gptel-runner-start :context`, registered agent, then each node from the root
through the agent step.  The step's ordinary `:prompt` follows all starting
context.  Context on a sequence, repeat, parallel, or branch is inherited only
by agent steps in that subtree.

Relative file names at every context scope are resolved from the receiving
agent node's effective workspace.  Use absolute paths for files shared across
different workspaces.  Files and context
functions are evaluated when each call is prepared, so a later sequence step
or loop iteration sees updated artifact contents.  Missing or unreadable files
and non-string function results are reported as context errors.  Use
`gptel-runner-start :context` for context specific to one run:

```elisp
(gptel-runner-start 'context-aware
 :goal "Implement the requested change"
 :workspace (project-root (project-current t))
 :context "The migration must remain backward compatible."
 :allow-writes t)
```

Every workflow has decision memory enabled by default.  Record a workflow-wide,
run-scoped choice from Lisp with:

```elisp
(gptel-runner-record-decision
 run
 "Use SQLite instead of JSON for concurrent state updates"
 "The implementation requires atomic writes")
```

`gptel-runner-record-decision` appends an entry with its time and, when called
from a runner worker, its source call and node.  Its first argument may also be
the displayed run ID, such as `"run-17"`.  `gptel-runner-decisions` returns the
ordered log.  Before each later agent call, the runner adds the current
decisions to its prompt as workflow constraints.  Set
`:decision-memory nil` in the workflow options or `gptel-runner-start` call to
disable this automatic prompt propagation.

Use `M-x gptel-runner-add-decision` to record a decision manually.  In a
worker buffer it uses the current call as provenance; in the dashboard it uses
the run or call row at point; elsewhere it prompts for a session-local run.
It is also available as `d` inside the dashboard action menu.

Add `:pause-after t` to an agent step when its successful response must be
reviewed before the node completes and the next pipeline node starts:

```elisp
(gptel-runner-agent-step
 :id 'plan
 :agent 'planner
 :prompt #'plan-prompt
 :save-as 'implementation-plan
 :pause-after t)
```

The dashboard refreshes automatically every two seconds by default.  Set the
interval in seconds, or use `nil` to disable automatic refresh:

```elisp
(setq gptel-runner-dashboard-refresh-interval 5.0)
```

Changes take effect on the next refresh; press `g` to apply one immediately.
The dashboard keeps its permanent keymap small:

| Key | Dashboard action |
| --- | --- |
| `RET` | Visit a call transcript, or inspect a run journal |
| `?` | Open the dashboard action menu |
| `g` | Refresh the dashboard |
| `q` | Quit the dashboard |

The action menu groups inspection, call, run, persistence, and management
commands in a transient popup.  Each action has a visible mnemonic suffix, and
actions that do not apply to the selected row or its current state are greyed
out.  For example, `a` accepts a call only while it awaits human feedback,
whereas `r` resumes an entire paused run.  Snapshot loading, column changes,
and cleanup remain available in the menu without occupying permanent keys;
all actions can also be invoked by command name through `M-x`.
Workflow headers contain their run summary rows, and each run contains its
agent and Elisp call rows; registered workflows with no runs remain visible.  The
automatic refresh timer is stopped when the dashboard buffer is closed or
changes to another major mode.

While a tool call is in progress, its agent-call row enters the
`waiting-tool` state and displays `Calling TOOL-NAME...`.  This makes tools
that wait for user input, such as `AskUserQuestion`, visible without opening
the agent transcript.  Calls that still need approval continue to display the
separate `waiting-confirmation` state until they are accepted.

Dashboard columns are a configurable set.  Their order in the value is
ignored so the table keeps a stable canonical layout:

```elisp
(setq gptel-runner-dashboard-columns
      '(workflow run node job agent state elapsed))
```

Available symbols are `workflow`, `run`, `node`, `call`, `job`, `agent`, `state`, `elapsed`,
`attempts`, `iteration`, `requests`, and `calls`.  Press `g` in an open
dashboard after changing the variable, or choose `V` from the `?` action menu
to toggle one column interactively.  The same action is available as
`M-x gptel-runner-dashboard-toggle-column`.
The default includes job and agent names.  Rows stay on one line instead of
wrapping, and the current row is highlighted.  Values longer than their column
are shortened with an ellipsis so later columns stay aligned; hovering the
shortened value shows its complete text.  The spelling
`gptel-runner-dashboards-column` is accepted as an alias, although
`gptel-runner-dashboard-columns` is the preferred option name.

The action menu uses `x` to forget the selected run, `X` to unregister the
selected workflow and forget its retained runs, and `C` to clear all terminal
runs.  These commands are also available through `M-x` with the
`gptel-runner-dashboard-` prefix.  With a prefix argument, they delete the
corresponding snapshots as well.  Cleanup kills retained worker and event
buffers but preserves durable snapshots by default.  Active runs are
protected: pause or abort them before cleanup.  Paused runs may be forgotten
because their snapshot remains recoverable unless snapshot deletion was
explicitly chosen.  Programmatically use `gptel-runner-abort-call` and
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

Use `M-x gptel-runner-pause-call` in an active worker buffer, or select its
dashboard row and choose `p` from the `?` action menu.  The provider request is
stopped without failing the workflow node and the buffer becomes an ordinary
gptel conversation.  Add your correction, use the normal gptel commands to
obtain a new response, then run
`M-x gptel-runner-complete-call-from-buffer`.  An active region is used when
present; otherwise the latest gptel response is returned to the workflow.  The
workflow then continues from that node's original continuation.

This is deliberately different from `gptel-runner-abort-call`: aborting is a
terminal cancellation, while pausing enters the `waiting-feedback` state.

For a planned approval point, put `:pause-after t` on the agent step instead
of racing to pause it manually.  After the provider returns a non-empty
response, the call automatically enters `waiting-feedback`; the response has
not yet completed the node, been stored under `:save-as`, or reached the next
node.  Choose `v` from the dashboard action menu to visit the transcript, give
the agent feedback with ordinary gptel commands, then choose `a` from the menu
or run
`M-x gptel-runner-complete-call-from-buffer`.  The active region, or otherwise
the latest gptel response, becomes the actual node result and releases the
original workflow continuation.  Parsers and validators still apply to that
accepted result.

The call-level gate keeps the in-memory continuation alive.  For a named
workflow, you can additionally choose `P` from the dashboard action menu to
pause and snapshot the whole run; completing the retained call in the same
Emacs session resumes the run.  A
snapshot loaded in a later session has no retained transcript or continuation,
so the unfinished gated node restarts statelessly on resume.

### Collecting loop history for a final summary

Agent `:save-as` values normally describe the latest iteration because each
iteration writes the same blackboard keys.  A repeat node can retain ordered
snapshots of selected keys with `:collect-keys` and `:save-history-as`:

```elisp
(gptel-runner-sequence
 :id 'review-and-summarize

 (gptel-runner-repeat-until
  :id 'review-cycle
  :max 5
  :until #'review-passed-p
  :collect-keys '(implementation-report review-result)
  :save-history-as 'review-history
  :body
  (gptel-runner-sequence
   (gptel-runner-agent-step
    :id 'implement
    :agent 'implementer
    :prompt #'implementation-prompt
    :save-as 'implementation-report)
   (gptel-runner-agent-step
    :id 'review
    :agent 'reviewer
    :prompt #'review-prompt
    :save-as 'review-result)))

 (gptel-runner-agent-step
  :id 'summarize
  :agent 'summarizer
  :prompt
  (lambda (run _node)
    (format
     (concat "Create the final report from this ordered revision history. "
             "Write the requested document and return its path.\n\n%S")
     (gptel-runner-get run 'review-history)))
  :save-as 'final-report))
```

Both repeat options are required together.  Every collected key must be a
declared blackboard destination written somewhere in the repeat body, while
the history destination must not be written by the body.  After every
successful body iteration, before `:until` or `:stop-when` is evaluated, the
runner appends an entry to `review-history`:

```elisp
((:iteration 1
  :values ((implementation-report . "first attempt")
           (review-result . "needs revision")))
 (:iteration 2
  :values ((implementation-report . "revised attempt")
           (review-result . "approved"))))
```

Entries and collected keys retain declaration order, including the final
successful iteration.  A key is omitted when its producing node was skipped
in that iteration, even if an older value for the key remains on the
blackboard.  Collected structured values are copied when captured so later
iterations cannot mutate earlier list or vector data.  Because the history is
an ordinary blackboard value, durable snapshots preserve it and downstream
prompt functions read it with `gptel-runner-get`.

Finite request, call, and active-duration limits can be increased while a run
is still running or paused:

```elisp
(gptel-runner-extend my-run
 :additional-calls 6
 :additional-requests 8
 :additional-duration 300)
```

The new limits take effect immediately without restarting workflow nodes or
in-flight calls.  Extending active duration replaces the existing deadline
timer while retaining the time already consumed.  Unlimited budgets remain
unlimited.  From the dashboard, select a run or call row and choose `b` from
the action menu to enter additions for its finite limits.

The same function recovers a run that already failed because `:max-requests`,
`:max-calls`, or `:max-duration` was exhausted.  It increases the exhausted
finite budget and continues from the safe checkpoint:

```elisp
(gptel-runner-extend "run-5"
 :additional-calls 6
 :additional-requests 8)
```

Recovery preserves completed nodes and blackboard values.  Each exhausted
budget must be increased; the runner reports which keyword is missing rather
than restarting only to fail at the same limit.

### Retry unsuccessful work with the same goal

When a run failed for a reason other than an exhausted run or repeat budget,
retry its safe checkpoint without changing the goal:

```elisp
(gptel-runner-retry "run-17")
```

Previously succeeded nodes and their blackboard results remain complete,
except for completed blocked or stalled repeat bodies, which restart as a
whole.  The failed, blocked, stalled, or cancelled node is retried, and
following sequence nodes remain gated until it succeeds.  The function accepts either a run
object or the string ID displayed in the dashboard.  An optional `:callback`
applies to the retry's next terminal transition.  Select a dashboard run or
call row and choose `t` from its action menu for the same operation.

For a reviewer that returned a valid `BLOCKED` verdict, the call itself may
show `succeeded`: the enclosing repeat's `:stop-when` predicate blocked the
run.  Plain retry restarts that completed repeat body, including its
implementer.  To fix the environment and rerun only the reviewer before
continuing the pipeline:

1. Fix the problem in the workspace, such as installing the missing test
   dependency or starting a required service.
2. Open `M-x gptel-runner-show-dashboard` and select the reviewer's call row.
3. Press `?`, then `T` (Retry from this node).  Optionally describe what you
   fixed; the reviewer receives that note in its new prompt.

The same operation is available from Lisp:

```elisp
(gptel-runner-retry "run-17"
 :from-node 'review
 :feedback "Installed the missing test dependency. Please rerun the tests.")
```

Use the workflow step's `:id` for `:from-node`.  This creates a fresh agent
call against the current workspace with the original goal.  Earlier completed
steps and independent successful parallel siblings are kept.  The selected
node's saved result and later sequence results are cleared and recomputed;
downstream steps wait until their prerequisites succeed.  Selecting a
historical call retries its node in the current workflow state, not the
historical iteration.  Repeat histories remain available, and completing the
retried body consumes another iteration.  Existing call, request, duration,
and repeat limits still apply.

`:feedback` also works without `:from-node`, in which case it is included in
the next dispatched agent prompt.  After restarting Emacs, load a saved run
with `gptel-runner-load-run`, then use `gptel-runner-retry` with its run object
and node ID; restored snapshots do not contain historical call rows.  Durable
recovery requires enabling `:persist t` before leaving the original session.

`gptel-runner-retry` deliberately rejects exhausted request, call, duration,
and repeat limits because retrying without more capacity would immediately
fail again.  Use `gptel-runner-extend` for run-level budgets or
`gptel-runner-extend-repeat` for a repeat limit.  If work started by one of
those extension commands fails after consuming its added capacity, an explicit
retry reuses the previous increment instead of asking for the same extension
again.  The reapplied increment is recorded on the `run-retried` event.

### Follow up after a finished pipeline

After inspecting the workspace produced by a finished run, continue the same
pipeline with a human observation as its new goal:

```elisp
(gptel-runner-continue
 my-run
 "The command works, but its error output needs to name the invalid option."
 :additional-calls 4
 :additional-requests 4)
```

For a succeeded run, the complete workflow runs again against the same
workspace.  Values owned by workflow nodes are archived and then cleared
before that new cycle, preventing an old branch or review verdict from
satisfying it accidentally.  For a failed, blocked, stalled, or cancelled run,
continuation instead resumes at the safe checkpoint: completed nodes stay
complete, the failed or unfinished node is retried, and following nodes run
only after it succeeds.

In either mode, the observation becomes `gptel-runner-run-goal` and is
automatically included, together with the preceding goal, in every new agent
prompt.  Calls, events, decisions, and manually stored blackboard context
remain available.  Read the ordered archive with
`gptel-runner-continuations`; each entry includes the preceding goal, terminal
state, restart mode, observation, and saved workflow results.

Budget use is cumulative by default.  Add capacity as above, or reset consumed
accounting while keeping the configured limits:

```elisp
(gptel-runner-continue my-run "Address my follow-up findings"
                       :reset-budget t)
```

Resetting and increasing may be combined.  Unlimited limits stay unlimited.
An optional `:callback` applies to the next terminal transition.  In the
dashboard, select a finished run and choose `c` from the action menu; the
command asks for the observation and whether to keep, reset, or increase its
budget.

When the repeat node's own `:max` is exhausted instead, use the repeat-specific
operation to add more iterations without losing its saved history:

```elisp
(setq my-run
      (gptel-runner-start 'implement-review
                           :goal "Revise the implementation"
                           :allow-writes t))

(gptel-runner-extend-repeat
 my-run 'review-cycle 3
 :additional-calls 6
 :additional-requests 8)
```

`gptel-runner-start` returns the run object, so keeping it in a variable is the
most direct option.  Both extension functions also accept the string ID shown
in the dashboard:

```elisp
(gptel-runner-extend-repeat "run-5" 'review-cycle 3)
```

Use `(gptel-runner-run-id my-run)` to obtain an object's ID,
`(gptel-runner-find-run "run-5")` to retrieve a session-local run object, or
`(gptel-runner-list-runs)` to inspect all retained runs.  A snapshot must be
loaded before its run ID becomes available in a new Emacs session.

The third argument adds to the exhausted repeat limit for this run only; it
does not modify the registered workflow or other runs.  Iteration numbering
continues from the previous limit, and new `review-history` entries are
appended to the existing list.  Existing request and call budgets remain
charged.  Use `:additional-requests`, `:additional-calls`, or
`:additional-duration` when a finite run-level budget also needs more room.
An optional `:callback` is invoked at the extended run's next terminal state;
the callback already invoked for the original failure is not called again.

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
`~/.emacs.d/gptel-runner/snapshots/`).  Automatic checkpoints are coalesced
until the run has been quiet for `gptel-runner-checkpoint-delay` seconds
(five by default), then serialized and written in short event-loop slices.
Manual saves, pauses, and terminal transitions bypass that delay, but still
return after queueing the write.  A `snapshot-saved` or `snapshot-error` event
reports its eventual result; `gptel-runner-store-save-status` returns `clean`,
`pending`, `writing`, or `error` for programmatic status checks.  Pending
writes are flushed during normal Emacs shutdown; an abnormal exit can lose the
latest queued progress.

Pause the entire process with `M-x gptel-runner-pause-run` from a worker, or
choose `P` from the dashboard action menu.  The active-duration clock stops
immediately while the snapshot finishes asynchronously.

After restarting Emacs, load the same workflow and agent definitions, then:

```elisp
(setq my-run
      (gptel-runner-load-run
       "/path/to/run-12.snapshot.el"
       (lambda (run)
         (message "Restored run: %s" (gptel-runner-run-state run)))))

(gptel-runner-resume-run my-run "Please prefer the smaller public API")
```

Alternatively, choose `l` and then `r` from the dashboard action menu.

Version 2 snapshots preserve execution state and current node outcomes: the
goal, canonical workspace, blackboard, node states and outcome values, repeat
iterations, budgets, and timing/options data.  Historical calls, event journals,
and transcripts remain available for inspection in the original Emacs session
but are not restored.
The dashboard therefore has no historical call rows immediately after loading;
new rows appear when an extended or resumed run dispatches calls.  An
unfinished call restarts statelessly from its workflow node.
Use `gptel-runner-node-result` to read a completed node's output after
restoration, or keep using named blackboard entries with `:save-as`.
All outcome values and blackboard entries in persistent runs must be readable
Lisp data; buffers and processes cannot be serialized.  An unreadable outcome
produces a `snapshot-error` naming the node and leaves the prior snapshot
intact.  Recorded decisions use the reserved
`gptel-runner-decisions` blackboard key, so they are preserved by the same
snapshot mechanism.

Completion callbacks and live provider processes are not serializable; supply
a new callback to `gptel-runner-load-run`.  Calls consumed before pausing remain
charged to call and request budgets.  Version 1 snapshots are not accepted.
Snapshot files have mode `0600`, but outcomes and blackboard values can contain
prompts, outputs, tool results, and secrets; protect and delete them as sensitive data.

Run options include `:driver`, `:max-requests`, `:max-calls`,
`:max-concurrency`, `:max-duration`, `:allow-writes`,
`:allow-unconfirmed-tools`, `:decision-memory`, `:context`, and `:persist`.
Workflow defaults are used when an option is not provided.  A function passed
as per-run `:context` must be readable Emacs Lisp data for persistent runs, so
prefer a named function there.  `:decision-memory` is the
exception whose package fallback is `t`; use an explicit nil value to opt out.

## Background jobs and macOS schedules

A job is a reusable invocation of any named workflow.  It specifies the
workflow, an initialization file for a fresh worker Emacs, start arguments,
and optionally a recurring schedule.  Jobs do not depend on a repository
hosting service or a particular kind of agent.

Each invocation runs in a separate `emacs -Q --batch` process.  Parallel nodes
still execute concurrent agent requests inside that worker.  Your ordinary
Emacs reads the durable history and sends commands; it never takes ownership
of the worker's executable run.  Connecting another `emacsclient` to your
ordinary Emacs server would share its process and would not provide this
isolation.

### Define and run a job

The bundled [background example](examples/background-jobs.el) uses the fake
driver and makes no network requests.  It runs two agents in parallel, saves
both results, synthesizes a report, and publishes a report file:

```elisp
(require 'gptel-runner-jobs)
(require 'gptel-runner-jobs-ui)

(setq my-background-job
      (gptel-runner-job-save
       (list :name "Parallel example"
             :workflow 'background-example
             :init-file "/path/to/gptel-runner/examples/background-jobs.el"
             :preload-features nil ; This fake example needs no agent packages.
             :arguments
             '(:goal "Demonstrate a background pipeline"
               :workspace "/path/to/workspace/"
               :inputs ((subject . "my project")))
             :schedule '(:interval 3600))))

;; On macOS, launchd owns this worker, so it survives closing the editor.
(setq my-invocation (gptel-runner-job-run my-background-job))
(gptel-runner-show-dashboard)
```

Saving a job does not install its schedule or execute it.  The saved record
has a stable `:id`; pass that identifier or the record to job commands.  Use
`M-x gptel-runner-show-jobs` to manage definitions and schedules.

Workers automatically load `gptel` and `gptel-agent` **before** the job's
`:init-file`, then call `gptel-agent-update` to discover agent definitions
and refresh the package's standard presets.  Your init file runs afterward
and has final authority over configuration and preset overrides.  If it
changes `gptel-agent-dirs`, call `gptel-agent-update` there to discover those
additional definitions.  This bootstrap runs on every worker start,
including checkpoint recovery.  Other agent files still need explicit
runner registrations if they are used as pipeline steps.

Saving a new job captures the editor's external Lisp library directories,
so Straight installs and local packages remain available in a fresh `-Q`
process.  Emacs's bundled Lisp directories are excluded; the worker uses
those from its own executable.  For a checkout not already on `load-path`,
configure this once before saving jobs:

```elisp
(setq gptel-runner-job-load-path '("~/projects/gptel-agent/"))
```

The captured paths and `gptel-runner-job-preload-features` (default
`'(gptel gptel-agent)`) are stored as `:load-path` and `:preload-features`.
Existing values survive job updates and schedule changes.  Supply either
key explicitly to replace it; `:preload-features nil` disables automatic
package loading and agent discovery for fake-driver or standalone jobs.
Jobs created before this bootstrap was added must be re-saved once from
your configured Emacs to capture the new fields for future invocations.
Existing invocation histories retain their original init-controlled loading
when resumed.  A missing package or dependency produces a durable
`startup-error` before the init file runs.

The `:init-file` is trusted Emacs Lisp loaded **inside the worker**.  It can
focus on credentials and backends, runner agent registrations, the workflow,
and driver selection; extra packages can still be loaded there.  The worker
does not load your normal Emacs initialization or inherit live variable
values, backend objects, or interactive hooks from the editor.  Keep window,
keybinding, and interactive session setup out of the job init file.
Workers disable automatic native compilation and trampoline generation, so
starting a scheduled job does not require a compiler toolchain or update the
interactive editor's native compilation cache.  Already installed native
code can still run.  Tools must support headless execution; direct native
calls to input primitives cannot always be intercepted by Lisp input guards.
Named workflows and stable explicit node IDs are required for durable jobs,
just as they are for checkpoint recovery.

Optional job `:emacs` names an absolute executable path; saving captures the
current Emacs executable by default.  `:environment` is an explicit alist of
environment variable names and string values, for example
`'(("PATH" . "/opt/homebrew/bin:/usr/bin:/bin"))`.  A scheduler does not inherit
your interactive shell setup, so use absolute tool paths or configure `PATH`
in the job/bootstrap.

### Inputs, workspaces, and outputs

The start option `:inputs` accepts an alist or hash table and initializes the
blackboard before any prompt or node executes.  Read values using
`gptel-runner-get`; continue saving outputs with `:save-as` or
`gptel-runner-put`.  Inputs and durable outputs must be readable Lisp data.

Workflows can define a default `:workspace`.  An explicit start argument takes
precedence, and existing per-node overrides still resolve against the selected
run workspace.  Saving a job captures its `:base-directory`, so the directory
from which launchd starts Emacs cannot change file resolution.  Leave
`:arguments :workspace` absent to use the pipeline's default, including when
the pipeline is defined only inside the worker initialization file.  Relative
defaults resolve against the captured base directory.
The job layer does not create Git worktrees or copy working directories.
An agent editing these shared directories changes the same files your normal
Emacs visits.  Processes also share CPU, memory, and network resources.

History contains each agent's result, the run's saved values, transcripts,
events, errors, and logs.  A pipeline can publish any number of named files:

```elisp
(gptel-runner-publish-artifact run "report" "/tmp/report.txt" "text/plain")
```

Publishing copies the file into the invocation's private artifact directory;
later changes to the source file do not change that historical output.
Snapshot recovery data stays separate from inspection history.  Existing v2
snapshots and session-local run IDs remain supported; external identities also
include the invocation and worker generation to avoid cross-process collisions.

### Inspect and control background work

The dashboard displays external invocations alongside local runs.  Inspecting
a background row opens durable results or transcripts without restoring its
checkpoint in your interactive Emacs.  Its action menu sends commands to the
owning worker, including pause, resume, cancel, retry, continue, budget
extensions, decisions, call feedback, and tool approval or rejection.

The programmatic control interface is:

```elisp
(gptel-runner-job-command my-invocation 'pause)
(gptel-runner-job-command my-invocation 'resume
                          '(:feedback "Prefer the smaller implementation."))
```

Commands are data records with durable receipts.  The worker validates the
operation, target generation, and current state before applying a command.
An approval can authorize only the pending tool interaction shown in the
dashboard; stale approvals cannot authorize a later tool call.  Rejecting an
approval cancels that call.  Tool confirmation policy remains explicit;
running in the background does not automatically authorize tools.

The `background-example-feedback` workflow from the same initialization file
stops after synthesis.  Create a job using that workflow, inspect the final
call, and accept a result from the dashboard to release the pipeline.  Real
gptel workers also support sending feedback to the retained conversation
before accepting its new response.  Tools which directly read a synchronous
minibuffer cannot automatically become remote conversations; they need an
asynchronous input adapter and otherwise report an unsupported-input error.

Workers remain alive while paused or waiting for feedback/approval.  Existing
duration semantics apply: pausing a whole run stops its active-duration clock;
waiting for call feedback or approval continues to consume a configured run
duration limit.  Closing the dashboard neither accepts input nor stops work.
After a terminal exit, retry/continue starts another worker from the checkpoint
within the same invocation history.  A crashed worker loses live callbacks;
explicit recovery restarts unfinished nodes and old interactions are invalid.

Only one invocation of a job is active at a time.  Overlapping triggers are
recorded as skipped rather than queued.  Different jobs may run concurrently;
background calls declared as writers coordinate access to the same canonical
workspace.  This is orchestration, not a filesystem security boundary.

### Run from a shell or cron

The generic batch entry point can execute a saved job without launchd:

```sh
/absolute/path/to/emacs -Q --batch -L /path/to/gptel-runner \
  -l gptel-runner-jobs -f gptel-runner-jobs-batch-main -- \
  run JOB-ID --root /absolute/path/to/job-store/
```

Use the `:id` returned by `gptel-runner-job-save`; the default store is
`~/.emacs.d/gptel-runner/jobs/` unless `gptel-runner-job-directory` is changed.
`run` keeps that Emacs process alive until the pipeline finishes, including
human-input waits.  It returns exit code `0` for success or a skipped duplicate,
`1` for another terminal pipeline outcome, and `2` for startup or persistence
failure.  This same command can be placed in a user crontab with appropriate
stdout/stderr redirection.  It does not require an interactive Emacs server.

On macOS, the `trigger JOB-ID --root STORE` command dispatches through the
detached launcher and returns after the worker has adopted ownership.  That
handoff prevents a short-lived trigger from releasing its reservation too
early.  If worker startup times out, the exact worker service is stopped before
the reservation is released.  If stopping cannot be confirmed, the invocation
remains visibly uncertain and its reservation stays held until absence is
confirmed; it is not silently retried.

### Manage user schedules on macOS

macOS supports per-user cron jobs through `crontab -e`, without requiring root.
The integrated scheduler uses user LaunchAgents, Apple's recommended mechanism
for scheduled work.  It manages only package-owned property lists in
`~/Library/LaunchAgents` and services in the current user's `gui/<uid>` domain.
No system daemon is installed.

```elisp
(require 'gptel-runner-launchd)

;; Enable or update the saved recurring schedule.
(gptel-runner-launchd-install my-background-job)

;; Disable future triggers; an active worker continues.
(gptel-runner-launchd-disable my-background-job)

;; Remove the installed schedule without deleting job/history data.
(gptel-runner-launchd-remove my-background-job)

;; Obtain raw launchctl diagnostics and the last installation result.
(gptel-runner-launchd-diagnostics my-background-job)
```

Schedules support positive integer intervals such as `(:interval 900)` and
calendar rules such as `(:calendar ((:hour 9 :minute 0)))`.  Multiple calendar
rules are allowed; `:weekday 1` is Monday, while `0` or `7` is Sunday.  Supported
fields are `:minute`, `:hour`, `:day`, `:weekday`, and `:month`; omitted fields
are wildcards.  Rules follow launchd's semantics, including matching either
day-of-month or weekday when both are supplied.  Calendar times use the Mac's
local timezone.  Future one-off scheduling is not provided.

Scheduled triggers exit after dispatching a separate invocation service.  This
lets schedule disabling and worker cancellation remain independent operations.
Completed worker registrations are removed during later launches or jobs-view
refreshes; their history stays on disk.  Installation failures are stored in
the job's scheduler status and are visible in diagnostics.

User LaunchAgents require the login session.  Calendar triggers missed while
the Mac sleeps coalesce into one trigger on wake; interval triggers can be
missed.  Jobs do not run while the machine is powered off, and cron likewise
misses executions during sleep.  See Apple's
[scheduled jobs guide](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/ScheduledJobs.html)
and [LaunchAgent lifecycle documentation](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html).

## Semantics and safety

- Calls are stateless.  Prompts must explicitly include the goal, workspace,
  iteration, selected blackboard inputs, and prior findings.  Agents should
  reinspect files instead of trusting earlier reports.
- Decision memory is run-local and append-only.  A recorded decision is added
  to prompts resolved after it was saved.  It cannot alter an agent call that
  is already running; parallel branches should converge before a decision is
  expected to constrain their downstream work.
- A writable workflow is rejected unless the start call includes
  `:allow-writes t`.  Writers sharing a canonical workspace are serialized.
- `workspace-mode` is orchestration metadata, **not a security sandbox**.  Tool
  lists and gptel confirmation policy remain the real capability boundary.
  The runner does not create isolated worktrees or enforce read-only access;
  provision separate directories and tool restrictions as needed.
- Tool confirmation remains enabled unless callers also explicitly request
  `:allow-unconfirmed-tools t`.  Side-effecting tools should be idempotent:
  transport retries have at-least-once semantics, and arbitrary asynchronous
  tool effects cannot be rolled back generically.
- Request retries reuse one logical call and consume request attempts.  An
  agent-step retry creates a new call.  An implementation revision is a new
  workflow iteration.  These are intentionally separate counters.
- Requests honor `gptel-stream`, including preset overrides and backend/model
  support, just like ordinary gptel chats.  Tool calls and reasoning can stream
  into the worker transcript; a stream ending does not finish the agent until
  gptel's tool loop has completed.
- A successful provider request must still return a non-empty final response.
  Intermediate text accompanying tool calls stays in the transcript and event
  journal; only the final response is saved for downstream nodes.  A provider
  response that reports `tool_calls`, `tool_use`, or `function_call` without
  supplying tool calls is retried within the existing request/retry limits,
  and fails the node if those limits are exhausted.
  If it returns only `nil` or whitespace (for example, after ending on a tool
  call), the runner marks that call failed, asks the same agent once for a
  stateless repaired answer, and fails the node immediately if that answer is
  also empty.  Downstream nodes never receive the empty value.
- Runtime state is authoritative.  Compact snapshots are versioned execution
  projections written at safe checkpoints; they do not preserve session audit
  history, Lisp continuations, provider connections, timers, or external tool
  processes.

## Extension points

Custom agents combine any gptel preset with a runner parser, validator,
schema, retry policy, and metadata.  Custom drivers implement
`gptel-runner-driver-start`, `gptel-runner-driver-cancel`, and optionally
`gptel-runner-driver-pause` (which defaults to stopping external work without
terminalizing the runner call) and `gptel-runner-driver-await-feedback` (which
prepares an already-completed provider transcript for human editing).  Prompt,
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
| `empty-response` | The original call and its automatic one-shot repair both returned no final answer. |
| `invalid-output` | The original and one repair response both failed validation. |
| Cancellation cannot undo a tool | Stop/undo the external tool action separately. |
| Snapshot cannot find a node | Reload the same named workflow with stable explicit node IDs. |
| Snapshot cannot be saved | Keep node outcomes, blackboard keys/values, the run goal, and per-run context readable as Emacs Lisp data. |

## Development

```sh
make test
make compile
make checkdoc
make package-lint
make release-check
```

Live provider smoke tests are opt-in and excluded from ordinary CI.
