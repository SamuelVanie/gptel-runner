# gptel-runner

`gptel-runner` gives each agent node its own gptel chat. A researcher can
hand findings to a planner, answer a question in the same research buffer,
and let the planner continue with that answer. The final agent can use its
tools to carry out the plan and report what happened. Elisp defines the
order of work, budgets, approvals, and retries.

Use `M-x gptel-runner-show-dashboard` and press `RET` on an agent row to open
its chat or review its pending tool requests. Press `? m` on a run to see
the handoffs, questions, and replies.
From a chat, `M-x gptel-runner-visit-node-chat` switches nodes.
For a named workflow, `M-x gptel-runner-save-session` saves the whole run,
including node chats and their child transcripts. After restarting Emacs,
load the workflow definition and use `M-x gptel-runner-open-session` to
choose it by workflow, date, or goal. The runner dashboard manages the
session and its conversations together.

The package requires Emacs 29.1 or newer and, for live calls, gptel 0.9.9.6 or
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
researcher, planner, and implementer that exchange messages and pause for
human approval of the plan. The
[natural-language review loop](examples/natural-language-review-loop.el)
shows implementer and reviewer nodes communicating through a minimal file
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

### Handing work to the next agent

Use a model that supports tool calls. The gptel driver adds the applicable
workflow tools to each agent request. Each takes a single non-empty string
and must be the only tool call in that response.

| Tool | What happens |
| --- | --- |
| `RunnerHandoff(message)` | Submit the node's result. The next agent in the sequence receives the accepted message in its chat. On the final agent, this submits the final result. |
| `RunnerAskPrevious(question)` | Ask the preceding agent a question in its existing chat. That agent uses `RunnerHandoff` to reply. The requesting agent resumes with the answer and can ask again. |

For example, a planner trying to choose a migration strategy can ask its
researcher, “Which files still write the old format?” The researcher can use
its usual tools, then hand back the filenames and supporting evidence. The
planner receives the answer before deciding what to pass to the implementer.
The researcher's original node result stays available to Elisp code.

The sequence determines the recipient. Elisp steps and nested sequences
between two agents still execute in order. A parallel group, branch,
repeat, or other non-sequence container bounds the route. A node using
response completion also ends the route. Use `:save-as` and
`gptel-runner-get` to carry outputs across these
boundaries, for example when collecting parallel reviews. Follow-up turns
use the run's call and request budgets.

```elisp
(gptel-runner-defworkflow research-and-plan (:max-calls 8 :max-requests 20)
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-agent-step
    :id 'research :agent 'researcher
    :prompt "Inspect the storage format. Hand the evidence to the planner.")
   (gptel-runner-agent-step
    :id 'plan :agent 'planner :save-as 'plan
    :prompt "Plan the migration. Ask the researcher about any gaps first.")))
```

Ordinary assistant prose remains in the chat. If the model ends its turn
with prose, the call waits for feedback. From its dashboard row, use `? f`
to send another turn or `? a` to accept the current answer. Set
`:pause-after t` when even an explicit handoff needs human approval.
The runner applies the node's parser and validator to the accepted message
and stores the parsed result under `:save-as`. An agent with a JSON schema
receives the schema as instructions for the handoff message.

For a preset or provider that should complete a node on a normal text
response, set `:completion 'response` on its `gptel-runner-agent-step`.
That mode also supports the provider's usual structured-output mechanism.
To use response completion throughout a named workflow, write
`(:completion response)` in the options of `gptel-runner-defworkflow`.
For a single run, pass `:completion 'response` to `gptel-runner-start`.

### One pipeline API for local and background runs

Pipeline functions always receive the same run and node objects, whether
started with `gptel-runner-start` or by a job worker. Use these public contracts
throughout a pipeline; job definitions handle launching and scheduling.

| Need | Public API |
| --- | --- |
| Supply and read named data | `:inputs`, `gptel-runner-get` |
| Save a step result or another value | `:save-as`, `gptel-runner-put` |
| Inspect named values | `gptel-runner-outputs` (alist, including inputs and history) |
| Open a local node’s chat | `gptel-runner-node-buffer` (run and node object or ID) |
| Inspect a node | `gptel-runner-node-state`, `-outcome`, `-result`, `-error` |
| Resolve a node's workspace | `gptel-runner-node-workspace` (node object or ID) |
| Retain an existing file | `gptel-runner-publish-artifact` |
| Retain generated text | `gptel-runner-write-artifact` |
| List, locate, or read retained files | `gptel-runner-artifacts`, `-artifact-file`, `-read-artifact` |
| Feed a retained file to an agent | `gptel-runner-context-artifact` |

`gptel-runner-get`, `gptel-runner-outputs`, the four node inspection helpers,
and the three artifact readers also accept an invocation record returned by
`gptel-runner-invocation-get`. They read saved inspection data without loading
an executable run. Refresh the invocation record to observe a running worker.
Older invocation histories without exported node outcomes report that they are
unavailable. `gptel-runner-invocation-outputs` remains supported.

### Managed file outputs

Publishing owns storage for the copy and returns the same descriptor in both
environments. For generated text, no caller-managed temporary file is needed:

```elisp
(gptel-runner-elisp-step
 :id 'report :save-as 'report
 :function
 (lambda (run _node)
   (gptel-runner-write-artifact
    run "report.md"
    (format "# Report\n\n%s\n" (gptel-runner-get run 'analysis))
    "text/markdown")))

;; Or retain a copy of an existing file. Relative filenames use the run
;; workspace; for node overrides, expand from gptel-runner-node-workspace.
(gptel-runner-publish-artifact run "test-results" "results.json" "application/json")

;; These readers also work with a saved background invocation record.
(gptel-runner-artifacts run)
(gptel-runner-artifact-file run (gptel-runner-get run 'report))
(gptel-runner-read-artifact run (gptel-runner-get run 'report))

;; A later agent can consume the artifact saved under 'report.
(gptel-runner-agent-step
 :id 'review-report :agent 'reviewer :prompt "Review the report."
 :context (gptel-runner-context-artifact 'report))
```

Descriptors contain `:name`, `:file`, `:media-type`, `:epoch`, and
`:published-at`. `:file` is relative to managed output storage; always resolve
it through `gptel-runner-artifact-file`. `:epoch` is nil for local runs and the
worker generation for background runs. Publication appends to the artifact
registry; `:save-as` or `gptel-runner-put` separately names a descriptor on the
blackboard. Repeated publication under the same display name retains distinct
files, so retries and continuations do not overwrite earlier reports.

Local files live in a unique private directory under
`gptel-runner-output-directory`; jobs retain them inside invocation history.
Files have mode 0600 and artifact directories 0700. Publication becomes visible
after the complete file is moved into place. The file-copy API leaves its
source untouched; the text API writes UTF-8. Managed output storage does not
require `:allow-writes`, which governs declared workspace writes.

Snapshots retain the output directory and artifact registry. Files survive
completion, forgetting a run, and snapshot deletion. Remove retained directories
explicitly when no longer needed; copying a snapshot alone does not copy its
files. These files can contain the same sensitive data as other run outputs.

See [artifact-pipeline.el](examples/artifact-pipeline.el) for one pipeline that
runs unchanged locally or as a background job, without a provider.

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

### Bounded run finalization

Wrap the entire workflow in `gptel-runner-finalize` to publish partial results
after success, failure, blocked/stalled outcomes, or exhaustion of duration,
request, call, or repeat limits. Its `:body` and `:finally` are ordinary
workflow nodes; the wrapper must be the workflow root.

```elisp
(gptel-runner-start
 (gptel-runner-finalize
  :id 'publish-run
  :body my-workflow-node
  :max-duration 30               ; separate finalizer allowance, required
  :max-calls 0                   ; this finalizer only uses local Elisp
  :max-requests 0
  :on-cancel 'finalize            ; opt in; default is 'skip
  :finally
  (gptel-runner-elisp-step
   :id 'publish-partial-results :save-as 'report
   :function
   (lambda (run _node)
     (gptel-runner-write-artifact
      run "partial-results.el"
      (prin1-to-string
       (list :outcome (plist-get (gptel-runner-finalization-outcome run) :body)
             :results (gptel-runner-get run 'review-history)))))))
 :max-duration 600)
```

The finalizer shares the run's blackboard, node outcomes, workspace rules,
concurrency limits, and driver. It receives a fresh, separate budget:
`:max-duration` must be positive and finite; `:max-calls` and `:max-requests`
are nonnegative integers, each defaulting to 1. These allowances include
retries and repairs. Body work is stopped before finalization starts, and
late body results are ignored. The finalizer's allowance cannot be increased
with `gptel-runner-extend`; pause stops its active-duration clock and resume
uses what remains. As with Elisp steps, a timer cannot interrupt Lisp that
does not yield; elapsed duration is checked when an Elisp step returns.

Cancellation remains an immediate stop by default. `:on-cancel 'finalize`
explicitly permits finalization after a run or body cancellation. Cancelling
again while the finalizer runs always stops it. `try :finally` keeps its
existing cancellation and duration behavior.

The run stays `running` during finalization and calls its terminal callback
only afterward. The run's final state and terminal data preserve the **body's
outcome**, even if the finalizer fails, expires, or is cancelled. Inspect
`gptel-runner-finalization-outcome`: `:body` and `:result` are separate
`(:state STATE :value VALUE)` records, and `:phase` is `finalizing` or
`complete`. A skipped finalizer has `:result (:state skipped :value
run-cancelled)`. The record is nil until the body finishes. Budget records
are `gptel-runner-budget` objects: `:body-budget` retains body accounting,
while `:budget` records finalizer accounting after it runs. The ordinary run
budget refers to the active phase and is restored to the body budget when
finalization completes. A failed publication therefore remains observable
without replacing the original failure or exhaustion reason.

Snapshots retain both outcomes, the current phase, and its remaining budget.
Resuming a paused finalizer continues its safe checkpoint without rerunning
the body. Retrying, extending, or continuing a terminal run prepares a fresh
finalizer for the next body outcome; `retry :from-node` targets must belong
to the body. Finalizers should publish idempotently, since an unfinished step
can restart after pause or process interruption. Finalizers use the same
artifact APIs for local runs and background invocation history.

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
the next workflow prompt for each affected agent node.  A context value is one
source or a list of sources.  Each source can be an inline string, a function
of `(run node)` that returns a string (or nil to omit itself), or a file
descriptor produced by `gptel-runner-context-file` or
`gptel-runner-context-artifact`.  `node` is the target
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
| `RET` / click | Review pending tools on a node, otherwise open its chat or the selected run |
| `TAB` | Expand or collapse a run's nodes |
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
the agent transcript. When a node needs permission to run tools, its row shows
the pending approval count. Press `RET`, click the row, or use `? y` to review
the tool names, arguments, and requesting conversations together. Choose
**Approve once** for one request or press `a` to approve all requests shown for
that node. Use `? v` on the dashboard row to open the chat directly.

Dashboard columns are a configurable set.  Their order in the value is
ignored so the table keeps a stable canonical layout:

```elisp
(setq gptel-runner-dashboard-columns
      '(item agent state elapsed))
```

Available symbols are `item`, `workflow`, `run`, `node`, `call`, `job`, `agent`, `state`, `elapsed`,
`attempts`, `iteration`, `requests`, and `calls`.  Press `g` in an open
dashboard after changing the variable, or choose `V` from the `?` action menu
to toggle one column interactively.  The same action is available as
`M-x gptel-runner-dashboard-toggle-column`.
The default `item` column groups job/workflow runs and their nodes into a compact
tree.  Press `TAB` on a run or node to collapse or expand that run; press `RET`
or click a row to open it.  Separate workflow, run, node, and job columns remain
available for a wider table.  Rows stay on one line instead of
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

Each agent node owns a gptel buffer named `*gptel:WORKFLOW/NODE*`. The buffer
contains the prompt, assistant replies, and the tool and reasoning blocks
enabled by your gptel settings. You can edit or continue the conversation
with the usual gptel commands. While the run is unfinished, its node chats
stay alive. Close the window or bury the buffer to hide a chat. Use the
dashboard's `? p` to pause a node for feedback or `? k` to abort it.
An accepted workflow result changes when the
runner completes a new turn or you explicitly accept a revision. Set
`gptel-runner-retain-worker-buffers` to nil to close the buffers when the run
finishes. Elisp and control-flow nodes have inspectable outcomes.

### Human feedback in an agent buffer

Use `M-x gptel-runner-pause-call` in an active worker buffer, or select its
dashboard row and choose `p` from the `?` action menu.  The provider request is
stopped and the node waits for your input in its chat.  Add your correction, use the normal gptel commands to
obtain a new response, then run
`M-x gptel-runner-complete-call-from-buffer`.  An active region is used when
present; otherwise the latest gptel response is returned to the workflow.  The
workflow then continues from that node's original continuation.

You can also send the correction with `? f` on the waiting node. Feedback
starts a new user turn after the rendered replies and tool output. Any unsent
draft stays below that turn and is excluded from the request.

This is deliberately different from `gptel-runner-abort-call`: aborting is a
terminal cancellation, while pausing enters the `waiting-feedback` state.

For a planned approval point, put `:pause-after t` on the agent step. The
call holds the proposed handoff in `waiting-feedback` until you accept it.
Choose `f` from the dashboard action menu to request a revision, or use `v`
to visit the chat. Choose `a` to accept, or run
`M-x gptel-runner-complete-call-from-buffer`. With a region selected in the
chat, the selected text becomes the result. Otherwise the runner accepts
the latest revision, or the proposed handoff if you have kept it as submitted.
Acceptance runs the parser and validator and releases the workflow continuation.

For a named workflow, save the session while the gate is waiting. After
reopening it, visit the node, revise the answer if needed, then run
`gptel-runner-complete-call-from-buffer`. The runner validates the answer
and continues from that node. If parallel reviewers are waiting for feedback,
accept each saved answer before the workflow advances. Explicitly resuming
the entire run starts fresh turns for unfinished nodes.

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
and node ID, or select the saved agent row in the dashboard.  Durable
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

### Saving a session and resuming overnight

Use `M-x gptel-runner-save-session` for an explicit save or `:persist t`
for automatic checkpoints. All node chats, child discussions, and delegated
worker transcripts belong to their run, including while automatic saving
is off. Look them up in the runner dashboard. On a node row, `? v` opens
its chat and `? j` shows its child transcripts. On a run row, `? j` includes
children from every node. Saved discussion transcripts and worker diagnostic
text are available after reopening the run. New agent work is scheduled by
the owning workflow.

After opening a session, select its run row and press `?` for these actions.

| Action | When to use it |
| --- | --- |
| `r` Resume | Continue a paused run from its unfinished step, keeping completed nodes. |
| `t` Retry | Retry an unsuccessful run from its safe checkpoint with the same goal. |
| `R` Rerun | Start a finished run from the first node with the same goal, chats, and configured limits. Consumed budget counters start at zero. |
| `c` Continue | Give a finished run a new goal. A successful run starts another full cycle. An unsuccessful run continues from its safe checkpoint. |
| `K` Abort / Cancel | Stop an unfinished run and record its cancellation. |

From Lisp, `(gptel-runner-rerun run)` archives the preceding workflow
outputs in `(gptel-runner-continuations run)`. Each new history entry has
`:results` for named outputs and `:node-outcomes` for individual steps.
Inputs, recorded decisions, and conversations stay with the same run.
Pass `:reset-budget nil` to
share the remaining budget with the previous cycle. A background invocation
uses the same operation via `(gptel-runner-job-command invocation 'rerun)`.

Persistent runs use a named workflow. Explicit node IDs make saved work
easier to inspect. For generated IDs, recovery matches the workflow's node
layout against its checkpoint. Add `:persist t` to the workflow defaults or
start call:

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

Version 4 saves the node conversations, child transcripts and workflow
progress together. Opening it restores the run and its chat rows.
Visiting a node opens its editable buffer. The companion gptel-agent
package can encode the assistant and tool boundaries, settings, and drafts
inside the runner checkpoint. Plain gptel uses its own saved-chat format.
The runner keeps exclusive ownership even with global
`gptel-agent-session-mode` enabled. The global agent session browser is for
independent agent chats. Use `M-x gptel-runner-open-session` to recover
pipeline work, including chats waiting for human feedback.

Versions 2 and 3 still open with the data they originally recorded. Version
3 includes saved node chats. Child transcripts are available once captured
by a version 4 checkpoint.
Sessions saved before tool handoffs keep response completion when reopened.
New checkpoints include the inter-node messages and the outstanding
question, so a resumed reply can return to the agent that asked for it.

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

Run options include `:driver`, `:completion`, `:max-requests`, `:max-calls`,
`:max-concurrency`, `:max-duration`, `:allow-writes`,
`:allow-unconfirmed-tools`, `:decision-memory`, `:context`, and `:persist`.
Workflow defaults are used when an option is not provided.  A function passed
as per-run `:context` must be readable Emacs Lisp data for persistent runs, so
prefer a named function there.  `:decision-memory` is the
exception whose package fallback is `t`; use an explicit nil value to opt out.

## Background jobs and macOS schedules

A job specifies a named workflow, an initialization file for a fresh worker
Emacs, start arguments, and an optional recurring schedule. Register jobs and
apply schedules from your Emacs Lisp configuration. Put the workflow and its
agent definitions in the worker's initialization file, which loads on every
invocation.

Each invocation runs in a separate `emacs -Q --batch` process.  Parallel nodes
still execute concurrent agent requests inside that worker.  Your ordinary
Emacs reads the durable history and sends commands; it never takes ownership
of the worker's executable run.  Connecting another `emacsclient` to your
ordinary Emacs server would share its process and would not provide this
isolation.

### Register a job from Emacs Lisp

The bundled example runs two fake agents in parallel, saves both results, and
publishes a report file. With the checkout on `load-path`, evaluate this
definition in your editor or include it in your Emacs configuration.
[Example workflow](examples/background-jobs.el)

```elisp
(require 'gptel-runner)

(gptel-runner-register-job
 "parallel-example"
 :name "Parallel example"
 :workflow 'background-example
 :init-file (expand-file-name "examples/background-jobs.el"
                              (file-name-directory (locate-library "gptel-runner")))
 :preload-features nil
 :arguments '(:goal "Demonstrate a background pipeline"
              :inputs ((subject . "my project")))
 :schedule '(:interval 3600))
```

The first argument is a stable string ID made of letters, digits, hyphens,
or underscores. Registering a new job requires
`:workflow` and `:init-file`. The display name defaults to the ID.
Re-evaluating the definition updates the same job and returns its saved
record. You can update individual top-level properties in later calls.
Supplying `:arguments` replaces the entire argument plist.

```elisp
(gptel-runner-register-job "parallel-example" :name "Project report")
(gptel-runner-job-get "parallel-example")
```

Registration only saves the definition. To start one invocation now on macOS,
evaluate the following form when you want the worker to run. To enable
recurring triggers, use `gptel-runner-job-schedule` as described below.

```elisp
(setq my-invocation (gptel-runner-job-run "parallel-example"))
```

In `M-x gptel-runner-show-jobs`, select a job and press `?` or `RET` for
its menu. Use `r` in the menu to start it and `R` to visit the run dashboard.
The following shortcuts also work directly on a job row.

| Key | Action |
| --- | --- |
| `x` | Delete this job's saved definition and schedule after confirmation. Existing workers continue and their history stays available. |
| `C` | Clear this job's finished runs from the run dashboard after confirmation. Active runs stay visible, including paused workers. |

Cleanup uses the job ID, so jobs that share a workflow can be cleared
independently. Saved results and chats stay on disk. Use `C-u C` to delete
the cleared runs' recovery checkpoints as well.

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
Durable jobs use named workflows and the same node ID matching as local
checkpoint recovery.

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

The same publication API works locally. It copies the file into private managed
storage; later changes to the source file do not change that historical output.
Use `gptel-runner-write-artifact` to publish text directly, and the shared
output, node outcome, and artifact readers above to inspect the invocation.
Snapshot recovery data stays separate from inspection history.  Existing v2
snapshots and session-local run IDs remain supported; external identities also
include the invocation and worker generation to avoid cross-process collisions.

### Inspect and control background work

The dashboard displays background invocations alongside local runs. Press
`RET` on a node with pending tools to review its requests in the worker
controls. On other agent nodes, `RET` opens the published conversation.
You can always open a published chat with `? v`. The header says
**Local chat copy**. You can send follow-ups in this copy, and its edits stay
in your Emacs. To revise the answer used by the workflow, follow **Worker
controls** or use `? w` in the background dashboard. From there, **Feedback**
asks the worker for a revision and **Accept** returns its answer to the
workflow. Older invocations still open their saved text transcripts.

A background run row opens its controls. The panel links to the node chats,
shows pending tool requests, and sends actions to the owning worker.
Use `? m` on the run or node row to read its published handoffs, questions,
and replies. From a waiting node, `? f` sends feedback to its worker.
Use `n` and `p` to move between nodes, `TAB` to move between buttons, and
`q` to return to the dashboard.

Press `RET` or `? y` on a node with pending permissions. Its panel shows
the tool arguments for each request, including those from child conversations.

| Scope in the node panel | Approve once | Deny and let the agent continue |
| --- | --- | --- |
| Request at point | `y` | `d` |
| Requests in the active region (set the mark with `C-SPC`) | `y` | `d` |
| All pending requests shown for this node and its children | `a` | `D` |

The worker applies each decision to the selected request IDs. Denial
returns a rejection to the requesting conversation.
Choose **Reject & cancel node** to stop the owning node and its children.
The panel shows submission receipts and worker errors. Use `? h` for the
full receipt history, `? v` for the node's chat, and `? j` for child transcripts.

For outside-directory file requests from `gptel-agent`, **Approve all + allow
outside paths** (`A`) also remembers outside access in the conversations owning
those requests.  It does not grant access to other nodes or future children.
Ordinary tool confirmations, project instruction checks, and file reviews remain
active.  **Revoke outside access** restores per-operation approval.  These grants
also end on pause, completion, restart, or a working-directory change and are
never saved in recovery checkpoints.

The background `?` menu also provides `x` to forget the selected invocation
(or the invocation owning the selected call), `X` to forget the selected
workflow's background history, and `C` to clear all finished background runs.
`:` opens the full command chooser.  Cleanup hides these entries across
dashboard refreshes and Emacs restarts while preserving results, transcripts,
artifacts, and recovery checkpoints.  With a prefix argument, cleanup also
deletes the corresponding recovery checkpoints.  Job definitions and schedules
remain installed; `X` is limited to background history and `C` leaves local
runs alone.  Use the classic menu to clean up local runs.

Cleanup is disabled while workers are alive, including paused workers, or
their launch status is uncertain.  Cancel active background runs and wait for
their workers to stop first.  Interrupted runs can be forgotten when their
worker is confirmed dead.  Saved history remains accessible through
`gptel-runner-invocation-list` and the inspection APIs; resuming, retrying, or
continuing a forgotten invocation starts a new worker generation and makes
it visible again.  Programmatically use `gptel-runner-invocation-forget`.

The programmatic control interface is:

```elisp
(gptel-runner-job-command my-invocation 'pause)
(gptel-runner-job-command my-invocation 'resume
                          '(:feedback "Prefer the smaller implementation."))
```

Commands are data records with durable receipts.  The worker validates the
operation, target generation, and current state before applying a command.
One-time approvals authorize only the exact request IDs displayed in the
panel, even when submitted as a group; stale approvals cannot authorize a later
tool call.  The separate outside-path option explicitly grants future file
access using gptel-agent's scoped approval API.  The `deny` command takes
`:call-id`, `:call-epoch`, `:generation`, and a list of `:interaction-ids`.
The `reject` command cancels the owning call. Background workers follow
the configured tool confirmation policy.

Pipeline agent presets can include `Agent`, `Discussion`, and
`DiscussionControl` from `gptel-agent`.  The runner applies the preset's prompt
transforms so these tools receive their request-local dispatchers and allowed
agent definitions.  This works in local runs and isolated job workers.

This optional integration requires a gptel-agent version providing
`gptel-agent-supervise`, `gptel-agent-supervision-stop`,
`gptel-agent-supervision-retry`, and
`gptel-agent-supervision-request-live-p`. The adapter reports a missing API
with an update instruction. It uses one public supervision handle per node
invocation; ownership, descendant completion and deferred wakeups belong to
gptel-agent. The runner supplies approval and state callbacks and owns the
root request's scheduling and retry policy. It neither advises private
agent functions nor edits agent structs. Plain gptel presets continue to work
without gptel-agent installed.

A runner-owned request waits for its active discussion turns, nested
discussions, and queued discussion follow-ups before completing.  Idle
discussions do not hold the node open.  `Agent` workers, including workers
started with `background: true`, also finish before their owning request.
An explicit `DiscussionReport` resumes the parent for synthesis; an ordinary
child reply remains local, so ask for a report when its findings must affect
the node's result.  Silent child completion releases the held parent answer.
Cancelling or pausing a node stops its discussions and subagents.  Ordinary
gptel conversations outside a runner retain their independent discussion
behavior.

In jobs, tool confirmations from subagents and discussions appear as pending
interactions on the owning node.  Each includes its source buffer, tool names,
and arguments; several children can await approval simultaneously.  Approve
or deny them through the node panel. Each child receives its own tool
results or denials.
Replies to cancelled, completed, or superseded requests cannot run tools.
Question forms and separate interactive file-review interfaces still require
their own headless adapters; this bridge handles gptel tool confirmations.
Nested work is supervised inside the node, not registered as additional
pipeline nodes or separately charged against the runner's call/concurrency
limits.  Crash recovery restarts unfinished nodes rather than restoring live
child callbacks.

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

### Run a registered job from a shell

The generic batch entry point can execute a saved job without launchd:

```sh
/absolute/path/to/emacs -Q --batch -L /path/to/gptel-runner \
  -l gptel-runner-jobs -f gptel-runner-jobs-batch-main -- \
  run JOB-ID --root /absolute/path/to/job-store/
```

Use the string ID passed to `gptel-runner-register-job` or the `:id` of an
existing saved definition. The default store is
`~/.emacs.d/gptel-runner/jobs/` unless `gptel-runner-job-directory` is changed.
`run` keeps that Emacs process alive until the pipeline finishes, including
human-input waits.  It returns exit code `0` for success or a skipped duplicate,
`1` for another terminal pipeline outcome, and `2` for startup or persistence
failure. The command runs independently of an interactive Emacs server.

On macOS, the `trigger JOB-ID --root STORE` command dispatches through the
detached launcher and returns after the worker has adopted ownership.  That
handoff prevents a short-lived trigger from releasing its reservation too
early.  If worker startup times out, the exact worker service is stopped before
the reservation is released.  If stopping cannot be confirmed, the invocation
remains visibly uncertain and its reservation stays held until absence is
confirmed; it is not silently retried.

### Schedule a registered job on macOS

The package schedules recurring work through a LaunchAgent in the current
user's `gui/<uid>` domain. Its property list lives in `~/Library/LaunchAgents`.

Install the hourly rule from the definition above with
`gptel-runner-job-schedule`. Supply a rule as the second argument to save
and install a different schedule in one call.

```elisp
(gptel-runner-job-schedule "parallel-example")

;; Change the trigger to 09:00 every Monday.
(gptel-runner-job-schedule "parallel-example"
                           '(:calendar ((:weekday 1 :hour 9 :minute 0))))
```

After changing a registered job's `:schedule`, call
`gptel-runner-job-schedule` to apply the new rule. To stop future triggers,
remove the installed schedule with the following call. The saved rule stays
available to reinstall, active workers continue, and run history stays on disk.
Both `gptel-runner-job-schedule` and `gptel-runner-job-unschedule` accept a
job ID or saved record and return the updated record.

```elisp
(gptel-runner-job-unschedule "parallel-example")
```

To retire the job entirely, call `(gptel-runner-job-delete "parallel-example")`.
This removes its definition and schedule and preserves its invocation history.

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
refreshes; their history stays on disk. Inspect the registered job's
`:scheduler` property for the last installation result, or open diagnostics
from the jobs view for the underlying `launchctl` output.

User LaunchAgents require the login session.  Calendar triggers missed while
the Mac sleeps coalesce into one trigger on wake; interval triggers can be
missed.  Jobs do not run while the machine is powered off, and cron likewise
misses executions during sleep.  See Apple's
[scheduled jobs guide](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/ScheduledJobs.html)
and [LaunchAgent lifecycle documentation](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html).

## Semantics and safety

- Each agent node has its own conversation for the lifetime of a run. Repeats,
  semantic retries, and output repairs add turns to that conversation.
  Handoffs and replies deliver individual messages. The blackboard, artifacts,
  and prompt functions can carry additional context.
- Decision memory is run-local and append-only.  A recorded decision is added
  to prompts resolved after it was saved.  It cannot alter an agent call that
  is already running; parallel branches should converge before a decision is
  expected to constrain their downstream work.
- A writable workflow is rejected unless the start call includes
  `:allow-writes t`.  Writers sharing a canonical workspace are serialized.
- `workspace-mode` is orchestration metadata. Tool lists, argument policies
  and gptel confirmations govern the tool calls a worker can submit.
  The runner does not create isolated worktrees or enforce read-only access;
  provision separate directories and tool restrictions as needed.
- Set `:allow-unconfirmed-tools t` in `gptel-runner-defworkflow` or in
  `gptel-runner-start` to disable ordinary tool confirmations. With a version
  of `gptel-agent` that supports supervised outside reads, the node and its
  delegated workers and discussions can also use `Read`, `Glob`, `Grep`,
  and `PathInfo` on local paths outside their working directories without
  prompting. Outside edits and other file mutations still require approval.
  Pass `:allow-unconfirmed-tools nil` at startup to override a workflow's
  opt-in for one run.
- A gptel preset can select a named gptel-agent argument policy with
  `:agent-tool-policy 'mr-review`. Define the policy in trusted Emacs
  configuration loaded by the worker. Its allow and deny rules apply even
  with `:allow-unconfirmed-tools t`, including on a transport retry. A chat
  checkpoint retains the policy names and their original workspace roots.
  A node chat saved before its preset gained a policy resumes with its
  saved settings. Start a new invocation to use the newly protected preset.
  For tool-based completion the runner authorizes its own request's
  `RunnerHandoff` tool. To permit follow-up questions, include
  `RunnerAskPrevious` in the argument policy. Workflow-authored Lisp steps
  execute as trusted code in the owning Emacs.
- Side-effecting tools should be idempotent because transport retries have
  at-least-once semantics and arbitrary asynchronous tool effects cannot be
  rolled back generically.
- Request retries reuse one logical call and consume request attempts.  An
  agent-step retry creates a new call.  An implementation revision is a new
  workflow iteration.  These are intentionally separate counters.
- Requests honor `gptel-stream`, including preset overrides and backend/model
  support, just like ordinary gptel chats.  Tool calls and reasoning can stream
  into the worker transcript; a stream ending does not finish the agent until
  gptel's tool loop has completed.
- Agent outputs must be non-empty. With tool-based completion, the message
  submitted to `RunnerHandoff` supplies the result. With `:completion 'response`,
  the final assistant response supplies it. Intermediate commentary stays in
  the transcript. A provider response that reports `tool_calls`, `tool_use`,
  or `function_call` with no tool payload is retried within the request and
  retry limits. In response mode, an empty final answer gets one repair
  attempt before the node fails.
- Checkpoints preserve chat history and the workflow’s accepted results.
  Active requests, approval callbacks, timers, and external tool processes
  end with their owning Emacs session.

## Extension points

Custom agents combine any gptel preset with a runner parser, validator,
schema, retry policy, and metadata.  Custom drivers implement
`gptel-runner-driver-start`, `gptel-runner-driver-cancel`, and optionally
`gptel-runner-driver-pause` (which defaults to stopping external work without
terminalizing the runner call) and `gptel-runner-driver-await-feedback` (which
prepares an already-completed provider transcript for human editing).  Prompt,
branch, repeat-stop, and progress-key functions receive the live run and can
read explicit blackboard values. In the driver completion callback, pass
`success` with the submitted handoff text, `follow-up` with the question for
the preceding agent, or `discussion` with a reply awaiting human feedback.
Parsers and validators remain runner-owned,
so provider schema support is a reliability hint rather than an approval
authority.

## Compatibility

All private gptel references live in `gptel-runner-gptel.el`.  The adapter
checks required entry points before dispatch and reports a `compatibility`
failure with actionable details.  gptel's private preset and request machinery
can change; CI checks v0.9.9.6 and uses current master as an early-warning job.
Worker requests honor gptel's pre/post-tool hooks, request-local tool cleanup,
and steering when a tool call is rejected with new instructions.

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

The integration target checks the plain gptel adapter in a separate Emacs
process before loading gptel-agent for the optional delegation suite. It uses
real state machines and a mocked transport, including a background job worker:

```sh
make integration-test INTEGRATION_LOAD_PATH="-L /path/to/gptel -L /path/to/gptel-agent -L /path/to/compat -L /path/to/yaml.el"
```

Live provider smoke tests are opt-in and excluded from ordinary CI.
