# `gptel-runner`: proposed architecture

Karthink’s boundary is the correct one. `gptel` should remain responsible for a single logical interaction:

```text
request → model response
request → tool call → tool result → model response
```

`gptel-runner` should operate one level above it:

```text
agent invocation → agent result
workflow node → workflow node
implementation → review → revision → approval
parallel proposals → synthesis → execution
```

The package should treat `gptel-request` as its asynchronous execution primitive. That API already supports callbacks, arbitrary state through `:context`, custom FSMs, structured output, and returning an FSM that can be resumed later. The important constraint is that custom FSM support is explicitly described as unstable, so every use of `gptel--...` internals must be isolated behind one compatibility module. ([GitHub][1])

`gptel-agent` is strong prior art. It already models agents as presets containing a system prompt, tools, model/backend options, and other settings. Its subagents have independent contexts and return reports to their caller. Internally, its `Agent` tool applies an agent preset, constructs a specialized FSM, invokes `gptel-request`, accumulates the response, and returns that result through a callback. ([GitHub][2])

The new package generalizes that pattern from **one model delegating a task** to **a deterministic workflow engine coordinating multiple tasks**.

## Responsibility split

```text
┌─────────────────────────────────────────────────────────────┐
│                      gptel-runner UI                        │
│       run dashboard, approvals, logs, abort, resume         │
├─────────────────────────────────────────────────────────────┤
│                    Workflow interpreter                     │
│ sequence, parallel, repeat, branch, gate, convergence       │
├─────────────────────────────────────────────────────────────┤
│                       Run scheduler                         │
│ state, budgets, concurrency, blackboard, event journal      │
├─────────────────────────────────────────────────────────────┤
│                    gptel runner driver                      │
│ worker buffers, FSM handlers, retries, cancellation         │
├─────────────────────────────────────────────────────────────┤
│                           gptel                             │
│ providers, HTTP requests, model parsing, tool-call loops    │
├─────────────────────────────────────────────────────────────┤
│            gptel presets / optional gptel-agent             │
│          agent prompts, models, tools, permissions          │
└─────────────────────────────────────────────────────────────┘
```

The core package should not depend directly on `gptel-agent`. Instead:

* An agent normally references a gptel preset or preset plist.
* `gptel-agent` may optionally provide agent-file parsing and its tool collection.
* The runner should not depend on private variables such as `gptel-agent--agents`.
* The runner should not use the gptel-agent `Agent` tool as its scheduler. That tool can remain available inside an agent, but nested delegation through it is opaque to the workflow engine.

This keeps `gptel-runner` useful with plain gptel presets, custom tools, gptel-agent agents, or completely different agent collections.

---

# Execution terminology

We should use distinct names for three different levels of work.

### Request attempt

One HTTP interaction with a provider. It may fail with `429`, `503`, a timeout, or another transport error.

### Agent call

One logical invocation of an agent. It can contain several provider requests because gptel may perform:

```text
model request
  → tool calls
  → tool results
  → another model request
  → final response
```

A transient request retry remains part of the same agent call.

### Workflow iteration

A semantic repetition such as:

```text
implement → review → revise → review
```

A revision is not a request retry. It is a new agent call with new instructions and a new iteration number.

Keeping these concepts separate will prevent retry policy, accounting, and cancellation from becoming tangled.

---

# Deterministic control plane

The workflow engine should be deterministic Emacs Lisp.

A manager or coordinator LLM can be added as an ordinary workflow node, but it should not control the fundamental scheduler. In particular, the model should not be solely responsible for deciding:

* whether the workflow has finished;
* how many times to retry;
* which agents are allowed;
* whether a reviewer’s approval is sufficient;
* whether the cost or iteration budget has been exhausted;
* whether three agents should continue talking.

These decisions belong to the runner.

The model produces results; the workflow interpreter decides what happens next.

---

# Core data model

The initial model can remain small.

```elisp
(cl-defstruct (gptel-runner-agent
               (:constructor gptel-runner-agent-create))
  "Configuration used to invoke an agent."
  name
  preset
  workspace-mode                 ; read, write, isolated
  schema
  parser
  retry-policy
  metadata)

(cl-defstruct (gptel-runner-node
               (:constructor gptel-runner-node-create))
  "A node in a workflow definition."
  id
  kind                           ; agent, sequence, parallel, repeat, branch, gate
  properties
  children)

(cl-defstruct (gptel-runner-call
               (:constructor gptel-runner-call-create))
  "One logical agent invocation."
  id
  run
  node
  agent
  prompt
  workspace
  buffer
  fsm
  (state 'pending)
  (request-attempt 1)
  retry-timer
  (generation 0)
  response-parts
  result
  error)

(cl-defstruct (gptel-runner-run
               (:constructor gptel-runner-run-create))
  "Runtime state for one workflow execution."
  id
  workflow
  goal
  workspace
  (state 'pending)
  blackboard
  node-states
  active-calls
  events
  budget
  started-at
  finished-at
  callback)
```

Suggested node/call states:

```text
pending
ready
running
retry-wait
waiting-confirmation
succeeded
failed
blocked
stalled
cancelled
skipped
```

All state transitions should be idempotent. A late timer, callback, or tool result must not be able to complete an already cancelled node.

---

# The blackboard

Agents should not share one giant conversational transcript by default.

Instead, each run owns a blackboard:

```elisp
(gptel-runner-put run 'implementation-report report)
(gptel-runner-put run 'review review)
(gptel-runner-put run 'proposal-a proposal)
(gptel-runner-get run 'review)
```

The blackboard contains:

* structured agent results;
* review findings;
* decisions;
* references to files or artifacts;
* test results;
* iteration counters;
* messages explicitly routed between agents.

For coding work, the workspace itself is the primary source of truth. The blackboard records what agents claim to have done, but subsequent agents should inspect the actual files and repository state.

This also matches the useful property of gptel-agent subagents: they do not inherit the entire parent context and return focused reports instead. ([GitHub][2])

## Stateless turns for the first release

The first implementation should make every workflow agent call stateless.

For example, the second implementer invocation receives:

```text
Original goal
Current workflow iteration
Workspace path
Previous reviewer findings
Any relevant constraints
```

It then inspects the workspace again.

We should not initially preserve a private ongoing conversation for every agent because that introduces:

* hidden state;
* context growth;
* complicated persistence;
* unclear retry semantics;
* potentially stale beliefs about files that have changed.

Persistent per-agent sessions can be added later as an explicit option.

---

# Agent definitions

An agent should mostly be a thin reference to a gptel preset.

```elisp
(gptel-make-preset
 'runner-implementer
 :parents 'gptel-agent
 :description "Implements changes and verifies them."
 :system
 "You are the implementation agent. Inspect the workspace, implement the
requested change, run relevant verification, and return a concise report."
 :tools '("Read" "Glob" "Grep" "Edit" "Insert" "Write" "Diagnostics"))

(gptel-make-preset
 'runner-reviewer
 :parents 'gptel-agent
 :description "Reviews changes without modifying them."
 :system
 "You are an independent reviewer. Inspect the implementation and return a
structured verdict. Do not modify files."
 :tools '("Read" "Glob" "Grep" "Diagnostics"))
```

Then register runner-specific metadata:

```elisp
(gptel-runner-register-agent
 'implementer
 :preset 'runner-implementer
 :workspace-mode 'write)

(gptel-runner-register-agent
 'reviewer
 :preset 'runner-reviewer
 :workspace-mode 'read
 :parser #'gptel-runner-parse-review
 :schema gptel-runner-review-schema)
```

Tools in the preset are the real capability boundary. `:workspace-mode` is additional runner metadata and can later be enforced through isolated worktrees, file guards, or tool wrappers.

gptel presets already support model, backend, system message, tools, confirmation policy, request parameters, inheritance, and arbitrary gptel settings. They can also be applied buffer-locally through gptel’s internal preset setter, which is how gptel-agent creates dedicated sessions. ([GitHub][3])

Because the buffer-local setter is currently private, it belongs in `gptel-runner-gptel.el`, not in the public workflow code.

---

# Workflow algebra

The first release needs only five workflow constructs:

```text
agent-step
sequence
parallel
repeat-until
branch
```

A human approval gate can follow shortly afterward.

The workflow should be represented as a tree or AST rather than as nested callback closures. That gives us:

* stable node identifiers;
* inspectable state;
* persistence later;
* meaningful event logs;
* a UI that can display the workflow;
* easy enforcement of budgets and cancellation.

## Target public API

This is the API shape I recommend—not yet drop-in implementation code:

```elisp
(gptel-runner-defworkflow implement-review
  (:max-requests 30
   :max-concurrency 2
   :max-duration 3600)

  (gptel-runner-repeat-until
   :id 'review-cycle
   :max 5

   :until
   (lambda (run)
     (eq (plist-get (gptel-runner-get run 'review) :verdict)
         'pass))

   :stop-when
   (lambda (run)
     (eq (plist-get (gptel-runner-get run 'review) :verdict)
         'blocked))

   :progress-key #'my/review-progress-key

   :body
   (gptel-runner-sequence
    (gptel-runner-agent-step
     :id 'implement
     :agent 'implementer
     :prompt #'my/implementation-prompt
     :save-as 'implementation)

    (gptel-runner-agent-step
     :id 'review
     :agent 'reviewer
     :prompt #'my/review-prompt
     :save-as 'review))))
```

Starting it:

```elisp
(gptel-runner-start
 'implement-review
 :goal "Add bounded status-aware retries to the gptel request runner."
 :workspace (project-root (project-current t)))
```

Prompt functions receive the current run and node:

```elisp
(defun my/implementation-prompt (run _node)
  (let ((review (gptel-runner-get run 'review)))
    (concat
     "Implement the following goal in the current workspace:\n\n"
     (gptel-runner-run-goal run)
     (when review
       (format
        "\n\nThis is revision round %d. Address every applicable review issue:\n\n%S"
        (gptel-runner-iteration run 'review-cycle)
        review))
     "\n\nInspect the actual files before changing them. Run relevant tests."
     "\nReturn a concise report of changes and verification performed.")))
```

---

# Structured review protocol

The reviewer should return a machine-readable value:

```json
{
  "verdict": "pass",
  "summary": "Retry cancellation and terminal handling are correct.",
  "issues": []
}
```

Or:

```json
{
  "verdict": "revise",
  "summary": "Cancellation during backoff is not handled.",
  "issues": [
    {
      "severity": "error",
      "file": "gptel-runner-gptel.el",
      "line": 184,
      "message": "gptel-abort does not cancel the pending retry timer.",
      "suggested_fix": "Invalidate the call generation and cancel its timer."
    }
  ]
}
```

Allowed verdicts:

```text
pass
revise
blocked
```

The runner must parse and validate this result itself. gptel’s `:schema` support can improve reliability, but it is currently experimental and is not supported uniformly by every provider, so it cannot be the only validation mechanism. ([GitHub][1])

Invalid review output should produce a controlled outcome:

1. Ask the reviewer once to repair its output.
2. If still invalid, fail the review node with `invalid-output`.
3. Do not treat malformed output as approval.

## Convergence and stalled workflows

The loop must have more than a maximum iteration count.

A progress key can hash:

* normalized reviewer issues;
* the current Git diff;
* relevant test failures.

For example:

```elisp
(defun my/review-progress-key (run)
  (secure-hash
   'sha256
   (prin1-to-string
    (list
     (my/normalize-review
      (gptel-runner-get run 'review))
     (my/workspace-diff-hash
      (gptel-runner-run-workspace run))))))
```

When the same progress key appears for two or three consecutive iterations, the run becomes `stalled` rather than endlessly alternating between agents.

---

# Three-agent collaboration

For the first multi-agent pattern, use fan-out and synthesis rather than unrestricted group chat.

```elisp
(gptel-runner-defworkflow three-agent-design
  (:max-concurrency 3
   :max-requests 20)

  (gptel-runner-sequence
   (gptel-runner-parallel
    (gptel-runner-agent-step
     :id 'proposal-a
     :agent 'architect-a
     :prompt #'my/architecture-prompt
     :save-as 'proposal-a)

    (gptel-runner-agent-step
     :id 'proposal-b
     :agent 'architect-b
     :prompt #'my/architecture-prompt
     :save-as 'proposal-b)

    (gptel-runner-agent-step
     :id 'proposal-c
     :agent 'architect-c
     :prompt #'my/architecture-prompt
     :save-as 'proposal-c))

   (gptel-runner-agent-step
    :id 'synthesis
    :agent 'arbiter
    :prompt #'my/synthesis-prompt
    :save-as 'decision)))
```

The arbiter prompt receives the three blackboard results. The agents do not need to share contexts or directly call one another.

A later bounded `dialogue` node can support actual rounds of communication:

```elisp
(gptel-runner-dialogue
 :id 'design-dialogue
 :agents '(architect critic implementer)
 :max-rounds 4
 :stop-p #'my/dialogue-consensus-p
 :router #'my/dialogue-router)
```

Messages should use explicit envelopes:

```elisp
(:sender architect
 :recipients (critic implementer)
 :round 2
 :kind proposal
 :content "..."
 :artifacts ("docs/design.md"))
```

Every dialogue must have:

* a maximum number of rounds;
* a stop predicate;
* per-agent and total request budgets;
* explicitly allowed participants;
* a canonical finalizer or arbiter.

An unconstrained “three models talk until they agree” mode is too difficult to terminate, inspect, or reproduce.

---

# Workspace isolation

The first implementation-review workflow may use one shared workspace because the steps are sequential:

```text
implementer writes
reviewer reads
implementer writes
reviewer reads
```

Only one writer should operate on that workspace at a time.

Parallel writers require separate sandboxes, preferably Git worktrees:

```text
run/
  worktrees/
    agent-a/
    agent-b/
    agent-c/
```

A later merge node can compare, cherry-pick, or synthesize their changes. Running multiple writable agents against one directory introduces filesystem races that no prompt can reliably prevent.

The safe default should remain tool confirmation. gptel-agent similarly defaults to confirmation for actions other than safe reads and web retrieval. ([GitHub][2])

Full autonomous writes should require an explicit runner or agent setting and preferably an isolated worktree.

---

# gptel driver design

The workflow core should not require gptel directly.

Define a driver protocol:

```elisp
(cl-defgeneric gptel-runner-driver-start (driver call)
  "Start CALL using DRIVER.")

(cl-defgeneric gptel-runner-driver-cancel (driver call)
  "Cancel CALL using DRIVER.")
```

Then implement:

```elisp
(cl-defstruct gptel-runner-gptel-driver)
(cl-defstruct gptel-runner-fake-driver)
```

This lets ERT tests drive workflows without live API access.

The fake driver can synchronously or asynchronously emit:

```text
success
transient error
permanent error
tool confirmation
abort
late callback
malformed structured result
```

That is essential for testing retry and cancellation deterministically.

---

# One worker buffer per agent call

Every active agent call should own a private worker buffer:

```text
 *gptel-runner:<run-id>:<node-id>*
```

The buffer provides:

* buffer-local preset variables;
* the correct `default-directory`;
* isolation between concurrent calls;
* an unambiguous target for `gptel-abort`;
* a place for gptel tool confirmation UI when needed.

`gptel-abort` currently locates an active request by matching the request FSM’s `:buffer`, invokes its callback with `abort`, runs the request abort function, and transitions that FSM to `ABRT`. That makes one-buffer-per-call the cleanest cancellation identity. ([GitHub][1])

A call start will ultimately resemble:

```elisp
(with-current-buffer worker-buffer
  (setq-local default-directory workspace)
  (setq-local gptel-runner--call call)

  ;; Compatibility wrapper around gptel's buffer-local preset application.
  (gptel-runner--apply-preset-locally
   (gptel-runner-agent-preset agent))

  (let ((fsm (gptel-runner--make-fsm call)))
    (setf (gptel-runner-call-fsm call) fsm)
    (gptel-request prompt
      :buffer worker-buffer
      :stream nil
      :context call
      :schema (gptel-runner-agent-schema agent)
      :callback #'gptel-runner--request-callback
      :fsm fsm)))
```

Starting with `:stream nil` simplifies terminal accounting. Streaming can be added later as an event source.

---

# Request callback

The callback records response data but does not control the workflow:

```elisp
(defun gptel-runner--request-callback (response info)
  "Record RESPONSE for the runner call stored in INFO."
  (when-let ((call (plist-get info :context)))
    (unless (gptel-runner--call-terminal-p call)
      (pcase response
        ((pred stringp)
         (push response (gptel-runner-call-response-parts call))
         (gptel-runner--emit call 'response response))

        (`(reasoning . ,text)
         (gptel-runner--emit call 'reasoning text))

        (`(tool-call . ,calls)
         (gptel-runner--emit call 'tool-calls calls))

        (`(tool-result . ,results)
         (gptel-runner--emit call 'tool-results results))

        ('abort
         ;; ABRT handler performs terminalization.
         nil)

        ('nil
         ;; ERRS handler performs retry or terminalization.
         nil)))))
```

`DONE`, `ERRS`, and `ABRT` FSM handlers own terminal state. This avoids completing a call twice when callback and FSM transitions happen close together.

---

# Three separate retry policies

## 1. Request retry

Retry the same gptel FSM after a transient provider or transport failure.

Typical retryable statuses:

```text
408 425 429 500 502 503 504 529
```

Plus failures where there is no HTTP status because the connection, DNS lookup, TLS session, or curl process failed.

Properties:

* exponential backoff;
* jitter;
* maximum delay;
* maximum retries;
* optional provider-specific policy;
* `Retry-After` support when response headers become accessible;
* no automatic retry for ordinary authentication, validation, or context errors.

gptel’s FSM info contains the entire request context, and its `WAIT` handler explicitly clears transient fields such as `:error`, `:http-status`, `:tool-use`, and token metadata before issuing the next network request. This makes transitioning the same FSM back to `WAIT` the appropriate low-level retry mechanism. ([GitHub][1])

Conceptually:

```elisp
(defun gptel-runner--handle-error (fsm)
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (status (gptel-runner--http-status info)))
    (if (gptel-runner--retryable-p call status)
        (gptel-runner--schedule-retry
         call
         (lambda ()
           ;; Private gptel operation, isolated to this adapter.
           (gptel--fsm-transition fsm 'WAIT)))
      (gptel--handle-post fsm)
      (gptel-runner--finish-call
       call
       'failed
       (list :http-status status
             :status (plist-get info :status)
             :error (plist-get info :error))))))
```

A transient retry should not invoke gptel’s terminal post-handler. Cleanup runs only once after final success, permanent failure, or cancellation.

There is still an at-least-once caveat: when a provider processes a request but the response is lost, the client cannot know whether retrying may generate the same tool call again. Side-effecting tools should therefore be idempotent where possible or accept operation identifiers.

## 2. Agent-step retry

This starts a new agent call from the beginning.

It should be disabled by default for writable agents because it can repeat semantic work. It is suitable for:

* read-only research;
* output-format repair;
* a call known to be idempotent;
* switching to a fallback model.

## 3. Revision iteration

This is an intentional workflow step:

```text
implementation → review → revised implementation
```

It receives reviewer feedback and counts against a separate iteration budget. It is never described as a request retry.

---

# Cancellation and `gptel-abort`

The runner needs two cancellation entry points:

```elisp
(gptel-runner-abort-call call)
(gptel-runner-abort-run run)
```

Run cancellation recursively:

1. marks the run `cancelling`;
2. increments its cancellation generation;
3. cancels every pending retry timer;
4. calls `gptel-abort` for active worker buffers;
5. marks waiting or queued nodes cancelled;
6. prevents late callbacks from scheduling new work;
7. emits one terminal run event.

Calling `gptel-abort` directly in a runner worker buffer must perform the same local invalidation. Use conditional advice that does nothing in ordinary gptel buffers:

```elisp
(defvar-local gptel-runner--call nil
  "Runner call associated with the current worker buffer.")

(defun gptel-runner--invalidate-call (call reason)
  "Invalidate asynchronous work belonging to CALL."
  (when-let ((timer (gptel-runner-call-retry-timer call)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (gptel-runner-call-retry-timer call) nil))
  (cl-incf (gptel-runner-call-generation call))
  (setf (gptel-runner-call-state call) 'cancelling)
  (gptel-runner--emit call 'cancelling reason))

(defun gptel-runner--around-gptel-abort (original buffer)
  "Integrate `gptel-abort' with runner-owned worker buffers."
  (let* ((buffer (or buffer (current-buffer)))
         (call
          (and (buffer-live-p buffer)
               (buffer-local-value 'gptel-runner--call buffer))))
    (when (and call
               (not (gptel-runner--call-terminal-p call)))
      ;; This happens before gptel invokes the abort callback.
      (gptel-runner--invalidate-call call 'gptel-abort))

    (prog1
        (funcall original buffer)

      ;; During retry backoff or a tool-confirmation pause there may be
      ;; no active network process for `gptel-abort' to find.
      (when (and call
                 (not (gptel-runner--call-terminal-p call)))
        (gptel-runner--finish-call call 'cancelled nil)))))

(unless (advice-member-p #'gptel-runner--around-gptel-abort
                         #'gptel-abort)
  (advice-add #'gptel-abort
              :around
              #'gptel-runner--around-gptel-abort))
```

Every retry timer should capture the call generation:

```elisp
(let ((expected-generation
       (gptel-runner-call-generation call)))
  (setf
   (gptel-runner-call-retry-timer call)
   (run-at-time
    delay nil
    (lambda ()
      (when (and
             (= expected-generation
                (gptel-runner-call-generation call))
             (eq (gptel-runner-call-state call)
                 'retry-wait))
        (gptel--fsm-transition
         (gptel-runner-call-fsm call)
         'WAIT))))))
```

One limitation should be explicit: the runner can stop further model requests and ignore late callbacks, but it cannot generically terminate or undo arbitrary asynchronous tool functions. Tools that launch processes should eventually expose their own cancellation function.

---

# Events and observability

Every meaningful state change should generate an event:

```elisp
(:time 1784059912.24
 :run "run-17"
 :node review
 :call "call-8"
 :type request-retry-scheduled
 :data (:attempt 3 :delay 8.4 :http-status 503))
```

Useful event types:

```text
run-started
node-ready
call-started
request-started
tool-calls
tool-results
request-retry-scheduled
call-succeeded
call-failed
node-succeeded
node-stalled
run-cancelled
run-completed
```

The event journal should be the basis for:

* the dashboard;
* debugging;
* usage accounting;
* persistence;
* user hooks;
* tests.

Do not use `message` output as the primary state log.

---

# Package layout

The first version can remain compact:

```text
gptel-runner.el
    public structs, agent registry, run lifecycle, events

gptel-runner-flow.el
    workflow AST and interpreter
    sequence, parallel, repeat, branch

gptel-runner-gptel.el
    worker buffers
    preset application
    gptel-request callback
    custom FSM handlers
    retry and abort integration

test/
    gptel-runner-test.el
    gptel-runner-flow-test.el
    gptel-runner-gptel-test.el
```

Later additions:

```text
gptel-runner-store.el
    snapshots and event persistence

gptel-runner-ui.el
    tabulated run dashboard

gptel-runner-worktree.el
    isolated writable workspaces

gptel-runner-gptel-agent.el
    optional agent-file integration
```

Only `gptel-runner-gptel.el` should reference:

```text
gptel--fsm-transition
gptel--handle-wait
gptel--handle-post
gptel--apply-preset
gptel-send--transitions
other private gptel FSM handlers
```

Because custom FSM behavior is explicitly unstable, CI should test against both a pinned supported gptel version and current gptel master. ([GitHub][1])

---

# Initial development sequence

## Commit 1: execution seam

Implement:

* `gptel-runner-agent`;
* `gptel-runner-call`;
* worker buffer creation;
* buffer-local preset application;
* one custom FSM per call;
* status-aware same-FSM retry;
* cancellation and conditional `gptel-abort` advice;
* event emission;
* idempotent call terminalization.

This is the most technically sensitive part because it touches gptel internals.

## Commit 2: testable runner core

Implement:

* driver protocol;
* fake driver;
* run and node state;
* blackboard;
* `agent-step`;
* `sequence`;
* request and iteration budgets.

Core tests should not make network requests.

## Commit 3: implementation-review workflow

Implement:

* `repeat-until`;
* structured reviewer parser;
* `pass`, `revise`, and `blocked`;
* maximum iterations;
* no-progress detection;
* example implementer/reviewer presets.

This gives the first genuinely useful package release.

## Commit 4: parallel orchestration

Implement:

* `parallel`;
* concurrency limits;
* join semantics;
* failure policies such as `fail-fast`, `collect`, and `minimum-successes`;
* three-agent proposal/synthesis example.

## Commit 5: interaction and persistence

Implement:

* human approval gates;
* dashboard;
* snapshots;
* resume policy;
* Git worktree isolation;
* bounded dialogue workflows.

The first code we should stabilize is therefore **the runner driver and its tests**, not the macro DSL. Once call completion, retry, cancellation, and late-callback behavior are reliable, the implementation-review loop becomes a relatively small workflow interpreter feature.

[1]: https://github.com/karthink/gptel/raw/refs/heads/master/gptel-request.el "raw.githubusercontent.com"
[2]: https://github.com/karthink/gptel-agent/ "GitHub - karthink/gptel-agent: Agent mode for gptel · GitHub"
[3]: https://github.com/karthink/gptel/raw/refs/heads/master/gptel.el "raw.githubusercontent.com"
