;;; gptel-runner-core.el --- Runtime core for deterministic agent workflows -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; State, events, drivers, budgets, scheduling, and cancellation.  This module
;; deliberately has no dependency on gptel.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function gptel-runner-save-run "gptel-runner-store")
(declare-function gptel-runner-schedule-save "gptel-runner-store")
(declare-function gptel-runner-store-cancel-save "gptel-runner-store")
(declare-function gptel-runner--finalize-run "gptel-runner-flow")

(defgroup gptel-runner nil
  "Deterministic workflows with per-node gptel conversations."
  :group 'applications)

(defcustom gptel-runner-retain-worker-buffers t
  "When non-nil, keep node chat buffers open after their run finishes."
  :type 'boolean
  :group 'gptel-runner)

(defcustom gptel-runner-default-driver nil
  "Driver used by `gptel-runner-start' when none is supplied."
  :type 'sexp
  :group 'gptel-runner)

(defvar gptel-runner-event-hook nil
  "Hook run with one argument, the newly appended runner event.")

(defvar gptel-runner-call-admission-functions nil
  "Functions deciding whether a queued call may start.
Each function receives the call and must return non-nil to admit it.  They
run after the ordinary concurrency and workspace checks, and may acquire
external resources.  A refusal leaves the call queued.  Release functions
must tolerate a refusal after another admission function acquired a resource.")

(defvar gptel-runner-call-release-hook nil
  "Hook receiving a call when its admission resources must be released.
Functions must be idempotent.  This hook runs when an active call is
deactivated, and to roll back a refused or failed admission attempt.")

(defvar-local gptel-runner--call nil
  "Runner call associated with the current worker buffer.")
;; Ownership follows the conversation, even when its presentation mode changes.
(put 'gptel-runner--call 'permanent-local t)

(defvar gptel-runner--agents (make-hash-table :test #'eq)
  "Registered agents by symbolic name.")

(defvar gptel-runner--workflows (make-hash-table :test #'eq)
  "Registered workflows by symbolic name.")

(defvar gptel-runner--runs (make-hash-table :test #'equal)
  "Session-local runs by string identifier.")

(defvar gptel-runner--dispatch-deferred-runs nil
  "Runs whose queue dispatch is dynamically deferred during teardown.")

(defvar gptel-runner--next-id 0)

(defvar gptel-runner--session-id
  (substring (secure-hash 'sha256
                          (format "%s %s %s %s" (system-name) (emacs-pid)
                                  (current-time) (random)))
             0 12)
  "Process identifier used to keep saved run and call IDs distinct.")

(defconst gptel-runner-terminal-states
  '(succeeded failed blocked stalled cancelled skipped)
  "States after which an object cannot transition again.")

(defconst gptel-runner--retryable-statuses
  '(408 425 429 500 502 503 504 529))

(defconst gptel-runner-decisions-key 'gptel-runner-decisions
  "Reserved blackboard key containing the run's ordered decision log.")

(defconst gptel-runner-continuations-key 'gptel-runner-continuations
  "Reserved blackboard key containing completed workflow-cycle history.")

(cl-defstruct (gptel-runner-agent
               (:constructor gptel-runner-agent-create))
  "Configuration used to invoke an agent."
  name preset workspace-mode schema parser validator retry-policy metadata
  context)

(cl-defstruct (gptel-runner-node
               (:constructor gptel-runner-node-create))
  "A node in a workflow definition."
  id kind properties children generated-id-p)

(cl-defstruct (gptel-runner-workflow
               (:constructor gptel-runner-workflow-create))
  "A named workflow AST and its default OPTIONS."
  name options root)

(cl-defstruct (gptel-runner-retry-policy
               (:constructor gptel-runner-retry-policy-create))
  "Request retry settings."
  (max-retries 2) (base-delay 1.0) (max-delay 30.0) (jitter 0.2)
  (statuses gptel-runner--retryable-statuses))

(cl-defstruct (gptel-runner-budget
               (:constructor gptel-runner-budget-create))
  "Limits and current accounting for one run."
  max-requests max-calls max-duration (requests 0) (calls 0))

(cl-defstruct (gptel-runner-event
               (:constructor gptel-runner-event-create))
  "One immutable journal entry."
  time run-id node-id call-id type data)

(cl-defstruct (gptel-runner-call
               (:constructor gptel-runner-call-create))
  "One logical agent or Elisp invocation.
AGENT is nil for Elisp calls, which do not use a provider driver.
ITERATION is the one-based iteration of the innermost repeat when queued,
or nil for calls outside repeats and older checkpoints."
  id run node agent prompt workspace buffer fsm
  (state 'pending) (request-attempt 0) retry-timer (generation 0)
  response-parts result error started-at finished-at on-complete
  driver-data repair-p tool-names
  elisp-cleanup elisp-cleanup-reason elisp-timer elisp-deadline
  discussion-owner follow-up-p iteration)

(cl-defstruct (gptel-runner-run
               (:constructor gptel-runner-run-create))
  "Runtime state for one workflow execution."
  id workflow goal workspace (state 'pending) blackboard node-states
  (node-outcomes (make-hash-table :test #'equal))
  (chats (make-hash-table :test #'equal))
  iterations repeat-limits active-calls calls events budget driver queue
  dispatching continuations
  (continuation-epochs (make-hash-table :test #'equal))
  (active-count 0)
  (writer-active 0) started-at finished-at terminal-data
  extension-context finalization
  output-directory artifacts output-epoch
  callback callback-called
  duration-timer duration-remaining active-started-at
  paused-at snapshot-file (generation 0) options
  (child-chats (make-hash-table :test #'equal))
  (discussions (make-hash-table :test #'equal)))

(defun gptel-runner--source-root (source)
  "Return the workflow root represented by SOURCE.
Accept a run, workflow, registered workflow name, or subtree node."
  (cond
   ((gptel-runner-node-p source) source)
   ((gptel-runner-run-p source)
    (gptel-runner--source-root (gptel-runner-run-workflow source)))
   ((gptel-runner-workflow-p source) (gptel-runner-workflow-root source))
   ((and (symbolp source) (gethash source gptel-runner--workflows))
    (gptel-runner-workflow-root (gethash source gptel-runner--workflows)))
   (t (user-error "Not a runner run, workflow, or node: %S" source))))

(defun gptel-runner-map-nodes (function source)
  "Apply FUNCTION to every node in SOURCE and return the results in order.
SOURCE is a run, workflow object, registered workflow name, or subtree node.
Visit the root first, then each child subtree in definition order, including
unexecuted branches and try handlers.  Repeat bodies appear once, regardless
of their iteration count.  FUNCTION receives one node; do not mutate the
workflow tree while traversing it."
  (let (results)
    (cl-labels ((walk (node)
                 (push (funcall function node) results)
                 (mapc #'walk (gptel-runner-node-children node))))
      (walk (gptel-runner--source-root source)))
    (nreverse results)))

(defun gptel-runner-list-nodes (source &optional predicate)
  "Return SOURCE's nodes in root-first definition order.
SOURCE accepts the same forms as `gptel-runner-map-nodes'.  When PREDICATE
is non-nil, include only nodes for which it returns non-nil.  Filtering a
parent does not exclude its descendants.  The returned list is fresh; its
nodes are the actual workflow definitions."
  (let ((nodes (gptel-runner-map-nodes #'identity source)))
    (if predicate (cl-remove-if-not predicate nodes) nodes)))

(defun gptel-runner-find-node (source id)
  "Return the node with ID in SOURCE, or nil if absent.
SOURCE accepts the same forms as `gptel-runner-map-nodes'.  IDs are compared
with `equal'."
  (cl-labels ((find-node (node)
               (if (equal id (gptel-runner-node-id node)) node
                 (cl-some #'find-node (gptel-runner-node-children node)))))
    (find-node (gptel-runner--source-root source))))

(defun gptel-runner-node-path (source node)
  "Return the path from SOURCE's root to NODE, including both endpoints.
NODE is a node object or ID.  Return nil when it is absent.  SOURCE accepts
the same forms as `gptel-runner-map-nodes'."
  (let ((id (if (gptel-runner-node-p node) (gptel-runner-node-id node) node)))
    (cl-labels ((path (current)
                 (if (equal id (gptel-runner-node-id current)) (list current)
                   (when-let* ((tail (cl-some #'path
                                             (gptel-runner-node-children
                                              current))))
                     (cons current tail)))))
      (path (gptel-runner--source-root source)))))

(defun gptel-runner--resolve-node (run node)
  "Resolve NODE, an object or ID, in RUN or signal a user error."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (let ((id (if (gptel-runner-node-p node) (gptel-runner-node-id node) node)))
    (or (gptel-runner-find-node run id)
        (user-error "Unknown node %S in run %s" id (gptel-runner-run-id run)))))

(cl-defgeneric gptel-runner-node-state (run node)
  "Return NODE's current workflow state in RUN.
NODE is a node object or ID; an unknown ID signals a user error.  Nodes not
yet reached are `pending'.  This is workflow state, not the more detailed
state of an individual call, such as retry-wait or waiting-feedback.
RUN may also be a background invocation record for read-only inspection."
  (plist-get (gptel-runner-node-outcome run node) :state))

(cl-defmethod gptel-runner-node-state ((run gptel-runner-run) node)
  "Return the current workflow state of NODE in live RUN."
  (gethash (gptel-runner-node-id (gptel-runner--resolve-node run node))
           (gptel-runner-run-node-states run) 'pending))

(cl-defgeneric gptel-runner-node-outcome (run node)
  "Return NODE's current outcome in RUN as a fresh plist.
NODE is a node object or ID; an unknown ID signals a user error.
The plist contains `:node' and `:state'.  A known terminal outcome also has
`:value': the accepted output on success, or failure/cancellation/skip data
otherwise.  Use `plist-member' to distinguish a known nil value from an
unavailable value.  Pending and running nodes have no `:value'; completed
nodes loaded from older snapshots can also lack it.
Outcomes are independent of calls and `:save-as', and survive checkpoints.
They describe the current execution, not history: resetting a node for a
repeat iteration, retry, or continuation clears its previous outcome.
The value itself is shared with the run and should be treated as read-only.
RUN may also be a background invocation record for read-only inspection.")

(cl-defmethod gptel-runner-node-outcome ((run gptel-runner-run) node)
  "Return the outcome of NODE in live RUN."
  (let* ((id (gptel-runner-node-id (gptel-runner--resolve-node run node)))
         (state (gethash id (gptel-runner-run-node-states run) 'pending))
         (outcome (gethash id (gptel-runner-run-node-outcomes run))))
    (append (list :node id :state state)
            (when (and (gptel-runner--terminal-p state)
                       (eq state (plist-get outcome :state))
                       (plist-member outcome :value))
              (list :value (plist-get outcome :value))))))

(defun gptel-runner-node-result (run node &optional default)
  "Return NODE's successful output in RUN, or DEFAULT if unavailable.
NODE is a node object or ID.  A successful nil output returns nil even when
DEFAULT is non-nil.  RUN may also be a background invocation record.
Use `gptel-runner-node-outcome' for state and failure data."
  (let ((outcome (gptel-runner-node-outcome run node)))
    (if (and (eq (plist-get outcome :state) 'succeeded)
             (plist-member outcome :value))
        (plist-get outcome :value)
      default)))

(defun gptel-runner-node-error (run node &optional default)
  "Return NODE's unsuccessful outcome data in RUN, or DEFAULT.
NODE is a node object or ID.  Unsuccessful states are failed, blocked,
stalled, and cancelled.  Successful, skipped, unfinished, and unavailable
outcomes return DEFAULT.  Known nil error data returns nil.
RUN may also be a background invocation record."
  (let ((outcome (gptel-runner-node-outcome run node)))
    (if (and (memq (plist-get outcome :state) '(failed blocked stalled cancelled))
             (plist-member outcome :value))
        (plist-get outcome :value)
      default)))

(defun gptel-runner--node-workspace (node workspace &optional allow-missing)
  "Resolve NODE's workspace override relative to run WORKSPACE.
An absent or nil override falls back to WORKSPACE.  ALLOW-MISSING permits
restoring historical conversations after their workspace was removed."
  (let* ((override (plist-get (gptel-runner-node-properties node) :workspace))
         (directory
          (if override
              (progn
                (unless (and (stringp override) (not (string-empty-p override)))
                  (user-error "Node %S workspace must be a non-empty directory name"
                              (gptel-runner-node-id node)))
                (file-name-as-directory
                 (file-truename (expand-file-name override workspace))))
            workspace)))
    (when (and directory (not allow-missing) (not (file-directory-p directory)))
      (user-error "Node %S workspace is not a directory: %s"
                  (gptel-runner-node-id node) directory))
    directory))

(defun gptel-runner-node-workspace (run node)
  "Return the effective workspace for agent or Elisp NODE in RUN.
NODE's non-nil `:workspace' overrides the run workspace.  Relative paths
are resolved from the run workspace, and symbolic links are canonicalized.
NODE is a node object or ID.  Use this function in prompts and context
functions that need NODE's directory."
  (gptel-runner--node-workspace (gptel-runner--resolve-node run node)
                                (gptel-runner-run-workspace run)))

(cl-defgeneric gptel-runner-driver-start (driver call complete observe)
  "Start CALL with DRIVER.
COMPLETE accepts STATUS, VALUE, and optional METADATA.  OBSERVE accepts an
event type and data.  Success supplies an accepted handoff.  A `discussion'
status parks an ordinary reply for another turn, and `follow-up' supplies
a question for the preceding agent.  A driver must call COMPLETE at most
once.  The core ignores duplicate and late calls.  METADATA may include
`:retryable t' for a transient protocol failure despite a successful HTTP
status; request budgets, retry limits, and `:permanent' still apply.")

(cl-defgeneric gptel-runner-driver-cancel (driver call)
  "Ask DRIVER to cancel CALL.")

(cl-defgeneric gptel-runner-driver-counts-requests-p (driver)
  "Return non-nil if DRIVER accounts for each provider send itself.
By default the core charges one request per `gptel-runner-driver-start'.
An opting-in driver must check and consume the request budget before every
send, including tool continuations, and finish the call on exhaustion.")

(cl-defmethod gptel-runner-driver-counts-requests-p (_driver)
  "Keep dispatch-based accounting for drivers unless they opt in."
  nil)

(cl-defgeneric gptel-runner-driver-pause (driver call)
  "Pause CALL through DRIVER without terminalizing runner state.")

(cl-defmethod gptel-runner-driver-pause (driver call)
  "Default pause behavior asks DRIVER to cancel CALL's external work."
  (gptel-runner-driver-cancel driver call))

(cl-defgeneric gptel-runner-driver-await-feedback (driver call)
  "Prepare completed CALL through DRIVER for human feedback.")

(cl-defmethod gptel-runner-driver-await-feedback (_driver _call)
  "Default completed-call feedback behavior requires no driver action."
  nil)

(defun gptel-runner--id (prefix)
  "Return a new identifier beginning with PREFIX."
  (format "%s-%d" (if (member prefix '("run" "call"))
                       (concat prefix "-" gptel-runner--session-id)
                     prefix)
          (cl-incf gptel-runner--next-id)))

(defun gptel-runner--terminal-p (state)
  "Return non-nil when STATE is terminal."
  (memq state gptel-runner-terminal-states))

(defun gptel-runner--call-terminal-p (call)
  "Return non-nil when CALL is terminal."
  (gptel-runner--terminal-p (gptel-runner-call-state call)))

(defun gptel-runner--buffer-call (&optional buffer)
  "Return the call owned by BUFFER, defaulting to the current buffer.
A copied buffer-local call is not proof of ownership."
  (let ((buffer (or buffer (current-buffer))))
    (when-let* (((buffer-live-p buffer))
                (call (buffer-local-value 'gptel-runner--call buffer))
                ((gptel-runner-call-p call))
                ((eq buffer (gptel-runner-call-buffer call))))
      call)))

(defun gptel-runner--run-terminal-p (run)
  "Return non-nil when RUN is terminal."
  (gptel-runner--terminal-p (gptel-runner-run-state run)))

(defun gptel-runner--empty-output-p (value)
  "Return non-nil when VALUE is not a usable agent output."
  (or (null value)
      (and (stringp value) (string-blank-p value))))

(defun gptel-runner--empty-output-error ()
  "Return the structured failure used for an empty agent output."
  (list :type 'invalid-output :reason 'empty-response))

(defun gptel-runner--empty-output-error-p (value)
  "Return non-nil when VALUE describes an empty agent output failure."
  (and (listp value)
       (eq (plist-get value :type) 'invalid-output)
       (eq (plist-get value :reason) 'empty-response)))

(defun gptel-runner--emit (run type &optional node call data)
  "Append a TYPE event for RUN, NODE, and CALL containing DATA."
  (let ((event (gptel-runner-event-create
                :time (float-time)
                :run-id (gptel-runner-run-id run)
                :node-id (and node (gptel-runner-node-id node))
                :call-id (and call (gptel-runner-call-id call))
                :type type :data data)))
    (setf (gptel-runner-run-events run)
          (nconc (gptel-runner-run-events run) (list event)))
    (run-hook-with-args 'gptel-runner-event-hook event)
    event))

(defun gptel-runner-register-agent (name &rest properties)
  "Register NAME using PROPERTIES and return the new agent.
Recognized properties include `:preset', `:workspace-mode', `:schema',
`:parser', `:validator', `:retry-policy', `:metadata', and `:context'."
  (unless (symbolp name)
    (user-error "Agent name must be a symbol: %S" name))
  (let ((mode (or (plist-get properties :workspace-mode) 'read)))
    (unless (memq mode '(read write isolated))
      (user-error "Invalid workspace mode for %S: %S" name mode))
    (let ((agent
           (gptel-runner-agent-create
            :name name :preset (plist-get properties :preset)
            :workspace-mode mode :schema (plist-get properties :schema)
            :parser (plist-get properties :parser)
            :validator (plist-get properties :validator)
            :retry-policy (or (plist-get properties :retry-policy)
                              (gptel-runner-retry-policy-create))
            :metadata (plist-get properties :metadata)
            :context (plist-get properties :context))))
      (puthash name agent gptel-runner--agents)
      agent)))

(defun gptel-runner-unregister-agent (name)
  "Remove agent NAME and return non-nil when it existed."
  (prog1 (gethash name gptel-runner--agents)
    (remhash name gptel-runner--agents)))

(defun gptel-runner-unregister-workflow (name)
  "Remove registered workflow NAME and return it when it existed."
  (prog1 (gethash name gptel-runner--workflows)
    (remhash name gptel-runner--workflows)))

(defun gptel-runner--agent (name)
  "Return registered agent NAME or signal a user error."
  (or (gethash name gptel-runner--agents)
      (user-error "Unknown gptel-runner agent: %S" name)))

(cl-defgeneric gptel-runner-outputs (source)
  "Return SOURCE's named values as a fresh alist.
SOURCE is a run or background invocation record.  Include inputs and runner
history as well as values saved with `:save-as' or `gptel-runner-put'.
Values are shared with live runs and should be treated as read-only.")

(cl-defmethod gptel-runner-outputs ((source gptel-runner-run))
  "Return the named values of live SOURCE."
  (let (entries)
    (maphash (lambda (key value) (push (cons key value) entries))
             (gptel-runner-run-blackboard source))
    (nreverse entries)))

(cl-defgeneric gptel-runner-get (run key &optional default)
  "Return RUN's named value for KEY, or DEFAULT if absent.
RUN is a live run or background invocation record.  A stored nil value
returns nil even when DEFAULT is non-nil."
  (alist-get key (gptel-runner-outputs run) default nil #'equal))

(cl-defmethod gptel-runner-get ((run gptel-runner-run) key &optional default)
  "Return live RUN's blackboard value for KEY, or DEFAULT."
  (gethash key (gptel-runner-run-blackboard run) default))

(defun gptel-runner-put (run key value)
  "Store VALUE at KEY on RUN's blackboard and return VALUE."
  (puthash key value (gptel-runner-run-blackboard run))
  (gptel-runner--checkpoint run)
  value)

(defun gptel-runner-decisions (run)
  "Return a copy of RUN's ordered decision log."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (copy-tree (gptel-runner-get run gptel-runner-decisions-key) t))

(defun gptel-runner-continuations (run)
  "Return a copy of RUN's ordered continuation history.
Each entry records the preceding goal, terminal state and workflow outputs.
Its observation is the new goal for a continuation, or nil for a rerun."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (copy-tree (gptel-runner-get run gptel-runner-continuations-key) t))

(defun gptel-runner--latest-continuation (run)
  "Return RUN's latest continuation entry without copying its full history."
  (car (last (gptel-runner-get run gptel-runner-continuations-key))))

(defun gptel-runner-decision-memory-p (run)
  "Return non-nil when automatic decision propagation is enabled for RUN.
Decision memory defaults to enabled when the option is absent, including for
snapshots created before the option existed."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (let ((options (gptel-runner-run-options run)))
    (if (plist-member options :decision-memory)
        (plist-get options :decision-memory)
      t)))

(defun gptel-runner-record-decision (run text &optional rationale call)
  "Append decision TEXT with optional RATIONALE to RUN and return its entry.
RUN may be a run object or its displayed string identifier.
When CALL is supplied, record its call and node as the decision source.  In a
runner worker buffer, the current call is inferred when it belongs to RUN.
The entry is written to the blackboard, journaled, and checkpointed."
  (when (stringp run)
    (setq run (or (gptel-runner-find-run run)
                  (user-error "Unknown gptel-runner run: %s" run))))
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (unless (stringp text)
    (user-error "Decision text must be a string"))
  (setq text (string-trim text))
  (when (string-empty-p text)
    (user-error "Decision text cannot be empty"))
  (when (and rationale (not (stringp rationale)))
    (user-error "Decision rationale must be a string or nil"))
  (when rationale
    (setq rationale (string-trim rationale))
    (when (string-empty-p rationale)
      (setq rationale nil)))
  (when (and call (not (gptel-runner-call-p call)))
    (user-error "Decision source is not a runner call: %S" call))
  (when (and call (not (eq (gptel-runner-call-run call) run)))
    (user-error "Decision source call belongs to a different run"))
  (let* ((source-call
          (or call
              (when-let* ((owner (gptel-runner--buffer-call))
                          ((eq (gptel-runner-call-run owner) run)))
                owner)))
         (node (and source-call (gptel-runner-call-node source-call)))
         (entry
          (list :id (gptel-runner--id "decision")
                :text text
                :rationale rationale
                :node-id (and node (gptel-runner-node-id node))
                :call-id (and source-call
                              (gptel-runner-call-id source-call))
                :created-at (float-time)))
         (blackboard (or (gptel-runner-run-blackboard run)
                         (setf (gptel-runner-run-blackboard run)
                               (make-hash-table :test #'equal))))
         (decisions (gethash gptel-runner-decisions-key blackboard)))
    (puthash gptel-runner-decisions-key
             (append decisions (list entry)) blackboard)
    (gptel-runner--emit run 'decision-recorded node source-call entry)
    (gptel-runner--checkpoint run)
    (copy-tree entry t)))

(defun gptel-runner-format-decisions (run)
  "Format RUN's ordered decisions for inclusion in an agent prompt."
  (mapconcat
   (lambda (entry)
     (let ((id (plist-get entry :id))
           (text (replace-regexp-in-string
                  "\n" "\n  " (plist-get entry :text)))
           (rationale (plist-get entry :rationale))
           (node-id (plist-get entry :node-id)))
       (concat
        (format "[%s] %s" id text)
        (if rationale
            (format "\n  Reason: %s"
                    (replace-regexp-in-string "\n" "\n  " rationale))
          "")
        (if node-id (format "\n  Recorded by node: %s" node-id) ""))))
   (gptel-runner-decisions run)
   "\n\n"))

(defun gptel-runner-iteration (run node-id)
  "Return RUN's completed iteration count for NODE-ID."
  (gethash node-id (gptel-runner-run-iterations run) 0))

(defun gptel-runner-list-runs ()
  "Return a list of all session-local run objects, newest first."
  (let (runs)
    (maphash (lambda (_id run) (push run runs)) gptel-runner--runs)
    (sort runs (lambda (a b)
                 (> (gptel-runner-run-started-at a)
                    (gptel-runner-run-started-at b))))))

(defun gptel-runner-find-run (id)
  "Return the session-local run identified by string ID, or nil."
  (and (stringp id) (gethash id gptel-runner--runs)))

(defun gptel-runner--forgettable-run-p (run)
  "Return non-nil when RUN can be safely removed from session state."
  (or (gptel-runner--run-terminal-p run)
      (eq (gptel-runner-run-state run) 'paused)))

(defun gptel-runner-forget-run (run &optional delete-snapshot)
  "Remove RUN from session state and kill its inspection buffers.
RUN may be a run object or its identifier.  It must be terminal or paused.
When DELETE-SNAPSHOT is non-nil, also delete its durable snapshot file."
  (when (stringp run)
    (setq run (gethash run gptel-runner--runs)))
  (unless (gptel-runner-run-p run)
    (user-error "Unknown gptel-runner run"))
  (unless (gptel-runner--forgettable-run-p run)
    (user-error "Run %s is active; abort or pause it before removing it"
                (gptel-runner-run-id run)))
  (when (and (not delete-snapshot) (gptel-runner--persistent-p run)
             (fboundp 'gptel-runner-store-flush-run))
    (gptel-runner-store-flush-run run))
  (when (fboundp 'gptel-runner-store-cancel-save)
    (gptel-runner-store-cancel-save run))
  ;; The last checkpoint already owns these chats.  Closing the buffers must
  ;; not enqueue another save for a run that is leaving the registry.
  (clrhash (gptel-runner-run-chats run))
  (let* ((children (gptel-runner-run-child-chats run))
         (buffers (mapcar (lambda (record) (plist-get record :chat))
                          (hash-table-values children))))
    (clrhash children)
    (dolist (buffer buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  (dolist (call (gptel-runner-run-calls run))
    (when (buffer-live-p (gptel-runner-call-buffer call))
      (kill-buffer (gptel-runner-call-buffer call))
      (setf (gptel-runner-call-buffer call) nil)))
  (when-let* ((events-buffer
              (get-buffer (format "*gptel-runner events:%s*"
                                  (gptel-runner-run-id run)))))
    (kill-buffer events-buffer))
  (when (and delete-snapshot
             (gptel-runner-run-snapshot-file run)
             (file-exists-p (gptel-runner-run-snapshot-file run)))
    (delete-file (gptel-runner-run-snapshot-file run)))
  (remhash (gptel-runner-run-id run) gptel-runner--runs)
  run)

(defun gptel-runner-forget-workflow (name &optional delete-snapshots)
  "Remove workflow NAME registration and retained run history.
Signal if any associated run is still active.  When DELETE-SNAPSHOTS is
non-nil, also delete snapshot files belonging to the removed runs."
  (let* ((workflow (gethash name gptel-runner--workflows))
         (runs
          (cl-remove-if-not
           (lambda (run)
             (eq name (gptel-runner-workflow-name
                       (gptel-runner-run-workflow run))))
           (gptel-runner-list-runs)))
         (active (cl-find-if-not #'gptel-runner--forgettable-run-p runs)))
    (unless (or workflow runs)
      (user-error "Unknown gptel-runner workflow: %S" name))
    (when active
      (user-error "Workflow %S has active run %s; abort or pause it first"
                  name (gptel-runner-run-id active)))
    (dolist (run runs)
      (gptel-runner-forget-run run delete-snapshots))
    (gptel-runner-unregister-workflow name)
    workflow))

(defun gptel-runner--set-node-state (run node state &optional data handoff)
  "Set NODE state in RUN to STATE and emit its transition with DATA.
HANDOFF is the raw message accepted for this execution of an agent node."
  (let* ((states (gptel-runner-run-node-states run))
         (id (gptel-runner-node-id node))
         (old (gethash id states 'pending)))
    (unless (or (eq old state) (gptel-runner--terminal-p old))
      (puthash id state states)
      (if (gptel-runner--terminal-p state)
          (puthash id (append (list :state state :value data)
                              (when handoff (list :handoff handoff)))
                   (gptel-runner-run-node-outcomes run))
        (remhash id (gptel-runner-run-node-outcomes run)))
      (gptel-runner--emit run (intern (format "node-%s" state)) node nil data)
      t)))

(defun gptel-runner--reset-node-state (run id)
  "Reset node ID to pending in RUN and discard its previous outcome."
  (puthash id 'pending (gptel-runner-run-node-states run))
  (remhash id (gptel-runner-run-node-outcomes run)))

(defun gptel-runner--persistent-p (run)
  "Return non-nil when RUN has durable snapshotting enabled."
  (plist-get (gptel-runner-run-options run) :persist))

(defun gptel-runner--checkpoint (run &optional immediate)
  "Persist RUN at a safe checkpoint when persistence is enabled.
When IMMEDIATE is non-nil, bypass the automatic checkpoint debounce."
  (when (and (gptel-runner--persistent-p run)
             (fboundp 'gptel-runner-save-run))
    (condition-case err
        (if (fboundp 'gptel-runner-schedule-save)
            (gptel-runner-schedule-save run immediate)
          (gptel-runner-save-run run))
      (error
       (gptel-runner--emit run 'snapshot-error nil nil err)
       nil))))

(defun gptel-runner--stop-duration-clock (run)
  "Stop RUN's active-duration clock and preserve remaining seconds."
  (when (timerp (gptel-runner-run-duration-timer run))
    (cancel-timer (gptel-runner-run-duration-timer run)))
  (setf (gptel-runner-run-duration-timer run) nil)
  (when (and (numberp (gptel-runner-run-duration-remaining run))
             (numberp (gptel-runner-run-active-started-at run)))
    (setf (gptel-runner-run-duration-remaining run)
          (max 0 (- (gptel-runner-run-duration-remaining run)
                    (- (float-time)
                       (gptel-runner-run-active-started-at run))))))
  (setf (gptel-runner-run-active-started-at run) nil))

(defun gptel-runner--start-duration-clock (run)
  "Start or resume RUN's active-duration clock."
  (when-let* ((remaining (gptel-runner-run-duration-remaining run)))
    (if (<= remaining 0)
        (gptel-runner--duration-expired
         run (gptel-runner-run-generation run))
      (setf (gptel-runner-run-active-started-at run) (float-time)
            (gptel-runner-run-duration-timer run)
            (run-at-time remaining nil #'gptel-runner--duration-expired
                         run (gptel-runner-run-generation run))))))

(defun gptel-runner--finish-run (run state &optional data)
  "Finish RUN as STATE with DATA, allowing explicit run finalization."
  (unless (gptel-runner--run-terminal-p run)
    (if (and (gptel-runner-run-workflow run)
             (eq (gptel-runner-node-kind (gptel-runner--source-root run))
                 'finalize))
        (gptel-runner--finalize-run run state data)
      (gptel-runner--terminalize-run run state data))))

(defun gptel-runner--terminalize-run (run state &optional data)
  "Terminalize RUN as STATE once, recording DATA."
  (unless (gptel-runner--run-terminal-p run)
    (setf (gptel-runner-run-state run) state
          (gptel-runner-run-finished-at run) (float-time)
          (gptel-runner-run-terminal-data run) data)
    (gptel-runner--stop-duration-clock run)
    ;; Cancellation invalidates continuations, including a try's completion
    ;; closure and paused calls.  Finish entered nodes those closures can no
    ;; longer terminalize, while retaining pending nodes as never reached.
    (when (and (not (eq state 'succeeded)) (gptel-runner-run-workflow run))
      (gptel-runner-map-nodes
       (lambda (node)
         (when (eq (gethash (gptel-runner-node-id node)
                            (gptel-runner-run-node-states run)) 'running)
           (gptel-runner--set-node-state run node state data)))
       run))
    (gptel-runner--emit run
                        (if (eq state 'succeeded)
                            'run-completed
                          (intern (format "run-%s" state)))
                        nil nil data)
    (unless (gptel-runner-run-callback-called run)
      (setf (gptel-runner-run-callback-called run) t)
      (when-let* ((callback (gptel-runner-run-callback run)))
        (funcall callback run)))
    (gptel-runner--checkpoint run t)
    t))

(defun gptel-runner--budget-failure (run kind limit)
  "Return a structured budget failure for RUN of KIND at LIMIT."
  (list :type 'budget :kind kind :limit limit
        :requests (gptel-runner-budget-requests
                   (gptel-runner-run-budget run))
        :calls (gptel-runner-budget-calls
                (gptel-runner-run-budget run))))

(defun gptel-runner--consume-call (run)
  "Consume one logical call budget from RUN and return non-nil on success."
  (let* ((budget (gptel-runner-run-budget run))
         (next (1+ (gptel-runner-budget-calls budget)))
         (max (gptel-runner-budget-max-calls budget)))
    (if (and max (> next max))
        nil
      (setf (gptel-runner-budget-calls budget) next)
      t)))

(defun gptel-runner--consume-request (run)
  "Consume one provider attempt budget from RUN and return non-nil."
  (let* ((budget (gptel-runner-run-budget run))
         (next (1+ (gptel-runner-budget-requests budget)))
         (max (gptel-runner-budget-max-requests budget)))
    (if (and max (> next max))
        nil
      (setf (gptel-runner-budget-requests budget) next)
      t)))

(defun gptel-runner--request-budget-available-p (run)
  "Return non-nil when RUN can dispatch another provider attempt."
  (let* ((budget (gptel-runner-run-budget run))
         (maximum (gptel-runner-budget-max-requests budget)))
    (or (null maximum)
        (< (gptel-runner-budget-requests budget) maximum))))

(defun gptel-runner--elisp-call-p (call)
  "Return non-nil when CALL executes an Elisp step."
  (eq (gptel-runner-node-kind (gptel-runner-call-node call)) 'elisp))

(defun gptel-runner--writer-p (call)
  "Return non-nil when CALL declares writes to its workspace."
  (eq (if (gptel-runner--elisp-call-p call)
          (plist-get (gptel-runner-node-properties (gptel-runner-call-node call))
                     :workspace-mode)
        (gptel-runner-agent-workspace-mode (gptel-runner-call-agent call)))
      'write))

(defun gptel-runner--call-observe (call generation type data)
  "Record a TYPE driver observation for CALL at GENERATION using DATA."
  (when (and (= generation (gptel-runner-call-generation call))
             (not (gptel-runner--call-terminal-p call)))
    (let ((run (gptel-runner-call-run call)))
      (cond
       ((eq type 'waiting-tool)
        (setf (gptel-runner-call-state call) 'waiting-tool
              (gptel-runner-call-tool-names call) data))
       ((eq type 'waiting-confirmation)
        (setf (gptel-runner-call-state call) 'waiting-confirmation))
       ((and (eq type 'tool-results)
             (memq (gptel-runner-call-state call)
                   '(waiting-tool waiting-confirmation)))
        (setf (gptel-runner-call-state call) 'running
              (gptel-runner-call-tool-names call) nil)))
      (gptel-runner--emit run type (gptel-runner-call-node call) call data))))

(defun gptel-runner--retryable-p (call status metadata)
  "Return non-nil if CALL may retry STATUS described by METADATA."
  (let* ((policy (gptel-runner-agent-retry-policy
                  (gptel-runner-call-agent call)))
         (attempt (gptel-runner-call-request-attempt call)))
    (and (gptel-runner--request-budget-available-p
          (gptel-runner-call-run call))
         (< attempt (1+ (gptel-runner-retry-policy-max-retries policy)))
         (or (null status)
             (memq status (gptel-runner-retry-policy-statuses policy))
             (plist-get metadata :retryable))
         (not (plist-get metadata :permanent)))))

(defun gptel-runner--retry-delay (call metadata)
  "Compute CALL retry delay, respecting METADATA's retry-after value."
  (let* ((policy (gptel-runner-agent-retry-policy
                  (gptel-runner-call-agent call)))
         (retry-after (plist-get metadata :retry-after))
         (base (* (gptel-runner-retry-policy-base-delay policy)
                  (expt 2 (max 0 (1- (gptel-runner-call-request-attempt call))))))
         (capped (min (gptel-runner-retry-policy-max-delay policy)
                      (or retry-after base)))
         (jitter (gptel-runner-retry-policy-jitter policy)))
    (if (zerop jitter) capped
      (max 0 (* capped (+ 1 (- (* 2 jitter (cl-random 1.0)) jitter)))))))

(defun gptel-runner--deactivate-call (call)
  "Release CALL's concurrency and writer slots if currently active."
  (let ((run (gptel-runner-call-run call)))
    (when (memq call (gptel-runner-run-active-calls run))
      (setf (gptel-runner-run-active-calls run)
            (delq call (gptel-runner-run-active-calls run)))
      (cl-decf (gptel-runner-run-active-count run))
      (when (gptel-runner--writer-p call)
        (cl-decf (gptel-runner-run-writer-active run)))
      (run-hook-with-args 'gptel-runner-call-release-hook call)
      t)))

(defun gptel-runner--schedule-request-retry (call metadata)
  "Schedule a transient request retry for CALL using METADATA."
  (let* ((run (gptel-runner-call-run call))
         (delay (gptel-runner--retry-delay call metadata))
         (generation (gptel-runner-call-generation call)))
    (setf (gptel-runner-call-state call) 'retry-wait
          (gptel-runner-call-tool-names call) nil)
    (gptel-runner--deactivate-call call)
    (gptel-runner--emit
     run 'request-retry-scheduled (gptel-runner-call-node call) call
     (list :attempt (1+ (gptel-runner-call-request-attempt call))
           :delay delay :http-status (plist-get metadata :http-status)))
    (setf (gptel-runner-call-retry-timer call)
          (run-at-time
           delay nil
           (lambda ()
             (when (and (= generation (gptel-runner-call-generation call))
                        (eq (gptel-runner-call-state call) 'retry-wait)
                        (not (gptel-runner--run-terminal-p run)))
               (setf (gptel-runner-call-retry-timer call) nil)
               (setf (gptel-runner-call-state call) 'ready
                     (gptel-runner-run-queue run)
                     (nconc (gptel-runner-run-queue run) (list call)))
               (gptel-runner--drain-queue run)))))
    (gptel-runner--drain-queue run)))

(defun gptel-runner--driver-result (call generation status value metadata)
  "Handle driver STATUS for CALL at GENERATION with VALUE and METADATA."
  (when (and (= generation (gptel-runner-call-generation call))
             (not (gptel-runner--call-terminal-p call)))
    (pcase status
      ('discussion
       (if (gptel-runner--empty-output-p value)
           (gptel-runner--finish-call call 'failed (gptel-runner--empty-output-error))
         (setf (gptel-runner-call-result call) value
               (gptel-runner-call-driver-data call)
               (plist-put (gptel-runner-call-driver-data call) :discussion-wait t))
         (gptel-runner--suspend-call call 'waiting-feedback 'discussion t t)
         (gptel-runner--checkpoint (gptel-runner-call-run call) t)))
      ('follow-up
       (setf (gptel-runner-call-follow-up-p call) t)
       (gptel-runner--finish-call call 'succeeded value))
      ('success
       (if (gptel-runner--empty-output-p value)
           (gptel-runner--finish-call
            call 'failed (gptel-runner--empty-output-error))
         (if (and (not (gptel-runner-call-discussion-owner call))
                  (plist-get (gptel-runner-node-properties
                              (gptel-runner-call-node call)) :pause-after))
             (progn
               (setf (gptel-runner-call-result call) value)
               (gptel-runner--suspend-call
                call 'waiting-feedback 'pause-after t t)
               (gptel-runner--checkpoint (gptel-runner-call-run call) t))
           (gptel-runner--finish-call call 'succeeded value))))
      ('cancelled (gptel-runner--finish-call call 'cancelled value))
      ('blocked (gptel-runner--finish-call call 'blocked value))
      ('transient
       (let ((http-status (plist-get metadata :http-status)))
         (if (gptel-runner--retryable-p call http-status metadata)
             (gptel-runner--schedule-request-retry call metadata)
           (if (not (gptel-runner--request-budget-available-p
                     (gptel-runner-call-run call)))
               (let ((run (gptel-runner-call-run call)))
                 (gptel-runner--finish-call
                  call 'failed
                  (gptel-runner--budget-failure
                   run 'requests
                   (gptel-runner-budget-max-requests
                    (gptel-runner-run-budget run)))))
             (gptel-runner--finish-call
              call 'failed (list :type 'request-error :value value
                                 :metadata metadata))))))
      (_ (gptel-runner--finish-call
          call 'failed (list :type (or status 'driver-error)
                             :value value :metadata metadata))))))

(defun gptel-runner--elisp-current-p (call generation)
  "Return non-nil if CALL can accept an Elisp result from GENERATION."
  (and (= generation (gptel-runner-call-generation call))
       (eq (gptel-runner-call-state call) 'running)
       (eq (gptel-runner-run-state (gptel-runner-call-run call)) 'running)
       (not (gptel-runner-call-elisp-cleanup-reason call))))

(defun gptel-runner--cleanup-elisp (call reason)
  "Stop CALL's Elisp timer and invoke its cleanup once with REASON.
Remember the first reason even if startup has not returned its cleanup yet.
Cleanup errors are journaled without interrupting scheduler teardown."
  (unless (gptel-runner-call-elisp-cleanup-reason call)
    (setf (gptel-runner-call-elisp-cleanup-reason call) reason))
  (when (timerp (gptel-runner-call-elisp-timer call))
    (cancel-timer (gptel-runner-call-elisp-timer call)))
  (setf (gptel-runner-call-elisp-timer call) nil)
  (when-let* ((cleanup (gptel-runner-call-elisp-cleanup call)))
    ;; Detach before invoking user code, which may trigger a process sentinel.
    (setf (gptel-runner-call-elisp-cleanup call) nil)
    (condition-case err
        (with-temp-buffer
          (setq default-directory (gptel-runner-call-workspace call))
          (funcall cleanup (gptel-runner-call-elisp-cleanup-reason call)))
      ((error quit)
       (gptel-runner--emit (gptel-runner-call-run call) 'elisp-cleanup-error
                           (gptel-runner-call-node call) call err)))))

(defun gptel-runner--elisp-result (call generation state value)
  "Accept CALL's Elisp STATE and VALUE if GENERATION is still current."
  (when (gptel-runner--elisp-current-p call generation)
    (let ((run (gptel-runner-call-run call))
          (timeout (plist-get (gptel-runner-node-properties
                               (gptel-runner-call-node call)) :timeout)))
      ;; CPU-bound Lisp cannot service timers.  Check elapsed time before
      ;; publishing output or starting a retry or another workflow node.
      (cond
       ((and (gptel-runner-run-duration-remaining run)
             (gptel-runner-run-active-started-at run)
             (>= (- (float-time) (gptel-runner-run-active-started-at run))
                 (gptel-runner-run-duration-remaining run)))
        (gptel-runner--duration-expired run (gptel-runner-run-generation run)))
       ((and (gptel-runner-call-elisp-deadline call)
             (>= (float-time) (gptel-runner-call-elisp-deadline call)))
        (gptel-runner--finish-call
         call 'failed (list :type 'elisp-timeout :timeout timeout)))
       (t (gptel-runner--finish-call call state value))))))

(defun gptel-runner--dispatch-elisp (call)
  "Start CALL's synchronous or asynchronous Elisp function.
Defer inline completion until startup returns and installs its cleanup.
Run continuations outside the startup error handler so downstream errors
are never attributed to this function."
  (let* ((run (gptel-runner-call-run call))
         (node (gptel-runner-call-node call))
         (props (gptel-runner-node-properties node))
         (generation (gptel-runner-call-generation call))
         (complete (gptel-runner--continuation
                    run (lambda (state value)
                          (gptel-runner--elisp-result
                           call generation state value))
                    node))
         (timeout (plist-get props :timeout))
         (starting t)
         outcome)
    (when (and (eq (gptel-runner-run-state run) 'running)
               (not (gptel-runner--call-terminal-p call)))
      (setf (gptel-runner-call-state call) 'running)
      (cl-labels
          ((deliver (state value)
             (when (and (not outcome)
                        (gptel-runner--elisp-current-p call generation))
               (setq outcome (cons state value))
               (unless starting
                 (funcall complete state value))
               t)))
        (condition-case err
            (progn
              (when timeout
                (setf (gptel-runner-call-elisp-deadline call)
                      (+ (float-time) timeout)
                      (gptel-runner-call-elisp-timer call)
                      (run-at-time
                       timeout nil
                       (lambda ()
                         (deliver 'failed
                                  (list :type 'elisp-timeout
                                        :timeout timeout))))))
              (with-temp-buffer
                (setq default-directory (gptel-runner-call-workspace call))
                (if (plist-get props :async)
                    (let ((cleanup
                           (funcall
                            (plist-get props :function) run node
                            (lambda (value) (deliver 'succeeded value))
                            (lambda (error-data)
                              (deliver 'failed
                                       (list :type 'elisp-error
                                             :error error-data))))))
                      (unless (or (null cleanup) (functionp cleanup))
                        (error "Async Elisp function must return nil or a cleanup function"))
                      (setf (gptel-runner-call-elisp-cleanup call) cleanup))
                  (deliver 'succeeded
                           (funcall (plist-get props :function) run node)))))
          (error (deliver 'failed (list :type 'elisp-error :error err)))
          (quit (deliver 'cancelled (list :type 'elisp-quit :error err))))
        (setq starting nil)
        (if-let* ((reason (gptel-runner-call-elisp-cleanup-reason call)))
            ;; Cancellation or pause may have happened while startup yielded.
            (gptel-runner--cleanup-elisp call reason)
          (when outcome
            (funcall complete (car outcome) (cdr outcome))))))))

(defun gptel-runner--dispatch-attempt (call)
  "Dispatch one provider attempt for CALL."
  (let* ((run (gptel-runner-call-run call))
         (run-generation (gptel-runner-run-generation run))
         (driver-counts (gptel-runner-driver-counts-requests-p
                         (gptel-runner-run-driver run))))
    ;; Admission and call-started observers may have paused or cancelled the
    ;; call, including by yielding to an interactive command.  Never revive it.
    (when (and (eq (gptel-runner-run-state run) 'running)
               (eq (gptel-runner-call-state call) 'ready)
               (memq call (gptel-runner-run-active-calls run)))
      (if (not (or driver-counts (gptel-runner--consume-request run)))
          (gptel-runner--finish-call
           call 'failed
           (gptel-runner--budget-failure
            run 'requests
            (gptel-runner-budget-max-requests (gptel-runner-run-budget run))))
        (cl-incf (gptel-runner-call-request-attempt call))
        (setf (gptel-runner-call-state call) 'running
              (gptel-runner-call-tool-names call) nil)
        (let* ((generation (gptel-runner-call-generation call))
               (complete (gptel-runner--continuation
                          run (lambda (status &optional value metadata)
                                (gptel-runner--driver-result
                                 call generation status value metadata))
                          (or (gptel-runner-call-discussion-owner call)
                              (gptel-runner-call-node call)))))
          (unless driver-counts
            (gptel-runner--emit
             run 'request-started (gptel-runner-call-node call) call
             (list :attempt (gptel-runner-call-request-attempt call))))
          (when (and (= run-generation (gptel-runner-run-generation run))
                     (= generation (gptel-runner-call-generation call))
                     (eq (gptel-runner-run-state run) 'running)
                     (eq (gptel-runner-call-state call) 'running)
                     (memq call (gptel-runner-run-active-calls run)))
            (condition-case err
                (gptel-runner-driver-start
                 (gptel-runner-run-driver run) call
                 complete
                 (lambda (type &optional data)
                   (gptel-runner--call-observe call generation type data)))
              (error
               (funcall complete 'permanent err (list :permanent t))))))))))

(defun gptel-runner--finish-call (call state &optional value)
  "Terminalize CALL as STATE with VALUE exactly once."
  (unless (gptel-runner--call-terminal-p call)
    (let ((run (gptel-runner-call-run call)))
      (when (timerp (gptel-runner-call-retry-timer call))
        (cancel-timer (gptel-runner-call-retry-timer call)))
      (setf (gptel-runner-call-retry-timer call) nil
            (gptel-runner-call-state call) state
            (gptel-runner-call-tool-names call) nil
            (gptel-runner-call-finished-at call) (float-time))
      (if (eq state 'succeeded)
          (setf (gptel-runner-call-result call) value)
        (setf (gptel-runner-call-error call) value))
      (when (gptel-runner--elisp-call-p call)
        (gptel-runner--cleanup-elisp
         call (if (and (eq state 'failed)
                       (eq (plist-get value :type) 'elisp-timeout))
                  'timeout state)))
      (gptel-runner--deactivate-call call)
      (gptel-runner--emit run (intern (format "call-%s" state))
                          (gptel-runner-call-node call) call value)
      (when-let* ((done (gptel-runner-call-on-complete call)))
        (setf (gptel-runner-call-on-complete call) nil)
        (funcall done call state value))
      (gptel-runner--drain-queue run)
      (gptel-runner--checkpoint run)
      t)))

(defun gptel-runner--admit-call (call)
  "Acquire optional external admission resources for CALL.
Roll back acquisitions if any admission function refuses or signals."
  (let (admitted)
    (unwind-protect
        (setq admitted
              (run-hook-with-args-until-failure
               'gptel-runner-call-admission-functions call))
      (unless admitted
        (run-hook-with-args 'gptel-runner-call-release-hook call)))
    admitted))

(defun gptel-runner--can-start-p (run call)
  "Return non-nil when RUN can start queued CALL now."
  (and (< (gptel-runner-run-active-count run)
          (or (plist-get (gptel-runner-run-options run) :max-concurrency) 1))
       (or (not (gptel-runner--writer-p call))
           (not (cl-some
                 (lambda (active)
                   (and (gptel-runner--writer-p active)
                        (equal (gptel-runner-call-workspace active)
                               (gptel-runner-call-workspace call))))
                 (gptel-runner-run-active-calls run))))
       (gptel-runner--admit-call call)))

(defmacro gptel-runner--with-deferred-dispatch (run &rest body)
  "Evaluate BODY without dispatching queued calls in RUN.
Drain the queue on exit, after the outermost teardown for RUN finishes.
Failure propagation to ancestor groups must remain inside BODY so their
queued siblings cannot start before those groups cancel them."
  (declare (indent 1) (debug (form body)))
  (let ((run-var (make-symbol "run")))
    `(let ((,run-var ,run))
       (unwind-protect
           (let ((gptel-runner--dispatch-deferred-runs
                  (cons ,run-var gptel-runner--dispatch-deferred-runs)))
             ,@body)
         (gptel-runner--drain-queue ,run-var)))))

(defun gptel-runner--continuation-current-p (run generation owner)
  "Check RUN's GENERATION and optional node/epoch OWNER for a continuation."
  (and (= generation (gptel-runner-run-generation run))
       (eq (gptel-runner-run-state run) 'running)
       (or (null owner)
           (= (cdr owner) (gethash (car owner)
                                  (gptel-runner-run-continuation-epochs run) 0)))))

(defun gptel-runner--continuation (run function &optional node)
  "Wrap FUNCTION for RUN's current generation and optional owning NODE.
NODE may be a node or its ID; discussion follow-ups use their downstream owner.
Completions take priority over call dispatch.  The outermost scheduler drains
them synchronously; recursive completions only enqueue work.  In particular,
returning from `gptel-runner-start' still completes purely synchronous flows.
Subtree cancellation invalidates NODE's epoch even when its call has already
completed and detached its callback into this queue."
  (let* ((generation (gptel-runner-run-generation run))
         (id (if (gptel-runner-node-p node) (gptel-runner-node-id node) node))
         (owner (when id
                  (cons id (gethash id
                                    (gptel-runner-run-continuation-epochs run) 0)))))
    (lambda (&rest arguments)
      (when (gptel-runner--continuation-current-p run generation owner)
        (setf (gptel-runner-run-continuations run)
              (nconc (gptel-runner-run-continuations run)
                     (list (list generation function arguments
                                 (and (memq run gptel-runner--dispatch-deferred-runs)
                                      t)
                                 owner))))
        (gptel-runner--drain-queue run)))))

(defun gptel-runner--recovery-hook (hook)
  "Wrap HOOK functions so observer errors cannot interrupt error recovery."
  (mapcar (lambda (function)
            (lambda (&rest arguments)
              (condition-case nil
                  (apply function arguments)
                ((error quit) nil))))
          hook))

(defun gptel-runner--execution-error (run generation error-data)
  "Finish RUN after unexpected ERROR-DATA in execution GENERATION.
Invalidate old continuations and stop active work before running any explicit
finalizer.  An obsolete execution must not replace a newer phase's outcome.
Errors after terminalization still signal, including callbacks invoked by a
finalization transition that advanced the generation before calling them."
  (when (or (gptel-runner--run-terminal-p run)
            (and (= generation (gptel-runner-run-generation run))
                 (not (memq (gptel-runner-run-state run) '(running finishing)))))
    (signal (car error-data) (cdr error-data)))
  (when (and (= generation (gptel-runner-run-generation run))
             (memq (gptel-runner-run-state run) '(running finishing)))
    ;; The original error may itself have come from an observer.  Keep other
    ;; observers informed without letting that observer strand teardown too.
    (let ((gptel-runner-event-hook
           (gptel-runner--recovery-hook gptel-runner-event-hook))
          (gptel-runner-call-release-hook
           (gptel-runner--recovery-hook gptel-runner-call-release-hook)))
      (when (eq (plist-get (gptel-runner-run-finalization run) :phase) 'complete)
        ;; Finalization has committed its result and restored the body budget.
        ;; An observer must not interrupt publication or rewrite that outcome.
        (let ((body (plist-get (gptel-runner-run-finalization run) :body)))
          (gptel-runner--terminalize-run run (plist-get body :state)
                                       (plist-get body :value)))
        (signal (car error-data) (cdr error-data)))
      (setf (gptel-runner-run-state run) 'failing)
      (cl-incf (gptel-runner-run-generation run))
      (dolist (call (copy-sequence (gptel-runner-run-calls run)))
        (setf (gptel-runner-call-on-complete call) nil)
        (if (gptel-runner--call-terminal-p call)
            ;; Completion marks a call terminal before releasing resources.
            ;; An observer can have interrupted that release halfway through.
            (gptel-runner--deactivate-call call)
          (gptel-runner-abort-call call 'execution-error)))
      (gptel-runner--finish-run
       run (if (eq (car error-data) 'quit) 'cancelled 'failed)
       (list :type (if (eq (car error-data) 'quit)
                       'execution-quit 'execution-error)
             :error error-data)))))

(defun gptel-runner--dispatch-next (run)
  "Dispatch at most one eligible call in RUN, returning non-nil for progress."
  (let ((rest (gptel-runner-run-queue run)) found)
    (while (and rest (not found))
      (when (gptel-runner--can-start-p run (car rest))
        (setq found (car rest)))
      (setq rest (cdr rest)))
    (when found
      (setf (gptel-runner-run-queue run)
            (delq found (gptel-runner-run-queue run)))
      (unless (gptel-runner--call-terminal-p found)
        (push found (gptel-runner-run-active-calls run))
        (cl-incf (gptel-runner-run-active-count run))
        (when (gptel-runner--writer-p found)
          (cl-incf (gptel-runner-run-writer-active run)))
        (setf (gptel-runner-call-started-at found) (float-time))
        (gptel-runner--emit run 'call-started
                            (gptel-runner-call-node found) found nil)
        (if (gptel-runner--elisp-call-p found)
            (gptel-runner--dispatch-elisp found)
          (gptel-runner--dispatch-attempt found)))
      t)))

(defun gptel-runner--drain-queue (run)
  "Drain RUN's continuations and eligible calls without recursive dispatch.
The pump is per run, so a step can synchronously start a different run.
Always process completions before admitting the next call: a failed child
must be able to cancel queued siblings before their side effects begin.
Recheck state and generation between actions, including finalization phases.
Continuations queued inside a teardown retain its dispatch barrier while they
propagate failure to ancestors.  The barrier never delays an independent run.
These transient queues and guards are deliberately not persisted."
  (unless (or (gptel-runner-run-dispatching run)
              (memq run gptel-runner--dispatch-deferred-runs))
    (setf (gptel-runner-run-dispatching run) t)
    (unwind-protect
        (let ((progress t))
          (while (and progress (eq (gptel-runner-run-state run) 'running))
            (let ((generation (gptel-runner-run-generation run)))
              (condition-case err
                  (if-let* ((next (pop (gptel-runner-run-continuations run))))
                      (when (gptel-runner--continuation-current-p
                             run (car next) (nth 4 next))
                        (let ((gptel-runner--dispatch-deferred-runs
                               (if (nth 3 next)
                                   (cons run gptel-runner--dispatch-deferred-runs)
                                 gptel-runner--dispatch-deferred-runs)))
                          (apply (cadr next) (caddr next))))
                    (setq progress (gptel-runner--dispatch-next run)))
                ((error quit)
                 (gptel-runner--execution-error run generation err))))))
      (setf (gptel-runner-run-dispatching run) nil)
      (unless (eq (gptel-runner-run-state run) 'running)
        (setf (gptel-runner-run-continuations run) nil)))))

(defun gptel-runner--submit-call (run node agent prompt done &optional repair-p owner)
  "Queue a call for NODE in RUN, invoking DONE on terminalization.
AGENT and PROMPT are nil for Elisp steps.  REPAIR-P marks an agent repair.
OWNER identifies the downstream node whose question this call serves."
  (setq done (gptel-runner--continuation run done (or owner node)))
  (if (and (not (eq (gptel-runner-node-kind node) 'elisp))
           (not (gptel-runner--consume-call run)))
      (progn
        (funcall done nil 'failed
                 (gptel-runner--budget-failure
                  run 'calls
                  (gptel-runner-budget-max-calls
                   (gptel-runner-run-budget run))))
        nil)
    (let* ((repeat (cl-find 'repeat (gptel-runner-node-path run node)
                            :key #'gptel-runner-node-kind :from-end t))
           (call (gptel-runner-call-create
                  :id (gptel-runner--id "call") :run run :node node
                  :agent agent :prompt prompt
                  :workspace (gptel-runner-node-workspace run node)
                  :iteration (when repeat
                               (1+ (gptel-runner-iteration
                                    run (gptel-runner-node-id repeat))))
                  :state 'ready :on-complete done :repair-p repair-p
                  :discussion-owner owner)))
      (setf (gptel-runner-run-calls run)
            (nconc (gptel-runner-run-calls run) (list call))
            (gptel-runner-run-queue run)
            (nconc (gptel-runner-run-queue run) (list call)))
      (gptel-runner--emit run 'call-ready node call nil)
      (gptel-runner--drain-queue run)
      call)))

(defun gptel-runner-abort-call (call &optional reason)
  "Cancel CALL for REASON and invalidate its asynchronous callbacks."
  (unless (gptel-runner--call-terminal-p call)
    (let ((run (gptel-runner-call-run call)))
      (cl-incf (gptel-runner-call-generation call))
      (when (timerp (gptel-runner-call-retry-timer call))
        (cancel-timer (gptel-runner-call-retry-timer call)))
      (setf (gptel-runner-call-retry-timer call) nil
            (gptel-runner-run-queue run)
            (delq call (gptel-runner-run-queue run)))
      (when (and (not (gptel-runner--elisp-call-p call))
                 (memq (gptel-runner-call-state call)
                       '(running retry-wait waiting-tool waiting-confirmation
                         waiting-feedback)))
        (ignore-errors
          (gptel-runner-driver-cancel (gptel-runner-run-driver run) call)))
      (gptel-runner--finish-call call 'cancelled (or reason 'user)))))

(defun gptel-runner--suspend-call
    (call state reason keep-continuation &optional external-work-finished)
  "Suspend CALL in STATE for REASON.
When KEEP-CONTINUATION is nil, discard the in-memory workflow continuation so
the workflow can later be reconstructed from a snapshot.  When
EXTERNAL-WORK-FINISHED is non-nil, prepare the completed transcript for human
feedback without asking the driver to stop external work."
  (unless (or (gptel-runner--call-terminal-p call)
              (memq (gptel-runner-call-state call) '(paused waiting-feedback)))
    (let ((run (gptel-runner-call-run call)))
      (cl-incf (gptel-runner-call-generation call))
      (when (timerp (gptel-runner-call-retry-timer call))
        (cancel-timer (gptel-runner-call-retry-timer call)))
      (setf (gptel-runner-call-retry-timer call) nil
            (gptel-runner-run-queue run)
            (delq call (gptel-runner-run-queue run)))
      (if external-work-finished
          (ignore-errors
            (gptel-runner-driver-await-feedback
             (gptel-runner-run-driver run) call))
        (when (and (not (gptel-runner--elisp-call-p call))
                   (memq (gptel-runner-call-state call)
                         '(running retry-wait waiting-tool waiting-confirmation)))
          (ignore-errors
            (gptel-runner-driver-pause
             (gptel-runner-run-driver run) call))))
      (unless keep-continuation
        (setf (gptel-runner-call-on-complete call) nil))
      (setf (gptel-runner-call-state call) state
            (gptel-runner-call-tool-names call) nil)
      (when (gptel-runner--elisp-call-p call)
        (gptel-runner--cleanup-elisp call 'paused))
      (gptel-runner--deactivate-call call)
      (gptel-runner--emit run (intern (format "call-%s" state))
                          (gptel-runner-call-node call) call reason)
      (gptel-runner--drain-queue run)
      t)))

(defun gptel-runner-pause-call (call &optional reason)
  "Pause CALL for REASON without completing its workflow node.
The associated worker buffer remains a normal gptel buffer.  After continuing
the conversation, use `gptel-runner-complete-call-from-buffer' to return its
latest response to the workflow."
  (interactive
   (list (gptel-runner--buffer-call) 'user))
  (unless (gptel-runner-call-p call)
    (user-error "No runner call is associated with this buffer"))
  (when (gptel-runner--elisp-call-p call)
    (user-error "Elisp steps have no feedback conversation; pause the run instead"))
  (unless (gptel-runner--suspend-call
           call 'waiting-feedback (or reason 'user) t)
    (user-error "Call %s cannot be paused from state %s"
                (gptel-runner-call-id call) (gptel-runner-call-state call)))
  (gptel-runner--checkpoint (gptel-runner-call-run call) t)
  call)

(defun gptel-runner-pause-run (run &optional reason)
  "Pause RUN for REASON and queue a durable snapshot.
Active provider work is stopped.  Completed nodes and blackboard values are
preserved; unfinished nodes restart from their last safe checkpoint.  The
snapshot is committed asynchronously after this function returns."
  (interactive
   (list (when-let* ((call (gptel-runner--buffer-call)))
           (gptel-runner-call-run call))
         'user))
  (unless (gptel-runner-run-p run)
    (user-error "Run this command from a runner worker or the dashboard"))
  (unless (gptel-runner-workflow-name (gptel-runner-run-workflow run))
    (user-error "Pausing durably requires a named workflow"))
  (unless (fboundp 'gptel-runner-save-run)
    (user-error "Load gptel-runner-store before pausing a run"))
  (unless (or (gptel-runner--run-terminal-p run)
              (eq (gptel-runner-run-state run) 'paused))
    (setf (gptel-runner-run-state run) 'pausing)
    (cl-incf (gptel-runner-run-generation run))
    (gptel-runner--stop-duration-clock run)
    (dolist (call (copy-sequence (gptel-runner-run-calls run)))
      (unless (gptel-runner--call-terminal-p call)
        (gptel-runner--suspend-call call 'paused
                                    (or reason 'run-paused) nil)))
    (setf (gptel-runner-run-state run) 'paused
          (gptel-runner-run-paused-at run) (float-time)
          (gptel-runner-run-options run)
          (plist-put (gptel-runner-run-options run) :persist t))
    (gptel-runner--emit run 'run-paused nil nil (or reason 'user))
    (gptel-runner-save-run run)
    run))

(defun gptel-runner-abort-run (run &optional reason)
  "Cancel RUN for REASON, including queued and active work.
An explicit run finalizer with `:on-cancel' set to `finalize' may then run.
Aborting again during finalization stops it immediately."
  (unless (gptel-runner--run-terminal-p run)
    (cl-incf (gptel-runner-run-generation run))
    (setf (gptel-runner-run-state run) 'cancelling)
    (dolist (call (copy-sequence (gptel-runner-run-calls run)))
      (gptel-runner-abort-call call (or reason 'run-cancelled)))
    (gptel-runner--finish-run run 'cancelled (or reason 'user))))

(defun gptel-runner--duration-expired (run generation)
  "Fail RUN if its duration timer for GENERATION is still current."
  (when (and (= generation (gptel-runner-run-generation run))
             (not (gptel-runner--run-terminal-p run)))
    (setf (gptel-runner-run-state run) 'timing-out
          (gptel-runner-run-duration-timer run) nil
          (gptel-runner-run-duration-remaining run) 0
          (gptel-runner-run-active-started-at run) nil)
    (dolist (call (copy-sequence (gptel-runner-run-calls run)))
      (gptel-runner-abort-call call 'duration-budget))
    (gptel-runner--finish-run
     run 'failed
     (gptel-runner--budget-failure
      run 'duration
      (gptel-runner-budget-max-duration (gptel-runner-run-budget run))))))

(provide 'gptel-runner-core)
;;; gptel-runner-core.el ends here
