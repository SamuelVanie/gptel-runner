;;; gptel-runner-flow.el --- Workflow AST and interpreter -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Constructors, validation, and continuation-based execution for runner
;; workflow nodes.

;;; Code:

(require 'cl-lib)
(require 'gptel-runner-core)
(require 'gptel-runner-discussion)
(require 'gptel-runner-output)

(defun gptel-runner--make-node (kind properties &optional children)
  "Create a KIND node with PROPERTIES and CHILDREN, allocating an ID if needed."
  (let ((id (plist-get properties :id)))
    (gptel-runner-node-create
     :kind kind :properties properties :children children :generated-id-p (null id)
     :id (or id (intern (gptel-runner--id (symbol-name kind)))))))

(defun gptel-runner-agent-step (&rest properties)
  "Return an agent node described by PROPERTIES.
Required keys are `:agent' and `:prompt'.  Optional `:id', `:save-as',
`:retries', `:parser', `:validator', `:repair-invalid', `:pause-after',
`:workspace', `:completion', and `:context' customize its execution.
Completion defaults to `handoff'; use `response' for a plain-text result.
A non-nil `:pause-after'
holds a successful response for human feedback before completing the node.
`:workspace' selects an existing directory, relative to the run workspace
or absolute.  Omitted or
nil values inherit the workspace passed to `gptel-runner-start'."
  (gptel-runner--make-node 'agent properties))

(defun gptel-runner-elisp-step (&rest properties)
  "Return an Elisp node described by PROPERTIES.
`:function' is required and receives (RUN NODE).  Its return value, including
nil, is the node output; `:save-as' stores it on the run blackboard.
With `:async t', the function instead receives (RUN NODE RESOLVE REJECT).
Call RESOLVE with the output or REJECT with error data.  Return nil or a
cleanup function accepting a reason: succeeded, failed, cancelled, paused,
or timeout.  Cleanup runs once; late or duplicate callbacks are ignored.
Inline completion is deferred until the function returns its cleanup.
Optional `:timeout' is a positive number of seconds per attempt, starting
at admission.  Expiry fails the step with `:type' elisp-timeout.
Optional `:id', `:workspace', `:workspace-mode' (read or write), and `:retries'
customize execution.  Startup and cleanup run in temporary buffers with
`default-directory' set to the node workspace.  Write steps require
`:allow-writes t' and share workspace locks with agent steps.
Errors and rejections fail the step; `:retries' permits additional attempts,
including after timeout.  Pause stops work; resume starts a fresh attempt.
Timers cannot preempt Lisp that does not yield to Emacs.
Elisp steps consume concurrency slots but no agent-call or request budget."
  (gptel-runner--make-node 'elisp properties))

(defun gptel-runner-sequence (&rest arguments)
  "Return a fail-fast sequence described by ARGUMENTS.
Leading keyword/value pairs set properties such as `:id' and inherited
`:context'; the remaining arguments are child nodes."
  (let (properties)
    (while (keywordp (car arguments))
      (let ((key (pop arguments)))
        (unless arguments (error "Missing value for %S" key))
        (setq properties (plist-put properties key (pop arguments)))))
    (when (and (plist-member properties :id)
               (null (plist-get properties :id)))
      (user-error "Sequence :id cannot be nil"))
    (gptel-runner--make-node 'sequence properties arguments)))

(defun gptel-runner-branch (&rest properties)
  "Return a predicate branch described by PROPERTIES.
Use `:predicate' (or `:if'), `:then', `:else', and optional `:id'.  A
`:context' value is inherited by the selected subtree."
  (gptel-runner--make-node
   'branch properties (delq nil (list (plist-get properties :then)
                                      (plist-get properties :else)))))

(defun gptel-runner-repeat-until (&rest properties)
  "Return a bounded repeat node described by PROPERTIES.
The node requires `:body' and a positive `:max'.  `:until', `:stop-when', and
`:progress-key' are functions accepting the current run.  `:collect-keys' and
`:save-history-as' retain ordered per-iteration blackboard snapshots.
`:context' is inherited by every agent call in the body."
  (gptel-runner--make-node 'repeat properties (list (plist-get properties :body))))

(defun gptel-runner-try (&rest properties)
  "Return a recovery node described by PROPERTIES.
`:body' is required; provide at least one of `:catch' and `:finally'.
All three are workflow nodes.  `:catch' runs when the body fails, after its
own retries are exhausted.  `:catch-states' defaults to (failed) and may
also include blocked and stalled.  A successful handler recovers the try.
`:error-as' saves the body outcome as (:node ID :state STATE :value VALUE)
before recovery or cleanup.  It is nil when the body succeeds.
`:finally' runs after the body and any handler.  Successful cleanup preserves
their outcome; unsuccessful cleanup replaces it.  `:save-as' saves the
successful body or handler value, never the cleanup value.
Cancellation and run duration expiry stop execution immediately, including
cleanup.  Pausing checkpoints the current phase for later resumption.
Optional `:id' and inherited `:context' work as on other composite nodes."
  (gptel-runner--make-node
   'try properties (delq nil (list (plist-get properties :body)
                                   (plist-get properties :catch)
                                   (plist-get properties :finally)))))

(defun gptel-runner-finalize (&rest properties)
  "Return a run finalization wrapper described by PROPERTIES.
This node must be the workflow root.  `:body' and `:finally' are required
workflow nodes.  The finalizer runs after success, failure, blocked/stalled
outcomes, and budget exhaustion, with access to the same run and blackboard.
It has its own required positive finite `:max-duration', plus finite
`:max-calls' and `:max-requests' allowances, each defaulting to 1 (0 is valid).
`:on-cancel' is `skip' by default; `finalize' explicitly enables finalization
after cancellation.  Cancelling during finalization always stops it.
The run retains the body's terminal outcome, even if finalization fails.
Inspect `gptel-runner-finalization-outcome' for both outcomes and accounting.
The terminal callback runs after finalization.  Pause/resume checkpoints the
current phase.  Timers cannot preempt Lisp that does not yield to Emacs."
  (gptel-runner--make-node
   'finalize properties (list (plist-get properties :body)
                              (plist-get properties :finally))))

(defun gptel-runner-finalization-outcome (run)
  "Return RUN's finalization record, or nil before its body finishes.
The fresh plist has `:phase' (finalizing or complete), `:body' containing
a (:state STATE :value VALUE) plist, and, after finalization, `:result' in
the same format.  `:body-budget' and `:body-duration' retain the main allowance;
`:budget' and `:duration-remaining' record finalizer accounting on completion.
Values are shared with RUN and should be treated as read-only."
  (copy-sequence (gptel-runner-run-finalization run)))

(defun gptel-runner-parallel (&rest arguments)
  "Return a parallel node from ARGUMENTS.
Leading keyword/value pairs set `:id', `:policy', `:minimum-successes', and
`:save-as'.  A `:context' property is inherited by every child.  Remaining
arguments are child nodes."
  (let (properties)
    (while (keywordp (car arguments))
      (let ((key (pop arguments)))
        (unless arguments (error "Missing value for %S" key))
        (setq properties (plist-put properties key (pop arguments)))))
    (gptel-runner--make-node 'parallel properties arguments)))

(defun gptel-runner--generated-node-ids (workflow)
  "Return WORKFLOW's generated IDs and a digest of their layout, or nil."
  (let (ids layout)
    (dolist (node (gptel-runner-list-nodes workflow))
      (let ((props (gptel-runner-node-properties node))
            (generated (gptel-runner-node-generated-id-p node)))
        (when generated (push (gptel-runner-node-id node) ids))
        ;; Child counts delimit subtrees in preorder.  Named IDs and the
        ;; presence of a catch distinguish otherwise identical subtree layouts.
        (push (list (gptel-runner-node-kind node)
                    (unless generated (gptel-runner-node-id node))
                    (length (gptel-runner-node-children node))
                    (and (plist-get props :catch) t)
                    (plist-get props :agent) (plist-get props :save-as))
              layout)))
    (when ids
      (let ((print-level nil) (print-length nil) (print-circle t))
        (cons (secure-hash 'sha256 (prin1-to-string (nreverse layout)))
              (nreverse ids))))))

(defun gptel-runner--restore-node-ids (workflow saved)
  "Resolve SAVED generated node IDs against WORKFLOW for this run only."
  (if (null saved) workflow
    (let ((current (gptel-runner--generated-node-ids workflow)))
      (unless (and (proper-list-p saved) (equal (car saved) (car current))
                   (= (length saved) (length current))
                   (cl-every (lambda (id) (and id (symbolp id))) (cdr saved)))
        (user-error "Generated nodes no longer match workflow %S"
                    (gptel-runner-workflow-name workflow)))
      (if (equal current saved) workflow
        (let ((copy (copy-gptel-runner-workflow workflow))
              (copies (make-hash-table :test #'eq))
              (ids (cdr saved)))
          (cl-labels
              ((clone (node)
                 (let ((new (copy-gptel-runner-node node)))
                   (when (gptel-runner-node-generated-id-p node)
                     (setf (gptel-runner-node-id new) (pop ids)))
                   (setf (gptel-runner-node-children new)
                         (mapcar #'clone (gptel-runner-node-children node)))
                   ;; Composite nodes execute child references in their
                   ;; plist as well as the public children list.
                   (setf (gptel-runner-node-properties new)
                         (cl-loop for (key value) on (gptel-runner-node-properties node)
                                  by #'cddr append (list key (gethash value copies value))))
                   (puthash node new copies))))
            (setf (gptel-runner-workflow-root copy)
                  (clone (gptel-runner-workflow-root workflow))))
          copy)))))

(defmacro gptel-runner-defworkflow (name options &rest body)
  "Define workflow NAME with OPTIONS and one node in BODY."
  (declare (indent 2) (debug (symbolp form body)))
  (unless (= (length body) 1)
    (error "A workflow definition needs exactly one root node"))
  `(puthash ',name
            (gptel-runner-workflow-create
             :name ',name :options ',options :root ,(car body))
            gptel-runner--workflows))

(defun gptel-runner-context-file (file)
  "Return a context source that reads FILE from an agent node's workspace.
Absolute names are used as given.  Relative names are expanded from the
effective workspace of the agent node receiving the context."
  (unless (stringp file)
    (user-error "Context file must be a string: %S" file))
  (list :file file))

(defun gptel-runner-context-artifact (key)
  "Return a context source reading the artifact saved at blackboard KEY.
Resolve KEY before each agent call, so a preceding pipeline step can publish
the file with either artifact API and `:save-as'.  Missing values and
unreadable artifacts are context errors, just like missing context files."
  (list :artifact key))

(defun gptel-runner--context-artifact-p (source)
  "Return non-nil when SOURCE is a named artifact context descriptor."
  (and (proper-list-p source)
       (= (length source) 2)
       (eq (car source) :artifact)))

(defun gptel-runner--context-function (source)
  "Return the callable represented by context SOURCE, or nil."
  (cond
   ((functionp source) source)
   ((and (consp source)
         (eq (car source) 'function)
         (null (cddr source))
         (functionp (cadr source)))
    (cadr source))))

(defun gptel-runner--context-file-p (source)
  "Return non-nil when SOURCE is a context file descriptor."
  (and (proper-list-p source)
       (= (length source) 2)
       (eq (car source) :file)))

(defun gptel-runner--context-sources (context)
  "Normalize CONTEXT into an ordered list of individual sources."
  (cond
   ((null context) nil)
   ((or (stringp context)
        (gptel-runner--context-function context)
        (gptel-runner--context-artifact-p context)
        (gptel-runner--context-file-p context))
    (list context))
   ((proper-list-p context) context)
   (t (user-error "Invalid context value: %S" context))))

(defun gptel-runner--validate-context (context owner &optional workspace)
  "Validate CONTEXT associated with OWNER.
When WORKSPACE is non-nil, also require every declared file to be readable."
  (dolist (source (gptel-runner--context-sources context))
    (cond
     ((or (null source) (stringp source)
          (gptel-runner--context-function source)
          (gptel-runner--context-artifact-p source)))
     ((gptel-runner--context-file-p source)
      (unless (stringp (cadr source))
        (user-error "Invalid context source for %s: %S" owner source))
      (when workspace
        (let ((path (expand-file-name (cadr source) workspace)))
          (unless (and (file-regular-p path) (file-readable-p path))
            (user-error
             "Context file for %s is not a readable regular file: %s"
             owner path)))))
     (t (user-error "Invalid context source for %s: %S" owner source)))))

(defun gptel-runner--node-path (root target)
  "Return the path from ROOT to TARGET, including both nodes."
  (gptel-runner-node-path root target))

(defun gptel-runner--read-context-file (run node file)
  "Return FILE's contents, resolving it from NODE's workspace in RUN."
  (let ((path (expand-file-name file (gptel-runner-node-workspace run node))))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "Context file is not a readable regular file: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun gptel-runner--resolve-context-source (run node source owner)
  "Resolve SOURCE for RUN and agent NODE, labeling it with OWNER.
Return a cons of label and text, or nil when a function omits its context."
  (cond
   ((null source) nil)
   ((stringp source) (cons owner source))
   ((gptel-runner--context-artifact-p source)
    (cons (format "%s (artifact %s)" owner (cadr source))
          (gptel-runner-read-artifact
           run (gptel-runner-get run (cadr source)))))
   ((gptel-runner--context-file-p source)
    (let ((file (cadr source)))
      (cons (format "%s (file %s)" owner file)
            (gptel-runner--read-context-file run node file))))
   ((gptel-runner--context-function source)
    (let ((value (funcall (gptel-runner--context-function source) run node)))
      (unless (or (null value) (stringp value))
        (user-error "Context function for %s returned non-string value: %S"
                    owner value))
      (and value (cons owner value))))
   (t (user-error "Invalid context source for %s: %S" owner source))))

(defun gptel-runner--resolve-context (run node)
  "Return ordered context entries inherited by agent NODE in RUN."
  (let* ((workflow (gptel-runner-run-workflow run))
         (root (gptel-runner-workflow-root workflow))
         (workflow-context
          (plist-get (gptel-runner-workflow-options workflow) :context))
         (run-context (plist-get (gptel-runner-run-options run) :context))
         (agent (gptel-runner--agent
                 (plist-get (gptel-runner-node-properties node) :agent)))
         (layers
          (append
           (list (cons "Workflow" workflow-context)
                 (cons "Run" run-context)
                 (cons (format "Agent %s" (gptel-runner-agent-name agent))
                       (gptel-runner-agent-context agent)))
           (mapcar
            (lambda (ancestor)
              (cons (format "Node %s" (gptel-runner-node-id ancestor))
                    (plist-get (gptel-runner-node-properties ancestor)
                               :context)))
            (gptel-runner--node-path root node))))
         entries)
    (dolist (layer layers (nreverse entries))
      (dolist (source (gptel-runner--context-sources (cdr layer)))
        (when-let* ((entry (gptel-runner--resolve-context-source
                            run node source (car layer))))
          (push entry entries))))))

(defun gptel-runner--prepend-context (prompt entries)
  "Prepend resolved context ENTRIES to PROMPT."
  (if (null entries)
      prompt
    (concat
     "Starting context\n================\n\n"
     (mapconcat
      (lambda (entry)
        (format "%s context\n%s\n%s"
                (car entry)
                (make-string (+ (length (car entry)) 8) ?-)
                (cdr entry)))
      entries "\n\n")
     "\n\nTask\n====\n\n"
     prompt)))

(defun gptel-runner--node-save-key (node)
  "Return NODE's own blackboard destination key, if any."
  (let* ((kind (gptel-runner-node-kind node))
         (props (gptel-runner-node-properties node)))
    (pcase kind
      ((or 'agent 'elisp 'parallel 'try) (plist-get props :save-as))
      ('repeat (plist-get props :save-history-as)))))

(defun gptel-runner--find-node (root id)
  "Return the node named ID below ROOT, or nil when it is absent."
  (gptel-runner-find-node root id))

(defun gptel-runner--workflow-repeat-limits (root)
  "Return a table containing every repeat limit below ROOT."
  (let ((limits (make-hash-table :test #'equal)))
    (cl-labels
        ((walk
          (node)
          (when (eq (gptel-runner-node-kind node) 'repeat)
            (puthash (gptel-runner-node-id node)
                     (plist-get (gptel-runner-node-properties node) :max)
                     limits))
          (mapc #'walk (gptel-runner-node-children node))))
      (walk root))
    limits))

(defun gptel-runner--node-save-keys (node)
  "Return all blackboard keys written by NODE and its descendants."
  (let ((own (gptel-runner--node-save-key node)))
    (append (and own (list own))
            (when (eq (gptel-runner-node-kind node) 'try)
              (when-let* ((key (plist-get (gptel-runner-node-properties node)
                                          :error-as)))
                (list key)))
            (apply #'append
                   (mapcar #'gptel-runner--node-save-keys
                           (gptel-runner-node-children node))))))

(defun gptel-runner--node-writable-p (node)
  "Return non-nil when NODE or a descendant declares workspace writes."
  (or (and (eq (gptel-runner-node-kind node) 'elisp)
           (eq (plist-get (gptel-runner-node-properties node) :workspace-mode)
               'write))
      (and (eq (gptel-runner-node-kind node) 'agent)
           (let ((agent (gethash
                         (plist-get (gptel-runner-node-properties node) :agent)
                         gptel-runner--agents)))
             (and agent
                  (eq (gptel-runner-agent-workspace-mode agent) 'write))))
      (cl-some #'gptel-runner--node-writable-p
               (gptel-runner-node-children node))))

(defun gptel-runner--needs-driver-p (node)
  "Return non-nil when NODE or a descendant invokes an agent."
  (or (eq (gptel-runner-node-kind node) 'agent)
      (cl-some #'gptel-runner--needs-driver-p
               (gptel-runner-node-children node))))

(defun gptel-runner--validate-workflow
    (root &optional allow-writes workspace contexts)
  "Validate ROOT, including write opt-in ALLOW-WRITES, or signal an error.
When WORKSPACE is non-nil, validate context files in each agent's workspace.
CONTEXTS is an alist of labels and context sources inherited by every agent."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (entry contexts)
      (gptel-runner--validate-context (cdr entry) (car entry)))
    (cl-labels
        ((walk
          (node inherited-contexts)
          (unless (gptel-runner-node-p node)
            (user-error "Workflow child is not a runner node: %S" node))
          (let* ((id (gptel-runner-node-id node))
                 (kind (gptel-runner-node-kind node))
                 (props (gptel-runner-node-properties node))
                 (contexts (cons (cons (format "node %S" id)
                                       (plist-get props :context))
                                 inherited-contexts)))
            (when (gethash id ids)
              (user-error "Duplicate workflow node ID: %S" id))
            (puthash id t ids)
            (when (plist-member props :context)
              (gptel-runner--validate-context
               (plist-get props :context) (format "node %S" id)))
            (when (and (plist-get props :workspace)
                       (not (memq kind '(agent elisp))))
              (user-error
               "Only agent and Elisp nodes support :workspace (node %S)" id))
            (when (memq kind '(agent elisp))
              (let ((retries (or (plist-get props :retries) 0)))
                (unless (and (integerp retries) (>= retries 0))
                  (user-error "Node %S has invalid :retries" id))))
            (pcase kind
              ('finalize
               (unless (eq node root)
                 (user-error "Finalization node %S must be the workflow root" id))
               (dolist (key '(:body :finally))
                 (unless (gptel-runner-node-p (plist-get props key))
                   (user-error "Finalization node %S needs a %S node" id key)))
               (let ((duration (plist-get props :max-duration)))
                 (unless (and (numberp duration) (> duration 0)
                              (< duration 1.0e+INF))
                   (user-error "Finalization node %S needs a positive finite :max-duration" id)))
               (dolist (key '(:max-calls :max-requests))
                 (let ((limit (if (plist-member props key)
                                  (plist-get props key) 1)))
                   (unless (and (integerp limit) (>= limit 0))
                     (user-error "Finalization node %S needs a finite nonnegative %S" id key))))
               (unless (memq (or (plist-get props :on-cancel) 'skip)
                             '(skip finalize))
                 (user-error "Finalization node %S has invalid :on-cancel" id)))
              ('elisp
               (unless (functionp (plist-get props :function))
                 (user-error "Elisp node %S needs a :function" id))
               (unless (memq (plist-get props :async) '(nil t))
                 (user-error "Elisp node %S has invalid :async" id))
               (when-let* ((timeout (plist-get props :timeout)))
                 (unless (and (numberp timeout) (> timeout 0)
                              (< timeout 1.0e+INF))
                   (user-error "Elisp node %S needs a positive finite :timeout" id)))
               (unless (memq (or (plist-get props :workspace-mode) 'read)
                             '(read write))
                 (user-error "Elisp node %S has invalid :workspace-mode" id))
               (gptel-runner--node-workspace node workspace)
               (dolist (key '(:agent :prompt :parser :validator :repair-invalid
                             :pause-after :context))
                 (when (plist-member props key)
                   (user-error "Elisp node %S does not support %S" id key))))
              ('agent
               (when (and (plist-member props :completion)
                          (not (memq (plist-get props :completion) '(handoff response))))
                 (user-error "Agent node %S has invalid :completion" id))
               (let ((agent (gptel-runner--agent (plist-get props :agent)))
                     (directory (gptel-runner--node-workspace node workspace)))
                 (dolist (entry contexts)
                   (gptel-runner--validate-context
                    (cdr entry) (car entry) directory))
                 (gptel-runner--validate-context
                  (gptel-runner-agent-context agent)
                  (format "agent %S" (gptel-runner-agent-name agent))
                  directory))
               (unless (plist-member props :prompt)
                 (user-error "Agent node %S has no :prompt" id)))
              ('try
               (unless (gptel-runner-node-p (plist-get props :body))
                 (user-error "Try node %S needs a :body node" id))
               (unless (or (plist-get props :catch) (plist-get props :finally))
                 (user-error "Try node %S needs :catch or :finally" id))
               (dolist (key '(:catch :finally))
                 (when (and (plist-get props key)
                            (not (gptel-runner-node-p (plist-get props key))))
                   (user-error "Try node %S needs a node for %S" id key)))
               (when (plist-member props :catch-states)
                 (unless (and (plist-get props :catch)
                              (proper-list-p (plist-get props :catch-states))
                              (cl-every (lambda (state)
                                          (memq state '(failed blocked stalled)))
                                        (plist-get props :catch-states)))
                   (user-error "Try node %S has invalid :catch-states" id)))
               (when (and (plist-get props :error-as)
                          (equal (plist-get props :error-as)
                                 (plist-get props :save-as)))
                 (user-error "Try node %S needs distinct result and error keys" id)))
              ('repeat
               (let ((max (plist-get props :max)))
                 (unless (and (integerp max) (> max 0))
                   (user-error "Repeat node %S needs a positive :max" id)))
               (unless (gptel-runner-node-p (plist-get props :body))
                 (user-error "Repeat node %S needs a :body" id))
               (let ((collect-present (plist-member props :collect-keys))
                     (history-present (plist-member props :save-history-as))
                     (collect-keys (plist-get props :collect-keys))
                     (history-key (plist-get props :save-history-as)))
                 (unless (eq (not (null collect-present))
                             (not (null history-present)))
                   (user-error
                    (concat "Repeat node %S must use :collect-keys and "
                            ":save-history-as together")
                    id))
                 (when collect-present
                   (unless (and (proper-list-p collect-keys) collect-keys)
                     (user-error
                      "Repeat node %S needs non-empty :collect-keys" id))
                   (unless history-key
                     (user-error
                      "Repeat node %S needs non-nil :save-history-as" id))
                   (when (cl-some #'null collect-keys)
                     (user-error
                      "Repeat node %S has nil in :collect-keys" id))
                   (unless (= (length collect-keys)
                              (length (delete-dups
                                       (copy-sequence collect-keys))))
                     (user-error
                      "Repeat node %S has duplicate :collect-keys" id))
                   (let ((body-keys
                          (gptel-runner--node-save-keys
                           (plist-get props :body))))
                     (when (member history-key body-keys)
                       (user-error
                        (concat "Repeat node %S history key %S is also "
                                "written by its body")
                        id history-key))
                     (dolist (key collect-keys)
                       (unless (member key body-keys)
                         (user-error
                          (concat "Repeat node %S collects key %S, which is "
                                  "not written by its body")
                          id key)))))))
              ('branch
               (unless (functionp (or (plist-get props :predicate)
                                      (plist-get props :if)))
                 (user-error "Branch node %S needs a predicate" id))
               (unless (gptel-runner-node-p (plist-get props :then))
                 (user-error "Branch node %S needs a :then node" id)))
              ('parallel
               (unless (gptel-runner-node-children node)
                 (user-error "Parallel node %S has no children" id))
               (let ((policy (or (plist-get props :policy) 'fail-fast)))
                 (unless (memq policy '(fail-fast collect minimum-successes))
                   (user-error "Parallel node %S has bad policy %S" id policy))
                 (when (eq policy 'minimum-successes)
                   (let ((minimum (plist-get props :minimum-successes)))
                     (unless (and (integerp minimum) (> minimum 0)
                                  (<= minimum
                                      (length (gptel-runner-node-children node))))
                       (user-error "Parallel node %S has invalid minimum" id)))))
               (let ((seen (make-hash-table :test #'equal)))
                 (dolist (child (gptel-runner-node-children node))
                   (dolist (key (gptel-runner--node-save-keys child))
                     (when (gethash key seen)
                       (user-error
                        "Parallel children of %S both write blackboard key %S"
                        id key))
                     (puthash key t seen)))))
              ((or 'sequence) nil)
              (_ (user-error "Unknown node kind %S at %S" kind id)))
            (dolist (child (gptel-runner-node-children node))
              (walk child contexts)))))
      (walk root contexts))
    (when (and (gptel-runner--node-writable-p root) (not allow-writes))
      (user-error "Writable workflow requires explicit :allow-writes t"))
    t))

(defun gptel-runner--reset-subtree (run node)
  "Reset NODE and descendants to pending in RUN before a new iteration."
  (remhash (gptel-runner-node-id node) (gptel-runner-run-discussions run))
  (when (eq (gptel-runner-node-kind node) 'try)
    (remhash (list 'gptel-runner-try (gptel-runner-node-id node))
             (gptel-runner-run-blackboard run))
    (dolist (property '(:save-as :error-as))
      (when-let* ((key (plist-get (gptel-runner-node-properties node) property)))
        (remhash key (gptel-runner-run-blackboard run)))))
  (gptel-runner--reset-node-state run (gptel-runner-node-id node))
  (mapc (lambda (child) (gptel-runner--reset-subtree run child))
        (gptel-runner-node-children node)))

(defun gptel-runner--skip-subtree (run node &optional reason)
  "Mark pending NODE and descendants skipped in RUN because of REASON."
  (gptel-runner--set-node-state run node 'skipped reason)
  (mapc (lambda (child) (gptel-runner--skip-subtree run child reason))
        (gptel-runner-node-children node)))

(defun gptel-runner--subtree-ids (node)
  "Return IDs for NODE and all descendants."
  (cons (gptel-runner-node-id node)
        (apply #'append (mapcar #'gptel-runner--subtree-ids
                                (gptel-runner-node-children node)))))

(defun gptel-runner--cancel-subtree-calls (run node reason)
  "Cancel unfinished work in RUN below NODE for REASON."
  (let ((ids (gptel-runner--subtree-ids node)))
    (dolist (call (copy-sequence (gptel-runner-run-calls run)))
      (when (and (member (or (gptel-runner-call-discussion-owner call)
                             (gptel-runner-node-id (gptel-runner-call-node call)))
                         ids)
                 (not (gptel-runner--call-terminal-p call)))
        (gptel-runner-abort-call call reason)))))

(defun gptel-runner--prompt (run node)
  "Resolve NODE's prompt for RUN."
  (let* ((turn (gethash (gptel-runner-node-id node) (gptel-runner-run-discussions run)))
         (prompt (plist-get (gptel-runner-node-properties node) :prompt))
         (resolved (or (plist-get turn :prompt)
                       (gptel-runner--prepend-context
                        (if (functionp prompt) (funcall prompt run node) prompt)
                        (gptel-runner--resolve-context run node))))
         (message (unless turn (gptel-runner--incoming-handoff run node)))
         (decisions (and (gptel-runner-decision-memory-p run)
                         (gptel-runner-decisions run)))
         (continuation (gptel-runner--latest-continuation run))
         (retry-feedback (gptel-runner-get run 'gptel-runner-retry-feedback))
         (feedback (gethash 'gptel-runner-resume-feedback
                            (gptel-runner-run-blackboard run))))
    (when message (setq resolved (concat resolved "\n\n" message)))
    (when decisions
      (setq resolved
            (concat
             resolved
             "\n\nWorkflow decisions recorded by earlier stages\n"
             "===============================================\n\n"
             (gptel-runner-format-decisions run)
             "\n\nTreat these decisions as constraints for this stage.")))
    (when (plist-get continuation :observation)
      (setq resolved
            (concat
             resolved
             "\n\nHuman follow-up after an earlier workflow cycle"
             "\n================================================\n\n"
             (format "Previous goal:\n%s\n\n"
                     (plist-get continuation :previous-goal))
             (format "Current observation and goal:\n%s\n\n"
                     (plist-get continuation :observation))
             "Continue from the current workspace and take this observation "
             "into account.  Inspect the actual current state before making "
             "the changes needed for the current goal.")))
    (when (and retry-feedback
               (or (null (plist-get retry-feedback :node))
                   (equal (plist-get retry-feedback :node)
                          (gptel-runner-node-id node))))
      (remhash 'gptel-runner-retry-feedback (gptel-runner-run-blackboard run))
      (setq resolved
            (concat resolved
                    "\n\nHuman feedback supplied when retrying this node:\n\n"
                    (plist-get retry-feedback :text))))
    (if feedback
        (progn
          (remhash 'gptel-runner-resume-feedback
                   (gptel-runner-run-blackboard run))
          (concat resolved
                  "\n\nHuman feedback supplied when resuming this workflow:\n\n"
                  (format "%s" feedback)))
      resolved)))

(defun gptel-runner--subtree-state-p (run node state)
  "Return non-nil when NODE or a descendant has STATE in RUN."
  (or (eq (gethash (gptel-runner-node-id node)
                   (gptel-runner-run-node-states run) 'pending)
          state)
      (cl-some (lambda (child)
                 (gptel-runner--subtree-state-p run child state))
               (gptel-runner-node-children node))))

(defun gptel-runner--parse-agent-result (agent node value)
  "Parse and validate VALUE for AGENT and NODE.
Return (t . VALUE) on success or (nil . ERROR) on invalid output."
  (if (gptel-runner--empty-output-p value)
      (cons nil (gptel-runner--empty-output-error))
    (condition-case err
        (let* ((props (gptel-runner-node-properties node))
               (parser (or (plist-get props :parser)
                           (gptel-runner-agent-parser agent)))
               (validator (or (plist-get props :validator)
                              (gptel-runner-agent-validator agent)))
               (parsed (if parser (funcall parser value) value)))
          (if (and parser (null parsed))
              (cons nil (list :type 'invalid-output :reason 'empty-parse))
            (if (and validator (not (funcall validator parsed)))
                (cons nil (list :type 'invalid-output :reason 'validation))
              (cons t parsed))))
      (error (cons nil (list :type 'invalid-output :error err))))))

(defun gptel-runner--repair-prompt (run node value error-data)
  "Build an output repair prompt for RUN, NODE, VALUE, ERROR-DATA."
  (format
   (concat "Return only a corrected structured result for the previous call.\n"
           "Original goal: %s\nWorkspace: %s\nNode: %S\n"
           "Invalid output:\n%s\nValidation error: %S\n"
           "Do not perform the task again; repair only the output format.")
   (gptel-runner-run-goal run) (gptel-runner-node-workspace run node)
   (gptel-runner-node-id node) value error-data))

(defun gptel-runner--empty-output-repair-prompt (run node original-prompt)
  "Build a one-shot repair prompt for an empty result from NODE in RUN.
Include ORIGINAL-PROMPT so the repair instructions identify the task."
  (format
   (concat "The previous call completed without a non-empty final answer, "
           "possibly after a tool call.\n"
           "Complete the original task now and return its full final answer.\n"
           (if (gptel-runner-handoff-p run node)
               "Submit the full answer as the message in RunnerHandoff.\n"
             "After using tools, provide a non-empty final response.\n")
           "Original goal: %s\nWorkspace: %s\nNode: %S\n"
           "Original task:\n%s")
   (gptel-runner-run-goal run) (gptel-runner-node-workspace run node)
   (gptel-runner-node-id node) original-prompt))

(defun gptel-runner--execute-agent (run node done)
  "Execute agent NODE in RUN and invoke DONE with state and result."
  (let* ((props (gptel-runner-node-properties node))
         (agent (gptel-runner--agent (plist-get props :agent)))
         (semantic-left (or (plist-get props :retries) 0))
         (repair-allowed (or (plist-get props :repair-invalid)
                             (gptel-runner-agent-schema agent)))
         (repaired nil))
    (cl-labels
        ((finish (state value)
           (gptel-runner--set-node-state run node state value)
           (funcall done state value))
         (resolve-and-launch
          ()
          (let* ((generation (gptel-runner-run-generation run))
                 (resolved
                  (condition-case err
                      (cons t (if-let* ((parked (gptel-runner--resuming-chat run node)))
                                  (gptel-runner-call-prompt parked)
                                (gptel-runner--prompt run node)))
                    (error
                     (finish 'failed (list :type 'prompt :error err))
                     nil))))
            (when (and resolved (= generation (gptel-runner-run-generation run)))
              (launch (cdr resolved)
                      (plist-get (gethash (gptel-runner-node-id node)
                                          (gptel-runner-run-discussions run))
                                 :repair)))))
         (repair-empty
          (call error-data)
          (setq repaired t)
          (gptel-runner--emit run 'output-repair-started
                              node nil error-data)
          (launch (gptel-runner--empty-output-repair-prompt
                   run (gptel-runner-call-node call) (gptel-runner-call-prompt call)) t))
         (repair-invalid
          (value error-data)
          (setq repaired t)
          (gptel-runner--emit run 'output-repair-started
                              node nil error-data)
          (launch (gptel-runner--repair-prompt
                   run node value error-data) t))
         (launch
          (prompt repair-p)
          (unless (gptel-runner--run-terminal-p run)
            (gptel-runner--submit-discussion
             run node agent prompt
             (lambda (call state value)
               (pcase state
                 ('succeeded
                  (let* ((generation (gptel-runner-run-generation run))
                         (parsed (gptel-runner--parse-agent-result
                                  agent node value)))
                    ;; Parsing may yield while the body expires and its run
                    ;; finalizer starts.  Do not publish an obsolete result.
                    (when (= generation (gptel-runner-run-generation run))
                      (if (car parsed)
                          (progn
                            (gptel-runner--accept-agent-result call (cdr parsed) value)
                            (funcall done 'succeeded (cdr parsed)))
                        (cond
                         ((and (gptel-runner--empty-output-error-p (cdr parsed))
                               (not repaired))
                          (repair-empty call (cdr parsed)))
                         ((and repair-allowed (not repaired))
                          (repair-invalid value (cdr parsed)))
                         (t (finish 'failed (cdr parsed))))))))
                 ('blocked (finish 'blocked value))
                 ('cancelled (finish 'cancelled value))
                 (_
                  (cond
                   ((gptel-runner--empty-output-error-p value)
                    (if repaired
                        (finish 'failed value)
                      (repair-empty call value)))
                   ((and (> semantic-left 0)
                         (not (gptel-runner--run-terminal-p run)))
                    (cl-decf semantic-left)
                    (gptel-runner--emit run 'agent-step-retry
                                        node nil value)
                    (resolve-and-launch))
                   (t (finish 'failed value))))))
             repair-p))))
      (gptel-runner--set-node-state run node 'running)
      (resolve-and-launch))))

(defun gptel-runner--execute-elisp (run node done)
  "Execute Elisp NODE in RUN and invoke DONE with state and result."
  (let* ((props (gptel-runner-node-properties node))
         (generation (gptel-runner-run-generation run))
         (retries (or (plist-get props :retries) 0)))
    (cl-labels
        ((launch ()
           (when (and (= generation (gptel-runner-run-generation run))
                      (eq (gptel-runner-run-state run) 'running))
             (gptel-runner--submit-call
              run node nil nil
              (lambda (_call state value)
                (when (or (not (eq state 'succeeded))
                          (= generation (gptel-runner-run-generation run)))
                  (if (and (eq state 'failed) (> retries 0)
                           (eq (gptel-runner-run-state run) 'running))
                      (progn
                        (cl-decf retries)
                        (gptel-runner--emit run 'elisp-step-retry node nil value)
                        (launch))
                    (when (eq state 'succeeded)
                      (when-let* ((key (plist-get props :save-as)))
                        (gptel-runner-put run key value)))
                    (gptel-runner--set-node-state run node state value)
                    (funcall done state value))))))))
      (gptel-runner--set-node-state run node 'running)
      (launch))))

(defun gptel-runner--execute-sequence (run node done)
  "Execute sequence NODE in RUN and invoke DONE."
  (let ((remaining (copy-sequence (gptel-runner-node-children node))))
    (cl-labels
        ((next
          ()
          (if (null remaining)
              (progn
                (gptel-runner--set-node-state run node 'succeeded)
                (funcall done 'succeeded nil))
            (let ((child (pop remaining)))
              (gptel-runner--execute-node
               run child
               (lambda (state value)
                 (if (eq state 'succeeded)
                     (next)
                   (dolist (rest remaining)
                     (gptel-runner--skip-subtree run rest 'sequence-failed))
                   (gptel-runner--set-node-state run node state value)
                   (funcall done state value))))))))
      (gptel-runner--set-node-state run node 'running)
      (next))))

(defun gptel-runner--evaluate-flow-function (run node function next done)
  "Evaluate FUNCTION on RUN for NODE, then invoke NEXT or DONE.
NEXT receives a successful value.  Lisp errors and quits become terminal
node outcomes passed to DONE.  Invoke continuations outside the error
handler so errors in later nodes are not attributed to this function."
  (let ((outcome
         (condition-case err
             (cons 'succeeded (funcall function run))
           (error (cons 'failed (list :type 'flow-error :error err)))
           (quit (cons 'cancelled (list :type 'flow-quit :error err))))))
    (when (eq (gptel-runner-run-state run) 'running)
      (if (eq (car outcome) 'succeeded)
          (funcall next (cdr outcome))
        (gptel-runner--set-node-state run node (car outcome) (cdr outcome))
        (funcall done (car outcome) (cdr outcome))))))

(defun gptel-runner--execute-branch (run node done)
  "Execute branch NODE in RUN and invoke DONE."
  (let* ((props (gptel-runner-node-properties node))
         (predicate (or (plist-get props :predicate) (plist-get props :if))))
    (gptel-runner--set-node-state run node 'running)
    (gptel-runner--evaluate-flow-function
     run node predicate
     (lambda (selected)
       (let ((choice (plist-get props (if selected :then :else)))
             (unused (plist-get props (if selected :else :then))))
         (when unused (gptel-runner--skip-subtree run unused 'branch-not-selected))
         (if choice
             (gptel-runner--execute-node
              run choice
              (lambda (state value)
                (gptel-runner--set-node-state run node state value)
                (funcall done state value)))
           (gptel-runner--set-node-state run node 'succeeded)
           (funcall done 'succeeded nil))))
     done)))

(defun gptel-runner--node-key-succeeded-p (run node key)
  "Return non-nil when NODE or a descendant wrote KEY successfully in RUN."
  (or (and (equal (gptel-runner--node-save-key node) key)
           (eq (gethash (gptel-runner-node-id node)
                        (gptel-runner-run-node-states run) 'pending)
               'succeeded))
      (cl-some (lambda (child)
                 (gptel-runner--node-key-succeeded-p run child key))
               (gptel-runner-node-children node))))

(defun gptel-runner--collect-repeat-history (run node iteration)
  "Append NODE's configured blackboard snapshot for ITERATION in RUN.
Return the new history entry, or nil when NODE does not collect history."
  (let* ((props (gptel-runner-node-properties node))
         (keys (plist-get props :collect-keys))
         (history-key (plist-get props :save-history-as))
         (body (plist-get props :body)))
    (when keys
      (let ((missing (make-symbol "missing"))
            values)
        (dolist (key keys)
          (let ((value (gethash key (gptel-runner-run-blackboard run)
                                missing)))
            (when (and (not (eq value missing))
                       (gptel-runner--node-key-succeeded-p run body key))
              (push (cons key (copy-tree value t)) values))))
        (let ((entry (list :iteration iteration :values (nreverse values))))
          (gptel-runner-put
           run history-key
           (append (gptel-runner-get run history-key) (list entry)))
          entry)))))

(defun gptel-runner--repeat-limit (run node)
  "Return RUN's effective iteration limit for repeat NODE."
  (or (and (hash-table-p (gptel-runner-run-repeat-limits run))
           (gethash (gptel-runner-node-id node)
                    (gptel-runner-run-repeat-limits run)))
      (plist-get (gptel-runner-node-properties node) :max)))

(defun gptel-runner--execute-repeat (run node done)
  "Execute bounded repeat NODE in RUN and invoke DONE."
  (let* ((props (gptel-runner-node-properties node))
         (body (plist-get props :body))
         (maximum (gptel-runner--repeat-limit run node))
         (until (plist-get props :until))
         (stop (plist-get props :stop-when))
         (progress-fn (plist-get props :progress-key))
         (progress-slot (list 'gptel-runner-progress
                              (gptel-runner-node-id node)))
         (retry-slot (list 'gptel-runner-retry-repeat
                           (gptel-runner-node-id node)))
         (retry-body (gptel-runner-get run retry-slot))
         (previous-key (gptel-runner-get run progress-slot))
         (continue-current (or retry-body (gptel-runner--subtree-state-p
                                          run body 'succeeded))))
    (cl-labels
        ((finish (state value)
           (gptel-runner--set-node-state run node state value)
           (funcall done state value))
         (evaluate (function next)
           (gptel-runner--evaluate-flow-function run node function next done))
         (after-iteration (value key)
           (let* ((id (gptel-runner-node-id node))
                  (iteration (1+ (gptel-runner-iteration run id))))
             (puthash id iteration (gptel-runner-run-iterations run))
             (remhash retry-slot (gptel-runner-run-blackboard run))
             (gptel-runner--collect-repeat-history run node iteration)
             (gptel-runner--emit run 'iteration-completed node nil
                                 (list :iteration iteration :progress key))
             (evaluate
              (lambda (run)
                (cond ((and stop (funcall stop run)) 'blocked)
                      ((and until (funcall until run)) 'succeeded)))
              (lambda (decision)
                (cond
                 (decision (finish decision value))
                 ((and key previous-key (equal key previous-key))
                  (finish 'stalled (list :type 'stalled :progress-key key)))
                 (t
                  (setq previous-key key)
                  (puthash progress-slot key (gptel-runner-run-blackboard run))
                  (gptel-runner--checkpoint run)
                  (iterate nil)))))))
         (after-body (state value)
           (if (not (eq state 'succeeded))
               (finish state value)
             (evaluate (or progress-fn #'ignore)
                       (lambda (key) (after-iteration value key)))))
         (iterate (resume-body)
           (if (>= (gptel-runner-iteration run (gptel-runner-node-id node))
                   maximum)
               (finish 'failed (list :type 'iteration-budget :max maximum))
             (unless resume-body (gptel-runner--reset-subtree run body))
             (gptel-runner--execute-node run body #'after-body))))
      (gptel-runner--set-node-state run node 'running)
      (evaluate
       (lambda (run) (and (not retry-body) until (funcall until run)))
       (lambda (finished)
         (if finished
             (finish 'succeeded nil)
           (iterate continue-current)))))))

(defun gptel-runner--execute-parallel (run node done)
  "Execute parallel NODE in RUN and invoke DONE according to its join policy."
  (let* ((children (gptel-runner-node-children node))
         (props (gptel-runner-node-properties node))
         (policy (or (plist-get props :policy) 'fail-fast))
         (minimum (or (plist-get props :minimum-successes) (length children)))
         (remaining (length children)) (successes 0) (failures 0)
         results finalized)
    (cl-labels
        ((finish
          (state value)
          (unless finalized
            (setq finalized t)
            (when-let* ((key (plist-get props :save-as)))
              (gptel-runner-put run key value))
            (gptel-runner--set-node-state run node state value)
            (funcall done state value)))
         (cancel-others
          (except reason)
          (dolist (child children)
            (unless (eq child except)
              (gptel-runner--cancel-subtree-calls run child reason)
              (gptel-runner--skip-subtree run child reason))))
         (joined
          (child state value)
          (unless finalized
            (cl-decf remaining)
            (push (list :node (gptel-runner-node-id child)
                        :state state :value value)
                  results)
            (if (eq state 'succeeded) (cl-incf successes) (cl-incf failures))
            (pcase policy
              ('fail-fast
               (cond
                ((not (eq state 'succeeded))
                 (setq finalized t)
                 (cancel-others child 'parallel-fail-fast)
                 (gptel-runner--set-node-state run node state value)
                 (funcall done state value))
                ((zerop remaining) (finish 'succeeded (nreverse results)))))
              ('collect
               (when (zerop remaining)
                 (finish 'succeeded (nreverse results))))
              ('minimum-successes
               (cond
                ((and (>= successes minimum) (zerop remaining))
                 (finish 'succeeded (nreverse results)))
                ((< (+ successes remaining) minimum)
                 (setq finalized t)
                 (cancel-others child 'minimum-impossible)
                 (let ((failure (list :type 'minimum-successes
                                      :required minimum
                                      :successes successes
                                      :failures failures)))
                   (gptel-runner--set-node-state run node 'failed failure)
                   (funcall done 'failed failure)))))))))
      (gptel-runner--set-node-state run node 'running)
      (dolist (child children)
        (if finalized
            (gptel-runner--skip-subtree run child 'parallel-finished)
          (gptel-runner--execute-node
           run child (lambda (state value) (joined child state value))))))))

(defun gptel-runner--execute-try (run node done)
  "Execute recovery NODE in RUN, checkpointing phases before invoking DONE."
  (let* ((props (gptel-runner-node-properties node))
         (body (plist-get props :body))
         (handler (plist-get props :catch))
         (cleanup (plist-get props :finally))
         (states (if (plist-member props :catch-states)
                     (plist-get props :catch-states) '(failed)))
         (slot (list 'gptel-runner-try (gptel-runner-node-id node)))
         (checkpoint (or (gptel-runner-get run slot) '(:phase body)))
         (generation (gptel-runner-run-generation run))
         finalized)
    (cl-labels
        ((live-p ()
           (and (not finalized)
                (= generation (gptel-runner-run-generation run))
                (eq (gptel-runner-run-state run) 'running)))
         (remember (data)
           (setq checkpoint data)
           (puthash slot data (gptel-runner-run-blackboard run))
           (gptel-runner--checkpoint run))
         (reset (child)
           (when child
             (gptel-runner--clear-workflow-results run child)
             (gptel-runner--reset-subtree run child)
             (gptel-runner--clear-repeat-progress run child)))
         (finish (state value)
           (when (live-p)
             (setq finalized t)
             (when (eq state 'succeeded)
               (remember (list :phase 'complete :state state :value value))
               (when-let* ((key (plist-get props :save-as)))
                 (gptel-runner-put run key value)))
             (gptel-runner--set-node-state run node state value)
             (funcall done state value)))
         (cancel (state value)
           ;; In particular, aborting a sibling in a fail-fast parallel must
           ;; not enqueue a new cleanup call in the cancelled subtree.
           (dolist (child (list handler cleanup))
             (when child (gptel-runner--skip-subtree run child 'try-cancelled)))
           (finish state value))
         (after-cleanup (state value)
           (when (live-p)
             (if (eq state 'succeeded)
                 (let ((outcome-state (plist-get checkpoint :state))
                       (outcome-value (plist-get checkpoint :value)))
                   ;; A later retry targets the phase whose outcome cleanup
                   ;; preserved.  A cleanup failure instead resumes cleanup.
                   (remember (plist-put checkpoint :phase
                                        (plist-get checkpoint :source)))
                   (finish outcome-state outcome-value))
               (finish state value))))
         (finalize (source state value)
           (remember (list :phase 'finally :source source
                           :state state :value value))
           (if cleanup
               (gptel-runner--execute-node run cleanup #'after-cleanup)
             (after-cleanup 'succeeded nil)))
         (after-handler (state value)
           (when (live-p)
             (if (eq state 'cancelled)
                 (cancel state value)
               (finalize 'catch state value))))
         (recover ()
           (reset cleanup)
           (gptel-runner--execute-node run handler #'after-handler))
         (after-body (state value)
           (when (live-p)
             (when-let* ((key (plist-get props :error-as)))
               (gptel-runner-put
                run key (unless (eq state 'succeeded)
                          (list :node (gptel-runner-node-id body)
                                :state state :value value))))
             (cond
              ((eq state 'cancelled) (cancel state value))
              ((and handler (memq state states))
               (remember (list :phase 'catch))
               (recover))
              (t
               (when handler
                 (gptel-runner--skip-subtree run handler 'try-not-caught))
               (finalize 'body state value))))))
      (gptel-runner--set-node-state run node 'running)
      (pcase (plist-get checkpoint :phase)
        ('catch (recover))
        ('finally
         (if cleanup
             (gptel-runner--execute-node run cleanup #'after-cleanup)
           (after-cleanup 'succeeded nil)))
        (_
         (reset handler)
         (reset cleanup)
         (dolist (property '(:save-as :error-as))
           (when-let* ((key (plist-get props property)))
             (remhash key (gptel-runner-run-blackboard run))))
         (remember (list :phase 'body))
         (gptel-runner--execute-node run body #'after-body))))))

(defun gptel-runner--quiesce-finalization (run subtree state value)
  "Stop RUN's current work and finish entered SUBTREE nodes as STATE/VALUE."
  (setf (gptel-runner-run-state run) 'finishing)
  (cl-incf (gptel-runner-run-generation run))
  (gptel-runner--stop-duration-clock run)
  (dolist (call (copy-sequence (gptel-runner-run-calls run)))
    (unless (gptel-runner--call-terminal-p call)
      ;; Teardown may invoke driver callbacks or Elisp process sentinels.
      ;; They must never schedule more work in the phase being left.
      (setf (gptel-runner-call-on-complete call) nil)
      (gptel-runner-abort-call call 'run-finalization)))
  (gptel-runner-map-nodes
   (lambda (node)
     (when (eq (gethash (gptel-runner-node-id node)
                        (gptel-runner-run-node-states run)) 'running)
       (gptel-runner--set-node-state run node state value)))
   subtree))

(defun gptel-runner--complete-finalization (run state value)
  "Record finalizer STATE and VALUE, then publish RUN's original outcome."
  (let* ((root (gptel-runner--source-root run))
         (finalizer (plist-get (gptel-runner-node-properties root) :finally))
         (record (gptel-runner-run-finalization run))
         (body (plist-get record :body)))
    (gptel-runner--quiesce-finalization run finalizer state value)
    (setq record (plist-put record :phase 'complete)
          record (plist-put record :result (list :state state :value value))
          record (plist-put record :budget
                            (copy-gptel-runner-budget (gptel-runner-run-budget run)))
          record (plist-put record :duration-remaining
                            (gptel-runner-run-duration-remaining run)))
    (setf (gptel-runner-run-finalization run) record
          (gptel-runner-run-budget run) (plist-get record :body-budget)
          (gptel-runner-run-duration-remaining run) (plist-get record :body-duration))
    (gptel-runner--set-node-state run root (plist-get body :state)
                                (plist-get body :value))
    (gptel-runner--emit run 'finalization-completed root nil
                        (plist-get record :result))
    (gptel-runner--terminalize-run run (plist-get body :state)
                                 (plist-get body :value))))

(defun gptel-runner--execute-finalizer (run)
  "Execute or resume RUN's finalizer using its remaining private allowance."
  (let ((generation (gptel-runner-run-generation run))
        (finalizer (plist-get (gptel-runner-node-properties
                              (gptel-runner--source-root run)) :finally)))
    (gptel-runner--execute-node
     run finalizer
     (lambda (state value)
       (when (and (= generation (gptel-runner-run-generation run))
                  (eq (gptel-runner-run-state run) 'running))
         (gptel-runner--complete-finalization run state value))))))

(defun gptel-runner--finalize-run (run state value)
  "Handle terminal STATE and VALUE for RUN's explicit finalization wrapper."
  (pcase (plist-get (gptel-runner-run-finalization run) :phase)
    ('complete nil)
    ('finalizing (gptel-runner--complete-finalization run state value))
    (_
     (let* ((root (gptel-runner--source-root run))
            (props (gptel-runner-node-properties root))
            (finalizer (plist-get props :finally))
            (skip (and (eq state 'cancelled)
                       (not (eq (plist-get props :on-cancel) 'finalize)))))
       (gptel-runner--quiesce-finalization run (plist-get props :body) state value)
       (setf (gptel-runner-run-finalization run)
             (list :phase (if skip 'complete 'finalizing)
                   :body (list :state state :value value)
                   :body-budget (gptel-runner-run-budget run)
                   :body-duration (gptel-runner-run-duration-remaining run)))
       (if skip
           (progn
             (gptel-runner--skip-subtree run finalizer 'run-cancelled)
             (setf (gptel-runner-run-finalization run)
                   (plist-put (gptel-runner-run-finalization run) :result
                              '(:state skipped :value run-cancelled)))
             (gptel-runner--set-node-state run root state value)
             (gptel-runner--emit run 'finalization-skipped root nil 'run-cancelled)
             (gptel-runner--terminalize-run run state value))
         (setf (gptel-runner-run-budget run)
               (gptel-runner-budget-create
                :max-duration (plist-get props :max-duration)
                :max-calls (gptel-runner--option :max-calls props nil 1)
                :max-requests (gptel-runner--option :max-requests props nil 1))
               (gptel-runner-run-duration-remaining run) (plist-get props :max-duration)
               (gptel-runner-run-state run) 'running)
         (let ((generation (gptel-runner-run-generation run)))
           (gptel-runner--emit run 'finalization-started root nil
                               (plist-get (gptel-runner-run-finalization run) :body))
           (when (and (= generation (gptel-runner-run-generation run))
                      (eq (gptel-runner-run-state run) 'running))
             (gptel-runner--start-duration-clock run)
             (gptel-runner--execute-finalizer run))))))))

(defun gptel-runner--execute-finalize (run node done)
  "Execute the body of finalization NODE in RUN, forwarding its result to DONE."
  (let ((generation (gptel-runner-run-generation run)))
    (gptel-runner--set-node-state run node 'running)
    (gptel-runner--execute-node
     run (plist-get (gptel-runner-node-properties node) :body)
     (lambda (state value)
       (when (and (= generation (gptel-runner-run-generation run))
                  (eq (gptel-runner-run-state run) 'running))
         (funcall done state value))))))

(defun gptel-runner--execute-node (run node done)
  "Execute NODE in RUN, then call DONE with terminal state and value."
  (let ((saved-state
         (gethash (gptel-runner-node-id node)
                  (gptel-runner-run-node-states run) 'pending)))
    (cond
     ((eq saved-state 'succeeded)
      (let ((outcome (gethash (gptel-runner-node-id node)
                              (gptel-runner-run-node-outcomes run))))
        (funcall done 'succeeded
                 (if (plist-member outcome :value)
                     (plist-get outcome :value)
                   ;; Older v2 snapshots have no node outcomes.  Retain their
                   ;; previous resume behavior without inventing public data.
                   (if (eq (gptel-runner-node-kind node) 'try)
                       (plist-get (gptel-runner-get
                                   run (list 'gptel-runner-try
                                             (gptel-runner-node-id node))) :value)
                     (when (memq (gptel-runner-node-kind node) '(agent elisp))
                       (when-let* ((key (plist-get
                                        (gptel-runner-node-properties node)
                                        :save-as)))
                         (gptel-runner-get run key))))))))
     ((not (eq (gptel-runner-run-state run) 'running)) nil)
     (t
      (when (eq saved-state 'skipped)
        (gptel-runner--reset-node-state run (gptel-runner-node-id node)))
      (pcase (gptel-runner-node-kind node)
        ('agent (gptel-runner--execute-agent run node done))
        ('elisp (gptel-runner--execute-elisp run node done))
        ('sequence (gptel-runner--execute-sequence run node done))
        ('branch (gptel-runner--execute-branch run node done))
        ('repeat (gptel-runner--execute-repeat run node done))
        ('parallel (gptel-runner--execute-parallel run node done))
        ('try (gptel-runner--execute-try run node done))
        ('finalize (gptel-runner--execute-finalize run node done))
        (_ (funcall done 'failed
                    (list :type 'invalid-node
                          :kind (gptel-runner-node-kind node)))))))))

(defun gptel-runner--option (key explicit defaults fallback)
  "Select KEY from EXPLICIT, DEFAULTS, or FALLBACK."
  (cond ((plist-member explicit key) (plist-get explicit key))
        ((plist-member defaults key) (plist-get defaults key))
        (t fallback)))

(defun gptel-runner--input-blackboard (inputs)
  "Validate INPUTS and return a fresh blackboard containing their values.
INPUTS is an alist or hash table.  Runner-owned history keys are reserved."
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels
        ((add (key value)
           (when (memq key (list gptel-runner-decisions-key
                                gptel-runner-continuations-key))
             (user-error "Input key %S is reserved by gptel-runner" key))
           (puthash (copy-tree key t) (copy-tree value t) table)))
      (cond
       ((hash-table-p inputs) (maphash #'add inputs))
       ((and (proper-list-p inputs) (cl-every #'consp inputs))
        (dolist (entry inputs) (add (car entry) (cdr entry))))
       (t (user-error ":inputs must be an alist or hash table"))))
    table))

(cl-defun gptel-runner-start
    (workflow &rest arguments
              &key goal workspace inputs driver max-requests max-calls
              max-concurrency max-duration allow-writes completion
              allow-unconfirmed-tools decision-memory persist context callback
              &allow-other-keys)
  "Start WORKFLOW with keyword ARGUMENTS and return its run.
Agent calls are asynchronous; admitted Elisp steps run synchronously.
GOAL and WORKSPACE describe the task.  Agent and Elisp nodes may override
WORKSPACE with their own `:workspace' property.  An omitted WORKSPACE uses
the workflow's `:workspace' default, or `default-directory'.  INPUTS is an
alist or hash table copied into the run blackboard before any node starts.
DRIVER defaults to
`gptel-runner-default-driver'; Elisp-only workflows need no driver.
MAX-REQUESTS, MAX-CALLS, MAX-CONCURRENCY, and
MAX-DURATION override workflow defaults.  ALLOW-WRITES must be explicitly
non-nil for any workflow containing a write step.
Non-nil ALLOW-UNCONFIRMED-TOOLS skips gptel tool confirmations, including
approval for reads outside the working directory.  It defaults to the
workflow's setting.  Pass nil to restore the usual policy for this run.
DECISION-MEMORY controls automatic propagation of recorded decisions and
defaults to non-nil.
CONTEXT adds run-wide starting context to the workflow's own context.
COMPLETION defaults to `handoff'.  Set it to `response' to use a model's
ordinary final reply as its output.  Agent steps may override this option.
PERSIST enables versioned snapshots at workflow checkpoints.
CALLBACK runs exactly once with the terminal run."
  (ignore workspace max-requests max-calls max-concurrency max-duration
          allow-unconfirmed-tools decision-memory completion)
  (let* ((definition
          (cond ((gptel-runner-workflow-p workflow) workflow)
                ((symbolp workflow)
                 (or (gethash workflow gptel-runner--workflows)
                     (user-error "Unknown gptel-runner workflow: %S" workflow)))
                ((gptel-runner-node-p workflow)
                 (gptel-runner-workflow-create :name nil :options nil
                                               :root workflow))
                (t (user-error "Invalid workflow: %S" workflow))))
         (defaults (gptel-runner-workflow-options definition))
         (root (gptel-runner-workflow-root definition))
         (selected-driver (or driver gptel-runner-default-driver))
         (selected-workspace
          (gptel-runner--option :workspace arguments defaults default-directory))
         (directory
          (progn
            (unless (or (null selected-workspace)
                        (and (stringp selected-workspace)
                             (not (string-empty-p selected-workspace))))
              (user-error ":workspace must be a non-empty directory name"))
            (file-name-as-directory
             (file-truename (or selected-workspace default-directory)))))
         (blackboard (gptel-runner--input-blackboard inputs))
         (options
          (list :max-requests
                (gptel-runner--option :max-requests arguments defaults nil)
                :max-calls
                (gptel-runner--option :max-calls arguments defaults nil)
                :max-concurrency
                (gptel-runner--option :max-concurrency arguments defaults 1)
                :max-duration
                (gptel-runner--option :max-duration arguments defaults nil)
                :allow-writes allow-writes
                :allow-unconfirmed-tools
                (gptel-runner--option :allow-unconfirmed-tools arguments defaults nil)
                :completion (gptel-runner--option :completion arguments defaults 'handoff)
                :decision-memory (gptel-runner--option
                                  :decision-memory arguments defaults t)
                :context context
                :persist (gptel-runner--option
                          :persist arguments defaults persist))))
    (unless (file-directory-p directory)
      (user-error "Run workspace is not a directory: %s" directory))
    (unless (memq (plist-get options :completion) '(handoff response))
      (user-error ":completion must be handoff or response"))
    (when (and (plist-get options :persist)
               (null (gptel-runner-workflow-name definition)))
      (user-error "Persistent runs require a named workflow"))
    (unless (and (integerp (plist-get options :max-concurrency))
                 (> (plist-get options :max-concurrency) 0))
      (user-error ":max-concurrency must be positive"))
    (gptel-runner--validate-workflow
     root allow-writes directory
     (list (cons "workflow" (plist-get defaults :context))
           (cons "run" (plist-get options :context))))
    (when (and (gptel-runner--needs-driver-p root) (not selected-driver))
      (user-error "No gptel-runner driver configured"))
    (let* ((budget (gptel-runner-budget-create
                    :max-requests (plist-get options :max-requests)
                    :max-calls (plist-get options :max-calls)
                    :max-duration (plist-get options :max-duration)))
           (run (gptel-runner-run-create
                 :id (gptel-runner--id "run") :workflow definition
                 :goal goal :workspace directory :state 'running
                 :blackboard blackboard
                 :node-states (make-hash-table :test #'equal)
                 :iterations (make-hash-table :test #'equal)
                 :repeat-limits (gptel-runner--workflow-repeat-limits root)
                 :events nil :budget budget :driver selected-driver
                 :queue nil :active-calls nil :calls nil
                 :started-at (float-time) :callback callback :options options
                 :duration-remaining
                 (gptel-runner-budget-max-duration budget))))
      (puthash (gptel-runner-run-id run) run gptel-runner--runs)
      (gptel-runner--emit run 'run-started nil nil
                          (list :goal goal :workspace directory))
      (when-let* ((duration (gptel-runner-budget-max-duration budget)))
        (unless (and (numberp duration) (> duration 0))
          (user-error ":max-duration must be positive")))
      (gptel-runner--start-duration-clock run)
      (gptel-runner--execute-node
       run root
       (lambda (state value)
         (when (eq (gptel-runner-run-state run) 'running)
           (gptel-runner--finish-run run state value))))
      (gptel-runner--checkpoint run)
      run)))

(defun gptel-runner--try-completed-children (run node)
  "Return children of try NODE whose outcomes are already checkpointed in RUN."
  (when (eq (gptel-runner-node-kind node) 'try)
    (let* ((props (gptel-runner-node-properties node))
           (phase (plist-get
                   (gptel-runner-get
                    run (list 'gptel-runner-try (gptel-runner-node-id node)))
                   :phase)))
      (delq nil
            (mapcar (lambda (key) (plist-get props key))
                    (pcase phase
                      ('catch '(:body))
                      ('finally '(:body :catch))
                      ('complete '(:body :catch :finally))))))))

(defun gptel-runner--prepare-node-states-for-resume (run &optional subtree)
  "Reset unfinished node states in RUN, retaining completed work in SUBTREE.
SUBTREE defaults to the workflow root."
  (let* ((root (or subtree (gptel-runner--source-root run)))
         (completed (make-hash-table :test #'equal)))
    (cl-labels
        ((walk (node)
           (dolist (child (gptel-runner--try-completed-children run node))
             (dolist (id (gptel-runner--subtree-ids child))
               (puthash id t completed)))
           (mapc #'walk (gptel-runner-node-children node))))
      (walk root))
    (dolist (id (gptel-runner--subtree-ids root))
      (let ((state (gethash id (gptel-runner-run-node-states run))))
        (when (and state
                   (not (or (gethash id completed) (memq state '(succeeded skipped)))))
          (gptel-runner--reset-node-state run id))))))

(defun gptel-runner--prepare-terminal-repeat-bodies (run root)
  "Reset completed repeat bodies in RUN that made ROOT blocked or stalled.
Other failed composite nodes retain their successful descendants so execution
can resume at the narrowest safe checkpoint."
  (cl-labels
      ((walk
        (node)
        (if (and (eq (gptel-runner-node-kind node) 'repeat)
                 (memq (gethash (gptel-runner-node-id node)
                                (gptel-runner-run-node-states run))
                       '(blocked stalled))
                 (eq (gethash
                      (gptel-runner-node-id
                       (plist-get (gptel-runner-node-properties node) :body))
                      (gptel-runner-run-node-states run))
                     'succeeded))
            (gptel-runner--reset-subtree
             run (plist-get (gptel-runner-node-properties node) :body))
          (dolist (child (gptel-runner-node-children node))
            (unless (memq child (gptel-runner--try-completed-children run node))
              (walk child))))))
    (walk root)))

(defun gptel-runner--supersede-paused-calls (run)
  "Mark every unfinished historical call skipped before resuming RUN."
  (dolist (call (gptel-runner-run-calls run))
    (unless (or (gptel-runner--call-terminal-p call)
                (memq call gptel-runner--resuming-chats))
      (setf (gptel-runner-call-state call) 'skipped
            (gptel-runner-call-finished-at call) (float-time)
            (gptel-runner-call-on-complete call) nil)
      (gptel-runner--emit run 'call-skipped
                          (gptel-runner-call-node call) call
                          'superseded-by-resume))))

(defun gptel-runner--restart-run (run event data &optional callback)
  "Restart RUN from a safe checkpoint and emit EVENT containing DATA.
When CALLBACK is non-nil, replace the previous terminal callback with it."
  (when callback
    (setf (gptel-runner-run-callback run) callback
          (gptel-runner-run-callback-called run) nil))
  (gptel-runner--supersede-paused-calls run)
  (let* ((root (gptel-runner--source-root run))
         (phase (plist-get (gptel-runner-run-finalization run) :phase))
         (finalizer (and (eq (gptel-runner-node-kind root) 'finalize)
                         (plist-get (gptel-runner-node-properties root) :finally)))
         (subtree (if (eq phase 'finalizing) finalizer root)))
    (when (eq phase 'complete)
      (gptel-runner--clear-workflow-results run finalizer)
      (gptel-runner--reset-subtree run finalizer)
      (gptel-runner--clear-repeat-progress run finalizer)
      (setf (gptel-runner-run-finalization run) nil))
    (gptel-runner--prepare-terminal-repeat-bodies run subtree)
    (gptel-runner--prepare-node-states-for-resume run subtree))
  (cl-incf (gptel-runner-run-generation run))
  (setf (gptel-runner-run-state run) 'running
        (gptel-runner-run-paused-at run) nil
        (gptel-runner-run-finished-at run) nil
        (gptel-runner-run-terminal-data run) nil
        (gptel-runner-run-active-calls run) nil
        (gptel-runner-run-queue run) nil
        (gptel-runner-run-active-count run) 0
        (gptel-runner-run-writer-active run) 0)
  (gptel-runner--emit run event nil nil data)
  (let ((generation (gptel-runner-run-generation run)))
    (gptel-runner--start-duration-clock run)
    ;; An already exhausted body may have entered finalization while starting
    ;; the clock.  That transition has dispatched its finalizer itself.
    (when (and (= generation (gptel-runner-run-generation run))
               (eq (gptel-runner-run-state run) 'running))
      (if (eq (plist-get (gptel-runner-run-finalization run) :phase) 'finalizing)
          (gptel-runner--execute-finalizer run)
        (gptel-runner--execute-node
         run (gptel-runner-workflow-root (gptel-runner-run-workflow run))
         (lambda (state value)
           (when (eq (gptel-runner-run-state run) 'running)
             (gptel-runner--finish-run run state value)))))))
  (gptel-runner--checkpoint run)
  run)

(defun gptel-runner-resume-run (run &optional feedback callback)
  "Resume paused RUN with optional human FEEDBACK and CALLBACK.
Completed nodes remain complete.  The first unfinished agent prompt receives
FEEDBACK, and execution restarts from the workflow AST's safe checkpoint."
  (unless (eq (gptel-runner-run-state run) 'paused)
    (user-error "Run %s is not paused" (gptel-runner-run-id run)))
  (when feedback
    (puthash 'gptel-runner-resume-feedback feedback
             (gptel-runner-run-blackboard run)))
  (gptel-runner--restart-run
   run 'run-resumed (and feedback (list :feedback feedback)) callback))

(defun gptel-runner--resume-discussions (run)
  "Resume paused RUN with all its waiting chats held at their current turns.
Rebuild their continuations before sending feedback to a specific chat."
  (let ((gptel-runner--resuming-chats
         (cl-remove-if-not (lambda (call)
                             (eq (gptel-runner-call-state call) 'waiting-feedback))
                           (gptel-runner-run-calls run))))
    (gptel-runner-resume-run run)))

(defun gptel-runner--validate-extension-amount (value name integer-only)
  "Validate extension VALUE named NAME.
Require an integer when INTEGER-ONLY is non-nil."
  (when (and value
             (not (and (numberp value) (> value 0)
                       (or (not integer-only) (integerp value)))))
    (user-error "%s must be %s"
                name (if integer-only "a positive integer" "positive"))))

(defun gptel-runner--resolve-run (run)
  "Resolve RUN as a run object or a session-local string identifier."
  (when (stringp run)
    (setq run (gptel-runner-find-run run)))
  (unless (gptel-runner-run-p run)
    (user-error "Unknown gptel-runner run"))
  run)

(defun gptel-runner--terminal-failure-data (run)
  "Return RUN's retained or session-local terminal failure data."
  (or (gptel-runner-run-terminal-data run)
      (when-let* ((event
                  (cl-find-if
                   (lambda (candidate)
                     (eq (gptel-runner-event-type candidate) 'run-failed))
                   (gptel-runner-run-events run) :from-end t)))
        (gptel-runner-event-data event))))

(defun gptel-runner--budget-kinds-at-limit (run)
  "Return finite run-level budget kinds with no capacity left in RUN."
  (let ((budget (gptel-runner-run-budget run)))
    (delq
     nil
     (list
      (and (gptel-runner-budget-max-requests budget)
           (>= (gptel-runner-budget-requests budget)
               (gptel-runner-budget-max-requests budget))
           'requests)
      (and (gptel-runner-budget-max-calls budget)
           (>= (gptel-runner-budget-calls budget)
               (gptel-runner-budget-max-calls budget))
           'calls)
      (and (gptel-runner-budget-max-duration budget)
           (numberp (gptel-runner-run-duration-remaining run))
           (<= (gptel-runner-run-duration-remaining run) 0)
           'duration)))))

(defun gptel-runner--exhausted-budget-kinds (run)
  "Return run-level budget kinds exhausted by failed RUN.
For snapshots written before terminal data was retained, infer exhaustion from
finite limits whose accounting has reached the limit."
  (let ((failure (gptel-runner--terminal-failure-data run))
        (at-limit (gptel-runner--budget-kinds-at-limit run)))
    (cond
     ((and (listp failure) (eq (plist-get failure :type) 'budget))
      (delete-dups (cons (plist-get failure :kind) at-limit)))
     (failure nil)
     (t at-limit))))

(defun gptel-runner--extension-for-kind
    (kind additional-requests additional-calls additional-duration)
  "Return the requested extension for budget KIND.
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION are the
candidate increments."
  (pcase kind
    ('requests additional-requests)
    ('calls additional-calls)
    ('duration additional-duration)))

(defun gptel-runner--extend-budget (run slot amount)
  "Increase RUN's finite budget SLOT by AMOUNT when AMOUNT is non-nil."
  (when amount
    (let* ((budget (gptel-runner-run-budget run))
           (current
            (pcase slot
              ('requests (gptel-runner-budget-max-requests budget))
              ('calls (gptel-runner-budget-max-calls budget))
              ('duration (gptel-runner-budget-max-duration budget))))
           (new (and current (+ current amount))))
      (when new
        (pcase slot
          ('requests
           (setf (gptel-runner-budget-max-requests budget) new))
          ('calls
           (setf (gptel-runner-budget-max-calls budget) new))
          ('duration
           (setf (gptel-runner-budget-max-duration budget) new
                 (gptel-runner-run-duration-remaining run)
                 (+ (or (gptel-runner-run-duration-remaining run) 0)
                    amount))))
        (setf (gptel-runner-run-options run)
              (plist-put (gptel-runner-run-options run)
                         (intern (format ":max-%s" slot)) new))))))

(defun gptel-runner--record-live-extension
    (run additional-requests additional-calls additional-duration)
  "Record live budget increments in RUN's retained extension context.
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION are the
increments to accumulate."
  (let ((context (copy-sequence
                  (or (gptel-runner-run-extension-context run)
                      '(:kind run)))))
    (dolist (entry `((:additional-requests . ,additional-requests)
                     (:additional-calls . ,additional-calls)
                     (:additional-duration . ,additional-duration)))
      (when (cdr entry)
        (setq context
              (plist-put context (car entry)
                         (+ (or (plist-get context (car entry)) 0)
                            (cdr entry))))))
    (setf (gptel-runner-run-extension-context run) context)))

(defun gptel-runner--extend-live-budgets
    (run state additional-requests additional-calls additional-duration
         callback)
  "Extend finite budgets for RUN without restarting it from STATE.
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION are the
validated requested increments.
CALLBACK, when non-nil, replaces the callback for the next terminal state."
  (when (eq (plist-get (gptel-runner-run-finalization run) :phase) 'finalizing)
    (user-error "Run finalization has its own fixed allowance"))
  (let* ((budget (gptel-runner-run-budget run))
         (requests (and (gptel-runner-budget-max-requests budget)
                        additional-requests))
         (calls (and (gptel-runner-budget-max-calls budget)
                     additional-calls))
         (duration (and (gptel-runner-budget-max-duration budget)
                        additional-duration))
         (clock-running (and duration (eq state 'running))))
    (unless (or requests calls duration)
      (user-error "None of the requested budgets has a finite limit"))
    ;; Account for elapsed active time before replacing the duration timer.
    (when clock-running
      (gptel-runner--stop-duration-clock run))
    (gptel-runner--extend-budget run 'requests requests)
    (gptel-runner--extend-budget run 'calls calls)
    (gptel-runner--extend-budget run 'duration duration)
    (when clock-running
      (gptel-runner--start-duration-clock run))
    (gptel-runner--record-live-extension run requests calls duration)
    (when callback
      (setf (gptel-runner-run-callback run) callback
            (gptel-runner-run-callback-called run) nil))
    (gptel-runner--emit
     run 'run-extended nil nil
     (list :previous-state state :restarted nil
           :additional-requests requests
           :additional-calls calls
           :additional-duration duration))
    (gptel-runner--checkpoint run)
    run))

(defun gptel-runner--context-extension-for-kind (context kind)
  "Return CONTEXT's previously authorized extension for budget KIND."
  (plist-get context (intern (format ":additional-%s" kind))))

(defun gptel-runner--reapply-extension-budgets (run context kinds)
  "Reapply CONTEXT's budget increments for exhausted KINDS in RUN.
Return an ordered plist describing the increments that were reapplied."
  (let (reapplied)
    (dolist (kind kinds)
      (let ((amount (gptel-runner--context-extension-for-kind context kind)))
        (gptel-runner--extend-budget run kind amount)
        (setq reapplied
              (plist-put reapplied
                         (intern (format ":additional-%s" kind)) amount))))
    reapplied))

(defun gptel-runner--repeat-context-node (run context failure)
  "Return the repeat node in CONTEXT when FAILURE exhausted it in RUN."
  (when (and (eq (plist-get failure :type) 'iteration-budget)
             (eq (plist-get context :kind) 'repeat))
    (let* ((root (gptel-runner-workflow-root
                  (gptel-runner-run-workflow run)))
           (node-id (plist-get context :node-id))
           (node (gptel-runner--find-node root node-id)))
      (when (and node
                 (eq (gptel-runner-node-kind node) 'repeat)
                 (eq (gethash node-id (gptel-runner-run-node-states run))
                     'failed)
                 (>= (gptel-runner-iteration run node-id)
                     (gptel-runner--repeat-limit run node)))
        node))))

(defun gptel-runner--retry-node-path (run node-id)
  "Return the path to an executed step NODE-ID in RUN, or signal an error."
  (let ((root (gptel-runner-workflow-root (gptel-runner-run-workflow run))))
    (cl-labels
        ((walk (node)
           (if (equal (gptel-runner-node-id node) node-id)
               (list node)
             (when-let* ((path (cl-some #'walk
                                       (gptel-runner-node-children node))))
               (cons node path)))))
      (let* ((path (walk root))
             (target (car (last path))))
        (when (and (eq (gptel-runner-node-kind root) 'finalize)
                   (memq (plist-get (gptel-runner-node-properties root) :finally) path))
          (user-error "Retry targets must be in the workflow body"))
        (unless (and target (memq (gptel-runner-node-kind target) '(agent elisp)))
          (user-error "No agent or Elisp node %S in this workflow" node-id))
        (dolist (node path)
          (unless (memq (gethash (gptel-runner-node-id node)
                                (gptel-runner-run-node-states run))
                        '(succeeded failed blocked stalled cancelled running))
            (user-error "Node %S is not on an executed workflow path" node-id)))
        path))))

(defun gptel-runner--prepare-retry-path (run path)
  "Invalidate PATH's step and dependent sequence nodes in RUN.
Completed preceding nodes and independent parallel siblings remain complete.
Enclosing repeat histories and counters are retained."
  (cl-labels
      ((reset (node)
         (gptel-runner--clear-workflow-results run node)
         (gptel-runner--reset-subtree run node)
         (gptel-runner--clear-repeat-progress run node))
       (walk (remaining)
         (let ((node (car remaining))
               (child (cadr remaining)))
           (if (null child)
               (reset node)
             (walk (cdr remaining))
             (when (eq (gptel-runner-node-kind node) 'sequence)
               (mapc #'reset (cdr (memq child (gptel-runner-node-children node)))))
             (when (eq (gptel-runner-node-kind node) 'parallel)
               (when-let* ((key (gptel-runner--node-save-key node)))
                 (remhash key (gptel-runner-run-blackboard run))))
             (when (eq (gptel-runner-node-kind node) 'try)
               (let* ((props (gptel-runner-node-properties node))
                      (slot (list 'gptel-runner-try (gptel-runner-node-id node)))
                      (phase (cond ((eq child (plist-get props :body)) 'body)
                                   ((eq child (plist-get props :catch)) 'catch)
                                   (t 'finally))))
                 (puthash slot (plist-put (gptel-runner-get run slot)
                                          :phase phase)
                          (gptel-runner-run-blackboard run))
                 (when-let* ((key (plist-get props :save-as)))
                   (remhash key (gptel-runner-run-blackboard run)))))
             (when (eq (gptel-runner-node-kind node) 'repeat)
               (puthash (list 'gptel-runner-retry-repeat
                              (gptel-runner-node-id node))
                        t (gptel-runner-run-blackboard run)))
             (gptel-runner--reset-node-state
              run (gptel-runner-node-id node))))))
    (walk path)))

(cl-defun gptel-runner-retry (run &key from-node feedback callback)
  "Retry unsuccessful RUN from its safe checkpoint without changing its goal.
RUN may be a run object or its displayed string identifier.  By default,
completed work is kept except in completed blocked or stalled repeat bodies,
which restart as a whole.  The failed, blocked, stalled, or cancelled
checkpoint is reset and retried; later skipped nodes remain gated until it
succeeds.

FROM-NODE explicitly selects an executed agent or Elisp node by ID, even if it
succeeded but its verdict blocked a surrounding loop.  Rerun that node and
later sequence nodes, retaining earlier results and independent parallel
siblings.  Enclosing repeats retain their history and consume another
iteration on completion.  FEEDBACK is an optional string added to the selected
agent's next prompt, or the next dispatched agent prompt when FROM-NODE is nil
or selects an Elisp step.

Budget-exhaustion failures must normally use `gptel-runner-extend', and repeat
limits must normally use `gptel-runner-extend-repeat'.  When the failed work
was started by either extension command, retry reuses that command's increment
for any capacity that the failed attempt exhausted.  When CALLBACK is non-nil,
use it for the retried run's next terminal transition."
  (setq run (gptel-runner--resolve-run run))
  (unless (or (null feedback) (stringp feedback))
    (user-error "FEEDBACK must be a string"))
  (when feedback
    (setq feedback (string-trim feedback))
    (when (string-empty-p feedback) (setq feedback nil)))
  (let ((state (gptel-runner-run-state run)))
    (unless (memq state '(failed blocked stalled cancelled))
      (user-error "Run %s is not retryable from state %s"
                  (gptel-runner-run-id run) state))
    (let* ((path (and from-node (gptel-runner--retry-node-path run from-node)))
           (failure (and (eq state 'failed)
                         (gptel-runner--terminal-failure-data run)))
           (budgets (and (eq state 'failed)
                         (gptel-runner--budget-kinds-at-limit run)))
           ;; Retrying a local error needs no provider capacity.  If a later
           ;; agent needs more, its ordinary dispatch enforces that budget.
           (budgets (if (and (memq (plist-get failure :type)
                                  '(elisp-error elisp-timeout))
                             (or (null path)
                                 (eq (gptel-runner-node-kind (car (last path)))
                                     'elisp)))
                        (remq 'calls (remq 'requests budgets))
                      budgets))
           (context (gptel-runner-run-extension-context run))
           (missing
            (cl-remove-if
             (lambda (kind)
               (gptel-runner--context-extension-for-kind context kind))
             budgets))
           (repeat-node
            (and failure
                 (gptel-runner--repeat-context-node run context failure)))
           (repeat-failure
            (and (listp failure)
                 (eq (plist-get failure :type) 'iteration-budget))))
      ;; Validate every needed increment before mutating any limit.
      (when (and budgets missing)
        (user-error
         (concat "Run exhausted budget %S; use gptel-runner-extend "
                 "with the corresponding :additional-* value")
         budgets))
      (when (and repeat-failure (not repeat-node))
        (user-error
         (concat "Run exhausted a repeat limit; use "
                 "gptel-runner-extend-repeat")))
      (let ((reapplied
             (and budgets
                  (gptel-runner--reapply-extension-budgets
                   run context budgets)))
            repeat-data)
        (when repeat-node
          (let* ((node-id (gptel-runner-node-id repeat-node))
                 (old-limit (gptel-runner--repeat-limit run repeat-node))
                 (additional (plist-get context :additional-iterations))
                 (new-limit (+ old-limit additional)))
            (puthash node-id new-limit (gptel-runner-run-repeat-limits run))
            (unless path
              (gptel-runner--reset-subtree
               run (plist-get (gptel-runner-node-properties repeat-node) :body)))
            (setq repeat-data
                  (list :node-id node-id :additional additional
                        :old-limit old-limit :new-limit new-limit))))
        (when path (gptel-runner--prepare-retry-path run path))
        (remhash 'gptel-runner-retry-feedback (gptel-runner-run-blackboard run))
        (when feedback
          (puthash 'gptel-runner-retry-feedback
                   (list :node (and path
                                    (eq (gptel-runner-node-kind (car (last path)))
                                        'agent)
                                    from-node)
                         :text feedback)
                   (gptel-runner-run-blackboard run)))
        (gptel-runner--restart-run
         run 'run-retried
         (append
          (list :previous-state state
                :goal (gptel-runner-run-goal run))
          (and from-node (list :from-node from-node))
          (and feedback (list :feedback feedback))
          (and reapplied (list :reapplied-extension reapplied))
          (and repeat-data (list :reapplied-repeat-extension repeat-data)))
         callback)))))

(defun gptel-runner--reset-budget-accounting (run)
  "Reset RUN's consumed budget while retaining its configured limits."
  (let ((budget (gptel-runner-run-budget run)))
    (setf (gptel-runner-budget-requests budget) 0
          (gptel-runner-budget-calls budget) 0
          (gptel-runner-run-duration-remaining run)
          (gptel-runner-budget-max-duration budget))))

(defun gptel-runner--workflow-results (run root)
  "Return an ordered snapshot of saved results below ROOT in RUN."
  (let ((missing (make-symbol "missing")) results)
    (dolist (key (delete-dups (gptel-runner--node-save-keys root)))
      (let ((value (gethash key (gptel-runner-run-blackboard run) missing)))
        (unless (eq value missing)
          (push (cons key (copy-tree value t)) results))))
    (nreverse results)))

(defun gptel-runner--clear-workflow-results (run root)
  "Clear values written below ROOT before RUN reruns those nodes."
  (dolist (key (delete-dups (gptel-runner--node-save-keys root)))
    (unless (memq key (list gptel-runner-decisions-key
                            gptel-runner-continuations-key))
      (remhash key (gptel-runner-run-blackboard run)))))

(defun gptel-runner--clear-repeat-progress (run root)
  "Reset repeat counters and internal progress below ROOT in RUN."
  (cl-labels
      ((walk
        (node)
        (when (eq (gptel-runner-node-kind node) 'repeat)
          (remhash (gptel-runner-node-id node) (gptel-runner-run-iterations run))
          (remhash (list 'gptel-runner-retry-repeat
                         (gptel-runner-node-id node))
                   (gptel-runner-run-blackboard run))
          (remhash (list 'gptel-runner-progress
                         (gptel-runner-node-id node))
                   (gptel-runner-run-blackboard run)))
        (mapc #'walk (gptel-runner-node-children node))))
    (walk root)))

(cl-defun gptel-runner-continue
    (run observation
         &key reset-budget additional-requests additional-calls
         additional-duration callback)
  "Continue terminal RUN with OBSERVATION as a new goal.
RUN may be a run object or its displayed string identifier.  OBSERVATION must
be a non-empty string.  The workspace, decisions, calls, events, and other
run-local context are retained.  Results written by workflow nodes are
archived in `gptel-runner-continuations'.  A succeeded run begins a fresh
cycle: saved workflow results are cleared and all nodes and repeat counters
restart from the beginning.  An unsuccessful run instead restarts at its safe
checkpoint: succeeded nodes and their results remain complete, the failed or
unfinished node is retried, and later skipped nodes cannot run until that retry
succeeds.

By default, existing budget accounting and limits remain in effect.  When
RESET-BUDGET is non-nil, consumed request, call, and duration accounting is
reset while the configured limits are retained.  Positive
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION values increase
the corresponding finite limits and may be combined with RESET-BUDGET.
Unlimited limits remain unlimited.  When CALLBACK is non-nil, use it for the
continued run's next terminal transition."
  (unless (stringp observation)
    (user-error "OBSERVATION must be a string"))
  (setq observation (string-trim observation))
  (when (string-empty-p observation)
    (user-error "OBSERVATION cannot be empty"))
  (gptel-runner--start-cycle run observation reset-budget callback
                             additional-requests additional-calls additional-duration))

(cl-defun gptel-runner-rerun (run &key (reset-budget t) callback)
  "Run the complete workflow again with RUN's existing goal and chats.
RUN must be terminal, including when reopened from a checkpoint.  Archive
the prior workflow results and execute again from the root.  Retain
decisions, inputs, configured limits and conversation history.  Accounting
starts fresh unless RESET-BUDGET is explicitly nil.  When supplied, use
CALLBACK for the new cycle's terminal transition."
  (gptel-runner--start-cycle run nil reset-budget callback))

(defun gptel-runner--start-cycle (run observation reset-budget callback
                                     &optional additional-requests additional-calls
                                     additional-duration)
  "Restart terminal RUN and archive its previous workflow outputs.
With OBSERVATION, continue with that goal from a failed checkpoint or
begin a successful run again.  With nil OBSERVATION, rerun the whole
workflow with its existing goal.  RESET-BUDGET starts fresh accounting.
Apply ADDITIONAL-REQUESTS, ADDITIONAL-CALLS and ADDITIONAL-DURATION to
the limits.  CALLBACK optionally replaces the terminal callback."
  (setq run (gptel-runner--resolve-run run))
  (gptel-runner--validate-extension-amount
   additional-requests "ADDITIONAL-REQUESTS" t)
  (gptel-runner--validate-extension-amount
   additional-calls "ADDITIONAL-CALLS" t)
  (gptel-runner--validate-extension-amount
   additional-duration "ADDITIONAL-DURATION" nil)
  (unless (gptel-runner--run-terminal-p run)
    (user-error "Run %s is not finished" (gptel-runner-run-id run)))
  (let* ((root (gptel-runner-workflow-root
                (gptel-runner-run-workflow run)))
         (previous-goal (gptel-runner-run-goal run))
         (previous-state (gptel-runner-run-state run))
         (restart-mode (if (or (null observation) (eq previous-state 'succeeded))
                           'new-cycle
                         'checkpoint))
         (history (gptel-runner-get run gptel-runner-continuations-key))
         (entry
          (list :cycle (1+ (length history))
                :continued-at (float-time)
                :previous-goal (copy-tree previous-goal t)
                :previous-state previous-state
                :restart-mode restart-mode
                :terminal-data
                (copy-tree (gptel-runner-run-terminal-data run) t)
                :observation observation
                :node-outcomes (gptel-runner-map-nodes
                                (lambda (node)
                                  (copy-tree (gptel-runner-node-outcome run node) t))
                                root)
                :results (gptel-runner--workflow-results run root))))
    (when (eq restart-mode 'new-cycle)
      (gptel-runner--clear-workflow-results run root))
    (puthash gptel-runner-continuations-key
             (append history (list entry))
             (gptel-runner-run-blackboard run))
    (remhash 'gptel-runner-resume-feedback
             (gptel-runner-run-blackboard run))
    (remhash 'gptel-runner-retry-feedback
             (gptel-runner-run-blackboard run))
    (when observation (setf (gptel-runner-run-goal run) observation))
    (setf (gptel-runner-run-extension-context run) nil)
    (when (eq restart-mode 'new-cycle)
      (gptel-runner--reset-subtree run root)
      (gptel-runner--clear-repeat-progress run root))
    (when reset-budget
      (gptel-runner--reset-budget-accounting run))
    (gptel-runner--extend-budget run 'requests additional-requests)
    (gptel-runner--extend-budget run 'calls additional-calls)
    (gptel-runner--extend-budget run 'duration additional-duration)
    (gptel-runner--restart-run
     run (if observation 'run-continued 'run-rerun)
     (list :cycle (plist-get entry :cycle)
           :restart-mode restart-mode
           :previous-state previous-state
           :previous-goal previous-goal
           :observation observation
           :reset-budget (and reset-budget t)
           :additional-requests additional-requests
           :additional-calls additional-calls
           :additional-duration additional-duration)
     callback)))

(cl-defun gptel-runner-extend
    (run &key additional-requests additional-calls additional-duration callback)
  "Increase finite run-level budgets for RUN.
RUN may be a run object or its displayed string identifier.  Positive
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION values add to
the corresponding finite limits.  A running or paused run is updated in place
without restarting calls or workflow nodes.  Extending active duration stops
and replaces its deadline timer so the added time takes effect immediately.

For a failed run, every exhausted limit must be extended.  Completed nodes and
blackboard values remain intact; unfinished nodes restart from their safe
workflow checkpoint.  When CALLBACK is non-nil, use it for the run's next
terminal transition.

This function handles request, call, and duration budget exhaustion.  Use
`gptel-runner-extend-repeat' when a repeat node exhausted its `:max'."
  (setq run (gptel-runner--resolve-run run))
  (gptel-runner--validate-extension-amount
   additional-requests "ADDITIONAL-REQUESTS" t)
  (gptel-runner--validate-extension-amount
   additional-calls "ADDITIONAL-CALLS" t)
  (gptel-runner--validate-extension-amount
   additional-duration "ADDITIONAL-DURATION" nil)
  (unless (or additional-requests additional-calls additional-duration)
    (user-error "Provide at least one positive budget extension"))
  (let ((state (gptel-runner-run-state run)))
    (if (memq state '(running paused))
        (gptel-runner--extend-live-budgets
         run state additional-requests additional-calls additional-duration
         callback)
      (unless (eq state 'failed)
        (user-error "Run %s cannot be extended from state %s"
                    (gptel-runner-run-id run) state))
      (let ((exhausted (gptel-runner--exhausted-budget-kinds run)))
        (unless exhausted
          (if (let ((failure (gptel-runner--terminal-failure-data run)))
                (and (listp failure)
                     (eq (plist-get failure :type) 'iteration-budget)))
              (user-error
               "Run exhausted a repeat limit; use gptel-runner-extend-repeat")
            (user-error "Run %s did not exhaust a run-level budget"
                        (gptel-runner-run-id run))))
        (dolist (kind exhausted)
          (unless (gptel-runner--extension-for-kind
                   kind additional-requests additional-calls
                   additional-duration)
            (user-error "Run exhausted %s; provide :additional-%s"
                        kind kind)))
        (setf (gptel-runner-run-extension-context run)
              (list :kind 'run
                    :additional-requests additional-requests
                    :additional-calls additional-calls
                    :additional-duration additional-duration))
        (gptel-runner--extend-budget run 'requests additional-requests)
        (gptel-runner--extend-budget run 'calls additional-calls)
        (gptel-runner--extend-budget run 'duration additional-duration)
        (gptel-runner--restart-run
         run 'run-extended
         (list :exhausted exhausted
               :additional-requests additional-requests
               :additional-calls additional-calls
               :additional-duration additional-duration)
         callback)))))

(cl-defun gptel-runner-extend-repeat
    (run node-id additional-iterations
         &key additional-requests additional-calls additional-duration callback)
  "Continue a repeat in failed RUN for ADDITIONAL-ITERATIONS.
RUN may be a run object or its displayed string identifier.  NODE-ID must name
a repeat that exhausted its effective iteration limit.  The run-local
blackboard, saved repeat history, completed iteration count, calls, and events
are retained.  The repeat body is reset before execution so the last
successful iteration is not collected twice.

The registered workflow is not modified.  ADDITIONAL-REQUESTS,
ADDITIONAL-CALLS, and ADDITIONAL-DURATION increase corresponding finite run
budgets; unlimited budgets remain unlimited.  When CALLBACK is non-nil, use it
for the extended run's next terminal transition."
  (setq run (gptel-runner--resolve-run run))
  (unless additional-iterations
    (user-error "ADDITIONAL-ITERATIONS must be a positive integer"))
  (gptel-runner--validate-extension-amount
   additional-iterations "ADDITIONAL-ITERATIONS" t)
  (gptel-runner--validate-extension-amount
   additional-requests "ADDITIONAL-REQUESTS" t)
  (gptel-runner--validate-extension-amount
   additional-calls "ADDITIONAL-CALLS" t)
  (gptel-runner--validate-extension-amount
   additional-duration "ADDITIONAL-DURATION" nil)
  (unless (eq (gptel-runner-run-state run) 'failed)
    (user-error "Run %s did not fail at a repeat limit"
                (gptel-runner-run-id run)))
  (let* ((root (gptel-runner-workflow-root
                (gptel-runner-run-workflow run)))
         (node (gptel-runner--find-node root node-id)))
    (unless node
      (user-error "Workflow has no node %S" node-id))
    (unless (eq (gptel-runner-node-kind node) 'repeat)
      (user-error "Node %S is not a repeat" node-id))
    (let* ((old-limit (gptel-runner--repeat-limit run node))
           (iterations (gptel-runner-iteration run node-id))
           (state (gethash node-id (gptel-runner-run-node-states run)
                           'pending))
           (new-limit (+ old-limit additional-iterations))
           (limits (or (gptel-runner-run-repeat-limits run)
                       (setf (gptel-runner-run-repeat-limits run)
                             (make-hash-table :test #'equal)))))
      (unless (and (eq state 'failed) (>= iterations old-limit))
        (let ((budgets (gptel-runner--exhausted-budget-kinds run)))
          (if budgets
              (user-error
               (concat "Run exhausted run-level budget %S; use "
                       "gptel-runner-extend with :additional-%s")
               budgets (car budgets))
            (user-error
             "Repeat %S is %S at iteration %d; its effective limit is %d"
             node-id state iterations old-limit))))
      (setf (gptel-runner-run-extension-context run)
            (list :kind 'repeat :node-id node-id
                  :additional-iterations additional-iterations
                  :additional-requests additional-requests
                  :additional-calls additional-calls
                  :additional-duration additional-duration))
      (puthash node-id new-limit limits)
      (gptel-runner--extend-budget run 'requests additional-requests)
      (gptel-runner--extend-budget run 'calls additional-calls)
      (gptel-runner--extend-budget run 'duration additional-duration)
      (gptel-runner--reset-subtree
       run (plist-get (gptel-runner-node-properties node) :body))
      (gptel-runner--restart-run
       run 'repeat-extended
       (list :node-id node-id :additional additional-iterations
             :old-limit old-limit :new-limit new-limit
             :additional-requests additional-requests
             :additional-calls additional-calls
             :additional-duration additional-duration)
       callback))))

(defun gptel-runner--complete-restored-call (call value)
  "Complete restored CALL with VALUE and resume its reconstructed workflow."
  (let* ((run (gptel-runner-call-run call))
         (node (gptel-runner-call-node call))
         (owner (gptel-runner-call-discussion-owner call))
         (turns (gptel-runner-run-discussions run)))
    (if owner
        (let ((stack (plist-get (gethash owner turns) :stack)))
          (unless (and (cdr stack) (equal (car stack) (gptel-runner-node-id node)))
            (user-error "No pending question for this call"))
          (gptel-runner--route-discussion run owner stack call value))
      (let ((parsed (gptel-runner--parse-agent-result
                     (gptel-runner-call-agent call) node value)))
        (unless (car parsed)
          (user-error "Manual response is invalid: %S" (cdr parsed)))
        (remhash (gptel-runner-node-id node) turns)
        (gptel-runner--accept-agent-result call (cdr parsed) value)))
    (setf (gptel-runner-call-on-complete call) nil)
    (gptel-runner--finish-call call 'succeeded value)
    ;; Parallel reviewers can both have finished answers waiting for a human.
    ;; Restarting now would supersede the other reviewer's pending acceptance.
    (if (and (eq (gptel-runner-run-state run) 'paused)
             (not (cl-find 'waiting-feedback (gptel-runner-run-calls run)
                           :key #'gptel-runner-call-state)))
        (gptel-runner-resume-run run)
      (gptel-runner--checkpoint run t))))

(provide 'gptel-runner-flow)
;;; gptel-runner-flow.el ends here
