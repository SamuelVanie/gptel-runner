;;; gptel-runner-flow.el --- Workflow AST and interpreter -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Constructors, validation, and continuation-based execution for runner
;; workflow nodes.

;;; Code:

(require 'cl-lib)
(require 'gptel-runner-core)

(defun gptel-runner--node-id (properties kind)
  "Find an ID in PROPERTIES or make one based on KIND."
  (or (plist-get properties :id)
      (intern (gptel-runner--id (symbol-name kind)))))

(defun gptel-runner-agent-step (&rest properties)
  "Return an agent node described by PROPERTIES.
Required keys are `:id', `:agent', and `:prompt'.  `:save-as', `:retries',
`:parser', `:validator', `:repair-invalid', and `:pause-after' customize result
handling.  A non-nil `:pause-after' holds a successful response for human
feedback before completing the node."
  (gptel-runner-node-create
   :id (gptel-runner--node-id properties 'agent)
   :kind 'agent :properties properties))

(defun gptel-runner-sequence (&rest arguments)
  "Return a fail-fast sequence described by ARGUMENTS.
An optional leading `:id' and its value give the sequence a stable identity;
the remaining arguments are child nodes."
  (let ((id (when (eq (car arguments) :id)
              (pop arguments)
              (or (pop arguments) (user-error "Sequence :id cannot be nil")))))
    (gptel-runner-node-create
     :id (or id (intern (gptel-runner--id "sequence")))
     :kind 'sequence :children arguments)))

(defun gptel-runner-branch (&rest properties)
  "Return a predicate branch described by PROPERTIES.
Use `:predicate' (or `:if'), `:then', `:else', and optional `:id'."
  (gptel-runner-node-create
   :id (gptel-runner--node-id properties 'branch)
   :kind 'branch :properties properties
   :children (delq nil (list (plist-get properties :then)
                             (plist-get properties :else)))))

(defun gptel-runner-repeat-until (&rest properties)
  "Return a bounded repeat node described by PROPERTIES.
The node requires `:body' and a positive `:max'.  `:until', `:stop-when', and
`:progress-key' are functions accepting the current run.  `:collect-keys' and
`:save-history-as' retain ordered per-iteration blackboard snapshots."
  (gptel-runner-node-create
   :id (gptel-runner--node-id properties 'repeat)
   :kind 'repeat :properties properties
   :children (list (plist-get properties :body))))

(defun gptel-runner-parallel (&rest arguments)
  "Return a parallel node from ARGUMENTS.
Leading keyword/value pairs set `:id', `:policy', `:minimum-successes', and
`:save-as'.  Remaining arguments are child nodes."
  (let (properties)
    (while (keywordp (car arguments))
      (let ((key (pop arguments)))
        (unless arguments (error "Missing value for %S" key))
        (setq properties (plist-put properties key (pop arguments)))))
    (gptel-runner-node-create
     :id (gptel-runner--node-id properties 'parallel)
     :kind 'parallel :properties properties :children arguments)))

(defmacro gptel-runner-defworkflow (name options &rest body)
  "Define workflow NAME with OPTIONS and one node in BODY."
  (declare (indent 2) (debug (symbolp form body)))
  (unless (= (length body) 1)
    (error "A workflow definition needs exactly one root node"))
  `(puthash ',name
            (gptel-runner-workflow-create
             :name ',name :options ',options :root ,(car body))
            gptel-runner--workflows))

(defun gptel-runner--node-save-key (node)
  "Return NODE's own blackboard destination key, if any."
  (let* ((kind (gptel-runner-node-kind node))
         (props (gptel-runner-node-properties node)))
    (pcase kind
      ((or 'agent 'parallel) (plist-get props :save-as))
      ('repeat (plist-get props :save-history-as)))))

(defun gptel-runner--find-node (root id)
  "Return the node named ID below ROOT, or nil when it is absent."
  (if (equal (gptel-runner-node-id root) id)
      root
    (cl-loop for child in (gptel-runner-node-children root)
             thereis (gptel-runner--find-node child id))))

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
            (apply #'append
                   (mapcar #'gptel-runner--node-save-keys
                           (gptel-runner-node-children node))))))

(defun gptel-runner--node-writable-p (node)
  "Return non-nil when NODE has a registered write agent."
  (or (and (eq (gptel-runner-node-kind node) 'agent)
           (let ((agent (gethash
                         (plist-get (gptel-runner-node-properties node) :agent)
                         gptel-runner--agents)))
             (and agent
                  (eq (gptel-runner-agent-workspace-mode agent) 'write))))
      (cl-some #'gptel-runner--node-writable-p
               (gptel-runner-node-children node))))

(defun gptel-runner--validate-workflow (root &optional allow-writes)
  "Validate ROOT, including write opt-in ALLOW-WRITES, or signal an error."
  (let ((ids (make-hash-table :test #'equal)))
    (cl-labels
        ((walk
          (node)
          (unless (gptel-runner-node-p node)
            (user-error "Workflow child is not a runner node: %S" node))
          (let ((id (gptel-runner-node-id node))
                (kind (gptel-runner-node-kind node))
                (props (gptel-runner-node-properties node)))
            (when (gethash id ids)
              (user-error "Duplicate workflow node ID: %S" id))
            (puthash id t ids)
            (pcase kind
              ('agent
               (gptel-runner--agent (plist-get props :agent))
               (unless (plist-member props :prompt)
                 (user-error "Agent node %S has no :prompt" id))
               (let ((retries (or (plist-get props :retries) 0)))
                 (unless (and (integerp retries) (>= retries 0))
                   (user-error "Agent node %S has invalid :retries" id))))
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
            (mapc #'walk (gptel-runner-node-children node)))))
      (walk root))
    (when (and (gptel-runner--node-writable-p root) (not allow-writes))
      (user-error "Writable workflow requires explicit :allow-writes t"))
    t))

(defun gptel-runner--reset-subtree (run node)
  "Reset NODE and descendants to pending in RUN before a new iteration."
  (puthash (gptel-runner-node-id node) 'pending
           (gptel-runner-run-node-states run))
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
      (when (and (memq (gptel-runner-node-id (gptel-runner-call-node call)) ids)
                 (not (gptel-runner--call-terminal-p call)))
        (gptel-runner-abort-call call reason)))))

(defun gptel-runner--prompt (run node)
  "Resolve NODE's prompt for RUN."
  (let* ((prompt (plist-get (gptel-runner-node-properties node) :prompt))
         (resolved (if (functionp prompt) (funcall prompt run node) prompt))
         (decisions (and (gptel-runner-decision-memory-p run)
                         (gptel-runner-decisions run)))
         (continuation (gptel-runner--latest-continuation run))
         (feedback (gethash 'gptel-runner-resume-feedback
                            (gptel-runner-run-blackboard run))))
    (when decisions
      (setq resolved
            (concat
             resolved
             "\n\nWorkflow decisions recorded by earlier stages\n"
             "===============================================\n\n"
             (gptel-runner-format-decisions run)
             "\n\nTreat these decisions as constraints for this stage.")))
    (when continuation
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
  "Build a stateless output repair prompt for RUN, NODE, VALUE, ERROR-DATA."
  (format
   (concat "Return only a corrected structured result for the previous call.\n"
           "Original goal: %s\nWorkspace: %s\nNode: %S\n"
           "Invalid output:\n%s\nValidation error: %S\n"
           "Do not perform the task again; repair only the output format.")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
   (gptel-runner-node-id node) value error-data))

(defun gptel-runner--empty-output-repair-prompt (run node original-prompt)
  "Build a one-shot repair prompt for an empty result from NODE in RUN.
ORIGINAL-PROMPT is included because each repair call is stateless."
  (format
   (concat "The previous call completed without a non-empty final answer, "
           "possibly after a tool call.\n"
           "Complete the original task now and return its full final answer.\n"
           "Do not end on a tool call: after using tools, always provide a "
           "non-empty final response for the next workflow step.\n"
           "Original goal: %s\nWorkspace: %s\nNode: %S\n"
           "Original task:\n%s")
   (gptel-runner-run-goal run) (gptel-runner-run-workspace run)
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
         (repair-empty
          (call error-data)
          (setq repaired t)
          (gptel-runner--emit run 'output-repair-started
                              node nil error-data)
          (launch (gptel-runner--empty-output-repair-prompt
                   run node (gptel-runner-call-prompt call)) t))
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
            (gptel-runner--submit-call
             run node agent prompt
             (lambda (call state value)
               (pcase state
                 ('succeeded
                  (let ((parsed (gptel-runner--parse-agent-result
                                 agent node value)))
                    (if (car parsed)
                        (progn
                          (when-let ((key (plist-get props :save-as)))
                            (gptel-runner-put run key (cdr parsed)))
                          (finish 'succeeded (cdr parsed)))
                      (cond
                       ((and (gptel-runner--empty-output-error-p (cdr parsed))
                             (not repaired))
                        (repair-empty call (cdr parsed)))
                       ((and repair-allowed (not repaired))
                        (repair-invalid value (cdr parsed)))
                       (t (finish 'failed (cdr parsed)))))))
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
                    (launch (gptel-runner--prompt run node) nil))
                   (t (finish 'failed value))))))
             repair-p))))
      (gptel-runner--set-node-state run node 'running)
      (launch (gptel-runner--prompt run node) nil))))

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

(defun gptel-runner--execute-branch (run node done)
  "Execute branch NODE in RUN and invoke DONE."
  (let* ((props (gptel-runner-node-properties node))
         (predicate (or (plist-get props :predicate) (plist-get props :if)))
         (choice (if (funcall predicate run)
                     (plist-get props :then)
                   (plist-get props :else)))
         (unused (if (eq choice (plist-get props :then))
                     (plist-get props :else)
                   (plist-get props :then))))
    (gptel-runner--set-node-state run node 'running)
    (when unused (gptel-runner--skip-subtree run unused 'branch-not-selected))
    (if choice
        (gptel-runner--execute-node
         run choice
         (lambda (state value)
           (gptel-runner--set-node-state run node state value)
           (funcall done state value)))
      (gptel-runner--set-node-state run node 'succeeded)
      (funcall done 'succeeded nil))))

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
         (previous-key (gptel-runner-get run progress-slot))
         (continue-current (gptel-runner--subtree-state-p
                            run body 'succeeded)))
    (cl-labels
        ((iterate
          (resume-body)
          (if (>= (gptel-runner-iteration run (gptel-runner-node-id node))
                  maximum)
              (let ((failure (list :type 'iteration-budget :max maximum)))
                (gptel-runner--set-node-state run node 'failed failure)
                (funcall done 'failed failure))
            (unless resume-body (gptel-runner--reset-subtree run body))
            (gptel-runner--execute-node
             run body
             (lambda (state value)
               (if (not (eq state 'succeeded))
                   (progn
                     (gptel-runner--set-node-state run node state value)
                     (funcall done state value))
                 (let* ((id (gptel-runner-node-id node))
                        (iteration (1+ (gptel-runner-iteration run id)))
                        (key (and progress-fn (funcall progress-fn run))))
                   (puthash id iteration (gptel-runner-run-iterations run))
                   (gptel-runner--collect-repeat-history run node iteration)
                   (gptel-runner--emit run 'iteration-completed node nil
                                       (list :iteration iteration
                                             :progress key))
                   (cond
                    ((and stop (funcall stop run))
                     (gptel-runner--set-node-state run node 'blocked value)
                     (funcall done 'blocked value))
                    ((and until (funcall until run))
                     (gptel-runner--set-node-state run node 'succeeded value)
                     (funcall done 'succeeded value))
                    ((and key previous-key (equal key previous-key))
                     (let ((failure (list :type 'stalled :progress-key key)))
                       (gptel-runner--set-node-state run node 'stalled failure)
                       (funcall done 'stalled failure)))
                    (t
                     (setq previous-key key)
                     (puthash progress-slot key
                              (gptel-runner-run-blackboard run))
                     (gptel-runner--checkpoint run)
                     (iterate nil))))))))))
      (gptel-runner--set-node-state run node 'running)
      (if (and until (funcall until run))
          (progn
            (gptel-runner--set-node-state run node 'succeeded)
            (funcall done 'succeeded nil))
        (iterate continue-current)))))

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
            (when-let ((key (plist-get props :save-as)))
              (gptel-runner-put run key (nreverse results)))
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

(defun gptel-runner--execute-node (run node done)
  "Execute NODE in RUN, then call DONE with terminal state and value."
  (let ((saved-state
         (gethash (gptel-runner-node-id node)
                  (gptel-runner-run-node-states run) 'pending)))
    (cond
     ((eq saved-state 'succeeded)
      (funcall done 'succeeded
               (when (eq (gptel-runner-node-kind node) 'agent)
                 (when-let ((key (plist-get
                                  (gptel-runner-node-properties node)
                                  :save-as)))
                   (gptel-runner-get run key)))))
     ((not (eq (gptel-runner-run-state run) 'running)) nil)
     (t
      (when (eq saved-state 'skipped)
        (puthash (gptel-runner-node-id node) 'pending
                 (gptel-runner-run-node-states run)))
      (pcase (gptel-runner-node-kind node)
        ('agent (gptel-runner--execute-agent run node done))
        ('sequence (gptel-runner--execute-sequence run node done))
        ('branch (gptel-runner--execute-branch run node done))
        ('repeat (gptel-runner--execute-repeat run node done))
        ('parallel (gptel-runner--execute-parallel run node done))
        (_ (funcall done 'failed
                    (list :type 'invalid-node
                          :kind (gptel-runner-node-kind node)))))))))

(defun gptel-runner--option (key explicit defaults fallback)
  "Select KEY from EXPLICIT, DEFAULTS, or FALLBACK."
  (cond ((plist-member explicit key) (plist-get explicit key))
        ((plist-member defaults key) (plist-get defaults key))
        (t fallback)))

(cl-defun gptel-runner-start
    (workflow &rest arguments
              &key goal workspace driver max-requests max-calls
              max-concurrency max-duration allow-writes
              allow-unconfirmed-tools decision-memory persist callback
              &allow-other-keys)
  "Start WORKFLOW with keyword ARGUMENTS and return its run immediately.
GOAL and WORKSPACE describe the stateless task.  DRIVER defaults to
`gptel-runner-default-driver'.  MAX-REQUESTS, MAX-CALLS, MAX-CONCURRENCY, and
MAX-DURATION override workflow defaults.  ALLOW-WRITES must be explicitly
non-nil for any workflow containing a write agent.
ALLOW-UNCONFIRMED-TOOLS disables gptel confirmation only when explicitly set.
DECISION-MEMORY controls automatic propagation of recorded decisions and
defaults to non-nil.
PERSIST enables versioned snapshots at workflow checkpoints.
CALLBACK runs exactly once with the terminal run."
  (ignore max-requests max-calls max-concurrency max-duration
          allow-unconfirmed-tools decision-memory)
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
         (selected-driver (or driver gptel-runner-default-driver
                              (user-error "No gptel-runner driver configured")))
         (directory (file-name-as-directory
                     (file-truename (or workspace default-directory))))
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
                :allow-unconfirmed-tools allow-unconfirmed-tools
                :decision-memory (gptel-runner--option
                                  :decision-memory arguments defaults t)
                :persist (gptel-runner--option
                          :persist arguments defaults persist))))
    (when (and (plist-get options :persist)
               (null (gptel-runner-workflow-name definition)))
      (user-error "Persistent runs require a named workflow"))
    (unless (and (integerp (plist-get options :max-concurrency))
                 (> (plist-get options :max-concurrency) 0))
      (user-error ":max-concurrency must be positive"))
    (gptel-runner--validate-workflow root allow-writes)
    (let* ((budget (gptel-runner-budget-create
                    :max-requests (plist-get options :max-requests)
                    :max-calls (plist-get options :max-calls)
                    :max-duration (plist-get options :max-duration)))
           (run (gptel-runner-run-create
                 :id (gptel-runner--id "run") :workflow definition
                 :goal goal :workspace directory :state 'running
                 :blackboard (make-hash-table :test #'equal)
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
      (when-let ((duration (gptel-runner-budget-max-duration budget)))
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

(defun gptel-runner--prepare-node-states-for-resume (run)
  "Reset unfinished node states in RUN while preserving completed work."
  (maphash
   (lambda (id state)
     (unless (memq state '(succeeded skipped))
       (puthash id 'pending (gptel-runner-run-node-states run))))
   (gptel-runner-run-node-states run)))

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
                       '(blocked stalled)))
            (gptel-runner--reset-subtree
             run (plist-get (gptel-runner-node-properties node) :body))
          (mapc #'walk (gptel-runner-node-children node)))))
    (walk root)))

(defun gptel-runner--supersede-paused-calls (run)
  "Mark every unfinished historical call skipped before resuming RUN."
  (dolist (call (gptel-runner-run-calls run))
    (unless (gptel-runner--call-terminal-p call)
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
  (gptel-runner--prepare-terminal-repeat-bodies
   run (gptel-runner-workflow-root (gptel-runner-run-workflow run)))
  (gptel-runner--prepare-node-states-for-resume run)
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
  (gptel-runner--start-duration-clock run)
  (when (eq (gptel-runner-run-state run) 'running)
    (gptel-runner--execute-node
     run (gptel-runner-workflow-root (gptel-runner-run-workflow run))
     (lambda (state value)
       (when (eq (gptel-runner-run-state run) 'running)
         (gptel-runner--finish-run run state value)))))
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
      (when-let ((event
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

(cl-defun gptel-runner-retry (run &key callback)
  "Retry unsuccessful RUN from its safe checkpoint without changing its goal.
RUN may be a run object or its displayed string identifier.  Nodes that have
already succeeded and their blackboard results remain complete.  The failed,
blocked, stalled, or cancelled checkpoint is reset and retried; later skipped
nodes remain gated until it succeeds.

Budget-exhaustion failures must normally use `gptel-runner-extend', and repeat
limits must normally use `gptel-runner-extend-repeat'.  When the failed work
was started by either extension command, retry reuses that command's increment
for any capacity that the failed attempt exhausted.  When CALLBACK is non-nil,
use it for the retried run's next terminal transition."
  (setq run (gptel-runner--resolve-run run))
  (let ((state (gptel-runner-run-state run)))
    (unless (memq state '(failed blocked stalled cancelled))
      (user-error "Run %s is not retryable from state %s"
                  (gptel-runner-run-id run) state))
    (let* ((failure (and (eq state 'failed)
                         (gptel-runner--terminal-failure-data run)))
           (budgets (and (eq state 'failed)
                         (gptel-runner--budget-kinds-at-limit run)))
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
            (gptel-runner--reset-subtree
             run (plist-get (gptel-runner-node-properties repeat-node) :body))
            (setq repeat-data
                  (list :node-id node-id :additional additional
                        :old-limit old-limit :new-limit new-limit))))
        (gptel-runner--restart-run
         run 'run-retried
         (append
          (list :previous-state state
                :goal (gptel-runner-run-goal run))
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
  "Clear values written below ROOT before RUN begins a new goal cycle."
  (dolist (key (delete-dups (gptel-runner--node-save-keys root)))
    (unless (memq key (list gptel-runner-decisions-key
                            gptel-runner-continuations-key))
      (remhash key (gptel-runner-run-blackboard run)))))

(defun gptel-runner--clear-repeat-progress (run root)
  "Reset repeat counters and internal progress below ROOT in RUN."
  (clrhash (gptel-runner-run-iterations run))
  (cl-labels
      ((walk
        (node)
        (when (eq (gptel-runner-node-kind node) 'repeat)
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
  (setq run (gptel-runner--resolve-run run))
  (unless (stringp observation)
    (user-error "OBSERVATION must be a string"))
  (setq observation (string-trim observation))
  (when (string-empty-p observation)
    (user-error "OBSERVATION cannot be empty"))
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
         (restart-mode (if (eq previous-state 'succeeded)
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
                :results (gptel-runner--workflow-results run root))))
    (when (eq restart-mode 'new-cycle)
      (gptel-runner--clear-workflow-results run root))
    (puthash gptel-runner-continuations-key
             (append history (list entry))
             (gptel-runner-run-blackboard run))
    (remhash 'gptel-runner-resume-feedback
             (gptel-runner-run-blackboard run))
    (setf (gptel-runner-run-goal run) observation
          (gptel-runner-run-extension-context run) nil)
    (when (eq restart-mode 'new-cycle)
      (gptel-runner--reset-subtree run root)
      (gptel-runner--clear-repeat-progress run root))
    (when reset-budget
      (gptel-runner--reset-budget-accounting run))
    (gptel-runner--extend-budget run 'requests additional-requests)
    (gptel-runner--extend-budget run 'calls additional-calls)
    (gptel-runner--extend-budget run 'duration additional-duration)
    (gptel-runner--restart-run
     run 'run-continued
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
  "Continue failed RUN after increasing exhausted run-level budgets.
RUN may be a run object or its displayed string identifier.  Positive
ADDITIONAL-REQUESTS, ADDITIONAL-CALLS, and ADDITIONAL-DURATION values add to
the corresponding finite limits.  Every exhausted limit must be extended.
Completed nodes and blackboard values remain intact; unfinished nodes restart
from their safe workflow checkpoint.  When CALLBACK is non-nil, use it for the
extended run's next terminal transition.

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
  (unless (eq (gptel-runner-run-state run) 'failed)
    (user-error "Run %s is not failed" (gptel-runner-run-id run)))
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
               kind additional-requests additional-calls additional-duration)
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
     callback)))

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
         (agent (gptel-runner-call-agent call))
         (parsed (gptel-runner--parse-agent-result agent node value)))
    (unless (car parsed)
      (user-error "Manual response is invalid: %S" (cdr parsed)))
    (gptel-runner--finish-call call 'succeeded (cdr parsed))
    (when-let ((key (plist-get (gptel-runner-node-properties node) :save-as)))
      (gptel-runner-put run key (cdr parsed)))
    (puthash (gptel-runner-node-id node) 'succeeded
             (gptel-runner-run-node-states run))
    (if (eq (gptel-runner-run-state run) 'paused)
        (gptel-runner-resume-run run)
      (gptel-runner--checkpoint run))))

(provide 'gptel-runner-flow)
;;; gptel-runner-flow.el ends here
