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
`:parser', `:validator', and `:repair-invalid' customize result handling."
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
`:progress-key' are functions accepting the current run."
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

(defun gptel-runner--node-save-keys (node)
  "Return all blackboard keys written below NODE."
  (let ((own (and (memq (gptel-runner-node-kind node) '(agent parallel))
                  (plist-get (gptel-runner-node-properties node) :save-as))))
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
                 (user-error "Repeat node %S needs a :body" id)))
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
         (feedback (gethash 'gptel-runner-resume-feedback
                            (gptel-runner-run-blackboard run))))
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

(defun gptel-runner--execute-repeat (run node done)
  "Execute bounded repeat NODE in RUN and invoke DONE."
  (let* ((props (gptel-runner-node-properties node))
         (body (plist-get props :body))
         (maximum (plist-get props :max))
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
                   (gptel-runner--emit run 'iteration-completed node nil
                                       (list :iteration iteration :progress key))
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
              allow-unconfirmed-tools persist callback &allow-other-keys)
  "Start WORKFLOW with keyword ARGUMENTS and return its run immediately.
GOAL and WORKSPACE describe the stateless task.  DRIVER defaults to
`gptel-runner-default-driver'.  MAX-REQUESTS, MAX-CALLS, MAX-CONCURRENCY, and
MAX-DURATION override workflow defaults.  ALLOW-WRITES must be explicitly
non-nil for any workflow containing a write agent.
ALLOW-UNCONFIRMED-TOOLS disables gptel confirmation only when explicitly set.
PERSIST enables versioned snapshots at workflow checkpoints.
CALLBACK runs exactly once with the terminal run."
  (ignore max-requests max-calls max-concurrency max-duration
          allow-unconfirmed-tools)
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

(defun gptel-runner-resume-run (run &optional feedback callback)
  "Resume paused RUN with optional human FEEDBACK and CALLBACK.
Completed nodes remain complete.  The first unfinished agent prompt receives
FEEDBACK, and execution restarts from the workflow AST's safe checkpoint."
  (unless (eq (gptel-runner-run-state run) 'paused)
    (user-error "Run %s is not paused" (gptel-runner-run-id run)))
  (when feedback
    (puthash 'gptel-runner-resume-feedback feedback
             (gptel-runner-run-blackboard run)))
  (when callback
    (setf (gptel-runner-run-callback run) callback
          (gptel-runner-run-callback-called run) nil))
  (gptel-runner--supersede-paused-calls run)
  (gptel-runner--prepare-node-states-for-resume run)
  (cl-incf (gptel-runner-run-generation run))
  (setf (gptel-runner-run-state run) 'running
        (gptel-runner-run-paused-at run) nil
        (gptel-runner-run-finished-at run) nil
        (gptel-runner-run-active-calls run) nil
        (gptel-runner-run-queue run) nil
        (gptel-runner-run-active-count run) 0
        (gptel-runner-run-writer-active run) 0)
  (gptel-runner--emit run 'run-resumed nil nil
                      (and feedback (list :feedback feedback)))
  (gptel-runner--start-duration-clock run)
  (when (eq (gptel-runner-run-state run) 'running)
    (gptel-runner--execute-node
     run (gptel-runner-workflow-root (gptel-runner-run-workflow run))
     (lambda (state value)
       (when (eq (gptel-runner-run-state run) 'running)
         (gptel-runner--finish-run run state value)))))
  (gptel-runner--checkpoint run)
  run)

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
