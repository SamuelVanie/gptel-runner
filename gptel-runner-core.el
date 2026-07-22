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

(defgroup gptel-runner nil
  "Deterministic workflows over stateless agent calls."
  :group 'applications)

(defcustom gptel-runner-retain-worker-buffers t
  "When non-nil, gptel worker transcripts survive terminal calls.
Set this to nil to kill a worker buffer as soon as its call terminalizes."
  :type 'boolean)

(defcustom gptel-runner-default-driver nil
  "Driver used by `gptel-runner-start' when none is supplied."
  :type 'sexp)

(defvar gptel-runner-event-hook nil
  "Hook run with one argument, the newly appended runner event.")

(defvar-local gptel-runner--call nil
  "Runner call associated with the current worker buffer.")

(defvar gptel-runner--agents (make-hash-table :test #'eq)
  "Registered agents by symbolic name.")

(defvar gptel-runner--workflows (make-hash-table :test #'eq)
  "Registered workflows by symbolic name.")

(defvar gptel-runner--runs (make-hash-table :test #'equal)
  "Session-local runs by string identifier.")

(defvar gptel-runner--next-id 0)

(defconst gptel-runner-terminal-states
  '(succeeded failed blocked stalled cancelled skipped)
  "States after which an object cannot transition again.")

(defconst gptel-runner--retryable-statuses
  '(408 425 429 500 502 503 504 529))

(cl-defstruct (gptel-runner-agent
               (:constructor gptel-runner-agent-create))
  "Configuration used to invoke an agent."
  name preset workspace-mode schema parser validator retry-policy metadata)

(cl-defstruct (gptel-runner-node
               (:constructor gptel-runner-node-create))
  "A node in a workflow definition."
  id kind properties children)

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
  "One logical agent invocation."
  id run node agent prompt workspace buffer fsm
  (state 'pending) (request-attempt 0) retry-timer (generation 0)
  response-parts result error started-at finished-at on-complete
  driver-data repair-p)

(cl-defstruct (gptel-runner-run
               (:constructor gptel-runner-run-create))
  "Runtime state for one workflow execution."
  id workflow goal workspace (state 'pending) blackboard node-states
  iterations active-calls calls events budget driver queue (active-count 0)
  (writer-active 0) started-at finished-at callback callback-called
  duration-timer duration-remaining active-started-at
  paused-at snapshot-file (generation 0) options)

(cl-defgeneric gptel-runner-driver-start (driver call complete observe)
  "Start CALL with DRIVER.
COMPLETE accepts STATUS, VALUE, and optional METADATA.  OBSERVE accepts an
event type and data.  A driver must call COMPLETE at most once, but the core is
defensive against duplicate and late calls.")

(cl-defgeneric gptel-runner-driver-cancel (driver call)
  "Ask DRIVER to cancel CALL.")

(cl-defgeneric gptel-runner-driver-pause (driver call)
  "Pause CALL through DRIVER without terminalizing runner state.")

(cl-defmethod gptel-runner-driver-pause (driver call)
  "Default pause behavior asks DRIVER to cancel CALL's external work."
  (gptel-runner-driver-cancel driver call))

(defun gptel-runner--id (prefix)
  "Return a new identifier beginning with PREFIX."
  (format "%s-%d" prefix (cl-incf gptel-runner--next-id)))

(defun gptel-runner--terminal-p (state)
  "Return non-nil when STATE is terminal."
  (memq state gptel-runner-terminal-states))

(defun gptel-runner--call-terminal-p (call)
  "Return non-nil when CALL is terminal."
  (gptel-runner--terminal-p (gptel-runner-call-state call)))

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
`:parser', `:validator', `:retry-policy', and `:metadata'."
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
            :metadata (plist-get properties :metadata))))
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

(defun gptel-runner-get (run key &optional default)
  "Return RUN blackboard value for KEY, or DEFAULT."
  (gethash key (gptel-runner-run-blackboard run) default))

(defun gptel-runner-put (run key value)
  "Store VALUE at KEY on RUN's blackboard and return VALUE."
  (puthash key value (gptel-runner-run-blackboard run))
  (gptel-runner--checkpoint run)
  value)

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
  (dolist (call (gptel-runner-run-calls run))
    (when (buffer-live-p (gptel-runner-call-buffer call))
      (kill-buffer (gptel-runner-call-buffer call))
      (setf (gptel-runner-call-buffer call) nil)))
  (when-let ((events-buffer
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

(defun gptel-runner--set-node-state (run node state &optional data)
  "Set NODE state in RUN to STATE and emit its transition with DATA."
  (let* ((states (gptel-runner-run-node-states run))
         (id (gptel-runner-node-id node))
         (old (gethash id states 'pending)))
    (unless (or (eq old state) (gptel-runner--terminal-p old))
      (puthash id state states)
      (gptel-runner--emit run (intern (format "node-%s" state)) node nil data)
      t)))

(defun gptel-runner--persistent-p (run)
  "Return non-nil when RUN has durable snapshotting enabled."
  (plist-get (gptel-runner-run-options run) :persist))

(defun gptel-runner--checkpoint (run)
  "Persist RUN at a safe checkpoint when persistence is enabled."
  (when (and (gptel-runner--persistent-p run)
             (fboundp 'gptel-runner-save-run))
    (condition-case err
        (gptel-runner-save-run run)
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
  (when-let ((remaining (gptel-runner-run-duration-remaining run)))
    (if (<= remaining 0)
        (gptel-runner--duration-expired
         run (gptel-runner-run-generation run))
      (setf (gptel-runner-run-active-started-at run) (float-time)
            (gptel-runner-run-duration-timer run)
            (run-at-time remaining nil #'gptel-runner--duration-expired
                         run (gptel-runner-run-generation run))))))

(defun gptel-runner--finish-run (run state &optional data)
  "Terminalize RUN as STATE once, recording DATA."
  (unless (gptel-runner--run-terminal-p run)
    (setf (gptel-runner-run-state run) state
          (gptel-runner-run-finished-at run) (float-time))
    (gptel-runner--stop-duration-clock run)
    (gptel-runner--emit run
                        (if (eq state 'succeeded)
                            'run-completed
                          (intern (format "run-%s" state)))
                        nil nil data)
    (unless (gptel-runner-run-callback-called run)
      (setf (gptel-runner-run-callback-called run) t)
      (when-let ((callback (gptel-runner-run-callback run)))
        (funcall callback run)))
    (gptel-runner--checkpoint run)
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

(defun gptel-runner--writer-p (call)
  "Return non-nil when CALL's agent writes its workspace."
  (eq (gptel-runner-agent-workspace-mode (gptel-runner-call-agent call))
      'write))

(defun gptel-runner--call-observe (call generation type data)
  "Record a TYPE driver observation for CALL at GENERATION using DATA."
  (when (and (= generation (gptel-runner-call-generation call))
             (not (gptel-runner--call-terminal-p call)))
    (let ((run (gptel-runner-call-run call)))
      (cond
       ((eq type 'waiting-confirmation)
        (setf (gptel-runner-call-state call) 'waiting-confirmation))
       ((and (eq type 'tool-results)
             (eq (gptel-runner-call-state call) 'waiting-confirmation))
        (setf (gptel-runner-call-state call) 'running)))
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
             (memq status (gptel-runner-retry-policy-statuses policy)))
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
      t)))

(defun gptel-runner--schedule-request-retry (call metadata)
  "Schedule a transient request retry for CALL using METADATA."
  (let* ((run (gptel-runner-call-run call))
         (delay (gptel-runner--retry-delay call metadata))
         (generation (gptel-runner-call-generation call)))
    (setf (gptel-runner-call-state call) 'retry-wait)
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
      ('success
       (if (gptel-runner--empty-output-p value)
           (gptel-runner--finish-call
            call 'failed (gptel-runner--empty-output-error))
         (gptel-runner--finish-call call 'succeeded value)))
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

(defun gptel-runner--dispatch-attempt (call)
  "Dispatch one provider attempt for CALL."
  (let ((run (gptel-runner-call-run call)))
    (if (not (gptel-runner--consume-request run))
        (gptel-runner--finish-call
         call 'failed
         (gptel-runner--budget-failure
          run 'requests
          (gptel-runner-budget-max-requests (gptel-runner-run-budget run))))
      (cl-incf (gptel-runner-call-request-attempt call))
      (setf (gptel-runner-call-state call) 'running)
      (let ((generation (gptel-runner-call-generation call)))
        (gptel-runner--emit
         run 'request-started (gptel-runner-call-node call) call
         (list :attempt (gptel-runner-call-request-attempt call)))
        (condition-case err
            (gptel-runner-driver-start
             (gptel-runner-run-driver run) call
             (lambda (status &optional value metadata)
               (gptel-runner--driver-result
                call generation status value metadata))
             (lambda (type &optional data)
               (gptel-runner--call-observe call generation type data)))
          (error
           (gptel-runner--driver-result
            call generation 'permanent err (list :permanent t))))))))

(defun gptel-runner--finish-call (call state &optional value)
  "Terminalize CALL as STATE with VALUE exactly once."
  (unless (gptel-runner--call-terminal-p call)
    (let ((run (gptel-runner-call-run call)))
      (when (timerp (gptel-runner-call-retry-timer call))
        (cancel-timer (gptel-runner-call-retry-timer call)))
      (setf (gptel-runner-call-retry-timer call) nil
            (gptel-runner-call-state call) state
            (gptel-runner-call-finished-at call) (float-time))
      (if (eq state 'succeeded)
          (setf (gptel-runner-call-result call) value)
        (setf (gptel-runner-call-error call) value))
      (gptel-runner--deactivate-call call)
      (gptel-runner--emit run (intern (format "call-%s" state))
                          (gptel-runner-call-node call) call value)
      (when-let ((done (gptel-runner-call-on-complete call)))
        (setf (gptel-runner-call-on-complete call) nil)
        (funcall done call state value))
      (gptel-runner--drain-queue run)
      (gptel-runner--checkpoint run)
      t)))

(defun gptel-runner--can-start-p (run call)
  "Return non-nil when RUN can start queued CALL now."
  (and (< (gptel-runner-run-active-count run)
          (or (plist-get (gptel-runner-run-options run) :max-concurrency) 1))
       (or (not (gptel-runner--writer-p call))
           (zerop (gptel-runner-run-writer-active run)))))

(defun gptel-runner--drain-queue (run)
  "Process queued work in RUN as budgets and locks permit."
  (when (eq (gptel-runner-run-state run) 'running)
    (let ((progress t))
      (while progress
        (setq progress nil)
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
              (gptel-runner--dispatch-attempt found))
            (setq progress t)))))))

(defun gptel-runner--submit-call (run node agent prompt done &optional repair-p)
  "Queue an AGENT call for NODE in RUN, invoking DONE on terminalization."
  (if (not (gptel-runner--consume-call run))
      (progn
        (funcall done nil 'failed
                 (gptel-runner--budget-failure
                  run 'calls
                  (gptel-runner-budget-max-calls
                   (gptel-runner-run-budget run))))
        nil)
    (let ((call (gptel-runner-call-create
                 :id (gptel-runner--id "call") :run run :node node
                 :agent agent :prompt prompt
                 :workspace (gptel-runner-run-workspace run)
                 :state 'ready :on-complete done :repair-p repair-p)))
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
      (when (memq (gptel-runner-call-state call)
                  '(running retry-wait waiting-confirmation waiting-feedback))
        (ignore-errors
          (gptel-runner-driver-cancel (gptel-runner-run-driver run) call)))
      (gptel-runner--finish-call call 'cancelled (or reason 'user)))))

(defun gptel-runner--suspend-call (call state reason keep-continuation)
  "Suspend CALL in STATE for REASON.
When KEEP-CONTINUATION is nil, discard the in-memory workflow continuation so
the workflow can later be reconstructed from a snapshot."
  (unless (or (gptel-runner--call-terminal-p call)
              (memq (gptel-runner-call-state call) '(paused waiting-feedback)))
    (let ((run (gptel-runner-call-run call)))
      (cl-incf (gptel-runner-call-generation call))
      (when (timerp (gptel-runner-call-retry-timer call))
        (cancel-timer (gptel-runner-call-retry-timer call)))
      (setf (gptel-runner-call-retry-timer call) nil
            (gptel-runner-run-queue run)
            (delq call (gptel-runner-run-queue run)))
      (when (memq (gptel-runner-call-state call)
                  '(running retry-wait waiting-confirmation))
        (ignore-errors
          (gptel-runner-driver-pause (gptel-runner-run-driver run) call)))
      (gptel-runner--deactivate-call call)
      (unless keep-continuation
        (setf (gptel-runner-call-on-complete call) nil))
      (setf (gptel-runner-call-state call) state)
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
   (list (and (boundp 'gptel-runner--call) gptel-runner--call) 'user))
  (unless (gptel-runner-call-p call)
    (user-error "No runner call is associated with this buffer"))
  (unless (gptel-runner--suspend-call
           call 'waiting-feedback (or reason 'user) t)
    (user-error "Call %s cannot be paused from state %s"
                (gptel-runner-call-id call) (gptel-runner-call-state call)))
  (gptel-runner--checkpoint (gptel-runner-call-run call))
  call)

(defun gptel-runner-pause-run (run &optional reason)
  "Pause RUN for REASON and save a durable snapshot.
Active provider work is stopped.  Completed nodes and blackboard values are
preserved; unfinished nodes restart from their last safe checkpoint."
  (interactive
   (list (and (boundp 'gptel-runner--call)
              (gptel-runner-call-p gptel-runner--call)
              (gptel-runner-call-run gptel-runner--call))
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
  "Cancel RUN for REASON, including queued and active work."
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
