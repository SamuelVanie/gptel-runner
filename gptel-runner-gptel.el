;;; gptel-runner-gptel.el --- gptel compatibility driver -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.6"))

;;; Commentary:

;; The only compatibility boundary allowed to mention private gptel symbols.
;; Loading this file does not require gptel; compatibility is checked before a
;; live dispatch so the rest of gptel-runner remains usable with fake drivers.

;;; Code:

(require 'cl-lib)
(require 'text-property-search)
(require 'gptel-runner-core)

(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-make-fsm "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-fsm-state "gptel-request")
(declare-function gptel--fsm-transition "gptel-request")
(declare-function gptel--handle-post "gptel-request")
(declare-function gptel--apply-preset "gptel")
(declare-function gptel--insert-response "gptel")
(declare-function gptel-curl--stream-insert-response "gptel")
(declare-function gptel--update-tool-ask "gptel")
(declare-function gptel--update-tool-call "gptel")
(declare-function gptel--update-status "gptel")
(declare-function gptel--update-wait "gptel")
(declare-function gptel-mode "gptel")
(declare-function gptel-tool-name "gptel-request")
(declare-function gptel--accept-tool-calls "gptel")
(declare-function gptel-runner--complete-restored-call "gptel-runner-flow")
(declare-function gptel-runner-resume-run "gptel-runner-flow")

(defvar gptel-request--transitions)
(defvar gptel-request--handlers)
(defvar gptel-send--transitions)
(defvar gptel-send--handlers)
(defvar gptel-confirm-tool-calls)
(defvar gptel-default-mode)
(defvar gptel-stream)
(defvar gptel-prompt-transform-functions)

;; Optional integration uses only public gptel-agent APIs.  Agent lifecycle
;; objects and callbacks stay out of scheduler records and persisted jobs.
(declare-function gptel-agent-supervise "gptel-agent-tools")
(declare-function gptel-agent-supervision-stop "gptel-agent-tools")
(declare-function gptel-agent-supervision-retry "gptel-agent-tools")
(declare-function gptel-agent-supervision-request-live-p "gptel-agent-tools")

(cl-defstruct (gptel-runner-gptel-driver
               (:constructor gptel-runner-gptel-driver-create))
  "Driver backed by `gptel-request'.")

(defvar gptel-runner-gptel--cancelling nil)

(defvar gptel-runner-headless nil
  "Non-nil when runner interactions are handled without local UI.")

(defvar gptel-runner-interaction-hook nil
  "Hook receiving a call and a serializable pending interaction plist.
Live tool callbacks remain private to the driver's call data.  Consumers
must route replies back to that owning process.")

(defun gptel-runner-gptel--tool-data (tools &optional results)
  "Return data-only descriptions of TOOLS, optionally including RESULTS."
  (mapcar
   (lambda (entry)
     (append (list :name (gptel-tool-name (car entry))
                   :args (copy-tree (cadr entry) t))
             (when results (list :result (copy-tree (nth 2 entry) t)))))
   tools))

(defun gptel-runner-gptel--interactions (call)
  "Return CALL's private pending approvals, including legacy single entries."
  (let ((data (gptel-runner-call-driver-data call)))
    (or (plist-get data :pending-interactions)
        (when-let* ((pending (plist-get data :pending-interaction)))
          (list pending)))))

(defun gptel-runner-gptel--set-interactions (call pending)
  "Store PENDING approvals for CALL, keeping the single-entry view compatible."
  (setf (gptel-runner-call-driver-data call)
        (plist-put (plist-put (gptel-runner-call-driver-data call)
                              :pending-interactions pending)
                   :pending-interaction (car pending))))

(defun gptel-runner-gptel--interaction-live-p (call pending)
  "Whether PENDING still belongs to a live request in CALL's generation."
  (let ((description (plist-get pending :description))
        (fsm (plist-get pending :fsm)))
    (and (not (gptel-runner--call-terminal-p call))
         (= (gptel-runner-call-generation call)
            (plist-get description :generation))
         (buffer-live-p (or (plist-get pending :buffer)
                            (gptel-runner-call-buffer call)))
         (or (null (plist-get pending :agent-supervision))
             (gptel-agent-supervision-request-live-p
              (plist-get pending :agent-supervision) fsm))
         (or (null fsm)
             (and (eq (gptel-fsm-state fsm) 'TOOL)
                  (eq (plist-get pending :info) (gptel-fsm-info fsm)))))))

(defun gptel-runner-gptel-interaction-pending-p (call id)
  "Whether approval ID is still pending on CALL."
  (cl-some (lambda (pending)
             (and (equal id (plist-get (plist-get pending :description) :id))
                  (gptel-runner-gptel--interaction-live-p call pending)))
           (gptel-runner-gptel--interactions call)))

(defun gptel-runner-gptel--sync-interactions (call)
  "Discard stale approvals and retain CALL's waiting state for live ones."
  (let* ((previous (gptel-runner-gptel--interactions call))
         (pending (cl-remove-if-not
                  (lambda (entry)
                    (gptel-runner-gptel--interaction-live-p call entry))
                  previous)))
    (gptel-runner-gptel--set-interactions call pending)
    (cond
     ((and pending
           (not (eq (gptel-runner-call-state call) 'waiting-confirmation)))
      (gptel-runner-gptel--observe call 'waiting-confirmation))
     ((and previous (null pending)
           (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (gptel-runner-gptel--observe
       call 'waiting-tool (gptel-runner-call-tool-names call))))))

(defun gptel-runner-gptel--pending-interaction (call tools &optional fsm scope)
  "Publish CALL's approval for TOOLS from FSM, optionally supervised by SCOPE."
  (let ((interaction
         (list :id (gptel-runner--id "interaction") :kind 'tool-approval
               :call-id (gptel-runner-call-id call)
               :generation (gptel-runner-call-generation call)
               :source-buffer (buffer-name
                               (if fsm (plist-get (gptel-fsm-info fsm) :buffer)
                                 (gptel-runner-call-buffer call)))
               :tools (gptel-runner-gptel--tool-data tools))))
    (gptel-runner-gptel--set-interactions
     call (append (gptel-runner-gptel--interactions call)
                  (list (list :description interaction :tools tools :fsm fsm
                              :agent-supervision scope
                              :info (and fsm (gptel-fsm-info fsm))
                              :buffer (if fsm (plist-get (gptel-fsm-info fsm) :buffer)
                                        (gptel-runner-call-buffer call))))))
    (gptel-runner-gptel--sync-interactions call)
    (run-hook-with-args 'gptel-runner-interaction-hook call
                        (copy-tree interaction t))
    interaction))

(defun gptel-runner-gptel-answer-interaction (call interaction-id approve)
  "Answer CALL's pending INTERACTION-ID, running tools when APPROVE is non-nil.
Rejecting cancels the call.  Replies are accepted once and only while the
interaction's generation is current."
  (unless (gptel-runner-call-p call)
    (user-error "Unknown runner call"))
  (let* ((pending (cl-find interaction-id (gptel-runner-gptel--interactions call)
                           :key (lambda (entry)
                                  (plist-get (plist-get entry :description) :id))
                           :test #'equal))
         (description (plist-get pending :description)))
    (unless (and pending
                 (gptel-runner-gptel--interaction-live-p call pending))
      (user-error "Interaction %s is no longer pending" interaction-id))
    ;; Consume before executing: synchronous tools may create a new gate.
    (gptel-runner-gptel--set-interactions
     call (remq pending (gptel-runner-gptel--interactions call)))
    (gptel-runner--emit
     (gptel-runner-call-run call) 'interaction-answered
     (gptel-runner-call-node call) call
     (list :id interaction-id :approved (and approve t)))
    (if approve
        (with-current-buffer (or (plist-get pending :buffer)
                                 (gptel-runner-call-buffer call))
          (gptel-runner-gptel--observe
           call 'waiting-tool
           (mapcar (lambda (tool) (plist-get tool :name))
                   (plist-get description :tools)))
          (unwind-protect
              (gptel--accept-tool-calls (plist-get pending :tools))
            (gptel-runner-gptel--sync-interactions call)))
      (gptel-runner-abort-call call 'tool-rejected))
    call))

(defun gptel-runner-gptel--compatibility-error (&rest missing)
  "Signal an actionable compatibility error listing MISSING API pieces."
  (error (concat "gptel-runner compatibility failure: missing or unsupported "
                 "%S.  Install gptel >= 0.9.9.6 or update "
                 "gptel-runner-gptel.el for this gptel version")
         missing))

(defun gptel-runner-gptel--start-supervision (call)
  "Attach optional gptel-agent supervision to CALL's worker conversation."
  (when (featurep 'gptel-agent-tools)
    (dolist (function '(gptel-agent-supervise gptel-agent-supervision-stop
                        gptel-agent-supervision-retry
                        gptel-agent-supervision-request-live-p))
      (unless (fboundp function)
        (error "Update gptel-agent: runner integration requires %s" function)))
    (unless (plist-get (gptel-runner-call-driver-data call) :agent-supervision)
      (let ((generation (gptel-runner-call-generation call)))
        (setf (gptel-runner-call-driver-data call)
              (plist-put
               (gptel-runner-call-driver-data call) :agent-supervision
               (gptel-agent-supervise
                (gptel-runner-call-buffer call)
                :on-state-change
                (lambda (_scope _fsm event)
                  (when (and (= generation (gptel-runner-call-generation call))
                             (not (gptel-runner--call-terminal-p call)))
                    (when (eq event 'waiting-discussions)
                      (gptel-runner-gptel--observe call 'waiting-tool '("Discussion")))
                    (gptel-runner-gptel--sync-interactions call)))
                :on-tool-approval
                (lambda (scope fsm calls)
                  (when gptel-runner-headless
                    (when (and (= generation (gptel-runner-call-generation call))
                               (not (gptel-runner--call-terminal-p call)))
                      (gptel-runner-gptel--pending-interaction call calls fsm scope))
                    t)))))))))

(defun gptel-runner-gptel--stop-supervision (call)
  "Release CALL's optional agent supervisor and pending approvals."
  (when-let* ((scope (plist-get (gptel-runner-call-driver-data call)
                                :agent-supervision)))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :agent-supervision nil))
    (gptel-agent-supervision-stop scope))
  (gptel-runner-gptel--set-interactions call nil))

(defun gptel-runner-gptel--check-api ()
  "Load gptel and validate the supported request API shape."
  (unless (require 'gptel nil t)
    (gptel-runner-gptel--compatibility-error 'gptel))
  (unless (require 'gptel-request nil t)
    (gptel-runner-gptel--compatibility-error 'gptel-request-library))
  (dolist (function '(gptel-request gptel-abort gptel-make-fsm
                      gptel--fsm-transition gptel--handle-post
                      gptel--apply-preset gptel--insert-response
                      gptel-curl--stream-insert-response
                      gptel--update-tool-ask gptel--update-tool-call
                      gptel--update-status gptel--update-wait
                      gptel--accept-tool-calls gptel--handle-pre-tool
                      gptel--handle-post-tool gptel--handle-tool-steer
                      gptel--tool-steer-p))
    (unless (fboundp function)
      (gptel-runner-gptel--compatibility-error function)))
  t)

(defun gptel-runner-gptel--worker-buffer (call)
  "Return CALL's live worker buffer, creating it if necessary."
  (or (and (buffer-live-p (gptel-runner-call-buffer call))
           (gptel-runner-call-buffer call))
      (let ((buffer
             (generate-new-buffer
              (format "*gptel-runner:%s:%s:%s*"
                      (gptel-runner-run-id (gptel-runner-call-run call))
                      (gptel-runner-node-id (gptel-runner-call-node call))
                      (gptel-runner-call-id call)))))
        (setf (gptel-runner-call-buffer call) buffer)
        (with-current-buffer buffer
          (when (and (boundp 'gptel-default-mode)
                     (symbolp gptel-default-mode)
                     (fboundp gptel-default-mode))
            (funcall gptel-default-mode))
          (when (fboundp 'gptel-mode) (gptel-mode 1))
          ;; Major-mode initialization clears buffer-local variables, so set
          ;; runner identity and workspace only after the display modes.
          (setq-local default-directory (gptel-runner-call-workspace call))
          (setq-local gptel-runner--call call)
          (let ((inhibit-read-only t)
                (run (gptel-runner-call-run call)))
            (insert
             (format
              (concat "Runner call: %s\nRun: %s\nNode: %s\nAgent: %s\n"
                      "Workspace: %s\n\nUser prompt\n===========\n\n%s\n")
              (gptel-runner-call-id call)
              (gptel-runner-run-id run)
              (gptel-runner-node-id (gptel-runner-call-node call))
              (gptel-runner-agent-name (gptel-runner-call-agent call))
              (gptel-runner-call-workspace call)
              (gptel-runner-call-prompt call)))
            (goto-char (point-max))))
        buffer)))

(defun gptel-runner-gptel-restore-worker-buffer (call transcript)
  "Restore CALL's human-readable worker buffer from TRANSCRIPT."
  (gptel-runner-gptel--check-api)
  (let ((buffer (gptel-runner-gptel--worker-buffer call)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert transcript)
        (goto-char (point-max)))
      (setq-local gptel-runner--call call)
      (setq-local default-directory (gptel-runner-call-workspace call))
      (gptel-runner-gptel--apply-preset-locally
       (gptel-runner-agent-preset (gptel-runner-call-agent call))))
    buffer))

(defun gptel-runner-gptel--apply-preset-locally (preset)
  "Apply symbol or plist PRESET in the current worker buffer.
The symbol path is intentionally isolated here because gptel's buffer-local
preset setter is private and has changed across releases."
  (cond
   ((null preset) nil)
   ((or (listp preset) (symbolp preset))
    (unless (fboundp 'gptel--apply-preset)
      (gptel-runner-gptel--compatibility-error 'gptel--apply-preset))
    (gptel--apply-preset
     preset (lambda (symbol value)
              (set (make-local-variable symbol) value))))
   (t (gptel-runner-gptel--compatibility-error 'preset-shape))))

(defun gptel-runner-gptel--status-metadata (info)
  "Extract stable retry metadata from gptel callback INFO."
  (let ((status (plist-get info :http-status))
        (retry-after (or (plist-get info :retry-after)
                         (plist-get info :retry_after))))
    (list :http-status (and status
                            (if (stringp status)
                                (string-to-number status)
                              status))
          :retry-after (and retry-after
                            (if (stringp retry-after)
                                (string-to-number retry-after)
                              retry-after))
          :retryable (plist-get info :runner-incomplete-tool-call)
          :stop-reason (plist-get info :stop-reason)
          :status (plist-get info :status)
          :error (plist-get info :error))))

(defun gptel-runner-gptel--prepare-request (fsm)
  "Reset per-response state before sending runner FSM's next request."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context)))
    ;; Keep intermediate commentary in the transcript and journal, but hand
    ;; only the final model response to downstream nodes and output parsers.
    (when (gptel-runner-call-p call)
      (setf (gptel-runner-call-response-parts call) nil))
    (plist-put info :stop-reason nil)
    (plist-put info :runner-incomplete-tool-call nil)))

(defun gptel-runner-gptel--incomplete-tool-response-p (info)
  "Return non-nil for an incomplete tool response in request INFO.
Used as a TYPE transition predicate, after the full response is parsed."
  ;; Some providers report a tool-call stop with only introductory text
  ;; and no tool payload.  The default TYPE transition treats this as DONE.
  ;; Check before TOOL so schema tools that become final JSON remain valid.
  (when (and (not (plist-get info :error))
             (member (plist-get info :stop-reason)
                     '("tool_calls" "tool_use" "function_call"))
             (null (plist-get info :tool-use)))
    (plist-put info :runner-incomplete-tool-call t)
    (plist-put info :error
               (format "Incomplete tool response: stop reason %S without tool calls"
                       (plist-get info :stop-reason)))))

(defun gptel-runner-gptel--insert-response (response info &optional raw)
  "Render RESPONSE using the transport described by INFO, honoring RAW."
  (funcall (if (plist-get info :stream)
               #'gptel-curl--stream-insert-response
             #'gptel--insert-response)
           response info raw))

(defun gptel-runner-gptel--callback (call observe response info &optional raw)
  "Use OBSERVE to record gptel RESPONSE and INFO for CALL.
RAW is gptel's internal flag for already formatted transcript content."
  (cond
   ((stringp response)
    (unless raw
      (push response (gptel-runner-call-response-parts call))
      (funcall observe 'response response))
    (gptel-runner-gptel--insert-response response info raw))
   ;; Streaming completion ends one HTTP response, not the agent's tool loop.
   ((memq response '(nil abort t)) nil)
   ((consp response)
    (pcase (car response)
      ('reasoning
       (when (stringp (cdr response))
         (funcall observe 'reasoning (cdr response)))
       (gptel-runner-gptel--insert-response response info))
      ('tool-call
       (let ((data (if gptel-runner-headless
                       (gptel-runner-gptel--tool-data (cdr response))
                     (cdr response))))
         (funcall observe 'tool-calls data)
         (funcall observe 'waiting-confirmation data))
       (if gptel-runner-headless
           (gptel-runner-gptel--pending-interaction
            call (cdr response) (gptel-runner-call-fsm call))
         ;; The normal renderer adds its confirmation UI and callbacks.
         (gptel-runner-gptel--insert-response response info)))
      ('tool-result
       (funcall observe 'tool-results
                (if gptel-runner-headless
                    (gptel-runner-gptel--tool-data (cdr response) t)
                  (cdr response)))
       (gptel-runner-gptel--insert-response response info))
      (_ (funcall observe 'response-part response))))
   (t (funcall observe 'response-part response)))
  (gptel-runner-gptel--sync-interactions call))

(defun gptel-runner-gptel--post-once (call fsm)
  "Run gptel post-processing for CALL and FSM exactly once."
  (unless (plist-get (gptel-runner-call-driver-data call) :posted)
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :posted t))
    (gptel--handle-post fsm)))

(defun gptel-runner-gptel--complete-function (call)
  "Return the current scheduler completion closure for CALL."
  (plist-get (gptel-runner-call-driver-data call) :complete))

(defun gptel-runner-gptel--observe (call type &optional data)
  "Report a TYPE observation with DATA for CALL's current attempt."
  (when-let* ((observe
              (plist-get (gptel-runner-call-driver-data call) :observe)))
    (funcall observe type data)))

(defun gptel-runner-gptel--tool-names (info)
  "Return the tool names from gptel request INFO."
  (delq nil
        (mapcar
         (lambda (tool-use)
           (let ((name (and (listp tool-use)
                            (plist-get tool-use :name))))
             (cond
              ((stringp name) name)
              ((symbolp name) (symbol-name name)))))
         (plist-get info :tool-use))))

(defun gptel-runner-gptel--tool-started (fsm)
  "Mark the runner call for FSM as waiting for its tool results."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (names (gptel-runner-gptel--tool-names info)))
    (when (and (gptel-runner-call-p call) names
               (or (null (gptel-runner-call-fsm call))
                   (eq fsm (gptel-runner-call-fsm call))))
      (gptel-runner-gptel--observe call 'waiting-tool names))))

(defun gptel-runner-gptel--update-status (info message face)
  "Show MESSAGE with FACE in the worker buffer described by INFO."
  (when-let* ((buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (gptel--update-status message face))))

(defun gptel-runner-gptel--done (fsm)
  "Handle successful terminal state for runner FSM."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (value (mapconcat #'identity
                           (reverse (gptel-runner-call-response-parts call)) "")))
    (gptel-runner-gptel--update-status info " Ready" 'success)
    (gptel-runner-gptel--post-once call fsm)
    (funcall (gptel-runner-gptel--complete-function call) 'success value nil)))

(defun gptel-runner-gptel--error (fsm)
  "Handle error state for runner FSM, preserving it for transient retries."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (metadata (gptel-runner-gptel--status-metadata info))
         (status (plist-get metadata :http-status))
         (retryable (gptel-runner--retryable-p call status metadata)))
    (gptel-runner-gptel--update-status
     info (format " Error: %s"
                  (or (and (plist-get metadata :retryable)
                           (plist-get metadata :error))
                      status "request failed"))
     'error)
    (unless retryable (gptel-runner-gptel--post-once call fsm))
    (funcall (gptel-runner-gptel--complete-function call)
             (if (or retryable (plist-get metadata :retryable) (null status)
                     (memq status gptel-runner--retryable-statuses))
                 'transient
               'permanent)
             (plist-get info :error) metadata)))

(defun gptel-runner-gptel--aborted (fsm)
  "Handle aborted terminal state for runner FSM."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context)))
    (gptel-runner-gptel--update-status info " Abort" 'error)
    (gptel-runner-gptel--post-once call fsm)
    (if (plist-get (gptel-runner-call-driver-data call) :pausing)
        (setf (gptel-runner-call-driver-data call)
              (plist-put (gptel-runner-call-driver-data call) :pausing nil))
      (funcall (gptel-runner-gptel--complete-function call)
               'cancelled nil nil))))

(defun gptel-runner-gptel--make-fsm (&optional terminal-handler)
  "Create a per-call FSM with runner terminals and gptel status handlers.
When supplied, TERMINAL-HANDLER handles all three terminal states."
  (let ((handlers (copy-tree gptel-request--handlers))
        (table (copy-tree gptel-send--transitions)))
    ;; Streaming enters TYPE on the HTTP headers, before tool chunks arrive.
    ;; Validate when leaving TYPE so both transports have parsed the payload.
    (push (cons #'gptel-runner-gptel--incomplete-tool-response-p 'ERRS)
          (alist-get 'TYPE table))
    ;; `gptel-request--handlers' drive the transport but intentionally omit
    ;; the status handlers used by interactive gptel buffers.  Runner worker
    ;; buffers enable `gptel-mode', so compose those handlers back in.  The
    ;; TOOL state tracking and status must run before tool execution because a
    ;; synchronous tool can transition the FSM recursively before its handler
    ;; returns.
    ;; Use the send transition table and tool handlers so pre/post-tool hooks,
    ;; request-local cleanup and steering retain gptel's normal ordering.
    ;; Response insertion and terminal completion remain owned by the runner.
    (dolist (state '(TPRE TRET TSTR))
      (setf (alist-get state handlers)
            (copy-sequence (alist-get state gptel-send--handlers))))
    (setf (alist-get 'WAIT handlers)
          (append (list #'gptel-runner-gptel--prepare-request)
                  (alist-get 'WAIT handlers) (list #'gptel--update-wait))
          (alist-get 'TOOL handlers)
          (append (list #'gptel-runner-gptel--tool-started
                        #'gptel--update-tool-call)
                  (alist-get 'TOOL handlers)
                  (list #'gptel--update-tool-ask))
          (alist-get 'DONE handlers)
          (list (or terminal-handler #'gptel-runner-gptel--done))
          (alist-get 'ERRS handlers)
          (list (or terminal-handler #'gptel-runner-gptel--error))
          (alist-get 'ABRT handlers)
          (list (or terminal-handler #'gptel-runner-gptel--aborted)))
    (gptel-make-fsm :table table
                    :handlers handlers)))

(defun gptel-runner-gptel--cleanup (call)
  "Clean CALL's worker buffer unless retention is enabled."
  (gptel-runner-gptel--stop-supervision call)
  (when (and (not gptel-runner-retain-worker-buffers)
             (buffer-live-p (gptel-runner-call-buffer call)))
    (kill-buffer (gptel-runner-call-buffer call))
    (setf (gptel-runner-call-buffer call) nil)))

(cl-defmethod gptel-runner-driver-start
  ((_driver gptel-runner-gptel-driver) call complete observe)
  "Start CALL through gptel and report with COMPLETE and OBSERVE."
  (gptel-runner-gptel--check-api)
  (let* ((buffer (gptel-runner-gptel--worker-buffer call))
         (agent (gptel-runner-call-agent call))
         (schema (gptel-runner-agent-schema agent))
         (generation (gptel-runner-call-generation call))
         (completed nil)
         (wrapped
          (lambda (status &optional value metadata)
            (unless completed
              (setq completed t)
              (funcall complete status value metadata)
              (when (gptel-runner--call-terminal-p call)
                (gptel-runner-gptel--cleanup call))))))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call)
                     :complete wrapped))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call)
                     :observe observe))
    (with-current-buffer buffer
      (if (and (gptel-runner-call-fsm call)
               (eq (gptel-fsm-state (gptel-runner-call-fsm call)) 'ERRS))
          ;; A transport retry resumes the exact FSM after its backoff timer.
          (progn
            (when-let* ((scope (plist-get (gptel-runner-call-driver-data call)
                                          :agent-supervision)))
              (gptel-agent-supervision-retry scope (gptel-runner-call-fsm call)))
            (gptel--fsm-transition (gptel-runner-call-fsm call) 'WAIT))
        (gptel-runner-gptel--apply-preset-locally
         (gptel-runner-agent-preset agent))
        (if (plist-get (gptel-runner-run-options
                        (gptel-runner-call-run call))
                       :allow-unconfirmed-tools)
            (setq-local gptel-confirm-tool-calls nil)
          (when (gptel-runner--writer-p call)
            ;; A writable preset cannot silently weaken the run safety gate.
            (setq-local gptel-confirm-tool-calls t)))
        (gptel-runner-gptel--start-supervision call)
        (let ((fsm (gptel-runner-gptel--make-fsm)))
          (setf (gptel-runner-call-fsm call) fsm)
          (gptel-request
              (gptel-runner-call-prompt call)
            :buffer buffer :stream gptel-stream
            :context call :schema schema :fsm fsm
            :transforms gptel-prompt-transform-functions
            :callback
            (lambda (response info &optional raw)
              (when (and (= generation (gptel-runner-call-generation call))
                         (not (gptel-runner--call-terminal-p call)))
                (gptel-runner-gptel--callback
                 call observe response info raw)))))))))

(cl-defmethod gptel-runner-driver-cancel
  ((_driver gptel-runner-gptel-driver) call)
  "Cancel CALL using its unique worker buffer."
  (gptel-runner-gptel--stop-supervision call)
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (let ((gptel-runner-gptel--cancelling t))
      (gptel-abort (gptel-runner-call-buffer call)))
    (when (and (not (gptel-runner--call-terminal-p call))
               (gptel-runner-call-fsm call)
               (not (eq (gptel-fsm-state (gptel-runner-call-fsm call))
                        'ABRT)))
      ;; During retry backoff or confirmation there may be no process for
      ;; `gptel-abort' to find, so explicitly enter the same ABRT handler.
      (gptel--fsm-transition (gptel-runner-call-fsm call) 'ABRT))
    (gptel-runner-gptel--cleanup call)))

(cl-defmethod gptel-runner-driver-pause
  ((_driver gptel-runner-gptel-driver) call)
  "Stop CALL's provider work while retaining its transcript for feedback."
  (gptel-runner-gptel--stop-supervision call)
  (setf (gptel-runner-call-driver-data call)
        (plist-put (gptel-runner-call-driver-data call) :pausing t))
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (let ((gptel-runner-gptel--cancelling t))
      (gptel-abort (gptel-runner-call-buffer call)))
    (when (and (gptel-runner-call-fsm call)
               (not (eq (gptel-fsm-state (gptel-runner-call-fsm call))
                        'ABRT)))
      (gptel--fsm-transition (gptel-runner-call-fsm call) 'ABRT))
    (gptel-runner-gptel--insert-feedback-instructions call)))

(defun gptel-runner-gptel--insert-feedback-instructions (call)
  "Append human-feedback instructions to CALL's retained worker buffer."
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (with-current-buffer (gptel-runner-call-buffer call)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert
         (concat
          "\n\nRunner intervention\n===================\n\n"
          (if gptel-runner-headless
              (concat
               "This workflow call is waiting for your feedback.  Select "
               "the call in the background dashboard and use Feedback to "
               "request a revision.  Use Accept to return the latest or "
               "your edited response to the workflow.\n")
            (concat
             "This workflow call is waiting for your feedback.  Add feedback "
             "and continue with ordinary gptel commands in this buffer.  When "
             "the response should be returned to the workflow, select it or "
             "leave point after the latest response and run M-x "
             "gptel-runner-complete-call-from-buffer.\n"))))))))

(cl-defmethod gptel-runner-driver-await-feedback
  ((_driver gptel-runner-gptel-driver) call)
  "Make completed CALL's transcript available for human feedback."
  (gptel-runner-gptel--insert-feedback-instructions call))

(defun gptel-runner-gptel--last-response (buffer)
  "Return the last gptel response text found in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when-let* ((match (text-property-search-backward
                         'gptel 'response t)))
        (buffer-substring-no-properties
         (prop-match-beginning match) (prop-match-end match))))))

(defun gptel-runner-gptel--feedback-finished (call generation state fsm)
  "Finish CALL's feedback request at GENERATION in STATE using FSM."
  (unless (plist-get (gptel-fsm-info fsm) :runner-feedback-posted)
    (plist-put (gptel-fsm-info fsm) :runner-feedback-posted t)
    (gptel--handle-post fsm))
  (when (eq fsm (gptel-runner-call-fsm call))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :feedback-active nil)))
  (when (and (= generation (gptel-runner-call-generation call))
             (not (gptel-runner--call-terminal-p call)))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :feedback-active nil))
    (gptel-runner-gptel--stop-supervision call)
    (setf (gptel-runner-call-state call) 'waiting-feedback
          (gptel-runner-call-tool-names call) nil)
    (gptel-runner--deactivate-call call)
    (gptel-runner--emit
     (gptel-runner-call-run call)
     (if (eq state 'succeeded) 'feedback-completed 'feedback-failed)
     (gptel-runner-call-node call) call
     (list :state state
           :error (plist-get (gptel-fsm-info fsm) :error)))
    (gptel-runner--checkpoint (gptel-runner-call-run call) t)
    (gptel-runner--drain-queue (gptel-runner-call-run call))))

(defun gptel-runner-gptel-feedback (call text)
  "Asynchronously ask paused CALL to respond to feedback TEXT.
The conversation continues in its real worker buffer.  The new response
still requires explicit acceptance using
`gptel-runner-complete-call-from-buffer'."
  (unless (and (gptel-runner-call-p call)
               (memq (gptel-runner-call-state call) '(paused waiting-feedback))
               (buffer-live-p (gptel-runner-call-buffer call)))
    (user-error "Call is not available for feedback"))
  (unless (and (stringp text) (not (string-blank-p text)))
    (user-error "Feedback must be a non-empty string"))
  (when (plist-get (gptel-runner-call-driver-data call) :feedback-active)
    (user-error "A feedback request is already active"))
  (gptel-runner-gptel--check-api)
  (unless (gptel-runner--can-start-p (gptel-runner-call-run call) call)
    (user-error "Feedback cannot start while its workspace or concurrency slot is busy"))
  (push call (gptel-runner-run-active-calls (gptel-runner-call-run call)))
  (cl-incf (gptel-runner-run-active-count (gptel-runner-call-run call)))
  (when (gptel-runner--writer-p call)
    (cl-incf (gptel-runner-run-writer-active (gptel-runner-call-run call))))
  (let* ((generation (cl-incf (gptel-runner-call-generation call)))
         (fsm (gptel-runner-gptel--make-fsm
               (lambda (finished-fsm)
                 (gptel-runner-gptel--feedback-finished
                  call generation
                  (pcase (gptel-fsm-state finished-fsm)
                    ('DONE 'succeeded) ('ABRT 'cancelled) (_ 'failed))
                  finished-fsm))))
         (observe (lambda (type &optional data)
                    (gptel-runner--call-observe call generation type data))))
    ;; Use the ordinary request machinery and transcript parser, but never
    ;; release the workflow continuation until the user accepts the response.
    (setf (gptel-runner-call-fsm call) fsm
          (gptel-runner-call-state call) 'running
          (gptel-runner-call-driver-data call)
          (plist-put (plist-put (gptel-runner-call-driver-data call)
                                :observe observe)
                     :feedback-active t))
    (with-current-buffer (gptel-runner-call-buffer call)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\n\n" (substring-no-properties text) "\n"))
      (gptel-runner--emit (gptel-runner-call-run call) 'feedback-started
                          (gptel-runner-call-node call) call
                          (list :text text))
      (condition-case err
          (progn
            (gptel-runner-gptel--start-supervision call)
            (gptel-request
           nil :buffer (current-buffer) :stream gptel-stream
           :context call :fsm fsm
           :transforms gptel-prompt-transform-functions
           :callback
           (lambda (response info &optional raw)
             (when (and (= generation (gptel-runner-call-generation call))
                        (not (gptel-runner--call-terminal-p call)))
               (gptel-runner-gptel--callback call observe response info raw)))))
        (error
         (gptel-runner-gptel--stop-supervision call)
         (setf (gptel-runner-call-state call) 'waiting-feedback)
         (gptel-runner--deactivate-call call)
         (setf (gptel-runner-call-driver-data call)
               (plist-put (gptel-runner-call-driver-data call)
                          :feedback-active nil))
         (signal (car err) (cdr err)))))
    call))

(defun gptel-runner-complete-call-from-buffer (&optional result call)
  "Complete paused CALL with RESULT from its gptel worker buffer.
Interactively, use the active region when present, otherwise use the last text
marked by gptel as a response.  In the original Emacs session this releases
the preserved workflow continuation.  For a restored snapshot it records the
node result and reconstructs the continuation from the workflow AST."
  (interactive
   (list (and (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end)))
         nil))
  (setq call (or call
                 (and (boundp 'gptel-runner--call) gptel-runner--call)))
  (unless (gptel-runner-call-p call)
    (user-error "No runner call is associated with this buffer"))
  (unless (memq (gptel-runner-call-state call) '(waiting-feedback paused))
    (user-error "Call %s is not waiting for feedback" (gptel-runner-call-id call)))
  (when (plist-get (gptel-runner-call-driver-data call) :feedback-active)
    (user-error "Wait for the active feedback request before accepting"))
  (setq result
        (or result
            (and (buffer-live-p (gptel-runner-call-buffer call))
                 (gptel-runner-gptel--last-response
                  (gptel-runner-call-buffer call)))))
  (unless (and (stringp result) (not (string-blank-p result)))
    (user-error "Select a response or continue the gptel conversation first"))
  (let* ((run (gptel-runner-call-run call))
         (resume-afterward (eq (gptel-runner-run-state run) 'paused)))
    (if (gptel-runner-call-on-complete call)
        (gptel-runner--finish-call call 'succeeded result)
      (gptel-runner--complete-restored-call call result))
    (when (and resume-afterward
               (eq (gptel-runner-run-state run) 'paused))
      (gptel-runner-resume-run run)))
  call)

(defun gptel-runner-gptel--around-abort (original &optional buffer)
  "Integrate ORIGINAL `gptel-abort' with runner worker BUFFER."
  (let* ((target (or buffer (current-buffer)))
         (call (and (buffer-live-p target)
                    (buffer-local-value 'gptel-runner--call target))))
    (when (and call (not gptel-runner-gptel--cancelling)
               (memq (gptel-runner-call-state call)
                     '(running retry-wait waiting-tool waiting-confirmation))
               (not (gptel-runner--call-terminal-p call)))
      (gptel-runner-abort-call call 'gptel-abort))
    (funcall original target)))

(defun gptel-runner-gptel--around-accept-tool-calls
    (original &optional response overlay)
  "Mark a confirmed runner tool active before calling ORIGINAL.
RESPONSE and OVERLAY are the arguments accepted by
`gptel--accept-tool-calls'."
  (let* ((target (or (and (overlayp overlay) (overlay-buffer overlay))
                     (current-buffer)))
         (call (and (buffer-live-p target)
                    (buffer-local-value 'gptel-runner--call target))))
    (when (and (gptel-runner-call-p call)
               (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (gptel-runner-gptel--observe
       call 'waiting-tool (gptel-runner-call-tool-names call)))
    (funcall original response overlay)))

(defun gptel-runner-gptel-install-abort-advice ()
  "Install conditional runner integration around gptel commands."
  (interactive)
  (gptel-runner-gptel--check-api)
  (unless (advice-member-p #'gptel-runner-gptel--around-abort 'gptel-abort)
    (advice-add 'gptel-abort :around #'gptel-runner-gptel--around-abort))
  (unless (advice-member-p #'gptel-runner-gptel--around-accept-tool-calls
                           'gptel--accept-tool-calls)
    (advice-add 'gptel--accept-tool-calls :around
                #'gptel-runner-gptel--around-accept-tool-calls)))

(with-eval-after-load 'gptel
  (gptel-runner-gptel-install-abort-advice))

(unless gptel-runner-default-driver
  (setq gptel-runner-default-driver (gptel-runner-gptel-driver-create)))

(provide 'gptel-runner-gptel)
;;; gptel-runner-gptel.el ends here
