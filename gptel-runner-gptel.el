;;; gptel-runner-gptel.el --- gptel compatibility driver -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.6"))

;;; Commentary:

;; The only compatibility boundary allowed to mention private gptel symbols.
;; Loading this file does not require gptel; compatibility is checked before a
;; live dispatch so the rest of gptel-runner remains usable with fake drivers.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'text-property-search)
(require 'gptel-runner-core)
(require 'gptel-runner-discussion)

(declare-function gptel "gptel")
(declare-function org-in-block-p "org")
(declare-function gptel-prompt-prefix-string "gptel-request")
(declare-function gptel--save-state "gptel")
(declare-function gptel--restore-state "gptel")
(declare-function gptel--handle-post-insert "gptel")
(autoload 'gptel-runner-node-buffer "gptel-runner-store")
(autoload 'gptel-runner-call-chat-buffer "gptel-runner-store")
(autoload 'gptel-runner-store-track-chat "gptel-runner-store")
(autoload 'gptel-runner-store-track-child "gptel-runner-store")
(declare-function gptel-runner-store--child-owner "gptel-runner-store")
(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-make-fsm "gptel-request")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-fsm-state "gptel-request")
(declare-function gptel-fsm-handlers "gptel-request")
(declare-function gptel--fsm-live-p "gptel")
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
(declare-function gptel-tool-category "gptel-request")
(declare-function gptel--make-tool "gptel-request")
(declare-function gptel--parse-directive "gptel-request")
(declare-function gptel--accept-tool-calls "gptel")
(declare-function gptel--cleanup-tool-call-overlay "gptel")
(declare-function gptel-runner--complete-restored-call "gptel-runner-flow")
(declare-function gptel-runner--resume-discussions "gptel-runner-flow")
(declare-function gptel-agent-session--track "gptel-agent-session")

(defvar gptel-request--transitions)
(defvar gptel-request--handlers)
(defvar gptel--request-alist)
(defvar gptel-send--transitions)
(defvar gptel-send--handlers)
(defvar gptel-confirm-tool-calls)
(defvar gptel-default-mode)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-system-prompt)
(defvar gptel--schema)
(defvar gptel-agent-child-buffer-functions)
(defvar gptel-agent-session-managed-externally)
;; gptel ignores unknown preset keys.  Retain an explicitly selected policy
;; even in a plain runner process so it cannot dispatch without enforcement.
(defvar gptel-agent-tool-policy nil)
;; Set before gptel enables its major and minor modes, including when
;; gptel-agent is loaded later during an existing workflow.
(put 'gptel-agent-session-managed-externally 'permanent-local t)
(defvar gptel--fsm-last)
(defvar gptel-response-separator)
(defvar-local gptel-runner-gptel--submitted-end nil
  "End of the last workflow prompt submitted in this node chat.")
(defvar-local gptel-runner-gptel--rendered-end nil
  "End of rendered request output, including text without a gptel property.")
(defvar-local gptel-runner-gptel--manual-requests nil
  "Ordinary gptel sends still using this node chat.")
;; These belong to the conversation and its in-flight requests.  Preserve the
;; marker objects shared with the renderer, including their insertion behavior.
(dolist (symbol '(gptel-runner-gptel--submitted-end
                  gptel-runner-gptel--rendered-end
                  gptel-runner-gptel--manual-requests))
  (put symbol 'permanent-local t))
(defvar gptel-prompt-transform-functions)

;; Optional integration uses only public gptel-agent APIs.  Agent lifecycle
;; objects and callbacks stay out of scheduler records and persisted jobs.
(declare-function gptel-agent-supervise "gptel-agent-tools")
(declare-function gptel-agent-supervision-allow-outside-reads "gptel-agent-tools" (scope fsm))
(declare-function gptel-agent-supervision-stop "gptel-agent-tools")
(declare-function gptel-agent-supervision-retry "gptel-agent-tools")
(declare-function gptel-agent-supervision-request-live-p "gptel-agent-tools")
(declare-function gptel-agent-supervision-question-live-p "gptel-agent-tools" (scope batch))
(declare-function gptel-agent-supervision-answer-questions "gptel-agent-tools" (scope batch answers))
(declare-function gptel-agent-allow-outside-access "gptel-agent-access")
(declare-function gptel-agent-revoke-outside-access "gptel-agent-access")
(declare-function gptel-agent-policy-request-trust-tool "gptel-agent-policy-request" (fsm tool))
(declare-function gptel-agent-policy-buffer-ceiling "gptel-agent-policy" (&optional buffer))
(declare-function gptel-agent-policy-inherit "gptel-agent-policy" (ceiling))
(declare-function gptel-agent-policy-request-ceiling "gptel-agent-policy-request" (fsm))

(cl-defstruct (gptel-runner-gptel-driver
               (:constructor gptel-runner-gptel-driver-create))
  "Driver backed by `gptel-request'.")

(defvar gptel-runner-gptel--cancelling nil)

(defvar gptel-runner-gptel--approval-owner nil
  "Call, FSM and optional supervisor for the approval currently being drawn.")

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
  "Return CALL's private pending interactions, including legacy single entries."
  (let ((data (gptel-runner-call-driver-data call)))
    (or (plist-get data :pending-interactions)
        (when-let* ((pending (plist-get data :pending-interaction)))
          (list pending)))))

(defun gptel-runner-gptel--set-interactions (call pending)
  "Store PENDING interactions for CALL, keeping the single-entry view compatible."
  (setf (gptel-runner-call-driver-data call)
        (plist-put (plist-put (gptel-runner-call-driver-data call)
                              :pending-interactions pending)
                   :pending-interaction (car pending))))

(defun gptel-runner-gptel--interaction-overlay (pending)
  "Return the live native preview that still belongs to PENDING."
  (when-let* ((overlay (plist-get pending :overlay))
              ((overlay-buffer overlay))
              ((equal (cdr (overlay-get overlay 'gptel-runner-interaction))
                      (plist-get (plist-get pending :description) :id))))
    overlay))

(defun gptel-runner-gptel--dismiss-preview (pending)
  "Remove PENDING's native prompt and release its tool-specific previews."
  (when-let* ((overlay (gptel-runner-gptel--interaction-overlay pending)))
    (with-current-buffer (overlay-buffer overlay)
      (save-restriction
        (widen)
        (if (fboundp 'gptel--cleanup-tool-call-overlay)
            (gptel--cleanup-tool-call-overlay overlay)
          ;; Older gptel releases expose preview teardown on the overlay.
          (dolist (teardown (overlay-get overlay 'previews))
            (when (car teardown) (apply teardown)))
          (let ((inhibit-read-only t))
            (dolist (prompt (overlay-get overlay 'prompt))
              (when (overlay-buffer prompt)
                (delete-region (overlay-start prompt) (overlay-end prompt)))))
          (delete-overlay overlay))))))

(defun gptel-runner-gptel--interaction-live-p (call pending)
  "Whether PENDING still belongs to a live request in CALL's generation."
  (let ((description (plist-get pending :description))
        (fsm (plist-get pending :fsm)))
    (and (not (gptel-runner--call-terminal-p call))
         (= (gptel-runner-call-generation call)
            (plist-get description :generation))
         (buffer-live-p (or (plist-get pending :buffer)
                            (gptel-runner-call-buffer call)))
         (or (null (plist-get pending :overlay))
             (gptel-runner-gptel--interaction-overlay pending))
         (if-let* ((batch (plist-get pending :question-batch)))
             ;; An implicit wait at turn completion can also hold a batch.
             (gptel-agent-supervision-question-live-p
              (plist-get pending :agent-supervision) batch)
           (and (or (null (plist-get pending :agent-supervision))
                    (gptel-agent-supervision-request-live-p
                     (plist-get pending :agent-supervision) fsm))
                (or (null fsm)
                    (and (eq (gptel-fsm-state fsm) 'TOOL)
                         (eq (plist-get pending :info) (gptel-fsm-info fsm)))))))))

(defun gptel-runner-gptel-pending-interactions (call)
  "Return data-only descriptions of the currently actionable requests on CALL."
  (cl-loop for entry in (gptel-runner-gptel--interactions call)
           when (gptel-runner-gptel--interaction-live-p call entry)
           collect (copy-tree (plist-get entry :description) t)))

(defun gptel-runner-gptel--live-interaction (call id)
  "Find the pending interaction with ID on CALL."
  (cl-find-if (lambda (pending)
                (and (equal id (plist-get (plist-get pending :description) :id))
                     (gptel-runner-gptel--interaction-live-p call pending)))
              (gptel-runner-gptel--interactions call)))

(defun gptel-runner-gptel-interaction-pending-p (call id)
  "Whether interaction ID is still pending on CALL."
  (and (gptel-runner-gptel--live-interaction call id) t))

(defun gptel-runner-gptel-check-interaction (call id kind &optional answers)
  "Check that ID on CALL is live and accepts KIND and optional ANSWERS.
Return the private pending entry.  For a question batch, ANSWERS must
be nil for cancellation or a list with one nonblank string per question."
  (let* ((pending (gptel-runner-gptel--live-interaction call id))
         (description (plist-get pending :description)))
    (unless pending (user-error "Interaction %s is no longer pending" id))
    (unless (eq kind (or (plist-get description :kind) 'tool-approval))
      (user-error "Interaction %s does not accept this action" id))
    (when (and answers (eq kind 'question)
               (not (and (proper-list-p answers)
                         (= (length answers) (length (plist-get description :questions)))
                         (cl-every (lambda (s) (and (stringp s) (not (string-blank-p s))))
                                   answers))))
      (user-error "Supply one nonblank answer per question"))
    pending))

(defun gptel-runner-gptel--sync-interactions (call)
  "Discard stale interactions and retain CALL's waiting state for live ones."
  (let* ((previous (gptel-runner-gptel--interactions call))
         (pending
          (cl-loop for entry in previous
                   if (gptel-runner-gptel--interaction-live-p call entry) collect entry
                   else do (gptel-runner-gptel--dismiss-preview entry))))
    (gptel-runner-gptel--set-interactions call pending)
    (cond
     ((and pending
           (not (eq (gptel-runner-call-state call) 'waiting-confirmation)))
      (gptel-runner-gptel--observe call 'waiting-confirmation))
     ((and previous (null pending)
           (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (gptel-runner-gptel--observe
       call 'waiting-tool (gptel-runner-call-tool-names call))))))

(defun gptel-runner-gptel--publish-interaction (call fsm scope description private)
  "Publish DESCRIPTION for CALL and retain PRIVATE handles from FSM and SCOPE."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (buffer (if info (plist-get info :buffer) (gptel-runner-call-buffer call)))
         (interaction (append (list :id (gptel-runner--id "interaction")
                                    :call-id (gptel-runner-call-id call)
                                    :generation (gptel-runner-call-generation call)
                                    :source-buffer (buffer-name buffer))
                              description)))
    (when-let* ((overlay (plist-get private :overlay)))
      (overlay-put overlay 'gptel-runner-interaction
                   (cons call (plist-get interaction :id))))
    (gptel-runner-gptel--set-interactions
     call (append (gptel-runner-gptel--interactions call)
                  (list (append (list :description interaction :fsm fsm
                                      :agent-supervision scope :info info :buffer buffer)
                                private))))
    (gptel-runner-gptel--sync-interactions call)
    (run-hook-with-args 'gptel-runner-interaction-hook call
                        (copy-tree interaction t))
    interaction))

(defun gptel-runner-gptel--pending-interaction (call tools &optional fsm scope overlay)
  "Publish CALL's TOOLS from FSM with optional supervisor SCOPE and OVERLAY."
  (gptel-runner-gptel--publish-interaction
   call fsm scope
   (list :kind 'tool-approval :tools (gptel-runner-gptel--tool-data tools)
         :outside-access
         (and (fboundp 'gptel-agent-allow-outside-access)
              fsm
              (cl-some
               (lambda (item)
                 (and (plist-get item :agent-access-pending)
                      (not (plist-get item :result))
                      (cl-some
                       (lambda (tool)
                         (and (equal (gptel-tool-name (car tool)) (plist-get item :name))
                              (eq (cadr tool) (plist-get item :args))))
                       tools)))
               (plist-get (gptel-fsm-info fsm) :tool-use))
              t))
   (list :tools tools :overlay overlay)))

(defun gptel-runner-gptel-answer-questions (call id answers)
  "Return ANSWERS to the question batch with ID on CALL, or cancel it with nil."
  (let ((pending (gptel-runner-gptel-check-interaction call id 'question answers)))
    ;; The callback can immediately start another batch or finish this call.
    (gptel-runner-gptel--set-interactions
     call (remq pending (gptel-runner-gptel--interactions call)))
    (gptel-runner--emit (gptel-runner-call-run call) 'interaction-answered
                        (gptel-runner-call-node call) call
                        (list :id id :decision (if answers 'answer 'cancel-questions)))
    (gptel-runner-gptel--observe call 'waiting-tool '("AskUserQuestion"))
    (unwind-protect
        (gptel-agent-supervision-answer-questions
         (plist-get pending :agent-supervision) (plist-get pending :question-batch) answers)
      (gptel-runner-gptel--sync-interactions call))))

(defun gptel-runner-gptel--display-tool-calls (original tools info &rest args)
  "Register the native approval drawn by ORIGINAL for TOOLS, INFO and ARGS."
  (let* ((call (plist-get info :context))
         (gptel-runner-gptel--approval-owner
          (and (gptel-runner-call-p call)
               (list call (gptel-runner-call-fsm call) nil)))
         ;; Supervision supplies the owner of a descendant during display.
         (prompt (apply original tools info args)))
    (when (and gptel-runner-gptel--approval-owner (overlayp prompt)
               (overlay-buffer prompt))
      (pcase-let ((`(,owner ,fsm ,scope) gptel-runner-gptel--approval-owner))
        (when-let* ((fsm)
                    ((eq (gptel-fsm-info fsm) info))
                    (overlay
                     (with-current-buffer (overlay-buffer prompt)
                       ;; A tool-specific preview can have no inline text.
                       (save-restriction
                         (widen)
                         (cl-find-if (lambda (ov) (memq prompt (overlay-get ov 'prompt)))
                                     (overlays-in (max (point-min) (1- (overlay-start prompt)))
                                                  (min (point-max) (1+ (overlay-end prompt)))))))))
          ;; The overlay contains the guarded tool functions and editable
          ;; callbacks that gptel will use when accepted in the conversation.
          (gptel-runner-gptel--pending-interaction
           owner (overlay-get overlay 'gptel-tool) fsm scope overlay))))
    prompt))

(defun gptel-runner-gptel--answer-interaction (call interaction-id decision &optional allow-outside)
  "Apply DECISION to CALL's live INTERACTION-ID once.
DECISION is `approve', `deny', or `cancel'.  ALLOW-OUTSIDE is an explicit
outside-file grant for the conversation owning an approved request."
  (unless (gptel-runner-call-p call)
    (user-error "Unknown runner call"))
  (let* ((pending (gptel-runner-gptel-check-interaction call interaction-id 'tool-approval))
         (description (plist-get pending :description)))
    (when (and allow-outside
               (not (and (eq decision 'approve) (plist-get description :outside-access)
                         (fboundp 'gptel-agent-allow-outside-access))))
      (user-error "This request cannot grant outside file access"))
    ;; Consume before executing: synchronous tools may create a new gate.
    (gptel-runner-gptel--set-interactions
     call (remq pending (gptel-runner-gptel--interactions call)))
    (gptel-runner-gptel--dismiss-preview pending)
    (gptel-runner--emit
     (gptel-runner-call-run call) 'interaction-answered
     (gptel-runner-call-node call) call
     (list :id interaction-id :approved (eq decision 'approve) :decision decision
           :allow-outside (and allow-outside t)))
    (if (eq decision 'cancel)
        (gptel-runner-abort-call call 'tool-rejected)
      (with-current-buffer (or (plist-get pending :buffer)
                               (gptel-runner-call-buffer call))
        (gptel-runner-gptel--observe
         call 'waiting-tool
         (mapcar (lambda (tool) (plist-get tool :name))
                 (plist-get description :tools)))
        (unwind-protect
            (cond
             ((eq decision 'deny)
              ;; Give each tool its result in the requesting conversation.
              (dolist (tool (plist-get pending :tools))
                (funcall (nth 2 tool) "Tool call rejected by user.")))
             (allow-outside
              ;; Retain owners for revocation, never for persistence or
              ;; inheritance by another call or child conversation.
              (setf (gptel-runner-call-driver-data call)
                    (plist-put
                     (gptel-runner-call-driver-data call) :outside-access-buffers
                     (cl-adjoin (current-buffer)
                               (plist-get (gptel-runner-call-driver-data call)
                                          :outside-access-buffers))))
              (gptel-agent-allow-outside-access (plist-get pending :tools)))
             (t (gptel--accept-tool-calls (plist-get pending :tools))))
          (gptel-runner-gptel--sync-interactions call))))
    call))

(defun gptel-runner-gptel-answer-interaction (call interaction-id approve
                                                &optional allow-outside)
  "Answer CALL's pending INTERACTION-ID, running tools when APPROVE is non-nil.
Rejecting cancels the call.  Replies are accepted once and only while the
interaction's generation is current.  ALLOW-OUTSIDE uses gptel-agent's public
approval action to remember outside file access in this request's conversation.
Ordinary confirmations and file reviews remain enabled."
  (gptel-runner-gptel--answer-interaction
   call interaction-id (if approve 'approve 'cancel) allow-outside))

(defun gptel-runner-gptel--check-interaction-ids (call ids)
  "Check that every displayed interaction in IDS is distinct and live on CALL."
  (unless (and ids (equal ids (delete-dups (copy-sequence ids))))
    (user-error "Supply a nonempty set of distinct interaction IDs"))
  (dolist (id ids)
    (gptel-runner-gptel-check-interaction call id 'tool-approval)))

(defun gptel-runner-gptel-deny-interactions (call ids)
  "Decline exactly the pending request IDS on CALL and let their agents continue."
  (gptel-runner-gptel--check-interaction-ids call ids)
  (dolist (id ids)
    (gptel-runner-gptel--answer-interaction call id 'deny)))

(defun gptel-runner-gptel-approve-interactions (call ids &optional allow-outside)
  "Approve exactly IDS on CALL, optionally remembering ALLOW-OUTSIDE access.
Validate the complete displayed set before executing any tools.  New requests
created by these tools are never included.  Every execution rechecks liveness."
  (gptel-runner-gptel--check-interaction-ids call ids)
  (let ((outside-ids
         (cl-loop for pending in (gptel-runner-gptel--interactions call)
                  for description = (plist-get pending :description)
                  when (and (member (plist-get description :id) ids)
                            (plist-get description :outside-access))
                  collect (plist-get description :id))))
    (when (and allow-outside
               (not (and outside-ids (fboundp 'gptel-agent-allow-outside-access))))
      (user-error "No displayed request can grant outside file access"))
    (dolist (id ids)
      (gptel-runner-gptel-answer-interaction
       call id t (and allow-outside (member id outside-ids))))))

(defun gptel-runner-gptel-revoke-outside-access (call)
  "Revoke outside file access granted through CALL's runner controls."
  (dolist (buffer (plist-get (gptel-runner-call-driver-data call)
                             :outside-access-buffers))
    (when (and (buffer-live-p buffer)
               (fboundp 'gptel-agent-revoke-outside-access))
      (with-current-buffer buffer (gptel-agent-revoke-outside-access))))
  (setf (gptel-runner-call-driver-data call)
        (plist-put (gptel-runner-call-driver-data call) :outside-access-buffers nil)))

(defun gptel-runner-gptel--compatibility-error (&rest missing)
  "Signal an actionable compatibility error listing MISSING API pieces."
  (error (concat "gptel-runner compatibility failure: missing or unsupported "
                 "%S.  Install gptel >= 0.9.9.6 or update "
                 "gptel-runner-gptel.el for this gptel version")
         missing))

(defun gptel-runner-gptel--start-supervision (call)
  "Return the gptel-agent supervisor for CALL, attaching one if available."
  (when (featurep 'gptel-agent-tools)
    (dolist (function '(gptel-agent-supervise gptel-agent-supervision-stop
                        gptel-agent-supervision-retry
                        gptel-agent-supervision-request-live-p))
      (unless (fboundp function)
        (error "Update gptel-agent: runner integration requires %s" function)))
    (unless (plist-get (gptel-runner-call-driver-data call) :agent-supervision)
      (let* ((generation (gptel-runner-call-generation call))
             (questions
              (when (and gptel-runner-headless
                         (fboundp 'gptel-agent-supervision-answer-questions))
                (lambda (scope fsm items batch)
                  (when (and (= generation (gptel-runner-call-generation call))
                             (not (gptel-runner--call-terminal-p call)))
                    (gptel-runner-gptel--publish-interaction
                     call fsm scope (list :kind 'question :questions items)
                     (list :question-batch batch)))))))
        (setf (gptel-runner-call-driver-data call)
              (plist-put
               (gptel-runner-call-driver-data call) :agent-supervision
               (apply #'gptel-agent-supervise
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
                  (cond
                   ((or (/= generation (gptel-runner-call-generation call))
                        (gptel-runner--call-terminal-p call)) t)
                   (gptel-runner-headless
                    (gptel-runner-gptel--pending-interaction call calls fsm scope)
                    t)
                   (t
                    (setq gptel-runner-gptel--approval-owner (list call fsm scope))
                    nil)))
                (when questions (list :on-question questions)))))))
    (plist-get (gptel-runner-call-driver-data call) :agent-supervision)))

(defun gptel-runner-gptel--stop-supervision (call)
  "Release CALL's optional agent supervisor and pending approvals."
  (dolist (entry (gptel-runner-gptel--interactions call))
    (gptel-runner-gptel--dismiss-preview entry))
  (gptel-runner-gptel-revoke-outside-access call)
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
  (dolist (function '(gptel-request gptel-abort gptel-make-fsm gptel--realize-query
                      gptel--fsm-transition gptel--handle-post
                      gptel--apply-preset gptel--insert-response
                      gptel-curl--stream-insert-response
                      gptel--update-tool-ask gptel--update-tool-call
                      gptel--update-status gptel--update-wait
                      gptel--accept-tool-calls gptel--display-tool-calls
                      gptel--handle-pre-tool
                      gptel--handle-post-tool gptel--handle-tool-steer
                      gptel--tool-steer-p))
    (unless (fboundp function)
      (gptel-runner-gptel--compatibility-error function)))
  t)

(defun gptel-runner-gptel-submitted-end (buffer)
  "Return the position after BUFFER's latest submitted workflow prompt."
  (when-let* ((marker (buffer-local-value 'gptel-runner-gptel--submitted-end buffer)))
    (marker-position marker)))

(defun gptel-runner-gptel-rendered-end (buffer)
  "Return the position after the latest rendered output in BUFFER."
  (when-let* ((marker (buffer-local-value 'gptel-runner-gptel--rendered-end buffer)))
    (marker-position marker)))

(defun gptel-runner-gptel-restore-submitted-end (buffer position &optional rendered)
  "Restore BUFFER's submitted prompt POSITION and optional RENDERED output end."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (when (and (integerp position) (<= (point-min) position (point-max)))
        (setq gptel-runner-gptel--submitted-end (copy-marker position)))
      (when (and (integerp rendered) (<= (point-min) rendered (point-max)))
        (setq gptel-runner-gptel--rendered-end (copy-marker rendered))))))

(defun gptel-runner-gptel--history-end ()
  "Return the end of the submitted conversation in this widened node chat."
  (save-excursion
    (goto-char (point-max))
    (let ((role (text-property-search-backward 'gptel)))
      (when role
        (goto-char (prop-match-end role))
        ;; Old Org snapshots have no output marker.  gptel marks a block's
        ;; payload but leaves its closing delimiter outside that region.
        (when (and (derived-mode-p 'org-mode)
                   (>= (point) (or gptel-runner-gptel--rendered-end (point-min)))
                   (looking-at "[ \t\n]*#\\+end_\\(tool\\|reasoning\\)[ \t]*\\(?:\n\\|\\'\\)"))
          (let ((end (match-end 0)))
            (when (org-in-block-p (list (match-string 1)))
              (goto-char end)))))
      (max (or gptel-runner-gptel--submitted-end (point-min))
           (or gptel-runner-gptel--rendered-end (point-min))
           (if role (point) (point-min))))))

(defun gptel-runner-gptel--mark-submitted ()
  "Remember point as the end of this turn, separating a trailing draft."
  (setq gptel-runner-gptel--submitted-end (point-marker))
  ;; The request parser rounds point to a word end.  Separate a pending
  ;; draft so even a draft starting with a word never enters this request.
  (unless (eobp) (save-excursion (insert "\n"))))

(defun gptel-runner-gptel--insert-turn (text)
  "Insert workflow prompt TEXT before any unsent draft and mark its end."
  (save-restriction
    (widen)
    (goto-char (gptel-runner-gptel--history-end))
    (let ((prefix (gptel-prompt-prefix-string)))
      (unless (bobp)
        (if (looking-at (regexp-quote gptel-response-separator))
            (goto-char (match-end 0))
          (insert gptel-response-separator)))
      (if (looking-at (regexp-quote prefix))
          (goto-char (match-end 0))
        (insert prefix))
      (insert (substring-no-properties (or text "")))
      (gptel-runner-gptel--mark-submitted))))

(defun gptel-runner-gptel--node-chat-name (call)
  "Return a human-readable buffer name for CALL's node conversation."
  (let* ((workflow (gptel-runner-run-workflow (gptel-runner-call-run call)))
         (name (and workflow (gptel-runner-workflow-name workflow)))
         (id (gptel-runner-node-id (gptel-runner-call-node call))))
    (format "*gptel:%s%s*" (if name (format "%s/" name) "") id)))

(defun gptel-runner-gptel--new-chat (name &optional text call)
  "Create a workflow-owned gptel chat with base NAME and initial TEXT.
When CALL is given, apply its agent preset in its workspace before returning."
  (when (and (featurep 'gptel-agent-session)
             (not (default-boundp 'gptel-agent-session-managed-externally)))
    (user-error "Update gptel-agent to keep runner chats in their pipeline session"))
  (let ((buffer (generate-new-buffer name)) initialized)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local gptel-agent-session-managed-externally t)
          (gptel (buffer-name buffer) nil text)
          (when call
            (setq-local default-directory (gptel-runner-call-workspace call))
            (gptel-runner-gptel--apply-preset-locally
             (gptel-runner-agent-preset (gptel-runner-call-agent call))))
          (setq initialized buffer))
      (when (and (not initialized) (buffer-live-p buffer))
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))))))

(defun gptel-runner-gptel--worker-buffer (call)
  "Prepare CALL's turn in its node's ordinary gptel chat."
  (or (and (plist-get (gptel-runner-call-driver-data call) :turn-started)
           (buffer-live-p (gptel-runner-call-buffer call))
           (gptel-runner-call-buffer call))
      (let* ((run (gptel-runner-call-run call))
             (node (gptel-runner-call-node call))
             (existing (and (gptel-runner-run-workflow run)
                            (gptel-runner-node-buffer run node)))
             (buffer (or existing
                         (gptel-runner-gptel--new-chat
                          (gptel-runner-gptel--node-chat-name call) nil call))))
        (with-current-buffer buffer
          (when existing
            (setq-local default-directory (gptel-runner-call-workspace call)))
          (gptel-runner-store-track-chat call buffer)
          (gptel-runner-gptel--insert-turn (gptel-runner-call-prompt call))
          (setf (gptel-runner-call-driver-data call)
                (plist-put (gptel-runner-call-driver-data call) :turn-started t)))
        buffer)))

(defun gptel-runner-gptel-restore-worker-buffer (call transcript)
  "Open legacy plain-text TRANSCRIPT as CALL's gptel chat."
  (gptel-runner-gptel--check-api)
  (gptel-runner-store-track-chat
   call (gptel-runner-gptel--new-chat
         (gptel-runner-gptel--node-chat-name call) transcript call)))

(defun gptel-runner-gptel-protected-request (buffer)
  "Return the protected workflow request owned by BUFFER, if any."
  (when-let* (((fboundp 'gptel-agent-policy-request-ceiling))
              (call (gptel-runner--buffer-call buffer))
              (fsm (gptel-runner-call-fsm call))
              ((gptel-agent-policy-request-ceiling fsm)))
    fsm))

(defun gptel-runner-gptel-chat-text (buffer)
  "Return BUFFER as a native gptel chat file, leaving its live text untouched."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((mode (cond ((derived-mode-p 'org-mode) 'org)
                        ((derived-mode-p 'markdown-mode) 'markdown)
                        (t 'text)))
            (submitted (gptel-runner-gptel-submitted-end buffer))
            (rendered (gptel-runner-gptel-rendered-end buffer))
            (workspace default-directory)
            (confirmation gptel-confirm-tool-calls)
            (ceiling (and (fboundp 'gptel-agent-policy-buffer-ceiling)
                          (cl-union (gptel-agent-policy-buffer-ceiling buffer)
                                    (when-let* ((fsm (gptel-runner-gptel-protected-request buffer)))
                                      (gptel-agent-policy-request-ceiling fsm))
                                    :test #'equal))))
        (with-temp-buffer
          (if (eq mode 'org) (org-mode) (text-mode))
          (dolist (setting '(gptel--preset gptel-backend gptel-model
                             gptel-system-prompt gptel-tools gptel-max-tokens
                             gptel-temperature gptel--num-messages-to-send))
            (set (make-local-variable setting) (buffer-local-value setting buffer)))
          (insert-buffer-substring buffer)
          (when submitted (setq submitted (copy-marker submitted)))
          (when rendered (setq rendered (copy-marker rendered)))
          ;; Start a separate locals block even when the user's draft itself
          ;; contains an example of Emacs file-local variables.
          (when (eq mode 'org) (gptel--save-state))
          (goto-char (point-max))
          (let ((footer (point)) (print-escape-newlines t))
            (insert "\n;; Local Variables:\n;; mode: " (symbol-name mode) "\n;; End:\n")
            (add-file-local-variable 'gptel-runner-directory workspace)
            (add-file-local-variable 'gptel-confirm-tool-calls confirmation)
            (when ceiling
              (add-file-local-variable 'gptel-runner-tool-policy-ceiling ceiling))
            (when submitted
              (add-file-local-variable 'gptel-runner-submitted-end
                                       (marker-position submitted)))
            (when rendered
              (add-file-local-variable 'gptel-runner-rendered-end
                                       (marker-position rendered)))
            (unless (eq mode 'org)
              (save-restriction
                (narrow-to-region footer (point-max))
                (gptel--save-state))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun gptel-runner-gptel-chat-from-text (text &optional external)
  "Open native chat TEXT in an independent, editable gptel buffer.
When EXTERNAL is non-nil, the caller owns the restored chat's persistence."
  (gptel-runner-gptel--check-api)
  (let (settings body)
    (with-temp-buffer
      (insert (substring-no-properties text))
      (goto-char (point-max))
      (unless (search-backward "\n;; Local Variables:\n" nil t)
        (user-error "Missing gptel chat settings"))
      (setq body (buffer-substring-no-properties (point-min) (point)))
      (narrow-to-region (point) (point-max))
      ;; Read the generated footer as data.  Never activate file/dir locals,
      ;; modes, or eval forms supplied by the transcript.
      ;; Emacs' file-locals scanner limits its search near the end of the
      ;; file.  A long tool conversation can have many kilobytes of bounds.
      (let ((read-circle nil))
        (while (re-search-forward "^;; \\([^ :\n]+\\):[ \t]*" nil t)
          (let ((key (intern-soft (match-string 1))))
            (when (memq key '(mode gptel-runner-submitted-end gptel-runner-rendered-end
                              gptel-runner-directory gptel-runner-tool-policy-ceiling
                              gptel-confirm-tool-calls gptel-model
                              gptel--preset gptel--backend-name gptel-system-prompt
                              gptel--tool-names gptel--bounds gptel-max-tokens
                              gptel--num-messages-to-send gptel-temperature))
              (push (cons key (read (current-buffer))) settings))))))
    (let* ((gptel-default-mode
            (pcase (alist-get 'mode settings)
              ('org 'org-mode)
              ('markdown (if (fboundp 'markdown-mode) 'markdown-mode 'text-mode))
              (_ 'text-mode)))
           ;; Keep even independent imports externally owned while restoring:
           ;; mode hooks and failed-import cleanup must not autosave partial
           ;; settings or a transcript whose tool policy was rejected.
           (buffer (gptel-runner-gptel--new-chat "*gptel-runner chat*" body)))
      (condition-case err
          (with-current-buffer buffer
            (dolist (entry settings)
              (when (pcase (car entry)
                      ((or 'gptel-model 'gptel--preset) (symbolp (cdr entry)))
                      ((or 'gptel--backend-name 'gptel-system-prompt) (stringp (cdr entry)))
                      ('gptel--tool-names (and (proper-list-p (cdr entry))
                                               (cl-every #'stringp (cdr entry))))
                      ('gptel--bounds (proper-list-p (cdr entry)))
                      ((or 'gptel-max-tokens 'gptel--num-messages-to-send) (natnump (cdr entry)))
                      ('gptel-temperature (numberp (cdr entry))))
                (set (make-local-variable (car entry)) (cdr entry))))
            ;; The native restorer checks for a visited file.  This chat
            ;; stays independent of the snapshot after restoration.
            (let ((buffer-file-name "gptel chat")) (gptel--restore-state))
            (when-let* ((workspace (alist-get 'gptel-runner-directory settings))
                        ((stringp workspace)) ((file-name-absolute-p workspace)))
              (setq-local default-directory (file-name-as-directory workspace)))
            (when-let* ((ceiling (alist-get 'gptel-runner-tool-policy-ceiling settings)))
              (unless (require 'gptel-agent-policy-request nil t)
                (user-error "Install gptel-agent with tool policy support to restore this chat"))
              (gptel-agent-policy-inherit ceiling))
            (when-let* ((entry (assq 'gptel-confirm-tool-calls settings))
                        ((memq (cdr entry) '(auto t nil))))
              (setq-local gptel-confirm-tool-calls (cdr entry)))
            (gptel-runner-gptel-restore-submitted-end
             buffer (alist-get 'gptel-runner-submitted-end settings)
             (alist-get 'gptel-runner-rendered-end settings))
            (goto-char (point-max))
            (unless external
              (setq-local gptel-agent-session-managed-externally nil)
              ;; Match the agent's standalone importer without rerunning
              ;; user mode hooks.  Late-loaded session mode discovers it too.
              (when (fboundp 'gptel-agent-session--track)
                (gptel-agent-session--track buffer)))
            buffer)
        ((error quit)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq-local gptel-agent-session-managed-externally t)
             (kill-buffer buffer)))
         (signal (car err) (cdr err)))))))

(defun gptel-runner-gptel-save-chat (source file)
  "Atomically save a native gptel chat from SOURCE to FILE.
SOURCE is a live buffer or text returned by `gptel-runner-gptel-chat-text'."
  (let ((temporary (make-temp-file
                    (expand-file-name ".chat-" (file-name-directory
                                                (expand-file-name file)))))
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (write-region (if (stringp source) source (gptel-runner-gptel-chat-text source))
                        nil temporary nil 'silent)
          (rename-file temporary file t)
          file)
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun gptel-runner-gptel-open-chat (file)
  "Open native gptel chat FILE in an independent editable buffer."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (gptel-runner-gptel-chat-from-text (buffer-string))))

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
              (when (and (eq symbol 'gptel-agent-tool-policy) value
                         (not (require 'gptel-agent-policy-request nil t)))
                (user-error "This preset requires gptel-agent tool policy support"))
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
    ;; Supervised discussions can wake a provisional DONE for synthesis.
    ;; Only the final, settled turn may transfer control to the workflow.
    (plist-put info :runner-transfer nil)
    (plist-put info :stop-reason nil)
    (plist-put info :runner-incomplete-tool-call nil)))

(defun gptel-runner-gptel--communication-tool (fsm name argument description status)
  "Make FSM's private tool NAME with ARGUMENT, DESCRIPTION and result STATUS."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (generation (gptel-runner-call-generation call)))
    ;; These closures must never enter gptel's global tool registry.
    (gptel--make-tool
     :name name :category "gptel-runner" :description description :include t
     :args (list (list :name argument :type 'string :description description))
     :function
     (lambda (text)
       (cond
        ((not (and (= generation (gptel-runner-call-generation call))
                   (eq fsm (gptel-runner-call-fsm call))
                   (eq (gptel-fsm-state fsm) 'TOOL)
                   (eq (current-buffer) (gptel-runner-call-buffer call))
                   (eq gptel-runner--call call)
                   (memq (gptel-runner-call-state call)
                         '(running waiting-tool waiting-confirmation))))
         "This tool belongs to a different workflow turn.")
        ((not (and (length= (plist-get info :tool-use) 1)
                   (equal name (plist-get (car (plist-get info :tool-use)) :name))))
         "Send this communication as the only tool call in a response.")
        ((not (and (stringp text) (not (string-blank-p text))))
         "Supply a nonempty message.")
        (t
         (plist-put info :runner-transfer (cons status (substring-no-properties text)))
         "Accepted by the workflow."))))))

(defun gptel-runner-gptel--routing-tool-p (tool)
  "Return non-nil when TOOL has a reserved workflow communication name."
  (member (gptel-tool-name tool) '("RunnerHandoff" "RunnerAskPrevious")))

(defun gptel-runner-gptel--install-communication (fsm)
  "Add private pipeline tools and their instructions to FSM's prompt buffer."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (handoff (gptel-runner-gptel--communication-tool
                   fsm "RunnerHandoff" "message"
                   "Deliver your complete final answer to the workflow." 'success))
         (previous (gptel-runner-previous-agent (gptel-runner-call-run call)
                                                (gptel-runner-call-node call)))
         (directive (gptel--parse-directive gptel-system-prompt 'raw))
         (schema (unless (gptel-runner-call-discussion-owner call)
                   (or (gptel-runner-agent-schema (gptel-runner-call-agent call))
                       gptel--schema)))
         (instructions
          (concat "You are a node in an Emacs workflow. Use your tools and the "
                  "conversation to finish your task. To deliver your final result, "
                  "call RunnerHandoff with the full answer in message. "
                  "Plain conversation leaves this node open for feedback. "
                  "Send each workflow communication as its own tool call."
                  (when previous
                    " To consult the previous agent, call RunnerAskPrevious with your question.")
                  (when schema
                    (concat " Encode the handoff message as JSON with this shape: "
                            (if (stringp schema) schema (json-encode schema)))))))
    ;; Only this turn's private completion closure can deliver its result.
    (when (fboundp 'gptel-agent-policy-request-trust-tool)
      (gptel-agent-policy-request-trust-tool fsm handoff))
    (setq-local gptel-tools
                (append
                 (list handoff)
                 (when previous
                   (list (gptel-runner-gptel--communication-tool
                          fsm "RunnerAskPrevious" "question"
                          "Ask the preceding agent for information needed to finish your task."
                          'follow-up)))
                 (when gptel-use-tools
                   (cl-remove-if #'gptel-runner-gptel--routing-tool-p gptel-tools))))
    (setq-local gptel-use-tools (or gptel-use-tools t)
                gptel--schema nil
                gptel-system-prompt
                (cons (string-join (delq nil (list (car directive) instructions)) "\n\n")
                      (cdr directive)))))

(defun gptel-runner-gptel--before-realize-query (fsm)
  "Apply the workflow's tool policy to FSM after all prompt transforms."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (source (plist-get info :buffer)))
    (when (and (gptel-runner-call-p call)
               (not (featurep 'gptel-agent-policy-request))
               (or (buffer-local-value 'gptel-agent-tool-policy source)
                   (buffer-local-value 'gptel-agent-tool-policy (plist-get info :data))))
      (user-error "Load gptel-agent tool policy support for this protected request"))
    ;; Presets can replace the transform that normally registers the request
    ;; with gptel-agent.  Attach the finalized root request in either case.
    (when-let* (((gptel-runner-call-p call))
                ((eq fsm (gptel-runner-call-fsm call)))
                ((eq (gptel-runner-call-state call) 'running))
                ((plist-get (gptel-runner-run-options (gptel-runner-call-run call))
                            :allow-unconfirmed-tools))
                ((fboundp 'gptel-agent-supervision-allow-outside-reads))
                (scope (gptel-runner-gptel--start-supervision call)))
      (gptel-agent-supervision-allow-outside-reads scope fsm))
    (cond
     ((and (gptel-runner-call-p call)
           (eq fsm (plist-get (gptel-runner-call-driver-data call) :discussion-fsm)))
      ;; An @preset may replace the transform hook.  Its async transforms
      ;; can also change tools; this is the final request construction point.
      (with-current-buffer (plist-get info :data)
        (gptel-runner-gptel--install-communication fsm)))
     ((and (buffer-live-p source)
           (with-current-buffer source (bound-and-true-p gptel-agent-session-managed-externally)))
      (with-current-buffer (plist-get info :data)
        ;; A descendant may copy the parent's final request configuration.
        (setq-local gptel-tools
                    (cl-remove-if (lambda (tool)
                                    (equal "gptel-runner" (gptel-tool-category tool)))
                                  gptel-tools)))))))

(defun gptel-runner-gptel--transferred-p (info)
  "Whether INFO contains an executed, accepted communication for this turn."
  (let ((tool (car (plist-get info :tool-use))))
    (and (plist-get info :runner-transfer)
         (not (plist-get info :error))
         (not (plist-get tool :error))
         (not (plist-get info :steering-message))
         (equal (plist-get tool :result) "Accepted by the workflow."))))

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

(defun gptel-runner-gptel--at-response-marker (info function &rest args)
  "Call FUNCTION with ARGS, making INFO's node chat marker accessible."
  (if-let* ((position (plist-get info :position))
            ((markerp position))
            (chat (marker-buffer position))
            ((with-current-buffer chat (buffer-narrowed-p)))
            ((gptel-runner--buffer-call chat)))
      (let ((caller (current-buffer)))
        (with-current-buffer chat
          (save-restriction
            (widen)
            (with-current-buffer caller (apply function args)))))
    (apply function args)))

(defun gptel-runner-gptel--render-at-marker (render response info &optional raw)
  "Call native RENDER with RESPONSE, INFO and RAW at the chat marker."
  (gptel-runner-gptel--at-response-marker info render response info raw))

(defun gptel-runner-gptel--handle-at-marker (handler fsm)
  "Run the native pre- or post-insert HANDLER at FSM's chat marker."
  (gptel-runner-gptel--at-response-marker (gptel-fsm-info fsm) handler fsm))

(defun gptel-runner-gptel--note-rendered-end (info)
  "Remember the native output marker from request INFO in its source chat."
  (when-let* ((marker (or (plist-get info :tracking-marker) (plist-get info :position)))
              ((markerp marker))
              (buffer (marker-buffer marker))
              ((eq buffer (plist-get info :buffer))))
    (with-current-buffer buffer
      (when (or (null gptel-runner-gptel--rendered-end)
                (>= marker gptel-runner-gptel--rendered-end))
        (setq gptel-runner-gptel--rendered-end marker)))))

(defun gptel-runner-gptel--insert-response (response info &optional raw)
  "Render RESPONSE using the transport described by INFO, honoring RAW."
  (prog1 (funcall (if (plist-get info :stream)
                      #'gptel-curl--stream-insert-response
                    #'gptel--insert-response)
                  response info raw)
    ;; The native renderer advances this marker through raw text and block
    ;; delimiters too.  Typing a draft at the marker leaves it in place.
    (when (and (eq (car-safe response) 'reasoning) (plist-get info :reasoning-marker))
      (plist-put info :runner-open-reasoning (stringp (cdr response))))
    (gptel-runner-gptel--note-rendered-end info)))

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
  "Run gptel post-processing for a failed or cancelled CALL and FSM once."
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
         (transfer (and (gptel-runner-gptel--transferred-p info)
                        (plist-get info :runner-transfer)))
         (value (or (cdr transfer)
                    (mapconcat #'identity
                               (reverse (gptel-runner-call-response-parts call)) ""))))
    (gptel-runner-gptel--update-status info " Ready" 'success)
    ;; A human may accept a proposed handoff after more manual chat.  Bound
    ;; the lookup to text written after the proposal, excluding its commentary.
    (when-let* ((transfer)
                (marker (or (plist-get info :tracking-marker) (plist-get info :position))))
      (gptel-runner-gptel-restore-submitted-end
       (plist-get info :buffer) (marker-position marker)))
    (funcall (gptel-runner-gptel--complete-function call)
             (or (car transfer)
                 (if (eq fsm (plist-get (gptel-runner-call-driver-data call) :discussion-fsm))
                     'discussion 'success))
             value nil)))

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

(defun gptel-runner-gptel--request-budget-exhausted (call fsm)
  "Stop CALL and FSM with a request budget failure before another send."
  (let* ((run (gptel-runner-call-run call))
         (failure (gptel-runner--budget-failure
                   run 'requests
                   (gptel-runner-budget-max-requests
                    (gptel-runner-run-budget run)))))
    ;; Feedback has its own terminal handler and may retain a completed
    ;; scheduler closure.  Fail the call directly after retiring this FSM,
    ;; before finalization can synchronously reuse the chat and its budget.
    (cl-incf (gptel-runner-call-generation call))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :feedback-active nil))
    (plist-put (gptel-fsm-info fsm) :error failure)
    (gptel-runner-gptel--retire-request call)
    (gptel--fsm-transition fsm 'ERRS)
    (unwind-protect
        (progn
          (gptel-runner-gptel--stop-supervision call)
          (gptel-runner-gptel--update-status
           (gptel-fsm-info fsm) " Error: request budget exhausted" 'error)
          (gptel--handle-post fsm))
      (gptel-runner--finish-call call 'failed failure))))

(defun gptel-runner-gptel--send-request (fsm handlers)
  "Charge runner FSM's next provider send before executing HANDLERS."
  (let* ((call (plist-get (gptel-fsm-info fsm) :context))
         (run (and (gptel-runner-call-p call) (gptel-runner-call-run call)))
         (generation (and run (gptel-runner-call-generation call))))
    (when (or (not run)
              (and (eq fsm (gptel-runner-call-fsm call))
                   (memq (gptel-runner-call-state call)
                         '(running waiting-tool waiting-confirmation))
                   (gptel-runner--check-duration run)))
      (if (and run
               (not (gptel-runner--consume-request run)))
          (gptel-runner-gptel--request-budget-exhausted call fsm)
        (gptel-runner-gptel--prepare-request fsm)
        (when run
          (gptel-runner--emit
           run 'request-started
           (gptel-runner-call-node call) call
           (list :attempt (gptel-runner-call-request-attempt call))))
        ;; An event hook can synchronously cancel or pause this attempt.
        ;; Retiring the FSM does not disarm our captured native handlers.
        (when (or (not run)
                  (and (= generation (gptel-runner-call-generation call))
                       (not (gptel-runner--call-terminal-p call))
                       (eq fsm (gptel-runner-call-fsm call))
                       (gptel-runner--check-duration run t)))
          (mapc (lambda (handler) (funcall handler fsm)) handlers)
          (when (eq (gptel-fsm-state fsm) 'WAIT)
            (gptel--update-wait fsm)))))))

(defun gptel-runner-gptel--post-insert (fsm terminal-handler)
  "Run native post-insert for FSM, then its current TERMINAL-HANDLER.
Complete even when post-processing signals, preserving the original condition.
Skip completion when a hook has cancelled, paused or replaced the request."
  (let* ((call (plist-get (gptel-fsm-info fsm) :context))
         (generation (and (gptel-runner-call-p call)
                          (gptel-runner-call-generation call))))
    (unwind-protect
        (gptel--handle-post-insert fsm)
      ;; Hooks run before completion and can synchronously start another turn.
      ;; The old terminal must never retire that turn or use its completion.
      (when (and (eq (gptel-fsm-state fsm) 'DONE)
                 (or (not (gptel-runner-call-p call))
                     (and (= generation (gptel-runner-call-generation call))
                          (eq fsm (gptel-runner-call-fsm call)))))
        (funcall terminal-handler fsm)))))

(defun gptel-runner-gptel--make-fsm (&optional terminal-handler)
  "Create a per-call FSM with runner terminals and gptel status handlers.
When supplied, TERMINAL-HANDLER handles all three terminal states."
  (let ((handlers (copy-tree gptel-request--handlers))
        (table (copy-tree gptel-send--transitions)))
    ;; Streaming enters TYPE on the HTTP headers, before tool chunks arrive.
    ;; Validate when leaving TYPE so both transports have parsed the payload.
    (push (cons #'gptel-runner-gptel--incomplete-tool-response-p 'ERRS)
          (alist-get 'TYPE table))
    ;; The TRET handler has already recorded the tool result in the native
    ;; chat.  A transfer ends this turn without requesting an acknowledgment.
    (push (cons #'gptel-runner-gptel--transferred-p 'DONE) (alist-get 'TRET table))
    ;; `gptel-request--handlers' drive the transport but intentionally omit
    ;; the status handlers used by interactive gptel buffers.  Runner worker
    ;; buffers enable `gptel-mode', so compose those handlers back in.  The
    ;; TOOL state tracking and status must run before tool execution because a
    ;; synchronous tool can transition the FSM recursively before its handler
    ;; returns.
    ;; Use the send transition table and tool handlers so pre/post-tool hooks,
    ;; request-local cleanup and steering retain gptel's normal ordering.
    ;; Response insertion and terminal completion remain owned by the runner.
    (dolist (state '(TYPE TPRE TRET TSTR))
      (setf (alist-get state handlers)
            (copy-sequence (alist-get state gptel-send--handlers))))
    (setf (alist-get 'WAIT handlers)
          ;; A separate guard cannot short-circuit gptel's handler list.
          ;; Enclose the entire native send chain in the budget gate.
          (let ((send-handlers (alist-get 'WAIT handlers)))
            (list (lambda (fsm)
                    (gptel-runner-gptel--send-request fsm send-handlers))))
          (alist-get 'TOOL handlers)
          (append (list #'gptel-runner-gptel--tool-started
                        #'gptel--update-tool-call)
                  (alist-get 'TOOL handlers)
                  (list #'gptel--update-tool-ask))
          (alist-get 'DONE handlers)
          (list (lambda (fsm)
                  (gptel-runner-gptel--post-insert
                   fsm (or terminal-handler #'gptel-runner-gptel--done))))
          (alist-get 'ERRS handlers)
          (list (or terminal-handler #'gptel-runner-gptel--error))
          (alist-get 'ABRT handlers)
          (list (or terminal-handler #'gptel-runner-gptel--aborted)))
    (gptel-make-fsm :table table
                    :handlers handlers)))

(defun gptel-runner-gptel--retire-request (call)
  "Disarm CALL's finished request before another turn can reuse this chat."
  (when-let* ((fsm (gptel-runner-call-fsm call)))
    (let ((info (gptel-fsm-info fsm)))
      ;; A paused stream may never deliver the native end-of-reasoning
      ;; callback.  Close its display block before inserting a user prompt.
      (when (and (plist-get info :runner-open-reasoning)
                 (buffer-live-p (plist-get info :buffer)))
        (gptel-runner-gptel--insert-response '(reasoning . t) info))
      ;; A delegated tool-only turn may advance the parent's request
      ;; position to reserve its child display before any response arrives.
      (gptel-runner-gptel--note-rendered-end info))
    ;; A late async tool result can advance its old FSM even after abort.
    ;; Each runner FSM owns its handler table.  Keep its request data for
    ;; inspection while preventing another send or insertion into the chat.
    (dolist (entry (gptel-fsm-handlers fsm)) (setcdr entry nil))))

(defun gptel-runner-gptel--release-chats (event)
  "Release node chats after the run's terminal EVENT when retention is disabled."
  (when (and (not gptel-runner-retain-worker-buffers)
             (memq (gptel-runner-event-type event)
                   '(run-completed run-failed run-blocked run-stalled run-cancelled)))
    (when-let* ((run (gptel-runner-find-run (gptel-runner-event-run-id event))))
      (dolist (call (gptel-runner-run-calls run))
        (when-let* ((buffer (gptel-runner-call-buffer call))
                    ((buffer-live-p buffer))
                    ((eq (buffer-local-value 'gptel-runner--call buffer) call)))
          (kill-buffer buffer)))
      (maphash (lambda (_id record)
                 (when-let* ((child (plist-get record :chat)) ((buffer-live-p child)))
                   (kill-buffer child)))
               (gptel-runner-run-child-chats run)))))

(add-hook 'gptel-runner-event-hook #'gptel-runner-gptel--release-chats)

(cl-defmethod gptel-runner-driver-counts-requests-p
  ((_driver gptel-runner-gptel-driver))
  "Charge each gptel transport send, including tool and feedback rounds."
  t)

(defun gptel-runner-gptel--request-turn (call fsm schema callback)
  "Submit CALL's chat up to its prompt marker using FSM, SCHEMA and CALLBACK."
  (save-restriction
    (widen)
    (goto-char gptel-runner-gptel--submitted-end)
    (let ((mark-active nil))
      (gptel-request nil
        :buffer (current-buffer) :stream gptel-stream :context call :fsm fsm
        :schema schema :transforms gptel-prompt-transform-functions
        :callback callback))))

(cl-defmethod gptel-runner-driver-start
  ((_driver gptel-runner-gptel-driver) call complete observe)
  "Start CALL through gptel and report with COMPLETE and OBSERVE."
  (gptel-runner-gptel--check-api)
  (let* ((buffer (gptel-runner-gptel--worker-buffer call))
         (agent (gptel-runner-call-agent call))
         (discussion (gptel-runner-handoff-p (gptel-runner-call-run call)
                                             (gptel-runner-call-node call)))
         (generation (gptel-runner-call-generation call))
         (completed nil)
         (wrapped
          (lambda (status &optional value metadata)
            (unless completed
              (setq completed t)
              ;; Completing can synchronously dispatch the same node again.
              ;; Its new turn must be able to claim this conversation.
              (unless (and (eq status 'transient)
                           (gptel-runner--retryable-p
                            call (plist-get metadata :http-status) metadata))
                (gptel-runner-gptel--stop-supervision call)
                (gptel-runner-gptel--retire-request call))
              (funcall complete status value metadata)))))
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
          (when discussion
            (setf (gptel-runner-call-driver-data call)
                  (plist-put (gptel-runner-call-driver-data call) :discussion-fsm fsm)))
          (gptel-runner-gptel--request-turn
           call fsm (unless discussion (gptel-runner-agent-schema agent))
           (lambda (response info &optional raw)
             (when (and (= generation (gptel-runner-call-generation call))
                        (not (gptel-runner--call-terminal-p call)))
               (gptel-runner-gptel--callback call observe response info raw)))))))))

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
    (gptel-runner-gptel--retire-request call)))

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
    (gptel-runner-gptel--retire-request call)
    (gptel-runner-gptel--show-feedback-status call)))

(defun gptel-runner-gptel--show-feedback-status (call)
  "Show the next action for paused CALL outside its conversation text."
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (with-current-buffer (gptel-runner-call-buffer call)
      (gptel--update-status " Feedback · send a reply or M-x gptel-runner-complete-call-from-buffer"
                            'warning))))

(cl-defmethod gptel-runner-driver-await-feedback
  ((_driver gptel-runner-gptel-driver) call)
  "Make completed CALL's transcript available for human feedback."
  (gptel-runner-gptel--show-feedback-status call))

(defun gptel-runner-gptel--last-response (buffer)
  "Return the last gptel response text found in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (when-let* ((match (text-property-search-backward 'gptel 'response t)))
          (when (>= (prop-match-beginning match)
                    (or gptel-runner-gptel--submitted-end (point-min)))
            (buffer-substring-no-properties
             (prop-match-beginning match) (prop-match-end match))))))))

(defun gptel-runner-gptel--feedback-finished (call generation state fsm)
  "Finish CALL's feedback request at GENERATION in STATE using FSM."
  (unless (plist-get (gptel-fsm-info fsm) :runner-feedback-posted)
    (plist-put (gptel-fsm-info fsm) :runner-feedback-posted t)
    (unless (eq state 'succeeded) (gptel--handle-post fsm)))
  (when (eq fsm (gptel-runner-call-fsm call))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :feedback-active nil)))
  (when (and (= generation (gptel-runner-call-generation call))
             (not (gptel-runner--call-terminal-p call)))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :feedback-active nil))
    (gptel-runner-gptel--stop-supervision call)
    (gptel-runner-gptel--retire-request call)
    (setf (gptel-runner-call-state call) 'waiting-feedback
          (gptel-runner-call-tool-names call) nil)
    (gptel-runner-gptel--show-feedback-status call)
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
An open discussion continues under the workflow's ordinary tool protocol.
At a human approval gate the new response awaits explicit acceptance using
`gptel-runner-complete-call-from-buffer'."
  (unless (and (gptel-runner-call-p call)
               (memq (gptel-runner-call-state call) '(paused waiting-feedback))
               (gptel-runner-call-chat-buffer call))
    (user-error "Call is not available for feedback"))
  (unless (and (stringp text) (not (string-blank-p text)))
    (user-error "Feedback must be a non-empty string"))
  (when (plist-get (gptel-runner-call-driver-data call) :feedback-active)
    (user-error "A feedback request is already active"))
  (gptel-runner-gptel--check-api)
  (if (gptel-runner-gptel--open-discussion-p call)
      (gptel-runner-gptel--continue-discussion call text)
    (gptel-runner-gptel--review-feedback call text)))

(defun gptel-runner-gptel--open-discussion-p (call)
  "Return non-nil when a reply to CALL should reenter the workflow."
  (and (gptel-runner-call-p call)
       (eq (gptel-runner-call-state call) 'waiting-feedback)
       (plist-get (gptel-runner-call-driver-data call) :discussion-wait)
       (pcase (gptel-runner-run-state (gptel-runner-call-run call))
         ('paused t)
         ('running (gptel-runner-call-on-complete call)))))

(defun gptel-runner-gptel--continue-discussion (call text &optional submitted)
  "Queue TEXT on open discussion CALL, optionally already at marker SUBMITTED."
  (let ((run (gptel-runner-call-run call)))
    (when (eq (gptel-runner-run-state run) 'paused)
      (gptel-runner--resume-discussions run))
    (unless (and (eq (gptel-runner-run-state run) 'running)
                 (eq (gptel-runner-call-state call) 'waiting-feedback)
                 (gptel-runner-call-on-complete call))
      (user-error "The workflow could not resume this discussion"))
    (when submitted
      (with-current-buffer (marker-buffer submitted)
        (goto-char submitted)
        (gptel-runner-gptel--mark-submitted)))
    (cl-incf (gptel-runner-call-generation call))
    (setf (gptel-runner-call-state call) 'ready
          (gptel-runner-call-fsm call) nil
          (gptel-runner-call-driver-data call) (list :turn-started (and submitted t))
          (gptel-runner-call-prompt call) text
          (gptel-runner-call-request-attempt call) 0
          (gptel-runner-call-result call) nil
          (gptel-runner-run-queue run)
          (nconc (gptel-runner-run-queue run) (list call)))
    (gptel-runner--emit run 'feedback-started (gptel-runner-call-node call) call
                        (list :text text))
    (gptel-runner--drain-queue run)
    call))

(defun gptel-runner-gptel--discard-feedback-request (fsm)
  "Cancel abandoned FSM's transport and release its request resources."
  (let* ((info (gptel-fsm-info fsm))
         (prompt (plist-get info :data))
         (request (cl-find fsm gptel--request-alist :key #'cadr)))
    ;; A post-request hook may signal after transport registration.  Abort
    ;; exactly this FSM, even if another request now uses the same buffer.
    (when request
      (setq gptel--request-alist (delq request gptel--request-alist))
      (ignore-error (error quit) (funcall (cddr request))))
    (unwind-protect
        (when (and info (not (plist-get info :runner-feedback-posted)))
          (plist-put info :runner-feedback-posted t)
          (gptel--handle-post fsm))
      ;; Before prompt transforms finish, :data is gptel's temporary buffer.
      ;; A signalled transform bypasses its disposal in gptel--realize-query.
      (when (and (buffer-live-p prompt) (not (eq prompt (plist-get info :buffer))))
        (kill-buffer prompt)))))

(defun gptel-runner-gptel--undo-feedback (changes original-tick)
  "Undo failed feedback CHANGES and update mode caches after ORIGINAL-TICK."
  (save-restriction
    (widen)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (changed (/= original-tick (buffer-modified-tick)))
          (old-size (buffer-size)))
      ;; Notify modes of one replacement.  Hooks can edit anywhere during
      ;; setup, and a failing hook must never interrupt the undo itself.
      (cl-flet ((notify (function &rest args)
                  (ignore-error (error quit) (apply function args))
                  nil))
        (when changed
          (run-hook-wrapped 'before-change-functions #'notify (point-min) (point-max)))
        (unwind-protect (cancel-change-group changes)
          (when changed
            (run-hook-wrapped 'after-change-functions #'notify
                              (point-min) (point-max) old-size)))))))

(defun gptel-runner-gptel--review-feedback (call text)
  "Send TEXT to CALL while keeping its explicit human review gate open."
  (with-current-buffer (gptel-runner-call-buffer call)
    (let* ((run (gptel-runner-call-run call))
           (buffer (current-buffer))
           (generation (1+ (gptel-runner-call-generation call)))
           (fsm (gptel-runner-gptel--make-fsm
                 (lambda (finished-fsm)
                   (gptel-runner-gptel--feedback-finished
                    call generation
                    (pcase (gptel-fsm-state finished-fsm)
                      ('DONE 'succeeded) ('ABRT 'cancelled) (_ 'failed))
                    finished-fsm))))
           (observe (lambda (type &optional data)
                      (gptel-runner--call-observe call generation type data)))
           (current-request-p
            (lambda ()
              (and (= generation (gptel-runner-call-generation call))
                   (eq fsm (gptel-runner-call-fsm call))
                   (plist-get (gptel-runner-call-driver-data call) :feedback-active)
                   (not (gptel-runner--call-terminal-p call)))))
           (previous-state (gptel-runner-call-state call))
           (previous-fsm (gptel-runner-call-fsm call))
           (previous-data (gptel-runner-call-driver-data call))
           (submitted gptel-runner-gptel--submitted-end)
           (rendered gptel-runner-gptel--rendered-end)
           (previous-last gptel--fsm-last)
           (writer (gptel-runner--writer-p call))
           (changes (prepare-change-group))
           (original-tick (buffer-modified-tick))
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           started)
      (unwind-protect
          (progn
            (activate-change-group changes)
            ;; Admission hooks can acquire an external workspace lock.
            ;; Acquire it only once the FSM and rollback data are ready.
            (let ((inhibit-quit t))
              (unless (gptel-runner--can-start-p run call)
                (user-error "Feedback cannot start while its workspace or concurrency slot is busy"))
              (push call (gptel-runner-run-active-calls run))
              (cl-incf (gptel-runner-run-active-count run))
              (when writer (cl-incf (gptel-runner-run-writer-active run)))
              (setf (gptel-runner-call-generation call) generation
                    (gptel-runner-call-fsm call) fsm
                    (gptel-runner-call-state call) 'running
                    (gptel-runner-call-driver-data call)
                    (plist-put (plist-put (copy-sequence previous-data) :observe observe)
                               :feedback-active t)))
            (gptel-runner-gptel--insert-turn text)
            (gptel-runner--emit run 'feedback-started
                                (gptel-runner-call-node call) call (list :text text))
            (when (funcall current-request-p)
              (gptel-runner-gptel--start-supervision call)
              (gptel-runner-gptel--request-turn
               call fsm nil
               (lambda (response info &optional raw)
                 (when (funcall current-request-p)
                   (gptel-runner-gptel--callback call observe response info raw)))))
            (setq started t))
        ;; A hook may have completed, cancelled or replaced this turn.
        ;; Only the owner can roll back its prompt and release the slot.
        (if (or started (not (funcall current-request-p)))
            (when (buffer-live-p buffer) (accept-change-group changes))
          (cl-incf (gptel-runner-call-generation call))
          ;; Preserve the setup error or quit even if cleanup hooks fail.
          (ignore-error (error quit) (gptel-runner-gptel--retire-request call))
          (ignore-error (error quit) (gptel-runner-gptel--stop-supervision call))
          (when (buffer-live-p buffer)
            (ignore-error (error quit)
              (gptel-runner-gptel--undo-feedback changes original-tick))
            (setq gptel-runner-gptel--submitted-end submitted
                  gptel-runner-gptel--rendered-end rendered)
            (when (eq gptel--fsm-last fsm) (setq gptel--fsm-last previous-last)))
          (setf (gptel-runner-call-fsm call) previous-fsm
                (gptel-runner-call-state call) previous-state
                (gptel-runner-call-driver-data call) previous-data)
          (ignore-error (error quit) (gptel-runner--deactivate-call call))
          (ignore-error (error quit) (gptel-runner-gptel--discard-feedback-request fsm))
          (gptel-runner--checkpoint run t)
          (gptel-runner--drain-queue run)))))
  call)

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
  (setq call (or call (gptel-runner--buffer-call)))
  (unless (gptel-runner-call-p call)
    (user-error "No runner call is associated with this buffer"))
  (unless (memq (gptel-runner-call-state call) '(waiting-feedback paused))
    (user-error "Call %s is not waiting for feedback" (gptel-runner-call-id call)))
  (when (plist-get (gptel-runner-call-driver-data call) :feedback-active)
    (user-error "Wait for the active feedback request before accepting"))
  (when-let* ((buffer (gptel-runner-call-chat-buffer call)))
    (with-current-buffer buffer
      (when (or (cl-some #'gptel--fsm-live-p gptel-runner-gptel--manual-requests)
                (and (not (eq gptel--fsm-last (gptel-runner-call-fsm call)))
                     (gptel--fsm-live-p)))
        (user-error "Wait for your gptel response before accepting"))))
  (setq result
        (or result
            (when-let* ((buffer (gptel-runner-call-chat-buffer call)))
              (gptel-runner-gptel--last-response buffer))
            (gptel-runner-call-result call)))
  (unless (and (stringp result) (not (string-blank-p result)))
    (user-error "Select a response or continue the gptel conversation first"))
  (if (and (gptel-runner-call-on-complete call)
           (not (eq (gptel-runner-run-state (gptel-runner-call-run call)) 'paused)))
      (gptel-runner--finish-call call 'succeeded result)
    (gptel-runner--complete-restored-call call result))
  call)

(defun gptel-runner-gptel--guard-indirect-chat (buffer)
  "Reject requests targeting an indirect view of runner-owned BUFFER text.
BUFFER may be a buffer or its name."
  (when-let* (((or (bufferp buffer) (stringp buffer)))
              (target (get-buffer buffer))
              (base (buffer-base-buffer target))
              ((or (gptel-runner--buffer-call base)
                   (and (fboundp 'gptel-runner-store--child-owner)
                        (gptel-runner-store--child-owner base)))))
    (user-error "Send from base chat %s; indirect views share its transcript"
                (buffer-name base))))

(defun gptel-runner-gptel--before-send (&optional arg)
  "Guard live node chats from duplicate sends, allowing menus and steering ARG."
  (unless (member arg '(0 (4)))
    (gptel-runner-gptel--guard-indirect-chat (current-buffer)))
  (when-let* (((not (member arg '(0 (4)))))
              (call (gptel-runner--buffer-call))
              ((memq (gptel-runner-call-state call)
                     '(ready running retry-wait waiting-tool waiting-confirmation))))
    (user-error "This node is running; pause it to send feedback")))

(defun gptel-runner-gptel--before-inline-send ()
  "Check the backing node before the inline prompt appends its reference."
  (when-let* ((base (buffer-base-buffer)))
    (with-current-buffer base (gptel-runner-gptel--before-send))))

(defun gptel-runner-gptel--around-send (original &optional arg)
  "Resume open node discussions on a normal send through ORIGINAL with ARG."
  (gptel-runner-gptel--before-send arg)
  (if (or arg (use-region-p)
          (not (gptel-runner-gptel--open-discussion-p (gptel-runner--buffer-call))))
      (funcall original arg)
    (save-restriction
      (widen)
      (let* ((end (point))
             (start (save-excursion
                      (goto-char (gptel-runner-gptel--history-end))
                      (dolist (prefix (list gptel-response-separator
                                            (gptel-prompt-prefix-string)))
                        (when (looking-at (regexp-quote prefix))
                          (goto-char (match-end 0))))
                      (point)))
             (text (and (< start end) (buffer-substring-no-properties start end))))
        (unless (and text (not (string-blank-p text)))
          (user-error "Type a reply after the latest response"))
        (gptel-runner-gptel--continue-discussion
         (gptel-runner--buffer-call) text (point-marker))))))

(defun gptel-runner-gptel--wake-chat-queue ()
  "Wake the chat's queued workflow turns after gptel or buffer cleanup."
  (when-let* ((call (gptel-runner--buffer-call))
              (run (gptel-runner-call-run call))
              ((gptel-runner-run-queue run)))
    (run-at-time 0 nil #'gptel-runner--drain-queue run)))

(put 'gptel-runner-gptel--wake-chat-queue 'permanent-local-hook t)

(defun gptel-runner-gptel--release-send (buffer fsm)
  "Release FSM's reservation of BUFFER, then admit waiting workflow turns."
  (when (gptel-runner--buffer-call buffer)
    (with-current-buffer buffer
      (gptel-runner-gptel--note-rendered-end (gptel-fsm-info fsm))
      (setq gptel-runner-gptel--manual-requests (delq fsm gptel-runner-gptel--manual-requests))
      (unless gptel-runner-gptel--manual-requests
        (remove-hook 'kill-buffer-hook #'gptel-runner-gptel--wake-chat-queue t)
        (when-let* ((call (gptel-runner--buffer-call))
                    (run (gptel-runner-call-run call))
                    ((gptel-runner-run-queue run)))
          (gptel-runner--drain-queue run))))))

(defun gptel-runner-gptel--track-send (original &optional prompt &rest args)
  "Track an ordinary send through ORIGINAL with PROMPT and ARGS in a node chat."
  (dolist (target (list (current-buffer) (plist-get args :buffer)
                        (when (markerp (plist-get args :position))
                          (marker-buffer (plist-get args :position)))))
    (gptel-runner-gptel--guard-indirect-chat target))
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
         (fsm (plist-get args :fsm)))
    (if (not (and (gptel-runner--buffer-call buffer)
                  fsm
                  (let ((handlers (gptel-fsm-handlers fsm)))
                    (or (eq handlers gptel-send--handlers)
                        (and handlers
                             (eq handlers (bound-and-true-p gptel-inline--handlers)))))
                  (not (plist-get args :dry-run))))
        (apply original prompt args)
      (with-current-buffer buffer
        (gptel-runner-gptel--before-send)
        ;; Reserve the conversation before asynchronous prompt preparation.
        (push fsm gptel-runner-gptel--manual-requests)
        (add-hook 'kill-buffer-hook #'gptel-runner-gptel--wake-chat-queue nil t))
      (condition-case err
          (prog1 (apply original prompt args)
            (if (gptel--fsm-live-p fsm)
                (let* ((info (gptel-fsm-info fsm))
                       ;; Keep the reservation until the terminal handlers unwind.
                       (release (lambda (&rest _)
                                  (run-at-time 0 nil #'gptel-runner-gptel--release-send buffer fsm))))
                  (plist-put info :post (cons release (plist-get info :post))))
              (gptel-runner-gptel--release-send buffer fsm)))
        ((error quit)
         (gptel-runner-gptel--release-send buffer fsm)
         (signal (car err) (cdr err)))))))

(defun gptel-runner-gptel--admit-chat (call)
  "Admit CALL when its node chat is free of ordinary sends."
  (let ((chat (gethash (gptel-runner-node-id (gptel-runner-call-node call))
                       (gptel-runner-run-chats (gptel-runner-call-run call)))))
    (not (and (buffer-live-p chat)
              (buffer-local-value 'gptel-runner-gptel--manual-requests chat)))))

(add-hook 'gptel-runner-call-admission-functions #'gptel-runner-gptel--admit-chat)

(defun gptel-runner-gptel--around-abort (original &optional buffer)
  "Integrate ORIGINAL `gptel-abort' with runner worker BUFFER."
  (let* ((target (or buffer (current-buffer)))
         (call (gptel-runner--buffer-call target)))
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
  (let* ((interaction (and (overlayp overlay) (overlay-get overlay 'gptel-runner-interaction)))
         (target (or (and interaction (plist-get (overlay-get overlay 'info) :buffer))
                     (and (overlayp overlay) (overlay-buffer overlay))
                     (current-buffer)))
         (call (or (car interaction)
                   (gptel-runner--buffer-call target))))
    ;; A gptel inspection buffer can retain the old overlay after a dashboard
    ;; approval or a pause.  Its edited callbacks must not execute a second time.
    (when (and interaction
               (not (gptel-runner-gptel-interaction-pending-p call (cdr interaction))))
      (user-error "This tool request is no longer pending"))
    (when (and (gptel-runner-call-p call)
               (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (gptel-runner-gptel--observe
       call 'waiting-tool (gptel-runner-call-tool-names call)))
    (if (not call) (funcall original response overlay)
      (unwind-protect
          (with-current-buffer target (funcall original response overlay))
        (gptel-runner-gptel--sync-interactions call)))))

(defun gptel-runner-gptel-install-abort-advice ()
  "Install conditional runner integration around gptel commands."
  (interactive)
  (gptel-runner-gptel--check-api)
  ;; Workflow turns and ordinary sends in a node chat share the native
  ;; renderers.  Keep the native callback and handler identities intact.
  (dolist (renderer '(gptel--insert-response gptel-curl--stream-insert-response))
    (unless (advice-member-p #'gptel-runner-gptel--render-at-marker renderer)
      (advice-add renderer :around #'gptel-runner-gptel--render-at-marker)))
  (dolist (handler '(gptel--handle-pre-insert gptel--handle-post-insert))
    (unless (advice-member-p #'gptel-runner-gptel--handle-at-marker handler)
      (advice-add handler :around #'gptel-runner-gptel--handle-at-marker)))
  (unless (advice-member-p #'gptel-runner-gptel--before-realize-query 'gptel--realize-query)
    (advice-add 'gptel--realize-query :before #'gptel-runner-gptel--before-realize-query))
  (unless (advice-member-p #'gptel-runner-gptel--around-send 'gptel-send)
    (advice-add 'gptel-send :around #'gptel-runner-gptel--around-send))
  (unless (advice-member-p #'gptel-runner-gptel--track-send 'gptel-request)
    (advice-add 'gptel-request :around #'gptel-runner-gptel--track-send))
  (unless (advice-member-p #'gptel-runner-gptel--around-abort 'gptel-abort)
    (advice-add 'gptel-abort :around #'gptel-runner-gptel--around-abort))
  (unless (advice-member-p #'gptel-runner-gptel--around-accept-tool-calls
                           'gptel--accept-tool-calls)
    (advice-add 'gptel--accept-tool-calls :around
                #'gptel-runner-gptel--around-accept-tool-calls))
  ;; Surround gptel-agent's supervision and display handlers.  Registration
  ;; needs both the owner they identify and the finished native preview.
  (unless (advice-member-p #'gptel-runner-gptel--display-tool-calls 'gptel--display-tool-calls)
    (advice-add 'gptel--display-tool-calls :around #'gptel-runner-gptel--display-tool-calls
                '((depth . -100)))))

(with-eval-after-load 'gptel
  (gptel-runner-gptel-install-abort-advice))

(with-eval-after-load 'gptel-inline
  (advice-add 'gptel-inline-send :before #'gptel-runner-gptel--before-inline-send))

(with-eval-after-load 'gptel-agent-tools
  (add-hook 'gptel-agent-child-buffer-functions #'gptel-runner-store-track-child))

(unless gptel-runner-default-driver
  (setq gptel-runner-default-driver (gptel-runner-gptel-driver-create)))

(provide 'gptel-runner-gptel)
;;; gptel-runner-gptel.el ends here
