;;; gptel-runner-gptel.el --- gptel compatibility driver -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.4"))

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
(declare-function gptel-mode "gptel")
(declare-function gptel-runner--complete-restored-call "gptel-runner-flow")
(declare-function gptel-runner-resume-run "gptel-runner-flow")

(defvar gptel-request--transitions)
(defvar gptel-request--handlers)
(defvar gptel-confirm-tool-calls)
(defvar gptel-default-mode)

(cl-defstruct (gptel-runner-gptel-driver
               (:constructor gptel-runner-gptel-driver-create))
  "Driver backed by `gptel-request'.")

(defvar gptel-runner-gptel--cancelling nil)

(defun gptel-runner-gptel--compatibility-error (&rest missing)
  "Signal an actionable compatibility error listing MISSING API pieces."
  (error (concat "gptel-runner compatibility failure: missing or unsupported "
                 "%S.  Install gptel >= 0.9.9.4 or update "
                 "gptel-runner-gptel.el for this gptel version")
         missing))

(defun gptel-runner-gptel--check-api ()
  "Load gptel and validate the supported request API shape."
  (unless (require 'gptel nil t)
    (gptel-runner-gptel--compatibility-error 'gptel))
  (unless (require 'gptel-request nil t)
    (gptel-runner-gptel--compatibility-error 'gptel-request-library))
  (dolist (function '(gptel-request gptel-abort gptel-make-fsm
                      gptel--fsm-transition gptel--handle-post
                      gptel--apply-preset gptel--insert-response))
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
          :status (plist-get info :status)
          :error (plist-get info :error))))

(defun gptel-runner-gptel--callback (call observe response info &optional raw)
  "Use OBSERVE to record gptel RESPONSE and INFO for CALL.
RAW is gptel's internal flag for already formatted transcript content."
  (cond
   ((stringp response)
    (unless raw
      (push response (gptel-runner-call-response-parts call))
      (funcall observe 'response response))
    (gptel--insert-response response info raw))
   ((eq response 'abort) nil)
   ((null response) nil)
   ((consp response)
    (pcase (car response)
      ('reasoning
       (funcall observe 'reasoning (cdr response))
       (gptel--insert-response response info))
      ('tool-call
       (funcall observe 'tool-calls (cdr response))
       (funcall observe 'waiting-confirmation (cdr response))
       ;; The normal gptel renderer adds its confirmation UI and callbacks.
       (gptel--insert-response response info))
      ('tool-result
       (funcall observe 'tool-results (cdr response))
       (gptel--insert-response response info))
      (_ (funcall observe 'response-part response))))
   (t (funcall observe 'response-part response))))

(defun gptel-runner-gptel--post-once (call fsm)
  "Run gptel post-processing for CALL and FSM exactly once."
  (unless (plist-get (gptel-runner-call-driver-data call) :posted)
    (setf (gptel-runner-call-driver-data call)
          (plist-put (gptel-runner-call-driver-data call) :posted t))
    (gptel--handle-post fsm)))

(defun gptel-runner-gptel--complete-function (call)
  "Return the current scheduler completion closure for CALL."
  (plist-get (gptel-runner-call-driver-data call) :complete))

(defun gptel-runner-gptel--done (fsm)
  "Handle successful terminal state for runner FSM."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (value (mapconcat #'identity
                           (nreverse (gptel-runner-call-response-parts call)) "")))
    (gptel-runner-gptel--post-once call fsm)
    (funcall (gptel-runner-gptel--complete-function call) 'success value nil)))

(defun gptel-runner-gptel--error (fsm)
  "Handle error state for runner FSM, preserving it for transient retries."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context))
         (metadata (gptel-runner-gptel--status-metadata info))
         (status (plist-get metadata :http-status))
         (retryable (gptel-runner--retryable-p call status metadata)))
    (unless retryable (gptel-runner-gptel--post-once call fsm))
    (funcall (gptel-runner-gptel--complete-function call)
             (if (or retryable (null status)
                     (memq status gptel-runner--retryable-statuses))
                 'transient
               'permanent)
             (plist-get info :error) metadata)))

(defun gptel-runner-gptel--aborted (fsm)
  "Handle aborted terminal state for runner FSM."
  (let* ((info (gptel-fsm-info fsm))
         (call (plist-get info :context)))
    (gptel-runner-gptel--post-once call fsm)
    (if (plist-get (gptel-runner-call-driver-data call) :pausing)
        (setf (gptel-runner-call-driver-data call)
              (plist-put (gptel-runner-call-driver-data call) :pausing nil))
      (funcall (gptel-runner-gptel--complete-function call)
               'cancelled nil nil))))

(defun gptel-runner-gptel--make-fsm ()
  "Create a per-call FSM with only runner terminal handlers replaced."
  (let ((handlers (copy-tree gptel-request--handlers)))
    (setf (alist-get 'DONE handlers) (list #'gptel-runner-gptel--done)
          (alist-get 'ERRS handlers) (list #'gptel-runner-gptel--error)
          (alist-get 'ABRT handlers) (list #'gptel-runner-gptel--aborted))
    (gptel-make-fsm :table (copy-tree gptel-request--transitions)
                    :handlers handlers)))

(defun gptel-runner-gptel--cleanup (call)
  "Clean CALL's worker buffer unless retention is enabled."
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
    (with-current-buffer buffer
      (if (and (gptel-runner-call-fsm call)
               (eq (gptel-fsm-state (gptel-runner-call-fsm call)) 'ERRS))
          ;; A transport retry resumes the exact FSM after its backoff timer.
          (gptel--fsm-transition (gptel-runner-call-fsm call) 'WAIT)
        (gptel-runner-gptel--apply-preset-locally
         (gptel-runner-agent-preset agent))
        (if (plist-get (gptel-runner-run-options
                        (gptel-runner-call-run call))
                       :allow-unconfirmed-tools)
            (setq-local gptel-confirm-tool-calls nil)
          (when (gptel-runner--writer-p call)
            ;; A writable preset cannot silently weaken the run safety gate.
            (setq-local gptel-confirm-tool-calls t)))
        (let ((fsm (gptel-runner-gptel--make-fsm)))
          (setf (gptel-runner-call-fsm call) fsm)
          (gptel-request
              (gptel-runner-call-prompt call)
            :buffer buffer :stream nil :context call :schema schema :fsm fsm
            :callback
            (lambda (response info &optional raw)
              (gptel-runner-gptel--callback
               call observe response info raw))))))))

(cl-defmethod gptel-runner-driver-cancel
  ((_driver gptel-runner-gptel-driver) call)
  "Cancel CALL using its unique worker buffer."
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
  (setf (gptel-runner-call-driver-data call)
        (plist-put (gptel-runner-call-driver-data call) :pausing t))
  (when (buffer-live-p (gptel-runner-call-buffer call))
    (let ((gptel-runner-gptel--cancelling t))
      (gptel-abort (gptel-runner-call-buffer call)))
    (when (and (gptel-runner-call-fsm call)
               (not (eq (gptel-fsm-state (gptel-runner-call-fsm call))
                        'ABRT)))
      (gptel--fsm-transition (gptel-runner-call-fsm call) 'ABRT))
    (with-current-buffer (gptel-runner-call-buffer call)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert
         (concat
          "\n\nRunner intervention\n===================\n\n"
          "This workflow call is paused.  Add feedback and continue with "
          "ordinary gptel commands in this buffer.  When the response should "
          "be returned to the workflow, select it or leave point after the "
          "latest response and run M-x "
          "gptel-runner-complete-call-from-buffer.\n"))))))

(defun gptel-runner-gptel--last-response (buffer)
  "Return the last gptel response text found in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when-let ((match (text-property-search-backward
                         'gptel 'response t)))
        (buffer-substring-no-properties
         (prop-match-beginning match) (prop-match-end match))))))

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
  (setq result
        (or result
            (and (buffer-live-p (gptel-runner-call-buffer call))
                 (gptel-runner-gptel--last-response
                  (gptel-runner-call-buffer call)))))
  (unless (and (stringp result) (not (string-empty-p result)))
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
                     '(running retry-wait waiting-confirmation))
               (not (gptel-runner--call-terminal-p call)))
      (gptel-runner-abort-call call 'gptel-abort))
    (funcall original target)))

(defun gptel-runner-gptel-install-abort-advice ()
  "Install conditional runner integration around `gptel-abort'."
  (interactive)
  (gptel-runner-gptel--check-api)
  (unless (advice-member-p #'gptel-runner-gptel--around-abort 'gptel-abort)
    (advice-add 'gptel-abort :around #'gptel-runner-gptel--around-abort)))

(with-eval-after-load 'gptel
  (gptel-runner-gptel-install-abort-advice))

(unless gptel-runner-default-driver
  (setq gptel-runner-default-driver (gptel-runner-gptel-driver-create)))

(provide 'gptel-runner-gptel)
;;; gptel-runner-gptel.el ends here
