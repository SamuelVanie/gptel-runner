;;; gptel-runner-store.el --- Durable snapshots for gptel-runner -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Versioned execution checkpoints and node chats for named workflows.  Automatic
;; checkpoints are debounced and all snapshots are serialized in short timer
;; slices so persistence does not monopolize the Emacs event loop.  Snapshots
;; contain the execution cursor and one native session per visited agent node.
;; Archived conversations become ordinary chat buffers on first visit.

;;; Code:

(require 'cl-lib)
(require 'gptel-runner-core)
(require 'gptel-runner-flow)

(autoload 'gptel-runner-gptel-chat-text "gptel-runner-gptel")
(autoload 'gptel-runner-gptel-chat-from-text "gptel-runner-gptel")
(autoload 'gptel-runner-gptel-save-chat "gptel-runner-gptel")
(autoload 'gptel-runner-gptel-open-chat "gptel-runner-gptel")
(declare-function gptel-runner-gptel-submitted-end "gptel-runner-gptel")
(declare-function gptel-runner-gptel-rendered-end "gptel-runner-gptel")
(declare-function gptel-runner-gptel-restore-submitted-end "gptel-runner-gptel")

(declare-function gptel-agent-session-buffer-record "gptel-agent-session")
(declare-function gptel-agent-session-buffer-from-record "gptel-agent-session")
(declare-function gptel-agent-session-export-buffer "gptel-agent-session")
(declare-function gptel-agent-session-write-record "gptel-agent-session")
(declare-function gptel-agent-session-import-buffer "gptel-agent-session")

(defgroup gptel-runner-store nil
  "Persistent snapshots for gptel-runner."
  :group 'gptel-runner)

(defcustom gptel-runner-snapshot-directory
  (expand-file-name "gptel-runner/snapshots/" user-emacs-directory)
  "Directory in which durable runner snapshots are stored."
  :type 'directory)

(defcustom gptel-runner-checkpoint-delay 5.0
  "Seconds of inactivity before an automatic checkpoint starts.
Repeated safe checkpoints reset this delay.  Explicit saves, pauses, and
terminal run transitions bypass it, but their writes are still asynchronous."
  :type 'number)

(defconst gptel-runner-snapshot-version 4
  "Current on-disk runner snapshot format version.")

(defconst gptel-runner-store--slice-seconds 0.01
  "Approximate maximum serialization time in one event-loop slice.")

(defconst gptel-runner-store--chunk-bytes (* 256 1024)
  "Approximate maximum buffered output before a snapshot write.")

(cl-defstruct (gptel-runner-store--token
               (:constructor gptel-runner-store--token-create))
  "One snapshot VALUE and the CONTEXT used for serialization errors."
  value context)

(cl-defstruct (gptel-runner-store--writer
               (:constructor gptel-runner-store--writer-create))
  "Incremental writer state for one snapshot generation."
  revision file temporary tokens parts (part-bytes 0) notify)

(cl-defstruct (gptel-runner-store--coordinator
               (:constructor gptel-runner-store--coordinator-create))
  "Save coordination state belonging to one runner RUN."
  run file (revision 0) (committed-revision 0) (state 'clean)
  timer writer error urgent notify)

(defvar gptel-runner-store--coordinators (make-hash-table :test #'eq)
  "Save coordinators indexed by live run object.")

(defvar-local gptel-runner-store--child-run nil
  "Run that owns this delegated chat or worker transcript.")
(defvar-local gptel-runner-store--child-id nil
  "Identifier of this transcript in its owning run.")
(dolist (symbol '(gptel-runner-store--child-run gptel-runner-store--child-id))
  (put symbol 'permanent-local t))

(defun gptel-runner-store--agent-sessions-p ()
  "Whether agent sessions support records under independent host ownership."
  (and (featurep 'gptel-agent-session)
       (default-boundp 'gptel-agent-session-managed-externally)
       (fboundp 'gptel-agent-session-buffer-record)))

(defun gptel-runner-store--require-agent-sessions ()
  "Load the reader needed for an archived gptel-agent chat."
  (unless (and (require 'gptel-agent nil t)
               (gptel-runner-store--agent-sessions-p))
    (user-error "Install gptel-agent with externally managed session support to open this chat")))

(defun gptel-runner-store--archive (buffer)
  "Capture BUFFER in its native chat format."
  (if (gptel-runner-store--agent-sessions-p)
      (cons 'gptel-agent (gptel-agent-session-buffer-record buffer))
    (cons 'gptel (gptel-runner-gptel-chat-text buffer))))

(defun gptel-runner-store--import-agent-chat (function data)
  "Use FUNCTION to open standalone agent DATA under runner ownership."
  (gptel-runner-store--require-agent-sessions)
  (funcall function data t))

(defun gptel-runner-store--text-buffer (text name)
  "Open a read-only worker transcript containing TEXT in a new buffer NAME."
  (with-current-buffer (generate-new-buffer name)
    (insert (substring-no-properties text))
    (goto-char (point-min))
    (special-mode)
    (set-buffer-modified-p nil)
    (current-buffer)))

(defun gptel-runner-store-export-chat (chat file)
  "Save live or archived CHAT to FILE and return its native format."
  (pcase chat
    (`(transcript . ,text) (gptel-runner-gptel-save-chat text file) 'transcript)
    (`(gptel-agent . ,record)
     (gptel-runner-store--require-agent-sessions)
     (gptel-agent-session-write-record record file)
     'gptel-agent)
    (`(gptel . ,text) (gptel-runner-gptel-save-chat text file) 'gptel)
    (_ (if (gptel-runner-store--agent-sessions-p)
           (progn (gptel-agent-session-export-buffer chat file) 'gptel-agent)
         (gptel-runner-gptel-save-chat chat file)
         'gptel))))

(defun gptel-runner-store-open-chat (file format)
  "Open FILE in FORMAT as a fresh, independently editable chat."
  (pcase format
    ('gptel-agent (gptel-runner-store--import-agent-chat
                   #'gptel-agent-session-import-buffer file))
    ('transcript (with-temp-buffer
                   (insert-file-contents file)
                   (gptel-runner-store--text-buffer
                    (buffer-string) "*gptel-runner child transcript*")))
    ('gptel (gptel-runner-gptel-open-chat file))
    (_ (user-error "Unknown runner chat format %S" format))))

(defun gptel-runner-store--chat-changed (&rest _)
  "Queue a checkpoint for the current chat when its run is persistent."
  (when gptel-runner--call
    (gptel-runner--checkpoint (gptel-runner-call-run gptel-runner--call))))

(defun gptel-runner-store--chat-owner ()
  "Return the call whose run owns the current node chat, if any."
  (when-let* ((call gptel-runner--call)
              (run (gptel-runner-call-run call))
              (id (gptel-runner-node-id (gptel-runner-call-node call)))
              ((eq (current-buffer) (gethash id (gptel-runner-run-chats run)))))
    call))

(defun gptel-runner-store--allow-chat-kill-p ()
  "Allow killing this chat once its run has finished or released it."
  (if-let* ((call (gptel-runner-store--chat-owner))
            (run (gptel-runner-call-run call))
            ((eq run (gptel-runner-find-run (gptel-runner-run-id run))))
            ((not (gptel-runner--run-terminal-p run))))
      (progn
        (message "Bury this node chat to hide it. Use ? in the runner dashboard to pause or abort.")
        nil)
    t))

(defun gptel-runner-store--close-chat ()
  "Retain a session record when a node conversation is closed."
  (when-let* ((call (gptel-runner-store--chat-owner))
              (run (gptel-runner-call-run call))
              (id (gptel-runner-node-id (gptel-runner-call-node call))))
    (when (memq (gptel-runner-call-state call)
                '(running waiting-confirmation waiting-tool retry-wait))
      (gptel-runner-pause-call call 'chat-closed))
    (setf (gptel-runner-call-driver-data call)
          (plist-put (plist-put (gptel-runner-call-driver-data call) :submitted-end
                                (gptel-runner-gptel-submitted-end (current-buffer)))
                     :rendered-end (gptel-runner-gptel-rendered-end (current-buffer))))
    (puthash id (gptel-runner-store--archive (current-buffer))
             (gptel-runner-run-chats run))
    (gptel-runner--checkpoint run)))

(defun gptel-runner-store-track-chat (call buffer)
  "Associate CALL with its node's ordinary gptel BUFFER and return BUFFER."
  (setf (gptel-runner-call-buffer call) buffer)
  (puthash (gptel-runner-node-id (gptel-runner-call-node call)) buffer
           (gptel-runner-run-chats (gptel-runner-call-run call)))
  (with-current-buffer buffer
    (setq-local gptel-runner--call call)
    (add-hook 'after-change-functions #'gptel-runner-store--chat-changed nil t)
    (add-hook 'kill-buffer-query-functions #'gptel-runner-store--allow-chat-kill-p -90 t)
    (add-hook 'kill-buffer-hook #'gptel-runner-store--close-chat nil t))
  buffer)

(defun gptel-runner-node-buffer (run node)
  "Return the chat for NODE in RUN, restoring it on demand.
NODE is a workflow node or its ID.  Return nil before the first chat turn
and for nodes that execute without a gptel conversation."
  (let* ((id (if (gptel-runner-node-p node) (gptel-runner-node-id node)
               (gptel-runner-node-id (gptel-runner--resolve-node run node))))
         (chat (gethash id (gptel-runner-run-chats run))))
    (cond
     ((buffer-live-p chat) chat)
     ((consp chat)
      (let ((call (cl-find-if
                   (lambda (c)
                     (and (equal id (gptel-runner-node-id (gptel-runner-call-node c)))
                          (or (gptel-runner-call-buffer c)
                              (plist-get (gptel-runner-call-driver-data c) :restored))))
                   (gptel-runner-run-calls run) :from-end t))
            buffer)
        (unless call (error "No call owns chat for node %S" id))
        (pcase (car chat)
          ('gptel-agent
           (setq buffer (gptel-runner-store--import-agent-chat
                         #'gptel-agent-session-buffer-from-record (cdr chat)))
           (let ((data (gptel-runner-call-driver-data call)))
             (gptel-runner-gptel-restore-submitted-end
              buffer (plist-get data :submitted-end) (plist-get data :rendered-end))))
          ('gptel (setq buffer (gptel-runner-gptel-chat-from-text (cdr chat))))
          (_ (user-error "Unknown saved chat format %S" (car chat))))
        (with-current-buffer buffer
          (setq-local default-directory (gptel-runner-call-workspace call)))
        (gptel-runner-store-track-chat call buffer))))))

(defun gptel-runner-call-chat-buffer (call)
  "Return CALL's node conversation, reopening a saved chat if necessary."
  (or (and (buffer-live-p (gptel-runner-call-buffer call))
           (gptel-runner-call-buffer call))
      (gptel-runner-node-buffer (gptel-runner-call-run call)
                                (gptel-runner-call-node call))))

(defun gptel-runner-store--child-archive (record)
  "Return the detached transcript for live or archived child RECORD."
  (let ((chat (plist-get record :chat)))
    (if (not (buffer-live-p chat)) chat
      (with-current-buffer chat
        (if (and (eq (plist-get record :kind) 'discussion)
                 (bound-and-true-p gptel-mode))
            (gptel-runner-store--archive chat)
          (save-restriction
            (widen)
            (cons 'transcript (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun gptel-runner-store--child-changed (&rest _)
  "Queue a runner checkpoint after the child transcript changes."
  (when-let* ((run gptel-runner-store--child-run)
              ((gethash gptel-runner-store--child-id (gptel-runner-run-child-chats run))))
    (gptel-runner--checkpoint run)))

(defun gptel-runner-store--close-child ()
  "Capture the complete child transcript before its buffer disappears."
  (when-let* ((run gptel-runner-store--child-run)
              (record (gethash gptel-runner-store--child-id
                               (gptel-runner-run-child-chats run)))
              ((eq (plist-get record :chat) (current-buffer))))
    (setf (plist-get record :name) (buffer-name)
          (plist-get record :chat) (gptel-runner-store--child-archive record))
    (gptel-runner--checkpoint run)))

(dolist (function '(gptel-runner-store--child-changed gptel-runner-store--close-child))
  (put function 'permanent-local-hook t))

(defun gptel-runner-store--attach-child (run record buffer)
  "Attach BUFFER to its RECORD in RUN for runner-only checkpointing."
  (setf (plist-get record :chat) buffer)
  (with-current-buffer buffer
    (setq-local gptel-runner-store--child-run run
                gptel-runner-store--child-id (plist-get record :id))
    (add-hook 'after-change-functions #'gptel-runner-store--child-changed nil t)
    (add-hook 'kill-buffer-hook #'gptel-runner-store--close-child 90 t))
  buffer)

(defun gptel-runner-store-track-child (parent child kind)
  "Register CHILD of PARENT as KIND in the parent's runner session, if any."
  (let* ((call (buffer-local-value 'gptel-runner--call parent))
         (run (or (and call (gptel-runner-call-run call))
                  (buffer-local-value 'gptel-runner-store--child-run parent))))
    (when (and run (not (buffer-local-value 'gptel-runner-store--child-id child)))
      (let* ((children (gptel-runner-run-child-chats run))
             (parent-id (buffer-local-value 'gptel-runner-store--child-id parent))
             (owner (gethash parent-id children))
             (id (gptel-runner--id "child"))
             (record (list :id id :kind kind :name (buffer-name child)
                           :node (if owner (plist-get owner :node)
                                   (gptel-runner-node-id (gptel-runner-call-node call)))
                           :call (if owner (plist-get owner :call) (gptel-runner-call-id call))
                           :parent-id parent-id :chat child)))
        (puthash id record children)
        (gptel-runner-store--attach-child run record child)
        (gptel-runner--checkpoint run)))))

(defun gptel-runner-child-buffer (run id)
  "Return child transcript ID in RUN, restoring its buffer on first visit."
  (let ((record (or (gethash id (gptel-runner-run-child-chats run))
                    (user-error "Unknown child transcript %s" id))))
    (pcase (plist-get record :chat)
      ((and buffer (pred buffer-live-p)) buffer)
      (chat
       (gptel-runner-store--attach-child
        run record
        (pcase chat
          (`(gptel-agent . ,data) (gptel-runner-store--import-agent-chat
                                   #'gptel-agent-session-buffer-from-record data))
          (`(gptel . ,text) (gptel-runner-gptel-chat-from-text text))
          (`(transcript . ,text) (gptel-runner-store--text-buffer text (plist-get record :name)))
          (_ (user-error "Unknown child transcript format"))))))))

(defun gptel-runner-store--alist-hash (alist &optional test)
  "Return a hash table populated from ALIST using TEST."
  (let ((table (make-hash-table :test (or test #'equal))))
    (dolist (entry alist table)
      (puthash (car entry) (cdr entry) table))))

(defun gptel-runner-store--default-file (run)
  "Return the default snapshot filename for RUN."
  (expand-file-name
   (concat (replace-regexp-in-string
            "[^[:alnum:]_.-]" "_" (gptel-runner-run-id run))
           ".snapshot.el")
   gptel-runner-snapshot-directory))

(defun gptel-runner-store--validate-run (run)
  "Validate that RUN can be queued for persistent storage."
  (unless (gptel-runner-run-p run)
    (user-error "Not a gptel-runner run: %S" run))
  (unless (gptel-runner-workflow-name (gptel-runner-run-workflow run))
    (user-error "Persistent runs require a named workflow")))

(defun gptel-runner-store--table-entries (table)
  "Return a shallow alist snapshot of hash TABLE."
  (let (entries)
    (maphash (lambda (key value) (push (cons key value) entries)) table)
    (nreverse entries)))

(defun gptel-runner-store--duration-remaining (run)
  "Return RUN's duration remaining at snapshot capture time."
  (let ((remaining (gptel-runner-run-duration-remaining run)))
    (if (and (numberp remaining)
             (numberp (gptel-runner-run-active-started-at run)))
        (max 0 (- remaining
                  (- (float-time)
                     (gptel-runner-run-active-started-at run))))
      remaining)))

(defun gptel-runner-store--terminal-data (run)
  "Return a durable classification of RUN's terminal failure."
  (when (eq (gptel-runner-run-state run) 'failed)
    (let ((data (gptel-runner-run-terminal-data run)))
      (cond
       ((and (listp data)
             (memq (plist-get data :type) '(budget iteration-budget)))
        (copy-tree data t))
       ((and (listp data)
             (memq (plist-get data :type) '(elisp-error elisp-timeout)))
        (list :type (plist-get data :type)))
       (t '(:type other))))))

(defun gptel-runner-store--chat-position (call key accessor)
  "Read CALL's chat position with ACCESSOR, or use its saved value at KEY."
  (let ((buffer (gptel-runner-call-buffer call)))
    (if (and (buffer-live-p buffer)
             (eq (buffer-local-value 'gptel-runner--call buffer) call))
        (funcall accessor buffer)
      (plist-get (gptel-runner-call-driver-data call) key))))

(defun gptel-runner-store--call-record (call)
  "Capture CALL's scheduling metadata for its node's saved chat."
  (list :id (gptel-runner-call-id call)
        :node (gptel-runner-node-id (gptel-runner-call-node call))
        :state (gptel-runner-call-state call)
        :discussion-owner (gptel-runner-call-discussion-owner call)
        :discussion-wait (plist-get (gptel-runner-call-driver-data call) :discussion-wait)
        :submitted-end (gptel-runner-store--chat-position
                        call :submitted-end #'gptel-runner-gptel-submitted-end)
        :rendered-end (gptel-runner-store--chat-position
                       call :rendered-end #'gptel-runner-gptel-rendered-end)
        :prompt (when-let* ((prompt (gptel-runner-call-prompt call)))
                  (substring-no-properties prompt))
        :result (let ((result (gptel-runner-call-result call)))
                  (if (stringp result) (substring-no-properties result) result))
        :error (gptel-runner-call-error call)
        :started-at (gptel-runner-call-started-at call)
        :finished-at (gptel-runner-call-finished-at call)
        :attempt (gptel-runner-call-request-attempt call)
        :generation (gptel-runner-call-generation call)
        :repair (gptel-runner-call-repair-p call)))

(defconst gptel-runner-store--title-prefix ";;; gptel-runner session · "
  "Prefix of the single-line session picker label in saved checkpoints.")

(defun gptel-runner-store-session-title (file)
  "Read FILE's picker label, or return its basename for older checkpoints."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file nil 0 2048)
          (goto-char (point-min))
          (when (looking-at (regexp-quote gptel-runner-store--title-prefix))
            (buffer-substring-no-properties (match-end 0) (line-end-position)))))
      (file-name-nondirectory file)))

(defun gptel-runner-store--snapshot-tokens (run)
  "Return incremental serialization tokens for a compact snapshot of RUN."
  (let ((budget (gptel-runner-run-budget run)) tokens)
    (cl-labels
        ((literal (text) (push text tokens))
         (value (object context)
           (push (gptel-runner-store--token-create
                  :value object :context context)
                 tokens))
         (table (name object strict)
           (literal (format "\n  :%s (" name))
           (dolist (entry (gptel-runner-store--table-entries object))
             (literal "\n   ")
             (value entry
                    (if strict
                        (format "blackboard entry %S" (car entry))
                      (format "%s entry %S" name (car entry)))))
           (literal ")")))
      (literal (concat
                gptel-runner-store--title-prefix
                (truncate-string-to-width
                 (replace-regexp-in-string
                  "[[:cntrl:][:space:]]+" " "
                  (format "%s · %s · %s  %s"
                          (gptel-runner-workflow-name (gptel-runner-run-workflow run))
                          (gptel-runner-run-state run)
                          (format-time-string "%Y-%m-%d %H:%M")
                          (or (gptel-runner-run-goal run) "")))
                 180 nil nil "…")
                "\n"))
      (literal (format "(:format gptel-runner-snapshot\n :version %d\n :saved-at "
                       gptel-runner-snapshot-version))
      (value (float-time) "save time")
      (literal "\n :run (:id ")
      (value (gptel-runner-run-id run) "run identifier")
      (literal "\n  :workflow ")
      (value (gptel-runner-workflow-name
              (gptel-runner-run-workflow run))
             "workflow name")
      (when-let* ((ids (gptel-runner--generated-node-ids (gptel-runner-run-workflow run))))
        (literal "\n  :generated-node-ids ")
        (value ids "generated workflow node IDs"))
      (literal "\n  :goal ")
      (value (gptel-runner-run-goal run) "run goal")
      (literal "\n  :workspace ")
      (value (gptel-runner-run-workspace run) "workspace")
      (literal "\n  :output-directory ")
      (value (gptel-runner-run-output-directory run) "output directory")
      (literal "\n  :artifacts ")
      (value (copy-tree (gptel-runner-run-artifacts run)) "artifacts")
      (literal "\n  :output-epoch ")
      (value (gptel-runner-run-output-epoch run) "output epoch")
      (literal "\n  :state ")
      (value (gptel-runner-run-state run) "run state")
      (table "blackboard" (gptel-runner-run-blackboard run) t)
      (table "node-states" (gptel-runner-run-node-states run) nil)
      (table "node-outcomes" (gptel-runner-run-node-outcomes run) nil)
      (table "discussions" (gptel-runner-run-discussions run) nil)
      (literal "\n  :messages ")
      (value (cl-remove-if-not
              (lambda (event) (memq (gptel-runner-event-type event)
                                    '(node-handoff node-follow-up node-reply)))
              (gptel-runner-run-events run))
             "node messages")
      (literal "\n  :chats (")
      (maphash
       (lambda (id chat)
         (literal "\n   ")
         (value (cons id (if (buffer-live-p chat)
                             (gptel-runner-store--archive chat)
                           chat))
                (format "chat for node %S" id)))
       (gptel-runner-run-chats run))
      (literal ")\n  :child-chats (")
      (maphash
       (lambda (id record)
         (let ((copy (copy-sequence record)))
           (when (buffer-live-p (plist-get record :chat))
             (setf (plist-get copy :name) (buffer-name (plist-get record :chat))))
           (setf (plist-get copy :chat) (gptel-runner-store--child-archive record))
           (literal "\n   ")
           (value (cons id copy) (format "child transcript %s" id))))
       (gptel-runner-run-child-chats run))
      (literal ")\n  :chat-calls (")
      (dolist (call (gptel-runner-run-calls run))
        (when (and (or (gptel-runner-call-buffer call)
                       (plist-get (gptel-runner-call-driver-data call) :restored))
                   (gethash (gptel-runner-node-id (gptel-runner-call-node call))
                            (gptel-runner-run-chats run)))
          (literal "\n   ")
          (value (gptel-runner-store--call-record call) "chat call")))
      (literal ")")
      (table "iterations" (gptel-runner-run-iterations run) nil)
      (table "repeat-limits" (gptel-runner-run-repeat-limits run) nil)
      (literal "\n  :budget ")
      (value
       (list :max-requests (gptel-runner-budget-max-requests budget)
             :max-calls (gptel-runner-budget-max-calls budget)
             :max-duration (gptel-runner-budget-max-duration budget)
             :requests (gptel-runner-budget-requests budget)
             :calls (gptel-runner-budget-calls budget))
       "budget")
      (literal "\n  :started-at ")
      (value (gptel-runner-run-started-at run) "start time")
      (literal "\n  :finished-at ")
      (value (gptel-runner-run-finished-at run) "finish time")
      (literal "\n  :terminal-data ")
      (value (gptel-runner-store--terminal-data run) "terminal result")
      (literal "\n  :extension-context ")
      (value (copy-tree (gptel-runner-run-extension-context run) t)
             "extension context")
      (literal "\n  :finalization ")
      (value (copy-tree (gptel-runner-run-finalization run) t) "finalization")
      (literal "\n  :paused-at ")
      (value (gptel-runner-run-paused-at run) "pause time")
      (literal "\n  :duration-remaining ")
      (value (gptel-runner-store--duration-remaining run)
             "remaining duration")
      (literal "\n  :generation ")
      (value (gptel-runner-run-generation run) "run generation")
      (literal "\n  :options ")
      (value (copy-sequence (gptel-runner-run-options run)) "run options")
      (literal "))\n")
      (nreverse tokens))))

(defun gptel-runner-store--serialize-token (token)
  "Serialize TOKEN and verify that the result can be read back."
  (condition-case err
      (let* ((print-circle t)
             (print-level nil)
             (print-length nil)
             (printed (prin1-to-string
                       (gptel-runner-store--token-value token)))
             (parsed (read-from-string printed)))
        (unless (= (cdr parsed) (length printed))
          (error "Serialized value has trailing unreadable data"))
        printed)
    (error
     (error "Snapshot %s cannot be persisted: %s"
            (gptel-runner-store--token-context token)
            (error-message-string err)))))

(defun gptel-runner-store--coordinator (run)
  "Return RUN's existing or newly allocated save coordinator."
  (or (gethash run gptel-runner-store--coordinators)
      (let ((coordinator
             (gptel-runner-store--coordinator-create :run run)))
        (puthash run coordinator gptel-runner-store--coordinators)
        coordinator)))

(defun gptel-runner-store-save-status (run)
  "Return the persistence status of RUN.
The result is one of `clean', `pending', `writing', or `error'."
  (if-let* ((coordinator
            (gethash run gptel-runner-store--coordinators)))
      (gptel-runner-store--coordinator-state coordinator)
    'clean))

(defun gptel-runner-store--cancel-timer (coordinator)
  "Cancel COORDINATOR's outstanding timer, if any."
  (when (timerp (gptel-runner-store--coordinator-timer coordinator))
    (cancel-timer (gptel-runner-store--coordinator-timer coordinator)))
  (setf (gptel-runner-store--coordinator-timer coordinator) nil))

(defun gptel-runner-store--delete-temporary (writer)
  "Delete WRITER's incomplete temporary snapshot."
  (when (and writer
             (gptel-runner-store--writer-temporary writer)
             (file-exists-p
              (gptel-runner-store--writer-temporary writer)))
    (delete-file (gptel-runner-store--writer-temporary writer))))

(defun gptel-runner-store-cancel-save (run)
  "Cancel pending persistence work for RUN and remove temporary data."
  (when-let* ((coordinator
              (gethash run gptel-runner-store--coordinators)))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary
     (gptel-runner-store--coordinator-writer coordinator))
    (remhash run gptel-runner-store--coordinators)
    t))

(defun gptel-runner-store--fail (coordinator err)
  "Record ERR and stop COORDINATOR's current save attempt."
  (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
         (notify (or (gptel-runner-store--coordinator-notify coordinator)
                     (and writer
                          (gptel-runner-store--writer-notify writer)))))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary writer)
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-state coordinator) 'error
          (gptel-runner-store--coordinator-error coordinator) err
          (gptel-runner-store--coordinator-urgent coordinator) nil
          (gptel-runner-store--coordinator-notify coordinator) nil)
    (condition-case nil
        (gptel-runner--emit
         (gptel-runner-store--coordinator-run coordinator)
         'snapshot-error nil nil err)
      (error nil))
    (when notify
      (message "Snapshot save failed: %s" (error-message-string err)))))

(defun gptel-runner-store--flush-parts (writer)
  "Append WRITER's buffered text parts to its temporary file."
  (when (gptel-runner-store--writer-parts writer)
    (let ((coding-system-for-write 'utf-8-emacs)
          (text (apply #'concat
                       (nreverse (gptel-runner-store--writer-parts writer)))))
      (write-region text nil
                    (gptel-runner-store--writer-temporary writer)
                    t 'silent))
    (setf (gptel-runner-store--writer-parts writer) nil
          (gptel-runner-store--writer-part-bytes writer) 0)))

(defun gptel-runner-store--append-part (writer text)
  "Add TEXT to WRITER's buffered output."
  (push text (gptel-runner-store--writer-parts writer))
  (cl-incf (gptel-runner-store--writer-part-bytes writer)
           (string-bytes text))
  (when (>= (gptel-runner-store--writer-part-bytes writer)
            gptel-runner-store--chunk-bytes)
    (gptel-runner-store--flush-parts writer)))

(defun gptel-runner-store--schedule-slice (coordinator)
  "Arrange another writer slice for COORDINATOR."
  (setf (gptel-runner-store--coordinator-timer coordinator)
        (run-at-time 0 nil #'gptel-runner-store--write-slice coordinator)))

(defun gptel-runner-store--commit (coordinator)
  "Commit COORDINATOR's completed temporary snapshot atomically."
  (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
         (run (gptel-runner-store--coordinator-run coordinator))
         (revision (gptel-runner-store--writer-revision writer))
         (file (gptel-runner-store--writer-file writer))
         (notify (gptel-runner-store--writer-notify writer)))
    (gptel-runner-store--flush-parts writer)
    (set-file-modes (gptel-runner-store--writer-temporary writer) #o600)
    (rename-file (gptel-runner-store--writer-temporary writer) file t)
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-committed-revision coordinator)
          revision
          (gptel-runner-store--coordinator-error coordinator) nil)
    (if (< revision (gptel-runner-store--coordinator-revision coordinator))
        (progn
          (setf (gptel-runner-store--coordinator-state coordinator) 'pending
                (gptel-runner-store--coordinator-urgent coordinator) t)
          (gptel-runner-store--schedule-begin coordinator 0))
      (setf (gptel-runner-store--coordinator-state coordinator) 'clean))
    (condition-case nil
        (gptel-runner--emit run 'snapshot-saved nil nil
                            (list :file file :revision revision))
      (error nil))
    (when notify
      (message "Snapshot saved to %s" file))))

(defun gptel-runner-store--write-slice (coordinator &optional drain)
  "Write one time slice for COORDINATOR.
When DRAIN is non-nil, finish all remaining work synchronously."
  (setf (gptel-runner-store--coordinator-timer coordinator) nil)
  (condition-case err
      (let* ((writer (gptel-runner-store--coordinator-writer coordinator))
             (deadline (+ (float-time) gptel-runner-store--slice-seconds))
             processed)
        (unless writer
          (error "Snapshot writer disappeared"))
        (while (and (gptel-runner-store--writer-tokens writer)
                    (or drain (not processed) (< (float-time) deadline)))
          (let ((token (pop (gptel-runner-store--writer-tokens writer))))
            (gptel-runner-store--append-part
             writer
             (if (stringp token)
                 token
               (gptel-runner-store--serialize-token token))))
          (setq processed t))
        (gptel-runner-store--flush-parts writer)
        (if (gptel-runner-store--writer-tokens writer)
            (gptel-runner-store--schedule-slice coordinator)
          (gptel-runner-store--commit coordinator)))
    (error (gptel-runner-store--fail coordinator err))))

(defun gptel-runner-store--begin (coordinator)
  "Begin an incremental snapshot for COORDINATOR's latest revision."
  (gptel-runner-store--cancel-timer coordinator)
  (condition-case err
      (let* ((run (gptel-runner-store--coordinator-run coordinator))
             (file (gptel-runner-store--coordinator-file coordinator))
             (directory (file-name-directory file))
             ;; An exporter can fail.  Capture before allocating a temp file.
             (tokens (gptel-runner-store--snapshot-tokens run)))
        (make-directory directory t)
        (setf
         (gptel-runner-store--coordinator-writer coordinator)
         (gptel-runner-store--writer-create
          :revision (gptel-runner-store--coordinator-revision coordinator)
          :file file
          :temporary
          (make-temp-file (expand-file-name ".gptel-runner-" directory)
                          nil ".snapshot")
          :tokens tokens
          :notify (gptel-runner-store--coordinator-notify coordinator))
         (gptel-runner-store--coordinator-state coordinator) 'writing
         (gptel-runner-store--coordinator-error coordinator) nil
         (gptel-runner-store--coordinator-urgent coordinator) nil
         (gptel-runner-store--coordinator-notify coordinator) nil)
        (gptel-runner-store--schedule-slice coordinator))
    (error (gptel-runner-store--fail coordinator err))))

(defun gptel-runner-store--schedule-begin (coordinator delay)
  "Begin saving COORDINATOR after DELAY seconds."
  (gptel-runner-store--cancel-timer coordinator)
  (setf (gptel-runner-store--coordinator-timer coordinator)
        (run-at-time delay nil #'gptel-runner-store--begin coordinator)))

(defun gptel-runner-store--queue (run file immediate notify)
  "Queue RUN for FILE, honoring IMMEDIATE and user NOTIFY flags."
  (gptel-runner-store--validate-run run)
  (setq file (expand-file-name
              (or file (gptel-runner-run-snapshot-file run)
                  (gptel-runner-store--default-file run))))
  (setf (gptel-runner-run-snapshot-file run) file
        (gptel-runner-run-options run)
        (plist-put (gptel-runner-run-options run) :persist t))
  (let* ((coordinator (gptel-runner-store--coordinator run))
         (already-queued
          (memq (gptel-runner-store--coordinator-state coordinator)
                '(pending writing))))
    (setf (gptel-runner-store--coordinator-file coordinator) file
          (gptel-runner-store--coordinator-notify coordinator)
          (or notify (gptel-runner-store--coordinator-notify coordinator)))
    (cl-incf (gptel-runner-store--coordinator-revision coordinator))
    (when immediate
      (setf (gptel-runner-store--coordinator-urgent coordinator) t))
    (unless (eq (gptel-runner-store--coordinator-state coordinator) 'writing)
      (setf (gptel-runner-store--coordinator-state coordinator) 'pending)
      (gptel-runner-store--schedule-begin
       coordinator
       (if (gptel-runner-store--coordinator-urgent coordinator)
           0
         (max 0 gptel-runner-checkpoint-delay))))
    (unless already-queued
      (gptel-runner--emit
       run 'snapshot-save-queued nil nil
       (list :file file
             :revision (gptel-runner-store--coordinator-revision coordinator)
             :immediate immediate)))
    file))

(defun gptel-runner-schedule-save (run &optional immediate)
  "Queue an automatic snapshot of RUN.
When IMMEDIATE is non-nil, bypass `gptel-runner-checkpoint-delay'."
  (gptel-runner-store--queue run nil immediate nil))

(defun gptel-runner-save-run (run &optional file)
  "Queue an atomic snapshot of RUN to FILE and return its absolute filename.
The command returns before serialization or disk I/O completes.  Completion
is reported through a `snapshot-saved' event; failures use `snapshot-error'."
  (gptel-runner-store--queue run file t t))

(defun gptel-runner-store--flush-coordinator (coordinator)
  "Synchronously commit COORDINATOR's latest queued revision."
  (when (memq (gptel-runner-store--coordinator-state coordinator)
              '(pending writing))
    (gptel-runner-store--cancel-timer coordinator)
    (gptel-runner-store--delete-temporary
     (gptel-runner-store--coordinator-writer coordinator))
    (setf (gptel-runner-store--coordinator-writer coordinator) nil
          (gptel-runner-store--coordinator-state coordinator) 'pending)
    (gptel-runner-store--begin coordinator)
    (gptel-runner-store--cancel-timer coordinator)
    (when (gptel-runner-store--coordinator-writer coordinator)
      (gptel-runner-store--write-slice coordinator t))))

(defun gptel-runner-store-flush-pending ()
  "Synchronously finish all queued snapshots.
This is used during normal Emacs shutdown; interactive saves remain
asynchronous."
  (maphash (lambda (_run coordinator)
             (gptel-runner-store--flush-coordinator coordinator))
           gptel-runner-store--coordinators))

(defun gptel-runner-store-flush-run (run)
  "Finish saving RUN before releasing its live session."
  (gptel-runner-schedule-save run t)
  (let ((coordinator (gptel-runner-store--coordinator run)))
    (gptel-runner-store--flush-coordinator coordinator)
    (when-let* ((err (gptel-runner-store--coordinator-error coordinator)))
      (signal (car err) (cdr err)))))

(defun gptel-runner-store--node (root id)
  "Find node ID below ROOT."
  (gptel-runner-find-node root id))

(defun gptel-runner-store--notice-id (id)
  "Advance the process-local identifier counter beyond string ID."
  (when (and (stringp id) (string-match "-\\([0-9]+\\)\\'" id))
    (setq gptel-runner--next-id
          (max gptel-runner--next-id
               (string-to-number (match-string 1 id))))))

(defun gptel-runner-store--read-file (file)
  "Read and validate session or legacy version 2 checkpoint FILE as data."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (unless (eq (plist-get data :format) 'gptel-runner-snapshot)
        (user-error "Not a gptel-runner snapshot: %s" file))
      (unless (memq (plist-get data :version) (list 2 3 gptel-runner-snapshot-version))
        (user-error "Unsupported snapshot version %S (expected v2, v3 or v4)"
                    (plist-get data :version)))
      data)))

(defun gptel-runner-store--restore-call (run record)
  "Restore RECORD's place in RUN without creating a buffer or a request."
  (let* ((node (gptel-runner--resolve-node run (plist-get record :node)))
         (id (plist-get record :id))
         (state (plist-get record :state)))
    (unless (and (stringp id) (string-match-p "\\`[[:alnum:]_-]+\\'" id)
                 (eq (gptel-runner-node-kind node) 'agent)
                 (gethash (gptel-runner-node-id node) (gptel-runner-run-chats run)))
      (user-error "Invalid saved chat call %S" id))
    (gptel-runner-store--notice-id id)
    (gptel-runner-call-create
     :id id :run run :node node :workspace (gptel-runner-node-workspace run node)
     :agent (gptel-runner--agent (plist-get (gptel-runner-node-properties node) :agent))
     :state (if (or (gptel-runner--terminal-p state) (eq state 'waiting-feedback))
                state 'paused)
     :discussion-owner (plist-get record :discussion-owner)
     :started-at (plist-get record :started-at) :finished-at (plist-get record :finished-at)
     :result (plist-get record :result) :error (plist-get record :error)
     :prompt (plist-get record :prompt) :generation (or (plist-get record :generation) 0)
     :repair-p (plist-get record :repair) :request-attempt (or (plist-get record :attempt) 0)
     :driver-data (list :restored t :submitted-end (plist-get record :submitted-end)
                        :rendered-end (plist-get record :rendered-end)
                        :discussion-wait (plist-get record :discussion-wait)))))

(defun gptel-runner-load-run (file &optional callback driver)
  "Open the run saved in FILE using CALLBACK and DRIVER.
The workflow and agents must already be registered.  Its node conversations
open on demand.  A version 2 checkpoint restores execution state only."
  (setq file (expand-file-name file))
  (let* ((snapshot (gptel-runner-store--read-file file))
         (data (plist-get snapshot :run))
         (run-id (plist-get data :id))
         (workflow-name (plist-get data :workflow))
         (registered (or (gethash workflow-name gptel-runner--workflows)
                         (user-error "Define workflow %S before loading snapshot"
                                     workflow-name)))
         (workflow (gptel-runner--restore-node-ids
                    registered (and (>= (plist-get snapshot :version) 3)
                                    (plist-get data :generated-node-ids))))
         (root (gptel-runner-workflow-root workflow))
         (repeat-limits
          (let ((limits (gptel-runner--workflow-repeat-limits root)))
            (dolist (entry (plist-get data :repeat-limits) limits)
              (puthash (car entry) (cdr entry) limits))))
         (budget-data (plist-get data :budget))
         (budget
          (gptel-runner-budget-create
           :max-requests (plist-get budget-data :max-requests)
           :max-calls (plist-get budget-data :max-calls)
           :max-duration (plist-get budget-data :max-duration)
           :requests (or (plist-get budget-data :requests) 0)
           :calls (or (plist-get budget-data :calls) 0)))
         (saved-state (plist-get data :state))
         (run
          (gptel-runner-run-create
           :id run-id :workflow workflow
           :goal (plist-get data :goal) :workspace (plist-get data :workspace)
           :output-directory (plist-get data :output-directory)
           :artifacts (plist-get data :artifacts)
           :output-epoch (plist-get data :output-epoch)
           :state (if (gptel-runner--terminal-p saved-state)
                      saved-state 'paused)
           :blackboard (gptel-runner-store--alist-hash
                        (plist-get data :blackboard))
           :node-states (gptel-runner-store--alist-hash
                         (plist-get data :node-states))
           :node-outcomes (gptel-runner-store--alist-hash
                           (plist-get data :node-outcomes))
           :chats (gptel-runner-store--alist-hash (plist-get data :chats))
           :child-chats (gptel-runner-store--alist-hash (plist-get data :child-chats))
           :discussions (gptel-runner-store--alist-hash (plist-get data :discussions))
           :iterations (gptel-runner-store--alist-hash
                        (plist-get data :iterations))
           :repeat-limits repeat-limits
           :events (plist-get data :messages) :budget budget
           :driver (or driver gptel-runner-default-driver
                       (and (gptel-runner--needs-driver-p root)
                            (require 'gptel-runner-gptel)
                            gptel-runner-default-driver))
           :queue nil :active-calls nil :calls nil
           :started-at (plist-get data :started-at)
           :finished-at (plist-get data :finished-at)
           :terminal-data (plist-get data :terminal-data)
           :extension-context (plist-get data :extension-context)
           :finalization (plist-get data :finalization)
           :paused-at (or (plist-get data :paused-at) (float-time))
           :duration-remaining (plist-get data :duration-remaining)
           :generation (or (plist-get data :generation) 0)
           :callback callback :snapshot-file file
           :options (let ((options (copy-sequence (plist-get data :options))))
                      ;; Checkpoints predating tool handoffs completed on prose.
                      (unless (plist-member options :completion)
                        (setq options (plist-put options :completion 'response)))
                      (plist-put options :persist t)))))
    (when (gethash run-id gptel-runner--runs)
      (user-error "Run %s is already loaded in this Emacs session" run-id))
    (when (and (gptel-runner--needs-driver-p root)
               (not (gptel-runner-run-driver run)))
      (user-error "No driver configured for restored run"))
    (gptel-runner--validate-workflow
     root (plist-get (gptel-runner-run-options run) :allow-writes)
     (gptel-runner-run-workspace run)
     (list (cons "workflow" (plist-get (gptel-runner-workflow-options workflow)
                                       :context))
           (cons "run" (plist-get (gptel-runner-run-options run) :context))))
    (gptel-runner--validate-discussions run)
    (dolist (entry (plist-get data :node-states))
      (unless (gptel-runner-store--node root (car entry))
        (user-error "Snapshot node %S is absent from workflow %S"
                    (car entry) workflow-name)))
    (dolist (entry (plist-get data :node-outcomes))
      (let ((state (plist-get (cdr entry) :state)))
        (unless (and (gptel-runner-find-node root (car entry))
                     (gptel-runner--terminal-p state)
                     (eq state (gethash (car entry)
                                        (gptel-runner-run-node-states run)))
                     (plist-member (cdr entry) :value))
          (user-error "Snapshot node outcome %S is invalid for workflow %S"
                      (car entry) workflow-name))))
    (dolist (entry (plist-get data :iterations))
      (unless (gptel-runner-store--node root (car entry))
        (user-error "Snapshot iteration node %S is absent from workflow %S"
                    (car entry) workflow-name)))
    (dolist (entry (plist-get data :repeat-limits))
      (let ((node (gptel-runner-store--node root (car entry))))
        (unless (and node
                     (eq (gptel-runner-node-kind node) 'repeat)
                     (integerp (cdr entry))
                     (> (cdr entry) 0))
          (user-error "Snapshot repeat limit %S is invalid for workflow %S"
                      entry workflow-name))))
    (setf (gptel-runner-run-calls run)
          (mapcar (lambda (record) (gptel-runner-store--restore-call run record))
                  (plist-get data :chat-calls)))
    (maphash
     (lambda (id record)
       (unless (and (stringp id) (equal id (plist-get record :id))
                    (stringp (plist-get record :name))
                    (memq (plist-get record :kind) '(agent discussion review-aside))
                    (memq (car-safe (plist-get record :chat)) '(gptel gptel-agent transcript))
                    (gptel-runner-find-node run (plist-get record :node))
                    (cl-find (plist-get record :call) (gptel-runner-run-calls run)
                             :key #'gptel-runner-call-id :test #'equal)
                    (or (null (plist-get record :parent-id))
                        (gethash (plist-get record :parent-id) (gptel-runner-run-child-chats run))))
         (user-error "Invalid child transcript %S" id))
       (gptel-runner-store--notice-id id))
     (gptel-runner-run-child-chats run))
    (gptel-runner-store--notice-id (gptel-runner-run-id run))
    (puthash (gptel-runner-run-id run) run gptel-runner--runs)
    (gptel-runner--emit run 'snapshot-loaded nil nil file)
    run))

(defun gptel-runner-resume-snapshot
    (file &optional feedback callback driver)
  "Load FILE and resume it with FEEDBACK, CALLBACK, and DRIVER."
  (gptel-runner-resume-run
   (gptel-runner-load-run file callback driver) feedback callback))

(defun gptel-runner-list-snapshots ()
  "Return known snapshot files, newest first."
  (when (file-directory-p gptel-runner-snapshot-directory)
    (sort (directory-files gptel-runner-snapshot-directory t
                           "\\.snapshot\\.el\\'")
          (lambda (a b)
            (time-less-p (file-attribute-modification-time
                          (file-attributes b))
                         (file-attribute-modification-time
                          (file-attributes a)))))))

(add-hook 'kill-emacs-hook #'gptel-runner-store-flush-pending)

(provide 'gptel-runner-store)
;;; gptel-runner-store.el ends here
