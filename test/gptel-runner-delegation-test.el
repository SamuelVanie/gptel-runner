;;; gptel-runner-delegation-test.el --- Optional agent integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-agent)
(require 'gptel-runner-gptel-test)
(require 'gptel-runner-jobs)

(defvar gptel-runner-delegation-test--stream nil)
(defvar gptel-runner-delegation-test--completion 'response)

(defmacro gptel-runner-delegation-test--with-run (&rest body)
  "Run BODY using real runner and agent handlers with queued transport."
  (declare (indent 0))
  `(gptel-runner-gptel-test--with-transport
     (let* ((gptel-runner-headless t)
            (gptel-stream gptel-runner-delegation-test--stream)
            (gptel-default-mode 'text-mode)
            (gptel-agent-use-project-instructions nil)
            (gptel-agent--runs (make-hash-table :test #'equal))
            (gptel-agent--run-history nil)
            (gptel-agent--agent-snapshot
             '(("worker" :system "Investigate" :task-timeout nil :max-request-rounds nil)))
            (gptel-agent-preset nil)
            (gptel-post-response-functions nil)
            (gptel-post-request-hook nil)
            (gptel-pre-response-hook nil)
            (gptel-pre-tool-call-functions nil)
            (gptel-post-tool-call-functions nil)
            (gptel-prompt-transform-functions
             '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize))
            executions interactions
            (gptel-tools
             (append (mapcar #'gptel-get-tool '("Agent" "Discussion" "DiscussionControl"))
                     (list (gptel-make-tool
                            :name "delegation_probe" :description "Record execution context."
                            :args nil :include t :confirm t
                            :function (lambda ()
                                        (push (current-buffer) executions)
                                        "Verified")))))
            (gptel-log-level nil)
            (gptel-runner-interaction-hook
             (list (lambda (_call pending) (push pending interactions))))
            (initial-buffers (buffer-list)))
       (gptel-runner-register-agent 'coordinator :preset nil)
       (let* ((pipeline
               (gptel-runner-start
                (gptel-runner-agent-step :id 'coordinate :agent 'coordinator
                                         :prompt "Investigate together"
                                         :completion gptel-runner-delegation-test--completion)
                :driver (gptel-runner-gptel-driver-create)))
              (call (car (gptel-runner-run-calls pipeline)))
              (fsm (gptel-runner-call-fsm call))
              (parent (gptel-runner-call-buffer call)))
         (unwind-protect (progn ,@body)
           (gptel-runner-abort-run pipeline)
           (gptel-runner-gptel--stop-supervision call)
           (dolist (buffer (cl-set-difference (buffer-list) initial-buffers))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))))

(defun gptel-runner-delegation-test--settle ()
  "Let event-driven agent and discussion notifications unwind."
  (accept-process-output nil 0.02))

(defun gptel-runner-delegation-test--tool (fsm name args)
  "Deliver a real tool response to FSM for NAME and ARGS."
  (gptel-runner-gptel-test--reply
   fsm nil "tool_calls"
   (vector (list :id "delegation-tool" :type "function"
                 :function (list :name name :arguments (gptel--json-encode args))))))

(defun gptel-runner-delegation-test--answer (fsm text)
  "Finish FSM's current response with TEXT."
  (gptel-runner-gptel-test--reply fsm text "stop"))

(defun gptel-runner-delegation-test--agent (fsm &optional background)
  "Spawn a worker from FSM, optionally in BACKGROUND."
  (gptel-runner-delegation-test--tool
   fsm "Agent" (list :subagent_type "worker" :description "Inspect evidence"
                    :prompt "Inspect evidence" :background (if background t :json-false)))
  (car (sort (hash-table-values gptel-agent--runs)
             (lambda (a b) (> (gptel-agent--run-started-at a)
                              (gptel-agent--run-started-at b))))))

(defun gptel-runner-delegation-test--discussion (fsm)
  "Spawn a discussion owned by FSM and return it."
  (gptel-runner-delegation-test--tool
   fsm "Discussion" '(:title "Investigation" :prompt "Inspect evidence"))
  (car (buffer-local-value 'gptel-agent-discussion--children
                           (plist-get (gptel-fsm-info fsm) :buffer))))

(ert-deftest gptel-runner-delegation-subagents-blocking-and-background ()
  (dolist (background '(nil t))
    (gptel-runner-delegation-test--with-run
      (let* ((child (gptel-runner-delegation-test--agent fsm background))
             (worker (gptel-agent--run-fsm child)))
        (should (gptel-agent--run-active-p child))
        (when background
          (gptel-runner-delegation-test--answer fsm "Provisional answer")
          (should (eq (gptel-fsm-state fsm) 'AGWT)))
        (should-not (gptel-runner--run-terminal-p pipeline))
        (gptel-runner-delegation-test--answer worker "Evidence")
        (when background (gptel-agent--workers-wake (gptel-agent--workers-for fsm)))
        (gptel-runner-delegation-test--answer fsm "Final synthesis")
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))
        (should (equal (gptel-runner-call-result call) "Final synthesis"))
        (should-not (plist-get (gptel-runner-call-driver-data call) :agent-supervision))))))

(ert-deftest gptel-runner-delegation-discussion-holds-node-until-idle ()
  (gptel-runner-delegation-test--with-run
    (let ((chat (gptel-runner-delegation-test--discussion fsm)))
      (gptel-runner-delegation-test--answer fsm "Final answer")
      (should (eq (gptel-fsm-state fsm) 'AGWT))
      (should-not (gptel-runner--run-terminal-p pipeline))
      (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Evidence")
      (gptel-runner-delegation-test--settle)
      (should (eq (gptel-runner-run-state pipeline) 'succeeded))
      (should (equal (gptel-runner-call-result call) "Final answer")))))

(ert-deftest gptel-runner-delegation-discussion-holds-explicit-handoff ()
  (let ((gptel-runner-delegation-test--completion 'handoff))
    (gptel-runner-delegation-test--with-run
      (let* ((chat (gptel-runner-delegation-test--discussion fsm))
             (child (gptel-agent-discussion--chat-fsm chat))
             (tool (cl-find "RunnerHandoff" (plist-get (gptel-fsm-info fsm) :tools)
                            :key #'gptel-tool-name :test #'equal)))
        (with-current-buffer (plist-get (gptel-fsm-info child) :buffer)
          (should (string-match-p "different workflow turn"
                                  (funcall (gptel-tool-function tool) "Child verdict"))))
        (gptel-runner-gptel-test--handoff fsm "Parent verdict")
        (should (eq (gptel-fsm-state fsm) 'AGWT))
        (should-not (gptel-runner--run-terminal-p pipeline))
        (gptel-runner-delegation-test--answer child "Evidence")
        (gptel-runner-delegation-test--settle)
        (should (eq 'succeeded (gptel-runner-run-state pipeline)))
        (should (equal (gptel-runner-call-result call) "Parent verdict"))))))

(ert-deftest gptel-runner-delegation-chats-belong-only-to-workflow ()
  (let ((gptel-agent-session-directory (make-temp-file "runner-agent-catalog-" t))
        (gptel-agent-session-auto-title nil))
    (unwind-protect
        (progn
          (gptel-agent-session-mode 1)
          (gptel-runner-delegation-test--with-run
            (let* ((ordinary (gptel (generate-new-buffer-name "*ordinary-test-chat*")))
                   (worker (gptel-runner-delegation-test--agent fsm t))
                   (chat (gptel-runner-delegation-test--discussion fsm))
                   (nested (gptel-runner-delegation-test--discussion (gptel-agent--run-fsm worker)))
                   (copy (gptel-agent-session-buffer-from-record
                          (gptel-agent-session-buffer-record parent) t))
                   (detached (gptel-runner-gptel-chat-from-text
                              (gptel-runner-gptel-chat-text parent)))
                   (owned (list parent (gptel-agent--run-child-buffer worker)
                                (gptel-agent-discussion--chat-buffer chat)
                                (gptel-agent-discussion--chat-buffer nested) copy detached)))
              ;; Cover the enable-time discovery scan as well as immediate hooks.
              (gptel-agent-session-mode -1)
              (gptel-agent-session-mode 1)
              (gptel-agent-session-save)
              (let ((catalog (gptel-agent-session--catalog)))
                (should (cl-find (buffer-name ordinary) catalog
                                 :key (lambda (r) (plist-get r :name)) :test #'equal))
                (dolist (buffer owned)
                  (with-current-buffer buffer
                    (should gptel-agent-session-managed-externally)
                    (should-not gptel-agent-session--id)
                    (should-not (gethash buffer gptel-agent-session--dirty))
                    (should-error (gptel-agent-session-save t) :type 'user-error)
                    (should-not (cl-find (buffer-name) catalog
                                         :key (lambda (r) (plist-get r :name))
                                         :test #'equal))))))))
      (gptel-agent-session-mode -1)
      (delete-directory gptel-agent-session-directory t))))

(ert-deftest gptel-runner-delegation-nested-discussions-hold-every-level ()
  (gptel-runner-delegation-test--with-run
    (let* ((chat (gptel-runner-delegation-test--discussion fsm))
           (child (gptel-agent-discussion--chat-fsm chat))
           (nested (gptel-runner-delegation-test--discussion child)))
      (gptel-runner-delegation-test--answer child "Child final")
      (gptel-runner-delegation-test--answer fsm "Parent final")
      (gptel-runner-delegation-test--settle)
      (should-not (gptel-runner--run-terminal-p pipeline))
      (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm nested) "Nested evidence")
      (gptel-runner-delegation-test--settle)
      (should (eq (gptel-runner-run-state pipeline) 'succeeded)))))

(ert-deftest gptel-runner-delegation-subagent-discussion-holds-node ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (chat (gptel-runner-delegation-test--discussion child)))
      (gptel-runner-delegation-test--answer child "Worker final")
      (gptel-runner-delegation-test--answer fsm "Provisional answer")
      (should (gptel-agent--run-active-p worker))
      (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Evidence")
      (gptel-runner-delegation-test--settle)
      (gptel-agent--workers-wake (gptel-agent--workers-for fsm))
      (gptel-runner-delegation-test--answer fsm "Final synthesis")
      (should (eq (gptel-runner-run-state pipeline) 'succeeded)))))

(ert-deftest gptel-runner-delegation-simultaneous-child-approvals ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (chat (gptel-runner-delegation-test--discussion fsm))
           (discussion (gptel-agent-discussion--chat-fsm chat)))
      (dolist (request (list child discussion))
        (with-current-buffer (plist-get (gptel-fsm-info request) :buffer)
          (setq-local gptel-confirm-tool-calls t))
        (gptel-runner-delegation-test--tool request "delegation_probe" nil))
      (should (= (length interactions) 2))
      (should-not executions)
      (should (equal interactions (read (prin1-to-string interactions))))
      ;; Activity in the parent must not conceal its children's approvals.
      (gptel-runner-delegation-test--tool fsm "AgentControl" '(:action "list"))
      (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (let ((gptel-runner-jobs--run pipeline)
            (gptel-runner-jobs--invocation '(:epoch 1)))
        (dolist (interaction interactions)
          (should (gptel-runner-jobs--interaction-current-p
                   (append interaction '(:epoch 1))))))
      (gptel-runner-gptel-answer-interaction call (plist-get (car interactions) :id) t)
      (should (= (length executions) 1))
      (should (eq (car executions) (gptel-agent-discussion--chat-buffer chat)))
      (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
      (gptel-runner-gptel-answer-interaction call (plist-get (cadr interactions) :id) t)
      (should (= (length executions) 2))
      (should (eq (car executions) (gptel-agent--run-child-buffer worker)))
      (should-error (gptel-runner-gptel-answer-interaction call (plist-get (car interactions) :id) t)
                    :type 'user-error))))

(ert-deftest gptel-runner-delegation-foreground-approval-targets-owning-child ()
  (gptel-runner-delegation-test--with-run
    (let* ((gptel-runner-headless nil)
           (worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (chat (gptel-runner-delegation-test--discussion fsm))
           (discussion (gptel-agent-discussion--chat-fsm chat)))
      (dolist (request (list child discussion))
        (with-current-buffer (plist-get (gptel-fsm-info request) :buffer)
          (setq-local gptel-confirm-tool-calls t))
        (gptel-runner-delegation-test--tool request "delegation_probe" nil))
      (let ((pending (gptel-runner-gptel-pending-interactions call)))
        (should (= (length pending) 2))
        (gptel-runner-gptel-answer-interaction call (plist-get (car pending) :id) t)
        (should (equal executions (list (gptel-agent--run-child-buffer worker))))
        (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
        (should (equal (gptel-runner-gptel-pending-interactions call) (cdr pending)))
        (gptel-runner-gptel-answer-interaction call (plist-get (cadr pending) :id) t)
        (should (eq (car executions) (gptel-agent-discussion--chat-buffer chat)))
        (should-not (gptel-runner-gptel-pending-interactions call))))))

(ert-deftest gptel-runner-delegation-batch-approves-both-child-conversations ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (chat (gptel-runner-delegation-test--discussion fsm))
           (discussion (gptel-agent-discussion--chat-fsm chat)))
      (dolist (request (list child discussion))
        (with-current-buffer (plist-get (gptel-fsm-info request) :buffer)
          (setq-local gptel-confirm-tool-calls t))
        (gptel-runner-delegation-test--tool request "delegation_probe" nil))
      (let ((ids (mapcar (lambda (item) (plist-get item :id)) interactions)))
        (gptel-runner-gptel-approve-interactions call ids)
        (should (= (length executions) 2))
        (should (memq (gptel-agent-discussion--chat-buffer chat) executions))
        (should (memq (gptel-agent--run-child-buffer worker) executions))
        (should-error (gptel-runner-gptel-approve-interactions call ids) :type 'user-error)))))

(ert-deftest gptel-runner-delegation-outside-grant-preserves-other-confirmations ()
  (let ((outside (make-temp-file "runner-outside-" nil ".txt" "Evidence")))
    (unwind-protect
        (gptel-runner-delegation-test--with-run
          (let* ((info (gptel-fsm-info fsm))
                 (args (list :file_path outside)))
            (with-current-buffer parent (setq-local gptel-confirm-tool-calls 'auto))
            (setf (plist-get info :tools)
                  (cons (gptel-get-tool "Read") (plist-get info :tools)))
            (gptel-runner-delegation-test--tool fsm "Read" args)
            (should (= (length interactions) 1))
            (should (plist-get (car interactions) :outside-access))
            (gptel-runner-gptel-approve-interactions
             call (list (plist-get (car interactions) :id)) t)
            (should (buffer-local-value 'gptel-agent-access--grant parent))
            ;; Future outside reads skip this specific guard, but explicit
            ;; confirmations for other tools still reach the dashboard.
            (setq interactions nil)
            (gptel-runner-delegation-test--tool fsm "Read" args)
            (should-not interactions)
            (gptel-runner-delegation-test--tool fsm "delegation_probe" nil)
            (should (= (length interactions) 1))
            (should-not (plist-get (car interactions) :outside-access))
            (gptel-runner-gptel-answer-interaction call (plist-get (car interactions) :id) t)
            (gptel-runner-gptel-revoke-outside-access call)
            (should-not (buffer-local-value 'gptel-agent-access--grant parent))
            (setq interactions nil)
            (gptel-runner-delegation-test--tool fsm "Read" args)
            (should (plist-get (car interactions) :outside-access))
            (gptel-runner-gptel-approve-interactions
             call (list (plist-get (car interactions) :id)) t)
            (gptel-runner-pause-call call)
            (should-not (buffer-local-value 'gptel-agent-access--grant parent))))
      (delete-file outside))))

(ert-deftest gptel-runner-delegation-outside-grant-belongs-only-to-requesting-child ()
  (let ((outside (make-temp-file "runner-child-outside-" nil ".txt" "Evidence")))
    (unwind-protect
        (gptel-runner-delegation-test--with-run
          (let* ((worker (gptel-runner-delegation-test--agent fsm t))
                 (child (gptel-agent--run-fsm worker))
                 (info (gptel-fsm-info child))
                 (owner (plist-get info :buffer)))
            (setf (plist-get info :tools)
                  (cons (gptel-get-tool "Read") (plist-get info :tools)))
            (gptel-runner-delegation-test--tool child "Read" (list :file_path outside))
            (should (plist-get (car interactions) :outside-access))
            (gptel-runner-gptel-approve-interactions
             call (list (plist-get (car interactions) :id)) t)
            (should (buffer-local-value 'gptel-agent-access--grant owner))
            (should-not (buffer-local-value 'gptel-agent-access--grant parent))
            (gptel-runner-gptel-revoke-outside-access call)
            (should-not (buffer-local-value 'gptel-agent-access--grant owner))))
      (delete-file outside))))

(ert-deftest gptel-runner-delegation-cancel-invalidates-children-and-approvals ()
  (dolist (action '(cancel pause reject))
    (gptel-runner-delegation-test--with-run
      (let* ((chat (gptel-runner-delegation-test--discussion fsm))
             (child (gptel-agent-discussion--chat-fsm chat)))
        (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
          (setq-local gptel-confirm-tool-calls t))
        (gptel-runner-delegation-test--tool child "delegation_probe" nil)
        (let ((id (plist-get (car interactions) :id)))
          (pcase action
            ('cancel (gptel-runner-abort-call call))
            ('pause (gptel-runner-pause-call call))
            ('reject (gptel-runner-gptel-answer-interaction call id nil)))
          (should-not (gptel-agent-discussion--active-p child))
          (should-not (plist-get (gptel-runner-call-driver-data call) :agent-supervision))
          (should-error (gptel-runner-gptel-answer-interaction call id t) :type 'user-error)
          (should-not executions))))))

(ert-deftest gptel-runner-delegation-retry-reopens-completion-gate ()
  (gptel-runner-delegation-test--with-run
    (let ((info (gptel-fsm-info fsm)))
      (setf (plist-get info :http-status) "503"
            (plist-get info :error) "Unavailable")
      (gptel--fsm-transition fsm 'ERRS)
      (should (eq (gptel-runner-call-state call) 'retry-wait))
      (gptel-runner-gptel-test--retry call)
      (setf (plist-get info :error) nil)
      (gptel-runner-delegation-test--answer fsm "Recovered")
      (should (eq (gptel-runner-run-state pipeline) 'succeeded)))))

(ert-deftest gptel-runner-delegation-report-resumes-parked-parent ()
  (gptel-runner-delegation-test--with-run
    (let* ((chat (gptel-runner-delegation-test--discussion fsm))
           (child (gptel-agent-discussion--chat-fsm chat)))
      (gptel-runner-delegation-test--answer fsm "Provisional answer")
      (gptel-runner-delegation-test--tool child "DiscussionReport" '(:message "New evidence"))
      (gptel-runner-delegation-test--settle)
      (should (eq (gptel-fsm-state fsm) 'WAIT))
      (should (string-match-p "New evidence" (prin1-to-string (plist-get (gptel-fsm-info fsm) :data))))
      (gptel-runner-delegation-test--answer child "Child done")
      (gptel-runner-delegation-test--answer fsm "Revised synthesis")
      (should (equal (gptel-runner-call-result call) "Revised synthesis")))))

(ert-deftest gptel-runner-delegation-report-to-subagent ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (chat (gptel-runner-delegation-test--discussion child))
           (discussion (gptel-agent-discussion--chat-fsm chat)))
      (gptel-runner-delegation-test--answer child "Provisional worker answer")
      (gptel-runner-delegation-test--tool discussion "DiscussionReport" '(:message "New evidence"))
      (gptel-runner-delegation-test--settle)
      (should (eq (gptel-fsm-state child) 'WAIT))
      (gptel-runner-delegation-test--answer discussion "Done")
      (gptel-runner-delegation-test--answer child "Revised worker answer")
      (ert-info ((gptel-agent--run-result worker))
        (should (eq (gptel-agent--run-state worker) 'completed)))
      (should (string-match-p "Revised worker answer" (gptel-agent--run-result worker)))
      (should-not (string-match-p "Provisional worker answer" (gptel-agent--run-result worker))))))

(ert-deftest gptel-runner-delegation-report-after-worker-approval ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker))
           (report (concat "Reported evidence " (make-string 3000 ?x))))
      (with-current-buffer (gptel-agent--run-child-buffer worker)
        (setq-local gptel-confirm-tool-calls t))
      (gptel-runner-delegation-test--tool child "delegation_probe" nil)
      (gptel-runner-gptel-answer-interaction call (plist-get (car interactions) :id) t)
      (with-current-buffer (gptel-agent--run-child-buffer worker)
        (setq-local gptel-confirm-tool-calls nil))
      (let* ((chat (gptel-runner-delegation-test--discussion child))
             (discussion (gptel-agent-discussion--chat-fsm chat)))
        (gptel-runner-delegation-test--answer child "Provisional worker answer")
        (gptel-runner-delegation-test--tool discussion "DiscussionReport" (list :message report))
        (gptel-runner-delegation-test--settle)
        (should (eq (gptel-fsm-state child) 'WAIT))
        (should (string-match-p "Reported evidence" (prin1-to-string (plist-get (gptel-fsm-info child) :data))))
        (gptel-runner-delegation-test--answer discussion "Done")
        (gptel-runner-delegation-test--answer child "Final worker answer")
        (should (eq (gptel-agent--run-state worker) 'completed))
        (should-not (string-match-p "Reported evidence" (gptel-agent--run-result worker)))))))

(ert-deftest gptel-runner-delegation-streamed-final-answer-waits-for-child ()
  (let ((gptel-runner-delegation-test--stream t))
    (gptel-runner-delegation-test--with-run
      (let ((chat (gptel-runner-delegation-test--discussion fsm)))
        (setf (plist-get (gptel-fsm-info fsm) :http-status) "200")
        (gptel--fsm-transition fsm)
        (gptel-runner-gptel-test--stream-chunk fsm '(:content "Streamed final") "stop")
        (funcall (plist-get (gptel-fsm-info fsm) :callback) t (gptel-fsm-info fsm))
        (gptel--fsm-transition fsm)
        (should (eq (gptel-fsm-state fsm) 'AGWT))
        (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Child done")
        (gptel-runner-delegation-test--settle)
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))
        (should (equal (gptel-runner-call-result call) "Streamed final"))))))

(ert-deftest gptel-runner-delegation-queued-followup-keeps-node-open ()
  (gptel-runner-delegation-test--with-run
    (let* ((chat (gptel-runner-delegation-test--discussion fsm))
           (child (gptel-agent-discussion--chat-fsm chat)))
      (gptel-runner-delegation-test--answer child "First answer")
      (gptel-runner-delegation-test--tool
       fsm "DiscussionControl"
       (list :action "send" :discussion_id (gptel-agent-discussion--chat-id chat)
             :message "Follow up"))
      (gptel-runner-delegation-test--answer fsm "Final answer")
      (gptel-runner-delegation-test--settle)
      (should-not (gptel-runner--run-terminal-p pipeline))
      (gptel-agent-discussion--flush (gptel-agent-discussion--chat-buffer chat))
      (should-not (eq child (gptel-agent-discussion--chat-fsm chat)))
      (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Followup answer")
      (gptel-runner-delegation-test--settle)
      (should (eq (gptel-runner-run-state pipeline) 'succeeded)))))

(ert-deftest gptel-runner-delegation-real-timer-releases-finished-discussion ()
  (gptel-runner-delegation-test--with-run
    (let ((chat (gptel-runner-delegation-test--discussion fsm)))
      (gptel-runner-delegation-test--answer fsm "Final answer")
      (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Evidence")
      (let ((deadline (+ (float-time) 2)))
        (while (and (not (gptel-runner--run-terminal-p pipeline)) (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (eq (gptel-runner-run-state pipeline) 'succeeded)))))

(ert-deftest gptel-runner-delegation-closed-child-invalidates-approval ()
  (gptel-runner-delegation-test--with-run
    (let* ((chat (gptel-runner-delegation-test--discussion fsm))
           (child (gptel-agent-discussion--chat-fsm chat)))
      (with-current-buffer (gptel-agent-discussion--chat-buffer chat)
        (setq-local gptel-confirm-tool-calls t))
      (gptel-runner-delegation-test--tool child "delegation_probe" nil)
      (let ((id (plist-get (car interactions) :id)))
        (gptel-agent-discussion--close chat)
        (gptel-runner-delegation-test--settle)
        (should-not (gptel-runner-gptel-interaction-pending-p call id))
        (should-error (gptel-runner-gptel-answer-interaction call id t) :type 'user-error)
        (should-not (eq (gptel-runner-call-state call) 'waiting-confirmation))
        (gptel-runner-delegation-test--answer fsm "Final answer")
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))
        (should-not executions)))))

(ert-deftest gptel-runner-delegation-cancelled-worker-invalidates-approval ()
  (gptel-runner-delegation-test--with-run
    (let* ((worker (gptel-runner-delegation-test--agent fsm t))
           (child (gptel-agent--run-fsm worker)))
      (with-current-buffer (gptel-agent--run-child-buffer worker)
        (setq-local gptel-confirm-tool-calls t))
      (gptel-runner-delegation-test--tool child "delegation_probe" nil)
      (let ((id (plist-get (car interactions) :id)))
        (gptel-runner-delegation-test--tool
         fsm "AgentControl"
         (list :action "cancel"
               :agent_id (gptel-agent--session-id (gptel-agent--run-session worker))))
        (should-not (gptel-runner-gptel-interaction-pending-p call id))
        (should-error (gptel-runner-gptel-answer-interaction call id t) :type 'user-error)
        (should-not executions)
        (gptel-runner-delegation-test--answer fsm "Synthesis after cancellation")
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))))))

(ert-deftest gptel-runner-delegation-feedback-uses-fresh-supervision ()
  (gptel-runner-delegation-test--with-run
    (let ((old (plist-get (gptel-runner-call-driver-data call) :agent-supervision)))
      (gptel-runner-pause-call call)
      (gptel-runner-gptel-feedback call "Reconsider with a discussion")
      (setq fsm (gptel-runner-call-fsm call))
      (should-not (eq old (plist-get (gptel-runner-call-driver-data call) :agent-supervision)))
      (let ((chat (gptel-runner-delegation-test--discussion fsm)))
        (gptel-runner-delegation-test--answer fsm "Feedback answer")
        (should (eq (gptel-fsm-state fsm) 'AGWT))
        (gptel-runner-delegation-test--answer (gptel-agent-discussion--chat-fsm chat) "Done")
        (gptel-runner-delegation-test--settle)
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should-not (plist-get (gptel-runner-call-driver-data call) :agent-supervision))
        (gptel-runner-complete-call-from-buffer "Feedback answer" call)
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))))))

(ert-deftest gptel-runner-delegation-background-job-publishes-child-approvals ()
  (require 'gptel-runner-jobs-test)
  (gptel-runner-jobs-test--isolated
    (let* ((init (expand-file-name "init.el" gptel-runner-job-directory))
           (evidence (expand-file-name "executions.txt" gptel-runner-job-directory))
           (gptel-runner-job-preload-features '(gptel gptel-agent)))
      (with-temp-file init
        (insert ";;; -*- lexical-binding: t; -*-\n")
        (prin1
         `(progn
            (require 'gptel-runner-delegation-test)
            (setq gptel-default-mode 'text-mode
                  gptel-backend (gptel-make-openai "delegation-job" :host "example.invalid" :models '(test))
                  gptel-model 'test gptel-stream nil gptel-use-curl t
                  gptel-confirm-tool-calls nil gptel-use-tools t
                  gptel-agent-use-project-instructions nil
                  gptel-prompt-transform-functions '(gptel-agent--localize-agent-tool gptel-agent-discussion--localize)
                  gptel-tools
                  (append (mapcar #'gptel-get-tool '("Agent" "Discussion" "DiscussionControl"))
                          (list (gptel-make-tool
                                 :name "delegation_probe" :description "Record worker execution" :args nil :include t :confirm t
                                 :function (lambda ()
                                             (write-region (concat (buffer-name) "\n") nil ,evidence t 'silent)
                                             "Verified")))))
            (setq-default gptel-agent--agent-snapshot
                          '(("worker" :system "Inspect" :task-timeout nil :max-request-rounds nil)))
            (let ((rounds (make-hash-table :test #'eq)))
              (fset 'gptel-curl-get-response
                    (lambda (request)
                      (run-at-time
                       0 nil
                       (lambda ()
                         (let* ((round (gethash request rounds 0))
                                (root (gptel-runner-call-p (plist-get (gptel-fsm-info request) :context))))
                           (puthash request (1+ round) rounds)
                           (cond
                            ((and root (= round 0))
                             (gptel-runner-delegation-test--tool
                              request "Agent" '(:subagent_type "worker" :description "Inspect" :prompt "Inspect" :background t)))
                            ((and root (= round 1))
                             (gptel-runner-delegation-test--tool request "Discussion" '(:title "Job discussion" :prompt "Inspect")))
                            ((and (not root) (= round 0))
                             (with-current-buffer (plist-get (gptel-fsm-info request) :buffer)
                               (setq-local gptel-confirm-tool-calls t))
                             (gptel-runner-delegation-test--tool request "delegation_probe" nil))
                            (t (gptel-runner-delegation-test--answer request "Final evidence")))))))))
            (gptel-runner-register-agent 'job-coordinator :preset nil)
            (gptel-runner-defworkflow delegation-job (:completion response)
              (gptel-runner-agent-step :id 'coordinate :agent 'job-coordinator :prompt "Inspect" :save-as 'result)))
         (current-buffer)))
      (let ((invocation (gptel-runner-job-run
                         (gptel-runner-jobs-test--job 'delegation-job "Delegation" init))))
        (setq invocation
              (gptel-runner-jobs-test--wait
               invocation (lambda (record) (= 2 (length (plist-get record :interactions))))))
        (should (eq (plist-get invocation :phase) 'open))
        (should-not (file-exists-p evidence))
        (dolist (interaction (plist-get invocation :interactions))
          (gptel-runner-jobs-test--command
           invocation 'approve
           (list :call-id (plist-get interaction :call-id) :call-epoch 1
                 :generation (plist-get interaction :generation)
                 :interaction-id (plist-get interaction :id))))
        (setq invocation (gptel-runner-jobs-test--finished invocation))
        (should (eq (plist-get invocation :state) 'succeeded))
        (should (equal (alist-get 'result (gptel-runner-invocation-outputs invocation)) "Final evidence"))
        (with-temp-buffer
          (insert-file-contents evidence)
          (should (= (length (split-string (buffer-string) "\n" t)) 2)))))))

(ert-deftest gptel-runner-delegation-child-transcripts-survive-restart ()
  (let ((gptel-runner-snapshot-directory (make-temp-file "runner-child-chats-" t))
        (gptel-runner--workflows (make-hash-table :test #'eq)))
    (unwind-protect
        (gptel-runner-delegation-test--with-run
          (setf (gptel-runner-run-workflow pipeline)
                (gptel-runner-defworkflow saved-children ()
                  (gptel-runner-workflow-root (gptel-runner-run-workflow pipeline))))
          (require 'gptel-agent-review-aside)
          (let* ((worker (gptel-runner-delegation-test--agent fsm))
                 (worker-fsm (gptel-agent--run-fsm worker))
                 (worker-buffer (gptel-agent--run-child-buffer worker))
                 (worker-id (buffer-local-value 'gptel-runner-store--child-id worker-buffer))
                 (chat (gptel-runner-delegation-test--discussion worker-fsm))
                 (chat-buffer (gptel-agent-discussion--chat-buffer chat))
                 (chat-id (buffer-local-value 'gptel-runner-store--child-id chat-buffer))
                 (aside (gptel-agent-review-aside--create
                         (gptel-agent-review--make-queue :owner parent)
                         (list :path (expand-file-name "sample.el" default-directory))))
                 (aside-id (buffer-local-value 'gptel-runner-store--child-id aside))
                 (children (gptel-runner-run-child-chats pipeline))
                 (file (expand-file-name "run.snapshot" gptel-runner-snapshot-directory)))
            (should (= (hash-table-count children) 3))
            (should (equal (plist-get (gethash chat-id children) :parent-id) worker-id))
            (with-current-buffer aside
              (should (bound-and-true-p gptel-agent-session-managed-externally))
              (insert "A review question"))
            (with-current-buffer chat-buffer
              (should (eq (car (gptel-runner-ui--run-context)) pipeline)))
            (gptel-runner-delegation-test--answer
             (gptel-agent-discussion--chat-fsm chat) "The nested finding")
            (gptel-runner-delegation-test--settle)
            (gptel-runner-delegation-test--answer worker-fsm "The worker finding")
            (gptel-runner-delegation-test--settle)
            (gptel-runner-delegation-test--answer fsm "Final synthesis")
            (gptel-runner-delegation-test--settle)
            (should (eq (gptel-runner-run-state pipeline) 'succeeded))
            (gptel-runner-save-run pipeline file)
            (gptel-runner-forget-run pipeline)
            (should-not (buffer-live-p chat-buffer))
            (should-not (buffer-live-p worker-buffer))
            (let* ((request-count (length requests))
                   (loaded (gptel-runner-load-run file))
                   (restored (gptel-runner-run-child-chats loaded)))
              (unwind-protect
                  (progn
                    (should (= 3 (hash-table-count restored)))
                    (should (equal (plist-get (gethash chat-id restored) :call)
                                   (gptel-runner-call-id call)))
                    (should-not (bufferp (plist-get (gethash chat-id restored) :chat)))
                    (let* ((gptel-runner-jobs--invocation
                            (list :id "child-test" :epoch 2 :directory gptel-runner-snapshot-directory))
                           (gptel-runner-jobs--exports (make-hash-table :test #'equal))
                           (gptel-runner-jobs--run loaded))
                      (gptel-runner-jobs--publish)
                      (let ((published (plist-get (car (plist-get gptel-runner-jobs--invocation :calls))
                                                  :children)))
                        (should (equal '(gptel-agent transcript transcript)
                                       (sort (mapcar (lambda (r) (plist-get r :chat-format)) published)
                                             #'string-lessp)))
                        (dolist (record published)
                          (should (file-readable-p
                                   (expand-file-name (plist-get record :chat)
                                                     gptel-runner-snapshot-directory))))))
                    (with-current-buffer (gptel-runner-child-buffer loaded worker-id)
                      (should buffer-read-only)
                      (should (string-search "The worker finding" (buffer-string))))
                    (with-current-buffer (gptel-runner-child-buffer loaded aside-id)
                      (should buffer-read-only)
                      (should-not (bound-and-true-p gptel-agent-review-aside-mode))
                      (should-not (bound-and-true-p gptel-mode))
                      (should (string-search "A review question" (buffer-string))))
                    (with-current-buffer (gptel-runner-child-buffer loaded chat-id)
                      (should (bound-and-true-p gptel-mode))
                      (should (bound-and-true-p gptel-agent-session-managed-externally))
                      (should (string-search "The nested finding" (buffer-string))))
                    (should (= request-count (length requests))))
                (gptel-runner-forget-run loaded)))))
      (delete-directory gptel-runner-snapshot-directory t))))

(provide 'gptel-runner-delegation-test)
;;; gptel-runner-delegation-test.el ends here
