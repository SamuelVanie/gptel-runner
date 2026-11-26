;;; gptel-runner-instructions-integration.el --- Managed context in node chats -*- lexical-binding: t; -*-

(require 'gptel-runner-session-test)

(defvar gptel-agent-use-project-instructions)

(defun gptel-runner-instructions-test--messages (fsm role)
  "Return the concatenated contents of FSM's outbound messages with ROLE."
  (should fsm)
  (let ((messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
    (mapconcat (lambda (message) (plist-get message :content))
               (seq-filter (lambda (message) (equal role (plist-get message :role)))
                           messages)
               "\n")))

(ert-deftest gptel-runner-instructions-refresh-on-retry-and-restored-feedback ()
  (skip-unless (and (require 'gptel-agent-instructions nil t)
                    (default-boundp 'gptel-agent-instructions-context-paths)))
  (gptel-runner-session-test--with-run
    (let* ((workspace (expand-file-name "project/" gptel-runner-snapshot-directory))
           (nested (expand-file-name "nested/" workspace))
           (rules (expand-file-name "AGENTS.md" nested))
           (gptel-agent-use-project-instructions t)
           (gptel-use-context nil))
      (make-directory nested t)
      (with-temp-file (expand-file-name "AGENTS.md" workspace) (insert "ROOT-RULE-V1"))
      (with-temp-file rules (insert "NESTED-RULE-V1"))
      (with-temp-file (expand-file-name "notes.txt" nested) (insert "PROJECT-EVIDENCE"))
      (gptel-runner-defworkflow scoped-context (:persist t :context ((:file "AGENTS.md")))
        (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect the code"
                                 :context '((:file "nested/notes.txt"))))
      (let* ((run (gptel-runner-start 'scoped-context :workspace workspace))
             (file (gptel-runner-run-snapshot-file run))
             (call (car (gptel-runner-run-calls run)))
             (first (pop requests))
             (system (gptel-runner-instructions-test--messages first "system"))
             (user (gptel-runner-instructions-test--messages first "user")))
        (with-temp-buffer
          (insert system)
          (should (= 1 (how-many "ROOT-RULE-V1" (point-min) (point-max))))
          (should (= 1 (how-many "NESTED-RULE-V1" (point-min) (point-max)))))
        (should (string-search "PROJECT-EVIDENCE" user))
        (should-not (string-search "-RULE-" user))
        ;; A malformed tool call retries the same request, after a disk edit.
        (gptel-runner-gptel-test--reply first "Inspecting" "tool_calls")
        (with-temp-file rules (insert "NESTED-RULE-V2"))
        (gptel-runner-gptel-test--retry call)
        (let* ((retry (pop requests))
               (system (gptel-runner-instructions-test--messages retry "system")))
          (should (string-search "NESTED-RULE-V2" system))
          (should-not (string-search "NESTED-RULE-V1" system))
          (gptel-runner-gptel-test--reply retry "Which detail?" "stop"))
        (gptel-runner-pause-run run)
        (gptel-runner-forget-run run)
        (with-temp-file rules (insert "NESTED-RULE-V3"))
        (setq run (gptel-runner-load-run file))
        (with-current-buffer (gptel-runner-node-buffer run 'read)
          (should (string-search "PROJECT-EVIDENCE" (buffer-string)))
          (should-not (string-search "-RULE-" (buffer-string)))
          (text-mode)
          (gptel-mode 1)
          (gptel-runner-gptel-feedback gptel-runner--call "Check the index"))
        (let* ((restored (pop requests))
               (system (gptel-runner-instructions-test--messages restored "system"))
               (user (gptel-runner-instructions-test--messages restored "user")))
          (should (string-search "ROOT-RULE-V1" system))
          (should (string-search "NESTED-RULE-V3" system))
          (should-not (string-search "NESTED-RULE-V2" system))
          (should (string-search "Check the index" user))
          (should-not (string-search "-RULE-" user))
          (gptel-runner-gptel-test--handoff restored "Index checked"))))))

(ert-deftest gptel-runner-instructions-disabled-skips-even-explicit-rules ()
  (skip-unless (and (require 'gptel-agent-instructions nil t)
                    (default-boundp 'gptel-agent-instructions-context-paths)))
  (gptel-runner-session-test--with-run
    (let ((gptel-agent-use-project-instructions nil))
      (with-temp-file (expand-file-name "AGENTS.md" gptel-runner-snapshot-directory)
        (insert "DISABLED-RULE"))
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                            :context '("Keep this" (:file "AGENTS.md")))
                   :workspace gptel-runner-snapshot-directory))
             (first (pop requests)))
        (should (string-search "Keep this"
                               (gptel-runner-instructions-test--messages first "user")))
        (should-not (string-search
                     "DISABLED-RULE" (prin1-to-string (plist-get (gptel-fsm-info first) :data))))
        (with-current-buffer (gptel-runner-node-buffer run 'read)
          (should-not (string-search "DISABLED-RULE" (buffer-string))))))))

(ert-deftest gptel-runner-instructions-plain-driver-preserves-explicit-file-history ()
  (gptel-runner-session-test--with-run
    (let ((gptel-agent-use-project-instructions nil))
      (with-temp-file (expand-file-name "AGENTS.md" gptel-runner-snapshot-directory)
        (insert "LEGACY-FILE-TEXT"))
      (gptel-runner-defworkflow legacy-context (:persist t :completion response)
        (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                 :context '((:file "AGENTS.md"))))
      ;; Emulate a plain runner even if another ERT already loaded the agent.
      (let* ((run (cl-progv '(gptel-agent-instructions-context-paths) '(nil)
                    (makunbound 'gptel-agent-instructions-context-paths)
                    (gptel-runner-start 'legacy-context
                                        :workspace gptel-runner-snapshot-directory)))
             (file (gptel-runner-run-snapshot-file run))
             (first (pop requests)))
        (should (string-search "LEGACY-FILE-TEXT"
                               (gptel-runner-instructions-test--messages first "user")))
        (gptel-runner-gptel-test--reply first "Reviewed" "stop")
        (gptel-runner-forget-run run)
        (delete-file (expand-file-name "AGENTS.md" gptel-runner-snapshot-directory))
        (setq run (gptel-runner-load-run file))
        (with-current-buffer (gptel-runner-node-buffer run 'read)
          (should (string-search "LEGACY-FILE-TEXT" (buffer-string))))))))

(defun gptel-runner-instructions-test--published-follow-up (format &optional opt-out)
  "Send from a standalone background chat in FORMAT, honoring local OPT-OUT."
  (gptel-runner-session-test--with-run
    (let* ((workspace (expand-file-name "project/" gptel-runner-snapshot-directory))
           (nested (expand-file-name "nested/" workspace))
           (rules (expand-file-name "AGENTS.md" nested))
           (gptel-agent-use-project-instructions t)
           (gptel-use-context nil)
           opened)
      (make-directory nested t)
      (with-temp-file (expand-file-name "AGENTS.md" workspace) (insert "PROJECT-RULE"))
      (with-temp-file rules (insert "ARCHIVED-RULE"))
      (with-temp-file (expand-file-name "notes.txt" nested) (insert "NOTE-CONTENT"))
      (let* ((gptel-runner-jobs--run
              (gptel-runner-start
               (gptel-runner-agent-step
                :id 'read :agent 'reader :prompt "Review notes" :completion 'response
                :context (append '((:file "nested/notes.txt"))
                                 (when (eq format 'gptel-agent)
                                   '((:file "nested/AGENTS.md")))))
               :workspace workspace))
             (gptel-runner-jobs--invocation
              (list :id "scope-copy" :epoch 1 :directory gptel-runner-snapshot-directory))
             (gptel-runner-jobs--exports (make-hash-table :test #'equal))
             (first (pop requests)))
        (should (string-search "ARCHIVED-RULE"
                               (gptel-runner-instructions-test--messages first "system")))
        (gptel-runner-gptel-test--reply first "Reviewed" "stop")
        (when opt-out
          (with-current-buffer (gptel-runner-node-buffer gptel-runner-jobs--run 'read)
            (setq-local gptel-agent-use-project-instructions nil)))
        (cl-letf (((symbol-function 'gptel-runner-store--agent-sessions-p)
                   (lambda () (eq format 'gptel-agent))))
          (gptel-runner-jobs--publish))
        (let* ((manifest (gptel-runner-jobs--read
                          (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)))
               (call (car (plist-get manifest :calls)))
               (target (list 'background-call "scope-copy" 1 (plist-get call :id)))
               (file (gptel-runner-jobs-ui--artifact-path manifest (plist-get call :chat))))
          (should (eq format (plist-get call :chat-format)))
          (with-temp-buffer
            (insert-file-contents file)
            (should-not (string-search "ARCHIVED-RULE" (buffer-string))))
          (with-temp-file rules (insert "CURRENT-RULE"))
          ;; The UI opens a worker's published files in a different Emacs.
          (gptel-runner-forget-run gptel-runner-jobs--run)
          (setq gptel-runner-jobs--run nil gptel-runner-jobs--invocation nil)
          (unwind-protect
              (save-window-excursion
                (gptel-runner-background-transcript (list manifest call target))
                (setq opened (current-buffer))
                (should-not (gptel-runner-store--chat-owner))
                (goto-char (point-max))
                (insert "Follow up from this copy")
                (gptel-send)
                (let* ((follow-up (pop requests))
                       (system (gptel-runner-instructions-test--messages follow-up "system"))
                       (user (gptel-runner-instructions-test--messages follow-up "user")))
                  (if opt-out
                      (should-not (string-search "-RULE" system))
                    (should (string-search "CURRENT-RULE" system))
                    (should (string-search "PROJECT-RULE" system)))
                  (should-not (string-search "ARCHIVED-RULE" system))
                  (should (string-search "NOTE-CONTENT" user))
                  (should (string-search "Follow up from this copy" user))
                  (should-not (string-search "-RULE" user))))
            (when (buffer-live-p opened) (kill-buffer opened))))))))

(ert-deftest gptel-runner-instructions-published-agent-copy-uses-current-scopes ()
  (skip-unless (and (require 'gptel-agent nil t)
                    (default-boundp 'gptel-agent-instructions-context-paths)
                    (gptel-runner-store--agent-sessions-p)))
  (gptel-runner-instructions-test--published-follow-up 'gptel-agent))

(ert-deftest gptel-runner-instructions-published-plain-copy-uses-current-scopes ()
  (skip-unless (and (require 'gptel-agent-instructions nil t)
                    (default-boundp 'gptel-agent-instructions-context-paths)))
  (gptel-runner-instructions-test--published-follow-up 'gptel))

(ert-deftest gptel-runner-instructions-published-plain-copy-preserves-opt-out ()
  (skip-unless (and (require 'gptel-agent-instructions nil t)
                    (default-boundp 'gptel-agent-instructions-context-paths)))
  (gptel-runner-instructions-test--published-follow-up 'gptel t))

(provide 'gptel-runner-instructions-integration)
;;; gptel-runner-instructions-integration.el ends here
