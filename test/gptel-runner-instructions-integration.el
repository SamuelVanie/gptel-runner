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

(provide 'gptel-runner-instructions-integration)
;;; gptel-runner-instructions-integration.el ends here
