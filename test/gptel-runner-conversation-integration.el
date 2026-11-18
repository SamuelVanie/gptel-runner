;;; gptel-runner-conversation-integration.el --- Pipeline chats -*- lexical-binding: t; -*-

(require 'gptel-runner-gptel-test)

(ert-deftest gptel-runner-conversation-nested-follow-ups ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'editor)
    (let* ((gptel-default-mode 'text-mode)
           (checks 0)
           (accepted 0)
           (run (gptel-runner-start
                 (gptel-runner-sequence
                  :id 'pipeline
                  (gptel-runner-agent-step
                   :id 'research :agent 'editor :prompt "Private research assignment"
                   :parser (lambda (text) (cl-incf accepted) (list :report text))
                   :save-as 'findings)
                  (gptel-runner-elisp-step
                   :id 'check :function (lambda (_run _node) (cl-incf checks)))
                  (gptel-runner-agent-step
                   :id 'plan :agent 'editor :prompt "Private planning assignment"
                   :save-as 'plan)
                  (gptel-runner-agent-step
                   :id 'implement :agent 'editor :prompt "Private implementation assignment"
                   :save-as 'report))
                 :driver (gptel-runner-gptel-driver-create)
                 :max-concurrency 1 :max-calls 7 :max-requests 7))
           (buffers (make-hash-table :test #'eq)))
      (cl-labels
          ((next-turn (node expected absent)
             (let* ((fsm (pop requests))
                    (info (gptel-fsm-info fsm))
                    (buffer (plist-get info :buffer))
                    (messages (prin1-to-string (plist-get (plist-get info :data)
                                                         :messages))))
               (should (eq buffer (gptel-runner-node-buffer run node)))
               (should (buffer-local-value 'gptel-mode buffer))
               (should (eq buffer (gethash node buffers buffer)))
               (puthash node buffer buffers)
               (dolist (text expected) (should (string-search text messages)))
               (dolist (text absent) (should-not (string-search text messages)))
               fsm))
           (ask (fsm question)
             (should (member "RunnerAskPrevious"
                             (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
             (gptel-runner-gptel-test--reply
              fsm nil "tool_calls"
              (vector (list :id "ask" :type "function"
                            :function (list :name "RunnerAskPrevious"
                                            :arguments (gptel--json-encode
                                                        (list :question question))))))))
        (gptel-runner-gptel-test--handoff
         (next-turn 'research '("Private research assignment") nil)
         "The index is in db.el." "Working notes for the researcher")
        (gptel-runner-gptel-test--handoff
         (next-turn 'plan '("The index is in db.el." "Private planning assignment")
                    '("Working notes for the researcher" "Private research assignment"))
         "Update index lookup.")
        (ask (next-turn 'implement '("Update index lookup.") '("Private planning assignment"))
             "How should lookup handle missing keys?")
        (ask (next-turn 'plan '("How should lookup handle missing keys?" "The index is in db.el.")
                        '("Private implementation assignment"))
             "What does a missing key return?")
        (gptel-runner-gptel-test--handoff
         (next-turn 'research '("What does a missing key return?" "Private research assignment")
                    '("Private planning assignment"))
         "It returns nil.")
        (gptel-runner-gptel-test--handoff
         (next-turn 'plan '("It returns nil." "How should lookup handle missing keys?")
                    '("Private research assignment"))
         "Preserve nil for missing keys.")
        (gptel-runner-gptel-test--handoff
         (next-turn 'implement '("Preserve nil for missing keys." "Update index lookup.")
                    '("What does a missing key return?"))
         "Lookup updated and tested.")
        (should-not requests)
        (should (= checks 1))
        (should (= accepted 1))
        (should (= 3 (length (delete-dups (hash-table-values buffers)))))
        (should (equal (gptel-runner-get run 'findings) '(:report "The index is in db.el.")))
        (should (equal (gptel-runner-get run 'plan) "Update index lookup."))
        (should (equal (gptel-runner-get run 'report) "Lookup updated and tested."))
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-conversation-rerun-uses-the-same-chat ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'editor)
    (let* ((gptel-default-mode 'text-mode)
           (run (gptel-runner-start
                 (gptel-runner-agent-step :id 'edit :agent 'editor
                                          :prompt "Review the files" :save-as 'report)
                 :goal "Review migration" :max-calls 1 :max-requests 1
                 :driver (gptel-runner-gptel-driver-create)))
           (buffer (gptel-runner-node-buffer run 'edit)))
      (should-error (gptel-runner-rerun run) :type 'user-error)
      (gptel-runner-gptel-test--handoff (pop requests) "First review complete")
      (gptel-runner-rerun (gptel-runner-run-id run))
      (let* ((fsm (pop requests))
             (info (gptel-fsm-info fsm)))
        (should (eq (plist-get info :buffer) buffer))
        (should (string-search "First review complete" (prin1-to-string (plist-get info :data))))
        (should (equal "Review migration" (gptel-runner-run-goal run)))
        (should-not (gptel-runner-get run 'report))
        (gptel-runner-gptel-test--handoff fsm "Second review complete"))
      (should (eq 'succeeded (gptel-runner-run-state run)))
      (should (equal "Second review complete" (gptel-runner-get run 'report))))))

(provide 'gptel-runner-conversation-integration)
;;; gptel-runner-conversation-integration.el ends here
