;;; gptel-runner-session-test.el --- Native node chat integration -*- lexical-binding: t; -*-

(require 'gptel-runner-gptel-test)
(require 'gptel-runner-conversation-integration)
(require 'gptel-runner-jobs-ui)

(defmacro gptel-runner-session-test--with-run (&rest body)
  "Run BODY with an isolated checkpoint directory and local test transport."
  (declare (indent 0))
  `(let ((gptel-runner-snapshot-directory (make-temp-file "runner-sessions-" t))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-default-mode 'text-mode))
     (unwind-protect
         (gptel-runner-gptel-test--with-transport
           (gptel-runner-register-agent 'reader :preset nil)
           ,@body)
       (maphash (lambda (run _) (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(ert-deftest gptel-runner-session-repeat-keeps-node-chat-and-unsent-draft ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-repeat-until
                  :id 'loop :max 2
                  :until (lambda (r) (equal "Done" (gptel-runner-get r 'answer)))
                  :body (gptel-runner-agent-step
                         :id 'check :agent 'reader :save-as 'answer
                         :prompt (lambda (r _node)
                                   (if (gptel-runner-get r 'answer)
                                       "Recheck the result" "Check the result"))))
                 :driver (gptel-runner-gptel-driver-create)))
           (chat (gptel-runner-node-buffer run 'check))
           (first (pop requests)))
      (should (buffer-local-value 'gptel-mode chat))
      (should-not (gptel-runner-node-buffer run 'loop))
      (with-current-buffer chat
        (goto-char (point-max))
        (insert "PRIVATE-UNSENT-DRAFT"))
      (gptel-runner-gptel-test--handoff first "First finding")
      (let* ((second (pop requests))
             (messages (plist-get (plist-get (gptel-fsm-info second) :data) :messages)))
        (should (eq chat (plist-get (gptel-fsm-info second) :buffer)))
        (should (string-search "First finding" (prin1-to-string messages)))
        (should (string-match-p "Recheck the result" (prin1-to-string messages)))
        (should-not (string-match-p "PRIVATE-UNSENT-DRAFT" (prin1-to-string messages)))
        (gptel-runner-gptel-test--handoff second "Done"))
      (should (eq 'succeeded (gptel-runner-run-state run)))
      (with-current-buffer chat
        (should (string-match-p "PRIVATE-UNSENT-DRAFT" (buffer-string))))
      (kill-buffer chat)
      (setq chat (gptel-runner-node-buffer run 'check))
      (should (buffer-local-value 'gptel-mode chat))
      (should (equal "Done" (gptel-runner-get run 'answer)))
      (should (string-search "Done" (with-current-buffer chat (buffer-string)))))))

(defun gptel-runner-session-test--checkpoint ()
  "Verify that an editable, paused chat survives the whole run lifecycle."
  (gptel-runner-session-test--with-run
    (gptel-runner-defworkflow session-handoff (:persist t)
      (gptel-runner-sequence
       :id 'workflow
       (gptel-runner-agent-step :id 'draft :agent 'reader :prompt "Draft a plan"
                                :pause-after t :save-as 'plan)
       (gptel-runner-agent-step :id 'review :agent 'reader
                                :prompt (lambda (r _) (gptel-runner-get r 'plan)))))
    (let* ((driver (gptel-runner-gptel-driver-create))
           (run (gptel-runner-start 'session-handoff :driver driver))
           (original (gptel-runner-node-buffer run 'draft))
           (file (gptel-runner-run-snapshot-file run)))
      (gptel-runner-gptel-test--reply (pop requests) "Proposed plan" "stop")
      (should-not requests)
      (with-current-buffer original
        (goto-char (point-max))
        (insert "Could you revise step two?"))
      (gptel-runner-pause-run run)
      (gptel-runner-forget-run run)
      (should-not (buffer-live-p original))
      (setq run (gptel-runner-load-run file nil driver))
      (should (eq (gptel-runner-run-state run) 'paused))
      (should-not requests)
      (with-current-buffer (gptel-runner-node-buffer run 'draft)
        (should gptel-mode)
        (should-not buffer-read-only)
        (should (equal gptel-model 'runner-test))
        (should (equal "Proposed plan" (gptel-runner-gptel--last-response (current-buffer))))
        (goto-char (point-min))
        (should (search-forward "Could you revise step two?" nil t))
        ;; Send the restored draft with the ordinary interactive chat command.
        (gptel-send)
        (let* ((fsm (pop requests))
               (payload (prin1-to-string (plist-get (gptel-fsm-info fsm) :data))))
          (should (string-match-p "Proposed plan" payload))
          (should (string-match-p "Could you revise step two?" payload))
          ;; The real curl transport supplies this default for ordinary sends.
          (unless (plist-get (gptel-fsm-info fsm) :callback)
            (setf (plist-get (gptel-fsm-info fsm) :callback) #'gptel--insert-response))
          (gptel-runner-gptel-test--reply fsm "Revised plan" "stop"))
        (should-not requests)
        (gptel-runner-complete-call-from-buffer))
      (should (equal (gptel-runner-get run 'plan) "Revised plan"))
      (let ((review (pop requests)))
        (should review)
        (should-not requests)
        (should (equal (plist-get (gptel-fsm-info review) :buffer)
                       (gptel-runner-node-buffer run 'review)))
        (gptel-runner-gptel-test--handoff review "Reviewed"))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (gptel-runner-forget-run run))))

(ert-deftest gptel-runner-session-plain-chat-checkpoint ()
  (cl-letf (((symbol-function 'gptel-runner-store--agent-sessions-p)
             (lambda () nil)))
    (gptel-runner-session-test--checkpoint)))

(ert-deftest gptel-runner-session-agent-chat-checkpoint ()
  (skip-unless (and (require 'gptel-agent nil t)
                    (gptel-runner-store--agent-sessions-p)))
  (gptel-runner-session-test--checkpoint))

(ert-deftest gptel-runner-session-agent-feedback-follows-restored-inbox ()
  (skip-unless (and (require 'gptel-agent nil t)
                    (gptel-runner-store--agent-sessions-p)))
  (gptel-runner-session-test--with-run
    (gptel-runner-defworkflow saved-inbox (:persist t)
      (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"))
    (let* ((gptel-runner-headless t)
           (run (gptel-runner-start 'saved-inbox))
           (file (gptel-runner-run-snapshot-file run))
           (chat (gptel-runner-node-buffer run 'read))
           (info (gptel-fsm-info (pop requests))))
      (with-current-buffer chat (goto-char (point-max)) (insert "Private draft"))
      (funcall (plist-get info :callback) "Checking" info)
      ;; A discussion delivers inbox messages through the raw callback.
      (funcall (plist-get info :callback) "Worker reported back" info t)
      (gptel-runner-pause-run run)
      (gptel-runner-forget-run run)
      (setq run (gptel-runner-load-run file))
      (with-current-buffer (gptel-runner-node-buffer run 'read)
        (gptel-runner-gptel-feedback gptel-runner--call "Continue")
        (goto-char (point-min))
        (should (search-forward "Worker reported back" nil t))
        (should (search-forward "Continue" nil t))
        (should (search-forward "Private draft" nil t)))
      (let ((payload (plist-get (gptel-fsm-info (pop requests)) :data)))
        (should (string-search "Worker reported back" (prin1-to-string payload)))
        (should-not (string-search "Private draft" (prin1-to-string payload)))))))

(ert-deftest gptel-runner-session-restore-outstanding-question ()
  (gptel-runner-session-test--with-run
    (gptel-runner-defworkflow saved-question (:persist t)
      (gptel-runner-sequence
       (gptel-runner-agent-step :id 'research :agent 'reader :prompt "Research"
                                :save-as 'report)
       (gptel-runner-agent-step :id 'plan :agent 'reader :prompt "Plan"
                                :save-as 'plan)))
    (let* ((run (gptel-runner-start 'saved-question))
           (file (gptel-runner-run-snapshot-file run)))
      (gptel-runner-gptel-test--handoff (pop requests) "original findings")
      (gptel-runner-gptel-test--reply
       (pop requests) nil "tool_calls"
       [(:id "ask" :type "function"
         :function (:name "RunnerAskPrevious" :arguments "{\"question\":\"Which version?\"}"))])
      (gptel-runner-gptel-test--reply (pop requests) "Version two" "stop")
      (gptel-runner-pause-run run)
      (gptel-runner-forget-run run)
      (setq run (gptel-runner-load-run file))
      (should-not requests)
      ;; Accept the answer in the research chat after restarting Emacs.
      (with-current-buffer (gptel-runner-node-buffer run 'research)
        (gptel-runner-complete-call-from-buffer))
      (should (equal (gptel-runner-get run 'report) "original findings"))
      (let* ((resumed (pop requests))
             (info (gptel-fsm-info resumed)))
        (should (eq (plist-get info :buffer) (gptel-runner-node-buffer run 'plan)))
        (should (string-search "Version two" (prin1-to-string (plist-get info :data))))
        (gptel-runner-gptel-test--handoff resumed "final plan"))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal "final plan" (gptel-runner-get run 'plan)))
      (should-not requests))))

(ert-deftest gptel-runner-session-parallel-feedback-survives-restart ()
  (gptel-runner-session-test--with-run
    (gptel-runner-defworkflow saved-reviews (:persist t :max-concurrency 2)
      (gptel-runner-parallel
       :id 'reviews
       (gptel-runner-agent-step :id 'first :agent 'reader :prompt "Read A"
                                :pause-after t :save-as 'a)
       (gptel-runner-agent-step :id 'second :agent 'reader :prompt "Read B"
                                :pause-after t :save-as 'b)))
    (let* ((run (gptel-runner-start 'saved-reviews))
           (file (gptel-runner-run-snapshot-file run)))
      (gptel-runner-gptel-test--reply (pop requests) "Answer A" "stop")
      (gptel-runner-gptel-test--reply (pop requests) "Answer B" "stop")
      (gptel-runner-pause-run run)
      (gptel-runner-forget-run run)
      (setq run (gptel-runner-load-run file))
      (with-current-buffer (gptel-runner-node-buffer run 'first)
        (gptel-runner-complete-call-from-buffer))
      (should (eq (gptel-runner-run-state run) 'paused))
      (should (equal (gptel-runner-get run 'a) "Answer A"))
      (with-current-buffer (gptel-runner-node-buffer run 'second)
        (gptel-runner-complete-call-from-buffer))
      (should (equal (gptel-runner-get run 'b) "Answer B"))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should-not requests))))

(ert-deftest gptel-runner-session-restored-discussion-preserves-parallel-approval ()
  (gptel-runner-session-test--with-run
    (let ((prompt-count 0))
      (gptel-runner-defworkflow saved-discussions (:persist t :max-concurrency 2)
        (gptel-runner-parallel
         (gptel-runner-agent-step :id 'talk :agent 'reader :save-as 'answer
                                  :prompt (lambda (_run _node)
                                            (cl-incf prompt-count) "Inspect"))
         (gptel-runner-agent-step :id 'gate :agent 'reader :prompt "Review"
                                  :pause-after t :save-as 'review)))
      (let* ((run (gptel-runner-start 'saved-discussions))
             (file (gptel-runner-run-snapshot-file run)))
        (gptel-runner-gptel-test--reply (pop requests) "Which file?" "stop")
        (gptel-runner-gptel-test--handoff (pop requests) "Ready for approval")
        (gptel-runner-pause-run run)
        (gptel-runner-forget-run run)
        (setq run (gptel-runner-load-run file))
        (with-current-buffer (gptel-runner-node-buffer run 'talk)
          (gptel-runner-gptel-feedback gptel-runner--call "Read design.el"))
        (should (= 1 prompt-count))
        (let ((resumed (pop requests)))
          (should-not requests)
          (should (string-search "Which file?" (prin1-to-string
                                                (plist-get (gptel-fsm-info resumed) :data))))
          (gptel-runner-gptel-test--handoff resumed "File inspected"))
        (should (equal "File inspected" (gptel-runner-get run 'answer)))
        (should-not (gptel-runner-get run 'review))
        (with-current-buffer (gptel-runner-node-buffer run 'gate)
          (gptel-runner-complete-call-from-buffer))
        (should (eq 'succeeded (gptel-runner-run-state run)))
        (should (equal "Ready for approval" (gptel-runner-get run 'review)))
        (should-not requests)))))

(ert-deftest gptel-runner-session-background-publishes-editable-chat ()
  (gptel-runner-session-test--with-run
    (let* ((gptel-runner-jobs--run
            (gptel-runner-start
             (gptel-runner-agent-step :id 'check :agent 'reader :prompt "Check code"
                                      :completion 'response)
             :driver (gptel-runner-gptel-driver-create)))
           (gptel-runner-jobs--invocation
            (list :id "session-test" :epoch 1 :directory gptel-runner-snapshot-directory))
           (gptel-runner-jobs--exports (make-hash-table :test #'equal))
           (source (gptel-runner-node-buffer gptel-runner-jobs--run 'check))
           opened)
      (gptel-runner-jobs--publish)
      (save-window-excursion
        (let* ((call (car (plist-get gptel-runner-jobs--invocation :calls)))
               (target (list 'background-call "session-test" 1 (plist-get call :id))))
          (gptel-runner-background-transcript (list gptel-runner-jobs--invocation call target))
          (should (kill-buffer (current-buffer)))
          (should-not (kill-buffer source))
          (should (eq (gptel-runner-run-state gptel-runner-jobs--run) 'running))))
      (gptel-runner-gptel-test--reply (pop requests) "Checked" "stop")
      (with-current-buffer source (goto-char (point-max)) (insert "Worker draft"))
      (gptel-runner-jobs--publish)
      (unwind-protect
          (save-window-excursion
            (let* ((manifest (gptel-runner-jobs--read
                              (gptel-runner-jobs--manifest-file gptel-runner-jobs--invocation)))
                   (call (car (plist-get manifest :calls)))
                   (target (list 'background-call "session-test" 1 (plist-get call :id)))
                   (selection (list manifest call target)))
              (should (plist-get call :chat))
              (gptel-runner-background-transcript selection)
              (setq opened (current-buffer))
              (should gptel-mode)
              (should-not (eq opened source))
              (should-not buffer-read-only)
              (should (equal "Checked" (gptel-runner-gptel--last-response opened)))
              (goto-char (point-min))
              (should (search-forward "Worker draft" nil t))
              (insert " with a local note")
              (gptel-runner-background-transcript selection)
              (should (eq opened (current-buffer)))
              (should-not (with-current-buffer source (string-match-p "local note" (buffer-string))))
              (with-current-buffer source
                (goto-char (point-max))
                (insert " updated before closing"))
              (kill-buffer source)
              (kill-buffer opened)
              (gptel-runner-jobs--publish)
              (let ((published (car (plist-get gptel-runner-jobs--invocation :calls))))
                (gptel-runner-background-transcript
                 (list gptel-runner-jobs--invocation published target)))
              (setq opened (current-buffer))
              (goto-char (point-min))
              (should (search-forward "Worker draft updated before closing" nil t))
              (should-not requests)))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest gptel-runner-session-clone-active-chat-is-independent ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                          :save-as 'answer)))
           (call (car (gptel-runner-run-calls run)))
           (source (gptel-runner-call-buffer call))
           (original (pop requests))
           (clone (with-current-buffer source (clone-buffer " *runner-clone*"))))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (should-not gptel-runner--call)
              (should-not gptel--fsm-last)
              (call-interactively #'gptel-abort)
              (goto-char (point-max))
              (insert "\nIndependent question")
              (gptel-send))
            (let* ((ordinary (pop requests)) (info (gptel-fsm-info ordinary)))
              (should (eq clone (plist-get info :buffer)))
              (should-not (gptel-runner-call-p (plist-get info :context)))
              (setf (plist-get (gptel-fsm-info ordinary) :callback) #'gptel--insert-response)
              (gptel-runner-gptel-test--reply ordinary "Clone answer" "stop"))
            (with-current-buffer clone
              (goto-char (point-max))
              (insert "Another independent question")
              (gptel-send))
            (let ((ordinary (pop requests)))
              (should (eq clone (plist-get (gptel-fsm-info ordinary) :buffer)))
              (should (gptel--fsm-live-p ordinary)))
            (should (kill-buffer clone))
            (should (eq 'running (gptel-runner-run-state run)))
            (should (eq 'running (gptel-runner-call-state call)))
            (should (eq original (gptel-runner-call-fsm call)))
            (should (eq source (gptel-runner-node-buffer run 'read)))
            (should-not requests)
            (gptel-runner-gptel-test--handoff original "Original answer")
            (should (eq 'succeeded (gptel-runner-run-state run)))
            (should (equal "Original answer" (gptel-runner-get run 'answer)))
            (with-current-buffer source
              (should-not (string-search "Independent question" (buffer-string)))
              (should-not (string-search "Clone answer" (buffer-string)))))
        (when (buffer-live-p clone) (kill-buffer clone))))))

(ert-deftest gptel-runner-session-clone-reply-does-not-resume-source ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                          :save-as 'answer)))
           (call (car (gptel-runner-run-calls run)))
           (source (gptel-runner-call-buffer call))
           (original (pop requests))
           clone)
      (gptel-runner-gptel-test--reply original "Which file?" "stop")
      (should (gptel-runner-gptel--open-discussion-p call))
      (unwind-protect
          (progn
            (setq clone (with-current-buffer source (clone-buffer " *runner-clone*")))
            (with-current-buffer clone
              (goto-char (point-max))
              (insert "Clone reply")
              (gptel-send))
            (let* ((ordinary (pop requests)) (info (gptel-fsm-info ordinary)))
              (should (eq clone (plist-get info :buffer)))
              (should-not (gptel-runner-call-p (plist-get info :context)))
              (setf (plist-get (gptel-fsm-info ordinary) :callback) #'gptel--insert-response)
              (gptel-runner-gptel-test--reply ordinary "Independent result" "stop"))
            (should (eq 'waiting-feedback (gptel-runner-call-state call)))
            (should (eq original (gptel-runner-call-fsm call)))
            (with-current-buffer source
              (goto-char (point-max))
              (insert "Original reply")
              (gptel-send))
            (let* ((resumed (pop requests)) (info (gptel-fsm-info resumed)))
              (should (eq source (plist-get info :buffer)))
              (should (eq call (plist-get info :context)))
              (should-not (string-search "Clone reply" (prin1-to-string (plist-get info :data))))
              (gptel-runner-gptel-test--handoff resumed "Source result"))
            (should (equal "Source result" (gptel-runner-get run 'answer)))
            (should (eq 'succeeded (gptel-runner-run-state run))))
        (when (buffer-live-p clone) (kill-buffer clone))))))

(ert-deftest gptel-runner-session-clone-preserves-source-reservations-and-hooks ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                          :pause-after t :save-as 'answer)
                 :completion 'response))
           (call (car (gptel-runner-run-calls run)))
           (source (gptel-runner-call-buffer call))
           clone)
      (gptel-runner-gptel-test--reply (pop requests) "Initial result" "stop")
      (with-current-buffer source
        (goto-char (point-max))
        (insert "Manual follow-up")
        (gptel-send))
      (let ((ordinary (pop requests))
            (reservations (buffer-local-value 'gptel-runner-gptel--manual-requests source))
            (submitted (buffer-local-value 'gptel-runner-gptel--submitted-end source))
            (rendered (buffer-local-value 'gptel-runner-gptel--rendered-end source))
            (hooks (mapcar (lambda (hook) (cons hook (copy-sequence (buffer-local-value hook source))))
                           '(after-change-functions kill-buffer-hook kill-buffer-query-functions)))
            checkpoints)
        (should (equal reservations (list ordinary)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gptel-runner--checkpoint)
                         (lambda (&rest args) (push args checkpoints))))
                (setq clone (with-current-buffer source (clone-buffer " *runner-clone*")))
                (with-current-buffer clone
                  (should-not gptel-runner-gptel--manual-requests)
                  (should-not gptel-runner-gptel--submitted-end)
                  (should-not gptel-runner-gptel--rendered-end)
                  (should-not gptel-agent-session-managed-externally)
                  (goto-char (point-max))
                  (insert "Clone-only draft")
                  (call-interactively #'gptel-abort))
                (should (kill-buffer clone)))
              (should-not checkpoints)
              (should (eq reservations (buffer-local-value 'gptel-runner-gptel--manual-requests source)))
              (should (eq ordinary (buffer-local-value 'gptel--fsm-last source)))
              (should (gptel--fsm-live-p ordinary))
              (should (eq source (marker-buffer submitted)))
              (should (eq source (marker-buffer rendered)))
              (should (eq submitted (buffer-local-value 'gptel-runner-gptel--submitted-end source)))
              (should (eq rendered (buffer-local-value 'gptel-runner-gptel--rendered-end source)))
              (dolist (entry hooks)
                (should (equal (cdr entry) (buffer-local-value (car entry) source))))
              (setf (plist-get (gptel-fsm-info ordinary) :callback) #'gptel--insert-response)
              (gptel-runner-gptel-test--reply ordinary "Manual result" "stop")
              (gptel-runner-gptel--release-send source ordinary)
              (with-current-buffer source (gptel-runner-complete-call-from-buffer))
              (should (equal "Manual result" (gptel-runner-get run 'answer)))
              (should (eq 'succeeded (gptel-runner-run-state run))))
          (when (buffer-live-p clone) (kill-buffer clone)))))))

(ert-deftest gptel-runner-session-clone-child-is-detached-from-run ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
           (source (gptel-runner-node-buffer run 'read))
           (child (generate-new-buffer " *runner-child*"))
           (grandchild (generate-new-buffer " *runner-grandchild*"))
           clone checkpoints)
      (unwind-protect
          (progn
            (with-current-buffer child
              (setq-local gptel-agent-session-managed-externally t)
              (insert "Original child transcript"))
            (gptel-runner-store-track-child source child 'worker)
            (let* ((id (buffer-local-value 'gptel-runner-store--child-id child))
                   (record (gethash id (gptel-runner-run-child-chats run))))
              (cl-letf (((symbol-function 'gptel-runner--checkpoint)
                         (lambda (&rest args) (push args checkpoints))))
                (setq clone (with-current-buffer child (clone-buffer " *runner-child-clone*")))
                (with-current-buffer clone
                  (should-not gptel-runner-store--child-run)
                  (should-not gptel-runner-store--child-id)
                  (should-not gptel-agent-session-managed-externally)
                  (insert "Clone-only text"))
                ;; Stale copied identities must also fail the registry's
                ;; source-buffer check when clone cleanup was bypassed.
                (with-current-buffer clone
                  (setq-local gptel-runner-store--child-run run
                              gptel-runner-store--child-id id)
                  (gptel-runner-store--child-changed)
                  (gptel-runner-store--close-child)
                  (should-not (gptel-runner-ui--run-context)))
                (gptel-runner-store-track-child clone grandchild 'worker)
                (should (kill-buffer clone)))
              (should-not checkpoints)
              (should (= 1 (hash-table-count (gptel-runner-run-child-chats run))))
              (should (eq child (plist-get record :chat)))
              (should (eq run (buffer-local-value 'gptel-runner-store--child-run child)))
              (with-current-buffer child
                (should (equal "Original child transcript" (buffer-string))))
              (should (eq 'running (gptel-runner-run-state run))))
            (gptel-runner-gptel-test--handoff (pop requests) "Source remains usable")
            (should (eq 'succeeded (gptel-runner-run-state run))))
        (dolist (buffer (list clone child grandchild))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gptel-runner-session-clone-stale-call-cannot-control-source ()
  (gptel-runner-session-test--with-run
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                          :pause-after t :save-as 'answer)
                 :completion 'response))
           (call (car (gptel-runner-run-calls run)))
           (source (gptel-runner-call-buffer call))
           (original (pop requests))
           (clone (with-current-buffer source (clone-buffer " *runner-stale-clone*"))))
      (unwind-protect
          (progn
            ;; Command boundaries must reject a copied pointer even if the
            ;; normal clone cleanup was bypassed by an older integration.
            (with-current-buffer clone
              (setq-local gptel-runner--call call)
              (call-interactively #'gptel-abort)
              (should-error (call-interactively #'gptel-runner-pause-call) :type 'user-error)
              (should-not (gptel-runner-ui--run-context))
              (let (checkpoint)
                (cl-letf (((symbol-function 'gptel-runner--checkpoint)
                           (lambda (&rest _) (setq checkpoint t))))
                  (gptel-runner-store--chat-changed))
                (should-not checkpoint))
              (goto-char (point-max))
              (insert "Independent stale-clone request")
              (gptel-send)
              (should-not gptel-runner-gptel--manual-requests))
            (should (eq 'running (gptel-runner-call-state call)))
            (let ((ordinary (pop requests)))
              (should (eq clone (plist-get (gptel-fsm-info ordinary) :buffer)))
              (setf (plist-get (gptel-fsm-info ordinary) :callback) #'gptel--insert-response)
              (gptel-runner-gptel-test--reply ordinary "Clone result" "stop"))
            (gptel-runner-gptel-test--reply original "Source result" "stop")
            (with-current-buffer clone
              (should-error (gptel-runner-complete-call-from-buffer "Wrong result") :type 'user-error))
            (should (eq 'waiting-feedback (gptel-runner-call-state call)))
            (should-not (gptel-runner-get run 'answer))
            (with-current-buffer source (gptel-runner-complete-call-from-buffer))
            (should (equal "Source result" (gptel-runner-get run 'answer)))
            (should (eq 'succeeded (gptel-runner-run-state run))))
        (when (buffer-live-p clone) (kill-buffer clone))))))

(ert-deftest gptel-runner-session-clone-agent-session-coexistence ()
  (skip-unless (and (require 'gptel-agent nil t)
                    (gptel-runner-store--agent-sessions-p)))
  (let ((gptel-agent-session-directory (make-temp-file "runner-clone-session-" t))
        (gptel-agent-session-mode nil)
        (gptel-agent-session-auto-title nil)
        (gptel-agent-session--buffers (make-hash-table :test #'equal))
        (gptel-agent-session--dirty (make-hash-table :test #'eq))
        (gptel-agent-session--queue nil)
        (gptel-agent-session--queue-tail nil)
        clone)
    (unwind-protect
        (gptel-runner-session-test--with-run
          (gptel-agent-session-mode 1)
          (let* ((run (gptel-runner-start
                       (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
                 (source (gptel-runner-node-buffer run 'read))
                 (original (pop requests)))
            (setq clone (with-current-buffer source (clone-buffer " *runner-session-clone*")))
            (let ((id (buffer-local-value 'gptel-agent-session--id clone)))
              (should (stringp id))
              (should (eq clone (gethash id gptel-agent-session--buffers)))
              (should-not (buffer-local-value 'gptel-agent-session--id source))
              (should (buffer-local-value 'gptel-agent-session-managed-externally source))
              (should-not (buffer-local-value 'gptel-agent-session-managed-externally clone))
              (with-current-buffer clone
                (goto-char (point-max))
                (insert "Independent saved draft")
                (call-interactively #'gptel-abort))
              (gptel-agent-session-save)
              (should (string-search "Independent saved draft"
                                     (plist-get (gptel-agent-session--read id) :text)))
              (should (eq source (gptel-runner-node-buffer run 'read)))
              (should (eq 'running (gptel-runner-run-state run)))
              (gptel-runner-gptel-test--handoff original "Original result")
              (should (eq 'succeeded (gptel-runner-run-state run))))))
      (gptel-agent-session-mode -1)
      (when (buffer-live-p clone) (kill-buffer clone))
      (delete-directory gptel-agent-session-directory t))))

(provide 'gptel-runner-session-test)
;;; gptel-runner-session-test.el ends here
