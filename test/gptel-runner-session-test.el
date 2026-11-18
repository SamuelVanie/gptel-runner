;;; gptel-runner-session-test.el --- Native node chat integration -*- lexical-binding: t; -*-

(require 'gptel-runner-gptel-test)
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
      (gptel-runner-gptel-test--reply first "First finding" "stop")
      (let* ((second (pop requests))
             (messages (plist-get (plist-get (gptel-fsm-info second) :data) :messages)))
        (should (eq chat (plist-get (gptel-fsm-info second) :buffer)))
        (should (cl-some (lambda (m) (and (equal (plist-get m :role) "assistant")
                                          (equal (plist-get m :content) "First finding")))
                         messages))
        (should (string-match-p "Recheck the result" (prin1-to-string messages)))
        (should-not (string-match-p "PRIVATE-UNSENT-DRAFT" (prin1-to-string messages)))
        (gptel-runner-gptel-test--reply second "Done" "stop"))
      (should (eq 'succeeded (gptel-runner-run-state run)))
      (with-current-buffer chat
        (should (string-match-p "PRIVATE-UNSENT-DRAFT" (buffer-string))))
      (kill-buffer chat)
      (setq chat (gptel-runner-node-buffer run 'check))
      (should (buffer-local-value 'gptel-mode chat))
      (should (equal "Done" (gptel-runner-gptel--last-response chat))))))

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
        (gptel-runner-gptel-test--reply review "Reviewed" "stop"))
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

(ert-deftest gptel-runner-session-background-publishes-editable-chat ()
  (gptel-runner-session-test--with-run
    (let* ((gptel-runner-jobs--run
            (gptel-runner-start
             (gptel-runner-agent-step :id 'check :agent 'reader :prompt "Check code")
             :driver (gptel-runner-gptel-driver-create)))
           (gptel-runner-jobs--invocation
            (list :id "session-test" :epoch 1 :directory gptel-runner-snapshot-directory))
           (gptel-runner-jobs--exports (make-hash-table :test #'equal))
           (source (gptel-runner-node-buffer gptel-runner-jobs--run 'check))
           opened)
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

(provide 'gptel-runner-session-test)
;;; gptel-runner-session-test.el ends here
