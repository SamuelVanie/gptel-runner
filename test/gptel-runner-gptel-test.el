;;; gptel-runner-gptel-test.el --- Adapter contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-openai)
(require 'gptel-anthropic)
(require 'gptel-runner)

(defmacro gptel-runner-gptel-test--with-transport (&rest body)
  "Run BODY with real request machinery and a queued network transport."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-backend (gptel-make-openai "runner-test" :host "runner.test"
                                         :stream t
                                         :models '(runner-test)))
         (gptel-model 'runner-test)
         (gptel-stream nil)
         (gptel-use-curl t)
         (gptel-use-tools t)
         (gptel-confirm-tool-calls nil)
         (gptel-tools
          (list (gptel-make-tool
                 :name "runner_read" :description "Read test evidence."
                 :function (lambda () "repository findings")
                 :args nil :include t)))
         (gptel-runner-retain-worker-buffers t)
         requests)
     (unwind-protect
         (cl-letf (((symbol-function 'gptel-curl-get-response)
                    (lambda (fsm)
                      (setq requests (nconc requests (list fsm))))))
           ,@body)
       (maphash
        (lambda (_id run)
          (gptel-runner-abort-run run)
          (dolist (call (gptel-runner-run-calls run))
            (when (buffer-live-p (gptel-runner-call-buffer call))
              (kill-buffer (gptel-runner-call-buffer call)))))
        gptel-runner--runs))))

(defun gptel-runner-gptel-test--reply (fsm text stop &optional tools)
  "Deliver TEXT, STOP reason and TOOLS through FSM's real response parser."
  (let* ((info (gptel-fsm-info fsm))
         (response
          (gptel--parse-response
           (plist-get info :backend)
           (list :choices
                 (vector (list :finish_reason stop
                               :message (list :role "assistant" :content text
                                              :tool_calls tools))))
           info)))
    (plist-put info :http-status "200")
    (gptel--fsm-transition fsm)
    (when response
      (funcall (plist-get info :callback) response info))
    (gptel--fsm-transition fsm)))

(defun gptel-runner-gptel-test--retry (call)
  "Fire CALL's pending retry deterministically."
  (let ((timer (gptel-runner-call-retry-timer call)))
    (should (timerp timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(defun gptel-runner-gptel-test--handoff (fsm message &optional commentary)
  "Send MESSAGE through FSM's real handoff tool with optional COMMENTARY."
  (gptel-runner-gptel-test--reply
   fsm commentary "tool_calls"
   (vector (list :id "handoff" :type "function"
                 :function (list :name "RunnerHandoff"
                                 :arguments (gptel--json-encode
                                             (list :message message)))))))

(ert-deftest gptel-runner-gptel-discussion-feedback-hands-off ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'report :agent 'reader
                                          :prompt "Inspect" :save-as 'answer)))
           (first (pop requests))
           (call (car (gptel-runner-run-calls run)))
           (buffer (gptel-runner-call-buffer call)))
      (gptel-runner-gptel-test--reply first "Which branch should I inspect?" "stop")
      (should (eq 'waiting-feedback (gptel-runner-call-state call)))
      (should-not (gptel-runner-get run 'answer))
      (gptel-runner-gptel-feedback call "Use main")
      (let* ((second (pop requests))
             (info (gptel-fsm-info second))
             (names (mapcar #'gptel-tool-name (plist-get info :tools))))
        (should (eq buffer (plist-get info :buffer)))
        (should (member "runner_read" names))
        (should (member "RunnerHandoff" names))
        (should-not (member "RunnerAskPrevious" names))
        (should (string-search "Which branch" (prin1-to-string (plist-get info :data))))
        (should-not (cl-find "RunnerHandoff" (buffer-local-value 'gptel-tools buffer)
                             :key #'gptel-tool-name :test #'equal))
        (gptel-runner-gptel-test--handoff second "The results" "I checked main."))
      (should (equal "The results" (gptel-runner-get run 'answer)))
      (should (eq 'succeeded (gptel-runner-run-state run)))
      (should (= 2 (gptel-runner-budget-requests (gptel-runner-run-budget run))))
      (should-not requests))))

(ert-deftest gptel-runner-gptel-feedback-follows-interrupted-org-tool-output ()
  (dolist (restore '(nil t))
    (gptel-runner-gptel-test--with-transport
      (let ((gptel-default-mode 'org-mode)
            (gptel-runner-headless restore))
        (gptel-runner-register-agent 'reader)
        (let* ((run (gptel-runner-start
                     (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
               (call (car (gptel-runner-run-calls run)))
               (chat (gptel-runner-call-buffer call))
               (request (pop requests)))
          (with-current-buffer chat
            (goto-char (point-max))
            (insert "Unsent private draft"))
          (gptel-runner-gptel-test--reply
           request "Checking" "tool_calls"
           [(:id "read-1" :type "function"
                 :function (:name "runner_read" :arguments "{}"))])
          (should (eq (pop requests) request))
          (gptel-runner-pause-call call)
          (when restore
            ;; Simulate resuming a checkpoint in a different worker.
            (let ((original chat))
              (setq chat (gptel-runner-gptel-chat-from-text
                          (gptel-runner-gptel-chat-text chat)))
              (gptel-runner-store-track-chat call chat)
              (kill-buffer original)))
          (gptel-runner-gptel-feedback call "Continue")
          (let* ((next (pop requests))
                 (messages (append (plist-get (plist-get (gptel-fsm-info next) :data)
                                              :messages) nil)))
            (should (equal (plist-get (car (last messages)) :content) "Continue"))
            (should (string-search "repository findings" (prin1-to-string messages)))
            (should-not (string-search "Unsent private draft" (prin1-to-string messages)))
            (gptel-runner-gptel-test--reply next "Revised" "stop")
            (let ((info (gptel-fsm-info next)))
              (funcall (plist-get info :callback) "Late text" info)))
          (with-current-buffer chat
            (goto-char (point-min))
            (should (search-forward "Checking" nil t))
            (should (search-forward "#+end_tool" nil t))
            (should (search-forward "Continue" nil t))
            (should (search-forward "Revised" nil t))
            (should (search-forward "Unsent private draft" nil t))
            (should-not (string-search "Late text" (buffer-string)))))))))

(ert-deftest gptel-runner-gptel-feedback-closes-interrupted-reasoning ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-default-mode 'org-mode) (gptel-stream t))
      (gptel-runner-register-agent 'reader)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
             (call (car (gptel-runner-run-calls run)))
             (info (gptel-fsm-info (pop requests))))
        (plist-put info :include-reasoning 'ignore)
        (funcall (plist-get info :callback) '(reasoning . "Partial thought") info)
        (gptel-runner-pause-call call)
        (gptel-runner-gptel-feedback call "Continue")
        (with-current-buffer (gptel-runner-call-buffer call)
          (goto-char (point-min))
          (should (search-forward "Partial thought" nil t))
          (should (search-forward "#+end_reasoning" nil t))
          (should (search-forward "Continue" nil t)))))))

(ert-deftest gptel-runner-gptel-history-recognizes-legacy-org-block-ends ()
  (dolist (block '("tool" "reasoning"))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_" block "\n" (propertize "Body" 'gptel 'ignore)
              "\n#+end_" block "\n")
      (let ((end (point)))
        (insert "My draft")
        (should (= end (gptel-runner-gptel--history-end)))))))

(ert-deftest gptel-runner-gptel-feedback-follows-ordinary-chat-output ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect"
                                          :pause-after t)
                 :completion 'response))
           (call (car (gptel-runner-run-calls run)))
           (chat (gptel-runner-call-buffer call)))
      (gptel-runner-gptel-test--reply (pop requests) "First report" "stop")
      (with-current-buffer chat
        (goto-char (point-max))
        (insert "Check manually")
        (gptel-send)
        (insert "My draft"))
      (let* ((ordinary (pop requests)) (info (gptel-fsm-info ordinary)))
        ;; A discussion can insert an inbox message with RAW after the model's
        ;; answer.  It is part of the conversation even without a role property.
        (setf (plist-get info :callback) #'gptel--insert-response)
        (gptel--insert-response "Reading" info)
        (gptel--insert-response "Inbox message" info t)
        (gptel-runner-gptel-test--reply ordinary nil "stop")
        (gptel-runner-gptel--release-send chat ordinary))
      (gptel-runner-gptel-feedback call "Continue")
      (with-current-buffer chat
        (goto-char (point-min))
        (should (search-forward "Inbox message" nil t))
        (should (search-forward "Continue" nil t))
        (should (search-forward "My draft" nil t))))))

(ert-deftest gptel-runner-gptel-reviewed-handoff-accepts-payload-or-later-reply ()
  (dolist (manual '(nil t))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'report :agent 'reader :prompt "Inspect"
                                            :pause-after t :save-as 'answer)))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-gptel-test--handoff (pop requests) "Final report" "Here it comes.")
        (should (eq 'waiting-feedback (gptel-runner-call-state call)))
        (when manual
          (gptel-runner-gptel-feedback call "Check again")
          (gptel-runner-gptel-test--reply (pop requests) "Revised report" "stop"))
        (gptel-runner-complete-call-from-buffer nil call)
        (should (equal (if manual "Revised report" "Final report")
                       (gptel-runner-get run 'answer)))
        (should-not requests)))))

(ert-deftest gptel-runner-gptel-send-continues-discussion-to-point ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
           (call (car (gptel-runner-run-calls run))))
      (gptel-runner-gptel-test--reply (pop requests) "Which directory?" "stop")
      (with-current-buffer (gptel-runner-call-buffer call)
        (goto-char (point-max))
        (should-error (gptel-send) :type 'user-error)
        (insert "Use src")
        (save-excursion (insert "unsent-draft"))
        (gptel-send)
        (should-error (gptel-send) :type 'user-error))
      (let* ((request (pop requests))
             (messages (plist-get (plist-get (gptel-fsm-info request) :data) :messages)))
        (should (equal "Use src" (plist-get (car (last (append messages nil))) :content)))
        (gptel-runner-gptel-test--handoff request "Checked"))
      (should (eq 'succeeded (gptel-runner-run-state run)))
      (with-current-buffer (gptel-runner-call-buffer call)
        (should (string-suffix-p "unsent-draft" (buffer-string)))))))

(ert-deftest gptel-runner-gptel-handoff-honors-approval-and-schema ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-runner-headless t) (gptel-confirm-tool-calls t)
          (gptel-use-tools nil))
      (gptel-runner-register-agent 'reader :schema '(:type object :properties (:answer (:type string))))
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Inspect")))
             (call (car (gptel-runner-run-calls run)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (data (plist-get info :data))
             (answer "{\"answer\":\"ready\"}"))
        (should-not (plist-get data :response_format))
        (should (string-search "\"answer\"" (plist-get (aref (plist-get data :messages) 0) :content)))
        (should (equal '("RunnerHandoff") (mapcar #'gptel-tool-name (plist-get info :tools))))
        (gptel-runner-gptel-test--handoff fsm answer)
        (should (eq 'waiting-confirmation (gptel-runner-call-state call)))
        (should-not (plist-get info :runner-transfer))
        (let* ((pending (car (gptel-runner-gptel--interactions call)))
               (id (plist-get (plist-get pending :description) :id)))
          (gptel-runner-gptel-answer-interaction call id t)
          (should-error (gptel-runner-gptel-answer-interaction call id t) :type 'user-error))
        (should (equal answer (gptel-runner-call-result call)))
        (should-not requests)))))

(ert-deftest gptel-runner-gptel-prompt-preset-keeps-communication-after-async-transform ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel--known-presets nil) resume)
      (gptel-make-preset 'alternate
        :prompt-transform-functions
        (list (lambda (continue fsm)
                (setq resume
                      (lambda ()
                        (with-current-buffer (plist-get (gptel-fsm-info fsm) :data)
                          (setq-local gptel-tools nil gptel-use-tools nil
                                      gptel-system-prompt "Preset instructions"))
                        (funcall continue))))))
      (gptel-runner-register-agent 'reader)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "@alternate Inspect")))
             (call (car (gptel-runner-run-calls run))))
        (should-not requests)
        (should (functionp resume))
        (funcall resume)
        (let* ((request (pop requests))
               (info (gptel-fsm-info request))
               (messages (plist-get (plist-get info :data) :messages)))
          (should (equal '("RunnerHandoff") (mapcar #'gptel-tool-name (plist-get info :tools))))
          (should (string-search "Preset instructions" (plist-get (aref messages 0) :content)))
          (should (string-search "RunnerHandoff" (plist-get (aref messages 0) :content)))
          (gptel-runner-gptel-test--reply request "A question" "stop")
          (should (eq 'waiting-feedback (gptel-runner-call-state call))))
        (gptel-runner-gptel-feedback call "Finish")
        (gptel-runner-gptel-test--handoff (pop requests) "Final answer")
        (should (eq 'succeeded (gptel-runner-run-state run)))))))

(defun gptel-runner-gptel-test--stream-chunk (fsm delta &optional stop)
  "Deliver a streaming DELTA with optional STOP through FSM's real parser."
  (let ((info (gptel-fsm-info fsm)))
    (with-temp-buffer
      (insert "data: "
              (gptel--json-encode
               (list :choices (vector (list :delta delta :finish_reason stop))))
              "\n\n")
      (when stop (insert "data: [DONE]\n\n"))
      (goto-char (point-min))
      (let ((text (gptel-curl--parse-stream (plist-get info :backend) info)))
        (unless (string-empty-p text)
          (funcall (plist-get info :callback) text info))))))

(ert-deftest gptel-runner-gptel-honors-stream-setting-and-backend-support ()
  (dolist (settings '((t nil t t) (nil nil t nil)
                      (nil (:stream t) t t) (t (:stream nil) t nil)
                      (t nil nil nil)))
    (pcase-let ((`(,default ,preset ,supported ,expected) settings))
      (gptel-runner-gptel-test--with-transport
        (let ((gptel-stream default))
          (setf (gptel-backend-stream gptel-backend) supported)
          (gptel-runner-register-agent 'plan :preset preset)
          (gptel-runner-start
           (gptel-runner-agent-step :id 'plan :agent 'plan :prompt "Research")
           :driver (gptel-runner-gptel-driver-create))
          (let ((info (gptel-fsm-info (pop requests))))
            (should (eq (plist-get info :stream) expected))
            (should (eq (plist-get (plist-get info :data) :stream)
                        (if expected t :json-false)))))))))

(ert-deftest gptel-runner-gptel-streamed-tools-complete-before-sequence-advances ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-stream t))
      (gptel-runner-register-agent 'plan :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-sequence
                    (gptel-runner-agent-step
                     :id 'first :agent 'plan :prompt "Research" :save-as 'first)
                    (gptel-runner-agent-step
                     :id 'second :agent 'plan :prompt "Report"))
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (call (plist-get info :context)))
        (should (plist-get info :stream))
        ;; The streaming transport enters TYPE on HTTP headers, before parsing.
        (plist-put info :http-status "200")
        (gptel--fsm-transition fsm)
        (gptel-runner-gptel-test--stream-chunk
         fsm '(:content "Let me inspect "))
        (gptel-runner-gptel-test--stream-chunk
         fsm '(:content "the workspace."))
        (gptel-runner-gptel-test--stream-chunk
         fsm '(:tool_calls [(:index 0 :id "read-1" :type "function"
                                   :function (:name "runner_read" :arguments "{"))]))
        (gptel-runner-gptel-test--stream-chunk
         fsm '(:tool_calls [(:index 0 :function (:arguments "}"))]) "tool_calls")
        (funcall (plist-get info :callback) t info)
        (should (eq (gptel-runner-call-state call) 'running))
        (should-not (gptel-runner-get run 'first))
        (gptel--fsm-transition fsm)
        ;; The real tool executed and its result is in the follow-up request.
        (should (eq (pop requests) fsm))
        (should (equal
                 (plist-get (car (last (append
                                       (plist-get (plist-get info :data) :messages)
                                       nil)))
                            :content)
                 "repository findings"))
        (should (= (length (gptel-runner-run-calls run)) 1))
        (plist-put info :http-status "200")
        (gptel--fsm-transition fsm)
        (gptel-runner-gptel-test--stream-chunk fsm '(:content "Complete "))
        (gptel-runner-gptel-test--stream-chunk fsm '(:content "proposal") "stop")
        (funcall (plist-get info :callback) t info)
        (should-not (gptel-runner-get run 'first))
        (gptel--fsm-transition fsm)
        (should (equal (gptel-runner-get run 'first) "Complete proposal"))
        (should (= (length (gptel-runner-run-calls run)) 2))
        (with-current-buffer (gptel-runner-call-buffer call)
          (should (string-match-p "Let me inspect the workspace\\." (buffer-string)))
          (should (string-match-p "Complete proposal" (buffer-string))))))))

(ert-deftest gptel-runner-gptel-streamed-missing-tools-checked-at-end ()
  (gptel-runner-gptel-test--with-transport
    ;; Anthropic exposes streaming stop reasons in request info.
    (let ((gptel-stream t)
          (gptel-backend (gptel-make-anthropic
                          "runner-anthropic-test" :stream t
                          :models '(runner-test))))
      (gptel-runner-register-agent 'plan :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step
                    :id 'plan :agent 'plan :prompt "Research" :save-as 'proposal)
                   :driver (gptel-runner-gptel-driver-create)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (call (plist-get info :context)))
        (plist-put info :http-status "200")
        (gptel--fsm-transition fsm)
        (with-temp-buffer
          (insert
           "event: content_block_delta\n"
           "data: {\"delta\":{\"text\":\"Let me inspect the workspace.\"}}\n\n"
           "event: message_delta\n"
           "data: {\"delta\":{\"stop_reason\":\"tool_use\"}}\n\n")
          (goto-char (point-min))
          (funcall (plist-get info :callback)
                   (gptel-curl--parse-stream gptel-backend info) info))
        (funcall (plist-get info :callback) t info)
        (gptel--fsm-transition fsm)
        (should (eq (gptel-runner-call-state call) 'retry-wait))
        (should-not (gptel-runner-get run 'proposal))
        (gptel-runner-gptel-test--retry call)
        (should (eq (pop requests) fsm))
        (should (plist-get info :stream))
        (should-not (gptel-runner-call-response-parts call))
        (should-not (plist-get info :stop-reason))))))

(ert-deftest gptel-runner-gptel-streamed-reasoning-kept-out-of-result ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-stream t)
          (gptel-default-mode 'text-mode))
      (gptel-runner-register-agent 'plan :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step
                    :id 'plan :agent 'plan :prompt "Research" :save-as 'proposal)
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (call (plist-get info :context))
             (callback (plist-get info :callback)))
        (plist-put info :http-status "200")
        (plist-put info :include-reasoning t)
        (gptel--fsm-transition fsm)
        (funcall callback '(reasoning . "Inspecting the code") info)
        (funcall callback '(reasoning . t) info)
        (funcall callback "Complete " info)
        (funcall callback "proposal" info)
        (funcall callback t info)
        (gptel--fsm-transition fsm)
        (should (equal (gptel-runner-get run 'proposal) "Complete proposal"))
        (with-current-buffer (gptel-runner-call-buffer call)
          (should (string-match-p "Inspecting the code" (buffer-string)))
          (should (string-match-p "Complete proposal" (buffer-string))))))))

(ert-deftest gptel-runner-gptel-missing-tool-call-retries-same-request ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'plan :preset nil)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step
                  :id 'first-proposal :agent 'plan :prompt "Research"
                  :save-as 'proposal-1)
                 :driver (gptel-runner-gptel-driver-create)))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests))
           (data (copy-tree (plist-get (gptel-fsm-info fsm) :data))))
      (gptel-runner-gptel-test--reply
       fsm "Let me inspect the workspace." "tool_calls")
      (should (eq (gptel-runner-call-state call) 'retry-wait))
      (should-not (gptel-runner-get run 'proposal-1))
      (should-not (plist-get (gptel-runner-call-driver-data call) :posted))
      (gptel-runner-gptel-test--retry call)
      (should (eq (pop requests) fsm))
      (should (equal (plist-get (gptel-fsm-info fsm) :data) data))
      (gptel-runner-gptel-test--handoff fsm "Complete proposal")
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'proposal-1) "Complete proposal"))
      (should (= (length (gptel-runner-run-calls run)) 1))
      (should (= (gptel-runner-budget-requests
                  (gptel-runner-run-budget run)) 2)))))

(ert-deftest gptel-runner-gptel-missing-tools-stop-at-retry-or-request-limit ()
  (dolist (request-limit '(1 10))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent
       'plan :preset nil
       :retry-policy (gptel-runner-retry-policy-create :max-retries 1))
      (let* ((run (gptel-runner-start
                   (gptel-runner-sequence
                    (gptel-runner-agent-step
                     :id 'first :agent 'plan :prompt "Research" :save-as 'first)
                    (gptel-runner-agent-step
                     :id 'last :agent 'plan :prompt "Report"))
                   :driver (gptel-runner-gptel-driver-create)
                   :max-requests request-limit))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-gptel-test--reply
         (pop requests) "Inspecting..." "tool_calls")
        (when (> request-limit 1)
          (gptel-runner-gptel-test--retry call)
          (gptel-runner-gptel-test--reply
           (pop requests) "Still inspecting..." "tool_calls" :null))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-not (gptel-runner-get run 'first))
        (should (= (length (gptel-runner-run-calls run)) 1))
        (should-not requests)
        (should (plist-get (gptel-runner-call-driver-data call) :posted))))))

(ert-deftest gptel-runner-gptel-sequence-waits-for-final-answer-after-tools ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'plan :preset nil)
    (gptel-runner-register-agent 'report :preset nil :workspace-mode 'write)
    (let* ((ids '(first-proposal second-proposal third-proposal
                                write-decision-report))
           (run (gptel-runner-start
                 (apply #'gptel-runner-sequence
                        (mapcar
                         (lambda (id)
                           (gptel-runner-agent-step
                            :id id :agent (if (eq id 'write-decision-report)
                                              'report 'plan)
                            :prompt "Research and report" :save-as id))
                         ids))
                 :allow-writes t :allow-unconfirmed-tools t
                 :driver (gptel-runner-gptel-driver-create))))
      (cl-loop
       for id in ids for count from 1
       for fsm = (pop requests)
       for call = (plist-get (gptel-fsm-info fsm) :context)
       do
       (should (eq (gptel-runner-node-id (gptel-runner-call-node call)) id))
       (dotimes (_round 2)
         (gptel-runner-gptel-test--reply
          fsm "Let me inspect the workspace." "tool_calls"
          [(:id "read-1" :type "function"
                :function (:name "runner_read" :arguments "{}"))])
         (should (eq (gptel-runner-call-state call) 'running))
         (should (= (length (gptel-runner-run-calls run)) count))
         (should-not (gptel-runner-get run id))
         (should (eq (pop requests) fsm)))
       (gptel-runner-gptel-test--handoff fsm (symbol-name id))
       (should (equal (gptel-runner-get run id) (symbol-name id))))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (= (length (gptel-runner-run-calls run)) 4))
      (should-not requests))))

(ert-deftest gptel-runner-gptel-tool-commentary-cannot-mask-empty-final-answer ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'plan :preset nil)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step
                  :id 'proposal :agent 'plan :prompt "Research" :save-as 'proposal)
                 :completion 'response
                 :driver (gptel-runner-gptel-driver-create)))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests)))
      (gptel-runner-gptel-test--reply
       fsm "Let me inspect the workspace." "tool_calls"
       [(:id "read-1" :type "function"
             :function (:name "runner_read" :arguments "{}"))])
      (should (eq (pop requests) fsm))
      (gptel-runner-gptel-test--reply fsm " " "stop")
      (should (eq (gptel-runner-call-state call) 'failed))
      (should-not (gptel-runner-get run 'proposal))
      (should (= (length (gptel-runner-run-calls run)) 2))
      (let* ((repair-fsm (pop requests))
             (repair-call (plist-get (gptel-fsm-info repair-fsm) :context)))
        (should (gptel-runner-call-repair-p repair-call))
        (gptel-runner-gptel-test--reply repair-fsm "Complete proposal" "stop"))
      (should (equal (gptel-runner-get run 'proposal) "Complete proposal")))))

(ert-deftest gptel-runner-gptel-rejected-tool-preserves-steering ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-confirm-tool-calls t)
          pending cleaned)
      (gptel-runner-register-agent 'plan :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'plan :agent 'plan :prompt "Research")
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm)))
        (cl-letf (((symbol-function 'gptel--display-tool-calls)
                   (lambda (calls _info) (setq pending calls))))
          (gptel-runner-gptel-test--reply
           fsm nil "tool_calls"
           [(:id "read-1" :type "function"
                 :function (:name "runner_read" :arguments "{}"))]))
        (should pending)
        (plist-put info :post-tool (list (lambda (_info) (push 'cleaned cleaned))))
        (with-current-buffer (plist-get info :buffer)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "Use the evidence already available")))
            (gptel--steer-tool-calls pending nil info)))
        (should (eq (pop requests) fsm))
        (let ((messages (append (plist-get (plist-get info :data) :messages) nil)))
          (should (equal (plist-get (car (last messages)) :content)
                         "Use the evidence already available"))
          (should (equal (plist-get (car (last messages 2)) :content)
                         "Tool call rejected by user.")))
        (should (equal cleaned '(cleaned)))
        (should-not (plist-get info :post-tool))
        (should-not (plist-get info :steering-message))
        (gptel-runner-gptel-test--reply fsm "Final report" "stop")
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-call-result (car (gptel-runner-run-calls run)))
                       "Final report"))))))

(ert-deftest gptel-runner-gptel-tool-hooks-can-replace-results ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-pre-tool-call-functions
           (list (lambda (_call) '(:result "Cached evidence"))))
          (gptel-post-tool-call-functions
           (list (lambda (call)
                   (should (equal (plist-get call :result) "Cached evidence"))
                   '(:result "Reviewed evidence")))))
      (gptel-runner-register-agent 'plan :preset nil)
      (gptel-runner-start
       (gptel-runner-agent-step :id 'plan :agent 'plan :prompt "Research")
       :driver (gptel-runner-gptel-driver-create))
      (let* ((fsm (pop requests))
             (info (gptel-fsm-info fsm)))
        (gptel-runner-gptel-test--reply
         fsm nil "tool_calls"
         [(:id "read-1" :type "function"
               :function (:name "runner_read" :arguments "{}"))])
        (should (eq (pop requests) fsm))
        (should (equal (plist-get (car (last (append
                                            (plist-get (plist-get info :data) :messages)
                                            nil))) :content)
                       "Reviewed evidence"))))))

(ert-deftest gptel-runner-gptel-post-response-hooks-release-downstream ()
  (dolist (condition '(nil error quit))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-sequence
                    (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                             :save-as 'answer)
                    (gptel-runner-agent-step :id 'next :agent 'reader :prompt "Continue"))
                   :completion 'response :max-requests 2 :max-concurrency 1))
             (call (car (gptel-runner-run-calls run)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (callback (plist-get info :callback))
             (posts 0)
             (hooks 0))
        (plist-put info :post
                   (cons (lambda (_info) (cl-incf posts)) (plist-get info :post)))
        (with-current-buffer (gptel-runner-call-buffer call)
          (setq-local gptel-post-response-functions
                      (list (lambda (_start _end)
                              (cl-incf hooks)
                              (should (= posts 1))
                              (should (eq (gptel-runner-call-state call) 'running))
                              (should-not (marker-insertion-type
                                           (plist-get info :tracking-marker)))
                              (should (string-suffix-p
                                       (concat gptel-response-separator
                                               (gptel-prompt-prefix-string))
                                       (buffer-string)))
                              (when condition
                                (signal condition '("User post-response hook failed")))))))
        (should
         (equal (condition-case err
                    (progn (gptel-runner-gptel-test--reply fsm "Answer" "stop") nil)
                  ((error quit) err))
                (and condition (list condition "User post-response hook failed"))))
        (should (eq (gptel-runner-call-state call) 'succeeded))
        (should (equal (gptel-runner-get run 'answer) "Answer"))
        (should (equal (gptel-runner-gptel--last-response
                        (gptel-runner-call-buffer call)) "Answer"))
        (should (= (length requests) 1))
        (should (= (gptel-runner-run-active-count run) 1))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 2))
        (should-not (cl-some #'cdr (gptel-fsm-handlers fsm)))
        (let ((transcript (with-current-buffer (gptel-runner-call-buffer call)
                            (buffer-string))))
          (funcall callback "Late response" info)
          (gptel--fsm-transition fsm 'WAIT)
          (gptel--fsm-transition fsm 'DONE)
          (should (equal transcript (with-current-buffer (gptel-runner-call-buffer call)
                                      (buffer-string)))))
        (should (= posts 1))
        (should (= hooks 1))
        (gptel-runner-gptel-test--reply (pop requests) "Downstream answer" "stop")
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (zerop (gptel-runner-run-active-count run)))
        (should-not requests)))))

(ert-deftest gptel-runner-gptel-post-response-hooks-preserve-discussion-handoff ()
  (dolist (condition '(nil error quit))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :save-as 'answer)
                   :max-requests 2))
             (call (car (gptel-runner-run-calls run)))
             (hooks 0))
        (with-current-buffer (gptel-runner-call-buffer call)
          (setq-local gptel-post-response-functions
                      (list (lambda (_start _end)
                              (cl-incf hooks)
                              (when condition
                                (signal condition '("Discussion hook failed")))))))
        (should
         (equal (condition-case err
                    (progn (gptel-runner-gptel-test--reply
                            (pop requests) "Which branch?" "stop") nil)
                  ((error quit) err))
                (and condition (list condition "Discussion hook failed"))))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should (zerop (gptel-runner-run-active-count run)))
        (should-not (gptel-runner-get run 'answer))
        (should-not requests)
        (gptel-runner-gptel-feedback call "Use main")
        (should
         (equal (condition-case err
                    (progn (gptel-runner-gptel-test--handoff
                            (pop requests) "The results" "I checked main.") nil)
                  ((error quit) err))
                (and condition (list condition "Discussion hook failed"))))
        (should (= hooks 2))
        (should (equal (gptel-runner-get run 'answer) "The results"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 2))
        (should-not requests)))))

(ert-deftest gptel-runner-gptel-post-response-hooks-keep-review-gate ()
  (dolist (condition '(nil error quit))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :pause-after t :save-as 'answer)
                   :completion 'response :max-requests 2))
             (call (car (gptel-runner-run-calls run)))
             (hooks 0))
        (gptel-runner-gptel-test--reply (pop requests) "Initial answer" "stop")
        (with-current-buffer (gptel-runner-call-buffer call)
          (setq-local gptel-post-response-functions
                      (list (lambda (_start _end)
                              (cl-incf hooks)
                              (should (plist-get (gptel-runner-call-driver-data call)
                                                 :feedback-active))
                              (when condition
                                (signal condition '("Review hook failed")))))))
        (gptel-runner-gptel-feedback call "Check again")
        (let ((fsm (pop requests)))
          (should
           (equal (condition-case err
                      (progn (gptel-runner-gptel-test--reply
                              fsm "Corrected answer" "stop") nil)
                    ((error quit) err))
                  (and condition (list condition "Review hook failed"))))
          (should-not (cl-some #'cdr (gptel-fsm-handlers fsm))))
        (should (= hooks 1))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should-not (plist-get (gptel-runner-call-driver-data call) :feedback-active))
        (should-not (gptel-runner-get run 'answer))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 2))
        (should-not requests)
        (gptel-runner-complete-call-from-buffer nil call)
        (should (equal (gptel-runner-get run 'answer) "Corrected answer"))
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-post-response-cleanup-error-completes ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader :preset nil)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :save-as 'answer)
                 :completion 'response))
           (call (car (gptel-runner-run-calls run)))
           (fsm (pop requests))
           (info (gptel-fsm-info fsm))
           (posts 0)
           (hooks 0))
      (plist-put info :post
                 (append (plist-get info :post)
                         (list (lambda (_info)
                                 (cl-incf posts)
                                 (error "Request cleanup failed")))))
      (with-current-buffer (gptel-runner-call-buffer call)
        (setq-local gptel-post-response-functions
                    (list (lambda (_start _end) (cl-incf hooks)))))
      (should (equal (should-error
                      (gptel-runner-gptel-test--reply fsm "Answer" "stop"))
                     '(error "Request cleanup failed")))
      (should (= posts 1))
      (should (zerop hooks))
      (should (eq (gptel-runner-run-state run) 'succeeded))
      (should (equal (gptel-runner-get run 'answer) "Answer"))
      (should (zerop (gptel-runner-run-active-count run)))
      (should-not requests))))

(ert-deftest gptel-runner-gptel-post-response-hook-cancellation-stays-terminal ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reader :preset nil)
    (let* ((run (gptel-runner-start
                 (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                          :save-as 'answer)
                 :completion 'response))
           (call (car (gptel-runner-run-calls run)))
           (hooks 0))
      (with-current-buffer (gptel-runner-call-buffer call)
        (setq-local gptel-post-response-functions
                    (list (lambda (_start _end)
                            (cl-incf hooks)
                            (gptel-runner-abort-run run)
                            (error "Hook cancelled the run")))))
      (should (equal (should-error
                      (gptel-runner-gptel-test--reply (pop requests) "Answer" "stop"))
                     '(error "Hook cancelled the run")))
      (should (= hooks 1))
      (should (eq (gptel-runner-run-state run) 'cancelled))
      (should (eq (gptel-runner-call-state call) 'cancelled))
      (should-not (gptel-runner-get run 'answer))
      (should (zerop (gptel-runner-run-active-count run)))
      (should-not requests))))

(ert-deftest gptel-runner-gptel-post-response-hook-preserves-replacement-feedback ()
  (dolist (condition '(nil error quit))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :save-as 'answer)
                   :completion 'response :max-requests 2))
             (call (car (gptel-runner-run-calls run)))
             (fsm (pop requests))
             (info (gptel-fsm-info fsm))
             (callback (plist-get info :callback))
             (hooks 0))
        (with-current-buffer (gptel-runner-call-buffer call)
          (setq-local gptel-post-response-functions
                      (list (lambda (_start _end)
                              (cl-incf hooks)
                              (setq-local gptel-post-response-functions nil)
                              (gptel-runner-pause-call call)
                              (gptel-runner-gptel-feedback call "Please revise this")
                              (when condition
                                (signal condition '("Replacement hook failed")))))))
        (should
         (equal (condition-case err
                    (progn (gptel-runner-gptel-test--reply fsm "Initial answer" "stop") nil)
                  ((error quit) err))
                (and condition (list condition "Replacement hook failed"))))
        (should (= hooks 1))
        (should (= (length requests) 1))
        (should-not (eq fsm (gptel-runner-call-fsm call)))
        (should (cl-some #'cdr (gptel-fsm-handlers (car requests))))
        (should (plist-get (gptel-runner-call-driver-data call) :feedback-active))
        (should (= (gptel-runner-run-active-count run) 1))
        (should-not (gptel-runner-get run 'answer))
        (let ((transcript (with-current-buffer (gptel-runner-call-buffer call)
                            (buffer-string))))
          (funcall callback "Late initial answer" info)
          (gptel--fsm-transition fsm 'DONE)
          (should (equal transcript (with-current-buffer (gptel-runner-call-buffer call)
                                      (buffer-string)))))
        (gptel-runner-gptel-test--reply (pop requests) "Revised answer" "stop")
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should-not (plist-get (gptel-runner-call-driver-data call) :feedback-active))
        (should (zerop (gptel-runner-run-active-count run)))
        (should (= (gptel-runner-budget-requests (gptel-runner-run-budget run)) 2))
        (should-not requests)
        (gptel-runner-complete-call-from-buffer nil call)
        (should (equal (gptel-runner-get run 'answer) "Revised answer"))
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-supported-api-shape ()
  (should (gptel-runner-gptel--check-api))
  (should (fboundp 'gptel--apply-preset))
  (should (fboundp 'gptel--fsm-transition))
  (should (boundp 'gptel-request--transitions))
  (should (boundp 'gptel-request--handlers)))

(ert-deftest gptel-runner-gptel-fsm-adds-status-and-replaces-terminals ()
  (let* ((fsm (gptel-runner-gptel--make-fsm))
         (handlers (gptel-fsm-handlers fsm)))
    ;; The native send handlers must be enclosed by a single budget gate.
    (should (= (length (alist-get 'WAIT handlers)) 1))
    (should (equal (alist-get 'TOOL handlers)
                   (append (list #'gptel-runner-gptel--tool-started
                                 #'gptel--update-tool-call)
                           (alist-get 'TOOL gptel-request--handlers)
                           (list #'gptel--update-tool-ask))))
    ;; Native post-insert and completion share an exception-safe handler.
    (should (= (length (alist-get 'DONE handlers)) 1))
    (should (equal (alist-get 'ERRS handlers)
                   (list #'gptel-runner-gptel--error)))
    (should (equal (alist-get 'ABRT handlers)
                   (list #'gptel-runner-gptel--aborted)))))

(ert-deftest gptel-runner-gptel-fsm-reflects-active-request-status ()
  (let* ((buffer (generate-new-buffer " *gptel-runner-status-test*"))
         (fsm (gptel-runner-gptel--make-fsm))
         (info (list :buffer buffer
                     :tool-use (list '(:name "AskUserQuestion" :args nil)))))
    (setf (gptel-fsm-info fsm) info)
    (unwind-protect
        (with-current-buffer buffer
          (text-mode)
          (gptel-mode 1)
          (cl-letf (((symbol-function 'gptel--handle-tool-use) #'ignore)
                    ((symbol-function 'gptel--handle-wait) #'ignore))
            (gptel--fsm-transition fsm 'TOOL)
            (should (string-match-p
                     "Calling tool.*AskUserQuestion"
                     (substring-no-properties (nth 1 header-line-format))))
            (gptel--fsm-transition fsm 'WAIT)
            (should (equal
                     (substring-no-properties (nth 1 header-line-format))
                     " Waiting..."))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-tool-state-includes-name ()
  (let* ((run (gptel-runner-run-create :id "run" :events nil))
         (node (gptel-runner-node-create :id 'work))
         (call (gptel-runner-call-create
                :id "call" :run run :node node :state 'running))
         (fsm (gptel-runner-gptel--make-fsm)))
    (setf (gptel-runner-call-driver-data call)
          (list :observe
                (lambda (type data)
                  (gptel-runner--call-observe call 0 type data)))
          (gptel-fsm-info fsm)
          (list :context call
                :tool-use (list '(:name "AskUserQuestion" :args nil))))
    (gptel-runner-gptel--tool-started fsm)
    (should (eq (gptel-runner-call-state call) 'waiting-tool))
    (should (equal (gptel-runner-call-tool-names call)
                   '("AskUserQuestion")))))

(ert-deftest gptel-runner-gptel-confirmed-tool-enters-waiting-tool-state ()
  (let* ((buffer (generate-new-buffer " *gptel-runner-tool-accept-test*"))
         (run (gptel-runner-run-create :id "run" :events nil))
         (node (gptel-runner-node-create :id 'work))
         (call (gptel-runner-call-create
                :id "call" :run run :node node :buffer buffer
                :state 'waiting-confirmation
                :tool-names '("AskUserQuestion")))
         state-during-call)
    (setf (gptel-runner-call-driver-data call)
          (list :observe
                (lambda (type data)
                  (gptel-runner--call-observe call 0 type data))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local gptel-runner--call call)
          (gptel-runner-gptel--around-accept-tool-calls
           (lambda (&rest _arguments)
             (setq state-during-call (gptel-runner-call-state call)))
           nil nil)
          (should (eq state-during-call 'waiting-tool)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-terminal-status-is-ready ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (run (gptel-runner-run-create :id "run"))
         (node (gptel-runner-node-create :id 'node))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :response-parts '("done")
                :workspace default-directory))
         (buffer (gptel-runner-gptel--worker-buffer call))
         (fsm (gptel-runner-gptel--make-fsm))
         completed)
    (setf (gptel-runner-call-driver-data call)
          (list :complete (lambda (&rest result) (setq completed result)))
          (gptel-fsm-info fsm) (list :buffer buffer :context call))
    (unwind-protect
        (progn
          (gptel-runner-gptel--done fsm)
          (should (equal completed '(success "done" nil)))
          (should
           (equal (with-current-buffer buffer
                    (substring-no-properties (nth 1 header-line-format)))
                  " Ready")))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-workers-use-node-workspaces ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'researcher :preset nil)
    (let* ((workspace (make-temp-file "gptel-runner-workspaces-" t))
           (directory (file-name-as-directory (file-truename workspace)))
           (node-directory (expand-file-name "node/" directory))
           (original-directory default-directory))
      (unwind-protect
          (progn
            (make-directory node-directory)
            (let* ((run (gptel-runner-start
                         (gptel-runner-parallel
                          (gptel-runner-agent-step
                           :id 'override :agent 'researcher :workspace "node"
                           :prompt "Research node")
                          (gptel-runner-agent-step
                           :id 'inherited :agent 'researcher :prompt "Research run"))
                         :workspace workspace :max-concurrency 2
                         :driver (gptel-runner-gptel-driver-create)))
                   (calls (gptel-runner-run-calls run)))
              (should (= (length requests) 2))
              (cl-mapc
               (lambda (call expected)
                 (let ((buffer (gptel-runner-call-buffer call)))
                   (should (equal (buffer-local-value 'default-directory buffer)
                                  expected))
                   (with-current-buffer buffer
                     (should gptel-mode)
                     (should-not (string-match-p "Runner call:" (buffer-string))))))
               calls (list node-directory directory))
              (should-not (eq (gptel-runner-call-buffer (car calls))
                              (gptel-runner-call-buffer (cadr calls))))
              (dolist (request requests)
                (gptel-runner-gptel-test--handoff request "Findings"))
              (should (eq (gptel-runner-run-state run) 'succeeded))
              (should (equal default-directory original-directory))))
        (delete-directory workspace t)))))

(ert-deftest gptel-runner-gptel-worker-is-private-and-local ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (run (gptel-runner-run-create :id "run"))
         (node (gptel-runner-node-create :id 'node))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :workspace default-directory :prompt "Inspect this node"))
         (buffer (gptel-runner-gptel--worker-buffer call)))
    (unwind-protect
        (progn
          (should (string-prefix-p "*gptel:node*" (buffer-name buffer)))
          (should (eq (buffer-local-value 'gptel-runner--call buffer) call))
          (should (equal (buffer-local-value 'default-directory buffer)
                         default-directory))
          (with-current-buffer buffer
            (should gptel-mode)
            (should (equal (buffer-string)
                           (concat (gptel-prompt-prefix-string) "Inspect this node")))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-worker-renders-final-response ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (run (gptel-runner-run-create :id "run"))
         (node (gptel-runner-node-create :id 'node))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :prompt "Inspect the project" :workspace default-directory))
         (buffer (gptel-runner-gptel--worker-buffer call)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-max))
          (let ((info (list :buffer buffer :position (point-marker))))
            (gptel-runner-gptel--callback
             call (lambda (&rest _ignored)) "Visible answer" info))
          (should (string-match-p "Inspect the project" (buffer-string)))
          (should (string-match-p "Visible answer" (buffer-string))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-repeated-node-continues-chat-before-draft ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'reviewer :preset nil)
    (let* ((run (gptel-runner-start
                 (gptel-runner-repeat-until
                  :id 'loop :max 2
                  :until (lambda (run) (equal (gptel-runner-get run 'answer) "Approved"))
                  :body (gptel-runner-agent-step
                         :id 'review :agent 'reviewer :save-as 'answer
                         :prompt "Review this change"))
                 :completion 'response
                 :driver (gptel-runner-gptel-driver-create)))
           (chat (gptel-runner-node-buffer run 'review))
           (first (pop requests)))
      (with-current-buffer chat
        (should-error (gptel-send) :type 'user-error)
        (goto-char (point-max))
        (insert "Draft for a later discussion"))
      (gptel-runner-gptel-test--reply first "Needs revision" "stop")
      ;; An obsolete callback or FSM transition must leave the new turn alone.
      (let ((info (gptel-fsm-info first)) (before (with-current-buffer chat (buffer-string))))
        (funcall (plist-get info :callback) "Stale answer" info)
        (gptel--fsm-transition first 'DONE)
        (should (equal before (with-current-buffer chat (buffer-string)))))
      (let* ((fsm (pop requests))
             (data (plist-get (gptel-fsm-info fsm) :data))
             (messages (append (plist-get data :messages) nil)))
        (should (equal (mapcar (lambda (m) (plist-get m :content))
                               (cl-remove-if
                                (lambda (m) (equal (plist-get m :role) "system"))
                                messages))
                       '("Review this change" "Needs revision" "Review this change")))
        (should (cl-every (lambda (call) (eq (gptel-runner-call-buffer call) chat))
                          (gptel-runner-run-calls run)))
        (with-current-buffer chat
          (should-not (gptel-runner-gptel--last-response chat)))
        (gptel-runner-gptel-test--reply fsm "Approved" "stop")
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (with-current-buffer chat
          (should (string-suffix-p "Draft for a later discussion" (buffer-string))))))))

(ert-deftest gptel-runner-gptel-protects-node-chats-until-run-finishes ()
  (gptel-runner-gptel-test--with-transport
    (gptel-runner-register-agent 'planner)
    (let* ((run (gptel-runner-start
                 (gptel-runner-sequence
                  (gptel-runner-agent-step :id 'plan :agent 'planner :prompt "Plan")
                  (gptel-runner-agent-step :id 'review :agent 'planner :prompt "Review"))))
           (chat (gptel-runner-node-buffer run 'plan))
           (call (car (gptel-runner-run-calls run))))
      (save-window-excursion
        (switch-to-buffer chat)
        (should-not (call-interactively #'kill-current-buffer))
        (quit-window)
        (should-not (eq (window-buffer) chat)))
      (should (buffer-live-p chat))
      (should (eq (gptel-runner-call-state call) 'running))
      (should (eq (gptel-runner-call-fsm call) (car requests)))
      (gptel-runner-gptel-test--handoff (pop requests) "Ready to review")
      (should (eq (gptel-runner-call-state call) 'succeeded))
      (should-not (kill-buffer chat))
      (let ((review (gptel-runner-node-buffer run 'review)))
        (gptel-runner-gptel-test--reply (pop requests) "Which branch?" "stop")
        (should-not (kill-buffer review))
        (should (eq (gptel-runner-call-state (buffer-local-value 'gptel-runner--call review))
                    'waiting-feedback))
        (gptel-runner-abort-run run)
        (should (kill-buffer review))
        (should (kill-buffer chat))))))

(ert-deftest gptel-runner-gptel-queues-workflow-turn-during-ordinary-send ()
  (dolist (end '(reply error abort kill))
    (gptel-runner-gptel-test--with-transport
      (gptel-runner-register-agent 'planner)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'plan :agent 'planner :prompt "Plan")
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (chat (gptel-runner-node-buffer run 'plan))
             (first (pop requests))
             manual)
        (gptel-runner-gptel-test--reply first "First plan" "stop")
        (with-current-buffer chat
          (goto-char (point-max))
          (insert "My follow-up")
          (gptel-send)
          (setq manual (pop requests)))
        (gptel-runner-continue run "Plan again")
        (should-not requests)
        (should (= 1 (length (gptel-runner-run-queue run))))
        (should (zerop (gptel-runner-run-active-count run)))
        (with-current-buffer chat
          (add-hook 'gptel-post-response-functions
                    (lambda (&rest _)
                      (gptel-runner--drain-queue run)
                      (should-not requests))
                    nil t))
        (pcase end
          ('reply
           (setf (plist-get (gptel-fsm-info manual) :callback) #'gptel--insert-response)
           (gptel-runner-gptel-test--reply manual "Manual answer" "stop"))
          ('error
           (setf (plist-get (gptel-fsm-info manual) :error) "Offline")
           (gptel--fsm-transition manual 'ERRS))
          ('abort
           (let ((gptel--request-alist (list (cons 'mock-process (cons manual #'ignore)))))
             (gptel-abort chat)))
          ('kill
           (should-not (kill-buffer chat))
           ;; Forced disposal must still archive the chat and release the queue.
           (with-current-buffer chat
             (let ((kill-buffer-query-functions nil)) (kill-buffer chat)))))
        ;; The next dispatch runs after the terminal handlers or kill hooks.
        (should-not requests)
        (accept-process-output nil 0.02)
        (should (= 1 (length requests)))
        (let ((next (pop requests)))
          (when (eq end 'reply)
            (should (string-search "Manual answer"
                                   (prin1-to-string (plist-get (gptel-fsm-info next) :data)))))
          (gptel-runner-gptel-test--reply next "Second plan" "stop"))
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-native-chat-round-trip ()
  (gptel-runner-gptel-test--with-transport
    (dolist (mode '(text-mode org-mode))
      (let* ((gptel-default-mode mode)
             (chat (gptel (generate-new-buffer-name "*runner round trip*") nil "My question"))
             restored)
        (unwind-protect
            (with-current-buffer chat
              (setq-local gptel-model 'runner-test)
              (setq-local default-directory temporary-file-directory)
              (setq-local gptel-confirm-tool-calls t)
              (setq-local gptel-runner-gptel--submitted-end (point-marker))
              (insert "\n\n" (propertize "The answer" 'gptel 'response) "\nUnsent draft")
              (let* ((before (buffer-string))
                     (text (gptel-runner-gptel-chat-text chat)))
                (should (equal before (buffer-string)))
                (setq restored (gptel-runner-gptel-chat-from-text text))
                (with-current-buffer restored
                  (should gptel-mode)
                  (should (eq major-mode mode))
                  (should (eq gptel-model 'runner-test))
                  (should gptel-confirm-tool-calls)
                  (should (equal default-directory (file-name-as-directory temporary-file-directory)))
                  (when (eq mode 'org-mode)
                    (with-temp-buffer
                      (insert (gptel-runner-gptel-chat-text restored))
                      (should (= (how-many "^:PROPERTIES:$" (point-min) (point-max)) 1))))
                  (should (equal (gptel-runner-gptel--last-response restored) "The answer"))
                  (should (string-suffix-p "Unsent draft" (buffer-string)))
                  (should (equal (buffer-substring-no-properties
                                  (- gptel-runner-gptel--submitted-end 11)
                                  gptel-runner-gptel--submitted-end) "My question")))))
          (kill-buffer chat)
          (when (buffer-live-p restored) (kill-buffer restored)))))))

(ert-deftest gptel-runner-gptel-direct-worker-abort-cancels-runner-only ()
  (let ((gptel-runner--agents (make-hash-table :test #'eq))
        (gptel-runner--runs (make-hash-table :test #'equal))
        (gptel-runner--next-id 0)
        (ordinary-calls 0))
    (gptel-runner-register-agent 'worker :preset nil)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (&rest arguments) (plist-get (cdr arguments) :fsm))))
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step
                    :id 'work :agent 'worker :prompt "wait")
                   :driver (gptel-runner-gptel-driver-create)))
             (call (car (gptel-runner-run-calls run)))
             (buffer (gptel-runner-call-buffer call)))
        (gptel-runner-gptel--around-abort
         (lambda (&optional _buffer) (cl-incf ordinary-calls)) buffer)
        (should (eq (gptel-runner-call-state call) 'cancelled))
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should (= ordinary-calls 1))
        (with-temp-buffer
          (gptel-runner-gptel--around-abort
           (lambda (&optional _buffer) (cl-incf ordinary-calls))
           (current-buffer)))
        (should (= ordinary-calls 2))))))

(ert-deftest gptel-runner-gptel-writers-force-confirmation-by-default ()
  (let ((gptel-runner--agents (make-hash-table :test #'eq))
        (gptel-runner--runs (make-hash-table :test #'equal))
        (gptel-runner--next-id 0)
        (gptel-confirm-tool-calls nil)
        observed)
    (gptel-runner-register-agent 'writer :preset nil :workspace-mode 'write)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (&rest arguments)
                 (setq observed gptel-confirm-tool-calls)
                 (plist-get (cdr arguments) :fsm))))
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step
                    :id 'write :agent 'writer :prompt "write")
                   :driver (gptel-runner-gptel-driver-create)
                   :allow-writes t))
             (call (car (gptel-runner-run-calls run))))
        (should (eq observed t))
        (gptel-runner-abort-call call)))))

(ert-deftest gptel-runner-gptel-paused-buffer-can-return-guided-response ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (node (gptel-runner-node-create :id 'node :kind 'agent))
         (driver (gptel-runner-gptel-driver-create))
         (run (gptel-runner-run-create
               :id "run" :state 'running :driver driver
               :budget (gptel-runner-budget-create)
               :options '(:max-concurrency 1)
               :events nil :queue nil :calls nil :active-count 1
               :active-calls nil))
         returned
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :state 'running :workspace default-directory
                :on-complete (lambda (_call state value)
                               (setq returned (cons state value)))))
         (buffer (gptel-runner-gptel--worker-buffer call)))
    (setf (gptel-runner-run-calls run) (list call)
          (gptel-runner-run-active-calls run) (list call))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-abort) #'ignore))
          (gptel-runner-pause-call call 'test)
          (should (eq (gptel-runner-call-state call) 'waiting-feedback))
          (with-current-buffer buffer
            (should-not (string-match-p "Runner intervention" (buffer-string)))
            (goto-char (point-max))
            (insert (propertize "guided answer" 'gptel 'response)))
          (gptel-runner-complete-call-from-buffer nil call)
          (should (eq (gptel-runner-call-state call) 'succeeded))
          (should (equal returned '(succeeded . "guided answer"))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-completed-call-can-await-feedback ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (node (gptel-runner-node-create :id 'node :kind 'agent))
         (driver (gptel-runner-gptel-driver-create))
         (run (gptel-runner-run-create :id "run"))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :state 'running :workspace default-directory))
         (buffer (gptel-runner-gptel--worker-buffer call))
         (aborts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-abort)
                   (lambda (&rest _arguments) (cl-incf aborts))))
          (gptel-runner-driver-await-feedback driver call)
          (should (zerop aborts))
          (with-current-buffer buffer
            (should-not (string-match-p "Runner intervention" (buffer-string)))
            (should (string-match-p
                     "Feedback" (nth 1 header-line-format)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-confirmed-tool-resumes-call-and-fsm ()
  (let* ((tool-runs 0)
         (backend (gptel-make-openai "runner-test" :host "runner.test"
                                     :models '(runner-test)))
         (tool (gptel-make-tool
                :name "runner_test_tool"
                :description "Return a deterministic test result."
                :function (lambda () (cl-incf tool-runs) "tool result")
                :args nil :confirm t :include t))
         (agent (gptel-runner-agent-create
                 :name 'a :preset nil :workspace-mode 'write))
         (node (gptel-runner-node-create :id 'node :kind 'agent))
         (run (gptel-runner-run-create
               :id "run" :state 'running
               :budget (gptel-runner-budget-create)
               :events nil :options '(:max-concurrency 1)))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :state 'running :workspace default-directory))
         (buffer (gptel-runner-gptel--worker-buffer call))
         (fsm (gptel-runner-gptel--make-fsm))
         (position (with-current-buffer buffer
                     (setq-local gptel-confirm-tool-calls t)
                     (copy-marker (point-max) t)))
         (info (list :backend backend :buffer buffer
                     :position position :tools (list tool)
                     :tool-use (list '(:name "runner_test_tool" :args nil))
                     :data (list :messages []) :context call)))
    (setf (gptel-runner-call-fsm call) fsm
          (gptel-fsm-state fsm) 'TOOL
          (gptel-fsm-info fsm) info)
    (plist-put
     info :callback
     (lambda (response callback-info &optional raw)
       (gptel-runner-gptel--callback
        call
        (lambda (type data)
          (gptel-runner--call-observe call 0 type data))
        response callback-info raw)))
    (unwind-protect
        (let ((gptel-confirm-tool-calls t))
          (should (functionp (plist-get info :callback)))
          (should (plist-get info :backend))
          (cl-letf (((symbol-function 'gptel--handle-wait) #'ignore))
            (gptel--handle-tool-use fsm)
            (should (= tool-runs 0))
            (should (memq 'waiting-confirmation
                          (mapcar #'gptel-runner-event-type
                                  (gptel-runner-run-events run))))
            (should (eq (gptel-runner-call-state call)
                        'waiting-confirmation))
            (with-current-buffer buffer
              (let ((overlay
                     (cl-find-if (lambda (candidate)
                                   (overlay-get candidate 'gptel-tool))
                                 (overlays-in (point-min) (point-max)))))
                (should (overlayp overlay))
                (goto-char (max (overlay-start overlay)
                                (1- (overlay-end overlay))))
                (call-interactively #'gptel--accept-tool-calls)))
            (should (= tool-runs 1))
            (should (eq (gptel-fsm-state fsm) 'WAIT))
            (should (= (length (plist-get (plist-get info :data) :messages)) 1))
            (should (eq (gptel-runner-call-state call) 'running))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest gptel-runner-gptel-dashboard-and-chat-share-tool-approval ()
  (dolist (action '(dashboard native pause))
    (gptel-runner-gptel-test--with-transport
      (let* ((gptel-confirm-tool-calls t)
             (executions 0) finish
             (gptel-tools
              (list (gptel-make-tool
                     :name "runner_async_read" :description "Read delayed test evidence."
                     :args nil :async t :include t
                     :function (lambda (done) (cl-incf executions) (setq finish done))))))
        (gptel-runner-register-agent 'reader :preset nil)
        (let* ((run (gptel-runner-start
                     (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                     :completion 'response :driver (gptel-runner-gptel-driver-create)))
               (call (car (gptel-runner-run-calls run)))
               (fsm (pop requests)))
          (gptel-runner-gptel-test--reply
           fsm nil "tool_calls"
           [(:id "read-1" :type "function" :function (:name "runner_async_read" :arguments "{}"))])
          (with-current-buffer (gptel-runner-call-buffer call)
            (let* ((interactions (gptel-runner-gptel-pending-interactions call))
                   (id (plist-get (car interactions) :id))
                   (overlay (cl-find-if (lambda (ov) (overlay-get ov 'gptel-tool))
                                        (overlays-in (point-min) (point-max))))
                   (calls (overlay-get overlay 'gptel-tool)))
              (should (= (length interactions) 1))
              (should (equal (plist-get (car interactions) :tools)
                             '((:name "runner_async_read" :args nil))))
              (pcase action
                ('dashboard
                 (save-restriction
                   ;; The preview may be outside the user's visible subtree.
                   (narrow-to-region (point-min) (point-min))
                   (save-window-excursion
                     (with-current-buffer (gptel-runner-review-tools call)
                       (unwind-protect
                           (call-interactively (key-binding (kbd "a")))
                         (kill-buffer (current-buffer)))))))
                ('native (gptel--accept-tool-calls calls overlay))
                ('pause (gptel-runner-pause-call call)))
              (should-not (overlay-buffer overlay))
              (should-not (gptel-runner-gptel-pending-interactions call))
              (should-error (gptel-runner-gptel-answer-interaction call id t) :type 'user-error)
              (should-error (gptel--accept-tool-calls calls overlay) :type 'user-error)
              (should-not requests)
              (if (eq action 'pause) (should (zerop executions))
                (should (= executions 1))
                (should (eq (gptel-fsm-state fsm) 'TOOL))
                (funcall finish "Evidence")
                (should (equal requests (list fsm)))))))))))

(ert-deftest gptel-runner-gptel-headless-approval-is-data-and-consumed-once ()
  (gptel-runner-gptel-test--with-transport
    (let* ((gptel-runner-headless t)
           (gptel-confirm-tool-calls t)
           interaction
           (gptel-runner-interaction-hook
            (list (lambda (_call pending) (setq interaction pending)))))
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (call (car (gptel-runner-run-calls run)))
             (fsm (pop requests)))
        (cl-letf (((symbol-function 'gptel--display-tool-calls)
                   (lambda (&rest _) (error "Unexpected local tool UI"))))
          (gptel-runner-gptel-test--reply
           fsm nil "tool_calls"
           [(:id "read-1" :type "function"
                 :function (:name "runner_read" :arguments "{}"))]))
        (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
        (should (eq (plist-get interaction :kind) 'tool-approval))
        (should (equal (plist-get interaction :tools)
                       '((:name "runner_read" :args nil))))
        (should (equal interaction (read (prin1-to-string interaction))))
        (gptel-runner-gptel-answer-interaction call (plist-get interaction :id) t)
        (should (eq (pop requests) fsm))
        (should-error
         (gptel-runner-gptel-answer-interaction call (plist-get interaction :id) t)
         :type 'user-error)
        (gptel-runner-gptel-test--reply fsm "Reviewed" "stop")
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-batch-validates-entire-snapshot-before-execution ()
  (with-temp-buffer
    (let* ((call (gptel-runner-call-create :buffer (current-buffer) :generation 3))
           (a (list :description '(:id "a" :generation 3) :buffer (current-buffer)))
           (b (list :description '(:id "b" :generation 3 :outside-access t)
                    :buffer (current-buffer)))
           answered)
      (gptel-runner-gptel--set-interactions call (list a b))
      (cl-letf (((symbol-function 'gptel-runner-gptel-answer-interaction)
                 (lambda (_call id _approve &optional outside)
                   (push (list id (and outside t)) answered)
                   ;; Simulate a new request appearing during execution.
                   (gptel-runner-gptel--set-interactions
                    call (append (gptel-runner-gptel--interactions call)
                                 (list (list :description '(:id "new" :generation 3)
                                             :buffer (current-buffer))))))))
        (should-error (gptel-runner-gptel-approve-interactions call '("a" "missing"))
                      :type 'user-error)
        (should-not answered)
        (should-error (gptel-runner-gptel-approve-interactions call '("a" "a"))
                      :type 'user-error)
        (gptel-runner-gptel-approve-interactions call '("a" "b"))
        (should (equal (reverse answered) '(("a" nil) ("b" nil))))))))

(ert-deftest gptel-runner-gptel-headless-rejection-and-stale-approval ()
  (dolist (action '(reject pause stale))
    (gptel-runner-gptel-test--with-transport
      (let* ((gptel-runner-headless t)
             (gptel-confirm-tool-calls t)
             interaction
             (gptel-runner-interaction-hook
              (list (lambda (_call pending) (setq interaction pending)))))
        (gptel-runner-register-agent 'reader :preset nil)
        (let* ((run (gptel-runner-start
                     (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                     :driver (gptel-runner-gptel-driver-create)))
               (call (car (gptel-runner-run-calls run)))
               (fsm (pop requests)))
          (gptel-runner-gptel-test--reply
           fsm nil "tool_calls"
           [(:id "read-1" :type "function"
                 :function (:name "runner_read" :arguments "{}"))])
          (pcase action
            ('reject
             (gptel-runner-gptel-answer-interaction call (plist-get interaction :id) nil)
             (should (eq (gptel-runner-run-state run) 'cancelled)))
            ('pause (gptel-runner-pause-call call))
            ('stale (cl-incf (gptel-runner-call-generation call))))
          (should-error
           (gptel-runner-gptel-answer-interaction call (plist-get interaction :id) t)
           :type 'user-error)
          (should-not requests))))))

(ert-deftest gptel-runner-gptel-headless-feedback-waits-for-explicit-accept ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-runner-headless t))
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :pause-after t :save-as 'answer)
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-gptel-test--reply (pop requests) "Initial answer" "stop")
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (with-current-buffer (gptel-runner-call-buffer call)
          (should (string-match-p "Feedback" (nth 1 header-line-format)))
          (should-not (string-match-p "background dashboard" (buffer-string))))
        (gptel-runner-gptel-feedback call "Please check the cancellation path")
        (let ((fsm (pop requests)))
          (should fsm)
          (should (string-match-p
                   "Please check the cancellation path"
                   (prin1-to-string (plist-get (gptel-fsm-info fsm) :data))))
          (should-error (gptel-runner-complete-call-from-buffer "Too early" call)
                        :type 'user-error)
          (gptel-runner-gptel-test--reply fsm "Corrected answer" "stop"))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should (eq (gptel-runner-run-state run) 'running))
        (should-not (gptel-runner-get run 'answer))
        (should (= (gptel-runner-run-active-count run) 0))
        (gptel-runner-complete-call-from-buffer nil call)
        (should (equal (gptel-runner-get run 'answer) "Corrected answer"))
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-headless-feedback-can-pause-and-restart ()
  (gptel-runner-gptel-test--with-transport
    (let ((gptel-runner-headless t))
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :pause-after t)
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-gptel-test--reply (pop requests) "Initial" "stop")
        (gptel-runner-gptel-feedback call "First correction")
        (let ((old-fsm (pop requests)))
          (gptel-runner-pause-call call)
          (should-not (plist-get (gptel-runner-call-driver-data call) :feedback-active))
          (should (= (gptel-runner-run-active-count run) 0))
          (gptel-runner-gptel-feedback call "Second correction")
          ;; A stale terminal callback cannot consume the new feedback request.
          (gptel-runner-gptel--feedback-finished call 0 'succeeded old-fsm)
          (should (plist-get (gptel-runner-call-driver-data call) :feedback-active))
          (gptel-runner-gptel-test--reply (pop requests) "Revised" "stop"))
        (gptel-runner-complete-call-from-buffer nil call)
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(ert-deftest gptel-runner-gptel-headless-feedback-tools-keep-approval-gate ()
  (gptel-runner-gptel-test--with-transport
    (let* ((gptel-runner-headless t)
           (gptel-confirm-tool-calls t)
           interaction
           (gptel-runner-interaction-hook
            (list (lambda (_call pending) (setq interaction pending)))))
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read"
                                            :pause-after t)
                   :completion 'response
                   :driver (gptel-runner-gptel-driver-create)))
             (call (car (gptel-runner-run-calls run)))
             (initial-fsm (pop requests))
             (initial-info (gptel-fsm-info initial-fsm))
             (stale-callback (plist-get initial-info :callback)))
        (gptel-runner-gptel-test--reply initial-fsm "Initial" "stop")
        (gptel-runner-gptel-feedback call "Inspect the evidence")
        (let ((fsm (pop requests)))
          (gptel-runner-gptel-test--reply
           fsm nil "tool_calls"
           [(:id "read-2" :type "function"
                 :function (:name "runner_read" :arguments "{}"))])
          (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
          (let ((current-id (plist-get interaction :id)))
            (funcall stale-callback
                     (cons 'tool-call (list (list (car gptel-tools) nil #'ignore)))
                     initial-info)
            (should (equal (plist-get interaction :id) current-id)))
          (gptel-runner-gptel-answer-interaction call (plist-get interaction :id) t)
          (should (eq (pop requests) fsm))
          (gptel-runner-gptel-test--reply fsm "Revised" "stop"))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (gptel-runner-complete-call-from-buffer nil call)
        (should (eq (gptel-runner-run-state run) 'succeeded))))))

(provide 'gptel-runner-gptel-test)
;;; gptel-runner-gptel-test.el ends here
