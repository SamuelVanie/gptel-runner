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
      (gptel-runner-gptel-test--reply fsm "Complete proposal" "stop")
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
       (gptel-runner-gptel-test--reply fsm (symbol-name id) "stop")
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

(ert-deftest gptel-runner-gptel-supported-api-shape ()
  (should (gptel-runner-gptel--check-api))
  (should (fboundp 'gptel--apply-preset))
  (should (fboundp 'gptel--fsm-transition))
  (should (boundp 'gptel-request--transitions))
  (should (boundp 'gptel-request--handlers)))

(ert-deftest gptel-runner-gptel-fsm-adds-status-and-replaces-terminals ()
  (let* ((fsm (gptel-runner-gptel--make-fsm))
         (handlers (gptel-fsm-handlers fsm)))
    (should (equal (alist-get 'WAIT handlers)
                   (append (list #'gptel-runner-gptel--prepare-request)
                           (alist-get 'WAIT gptel-request--handlers)
                           (list #'gptel--update-wait))))
    (should (equal (alist-get 'TOOL handlers)
                   (append (list #'gptel-runner-gptel--tool-started
                                 #'gptel--update-tool-call)
                           (alist-get 'TOOL gptel-request--handlers)
                           (list #'gptel--update-tool-ask))))
    (should (equal (alist-get 'DONE handlers)
                   (list #'gptel-runner-gptel--done)))
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
                :id "call" :run run :node node
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

(ert-deftest gptel-runner-gptel-worker-is-private-and-local ()
  (let* ((agent (gptel-runner-agent-create :name 'a :preset nil))
         (run (gptel-runner-run-create :id "run"))
         (node (gptel-runner-node-create :id 'node))
         (call (gptel-runner-call-create
                :id "call" :agent agent :run run :node node
                :workspace default-directory))
         (buffer (gptel-runner-gptel--worker-buffer call)))
    (unwind-protect
        (progn
          (should (string-prefix-p "*gptel-runner:" (buffer-name buffer)))
          (should (eq (buffer-local-value 'gptel-runner--call buffer) call))
          (should (equal (buffer-local-value 'default-directory buffer)
                         default-directory))
          (with-current-buffer buffer
            (should (string-match-p "Runner call: call" (buffer-string)))
            (should (string-match-p "User prompt" (buffer-string)))))
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
            (should (string-match-p "Runner intervention" (buffer-string)))
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
            (should (string-match-p "Runner intervention" (buffer-string)))
            (should (string-match-p
                     "waiting for your feedback" (buffer-string)))))
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

(provide 'gptel-runner-gptel-test)
;;; gptel-runner-gptel-test.el ends here
