;;; gptel-runner-gptel-test.el --- Adapter contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-openai)
(require 'gptel-runner)

(ert-deftest gptel-runner-gptel-supported-api-shape ()
  (should (gptel-runner-gptel--check-api))
  (should (fboundp 'gptel--apply-preset))
  (should (fboundp 'gptel--fsm-transition))
  (should (boundp 'gptel-request--transitions))
  (should (boundp 'gptel-request--handlers)))

(ert-deftest gptel-runner-gptel-fsm-replaces-only-terminal-handlers ()
  (let* ((fsm (gptel-runner-gptel--make-fsm))
         (handlers (gptel-fsm-handlers fsm)))
    (should (equal (alist-get 'WAIT handlers)
                   (alist-get 'WAIT gptel-request--handlers)))
    (should (equal (alist-get 'TOOL handlers)
                   (alist-get 'TOOL gptel-request--handlers)))
    (should (equal (alist-get 'DONE handlers)
                   (list #'gptel-runner-gptel--done)))
    (should (equal (alist-get 'ERRS handlers)
                   (list #'gptel-runner-gptel--error)))
    (should (equal (alist-get 'ABRT handlers)
                   (list #'gptel-runner-gptel--aborted)))))

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

(ert-deftest gptel-runner-gptel-confirmed-tool-resumes-call-and-fsm ()
  (let* ((tool-runs 0)
         (backend (gptel-make-openai "runner-test" :models '(runner-test)))
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
                     :data nil :context call)))
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
          (cl-letf (((symbol-function 'gptel--handle-tool-result)
                     (lambda (machine)
                       (let ((machine-info (gptel-fsm-info machine)))
                         (funcall (plist-get machine-info :callback)
                                  (cons 'tool-result
                                        (plist-get machine-info :tool-result))
                                  machine-info)
                         (setf (gptel-fsm-state machine) 'WAIT)))))
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
            (should (eq (gptel-runner-call-state call) 'running))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'gptel-runner-gptel-test)
;;; gptel-runner-gptel-test.el ends here
