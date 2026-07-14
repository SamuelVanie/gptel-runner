;;; gptel-runner-gptel-test.el --- Adapter contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-request)
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

(provide 'gptel-runner-gptel-test)
;;; gptel-runner-gptel-test.el ends here
