;;; gptel-runner-background-core-test.el --- Background runtime contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-background-core-test--isolated (&rest body)
  "Run BODY with fresh runtime registries and external lock hooks."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner-call-admission-functions nil)
         (gptel-runner-call-release-hook nil))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run) (gptel-runner-abort-run run))
                gptel-runner--runs))))

(ert-deftest gptel-runner-background-inputs-visible-before-dispatch ()
  (dolist (hashp '(nil t))
    (gptel-runner-background-core-test--isolated
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((value (list "original"))
             (inputs (if hashp (make-hash-table :test #'equal)
                       (list (cons 'request value))))
             (driver (gptel-runner-fake-driver-create)))
        (when hashp (puthash 'request value inputs))
        (gptel-runner-fake-queue driver 'reader '(:manual t))
        (let* ((run
                (gptel-runner-start
                 (gptel-runner-agent-step
                  :id 'read :agent 'reader
                  :context (lambda (run _node)
                             (format "Context: %s" (car (gptel-runner-get run 'request))))
                  :prompt (lambda (run _node)
                            (format "Input: %s" (car (gptel-runner-get run 'request)))))
                 :inputs inputs :driver driver))
               (call (car (gptel-runner-run-calls run))))
          (setcar value "mutated")
          (should (equal (gptel-runner-get run 'request) '("original")))
          (should (string-match-p "Context: original" (gptel-runner-call-prompt call)))
          (should (string-match-p "Input: original" (gptel-runner-call-prompt call))))))))

(ert-deftest gptel-runner-background-invalid-inputs-fail-before-dispatch ()
  (gptel-runner-background-core-test--isolated
    (gptel-runner-register-agent 'reader :preset nil)
    (let ((driver (gptel-runner-fake-driver-create))
          (root (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")))
      (dolist (inputs '(42 (:key "value") ((key . value) . bad)
                          ((gptel-runner-decisions . overridden))))
        (should-error (gptel-runner-start root :inputs inputs :driver driver)
                      :type 'user-error))
      (should-not (gptel-runner-fake-driver-starts driver))
      (should (= (hash-table-count gptel-runner--runs) 0)))))

(ert-deftest gptel-runner-background-workflow-workspace-default-and-override ()
  (gptel-runner-background-core-test--isolated
    (gptel-runner-register-agent 'reader :preset nil)
    (let* ((root-dir (make-temp-file "runner-workspace-default-" t))
           (default-dir (expand-file-name "default" root-dir))
           (override-dir (expand-file-name "override" root-dir))
           (driver (gptel-runner-fake-driver-create))
           (workflow
            (gptel-runner-workflow-create
             :name 'background :options (list :workspace default-dir)
             :root (gptel-runner-agent-step
                    :id 'read :agent 'reader :workspace "child"
                    :prompt #'gptel-runner-node-workspace))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "child" default-dir) t)
            (make-directory (expand-file-name "child" override-dir) t)
            (dotimes (_ 2) (gptel-runner-fake-queue driver 'reader '(:value "done")))
            (let ((a (gptel-runner-start workflow :driver driver))
                  (b (gptel-runner-start workflow :workspace override-dir :driver driver)))
              (should (equal (gptel-runner-run-workspace a)
                             (file-name-as-directory (file-truename default-dir))))
              (should (equal (gptel-runner-call-workspace (car (gptel-runner-run-calls b)))
                             (file-name-as-directory
                              (file-truename (expand-file-name "child" override-dir))))))
            (should-error (gptel-runner-start workflow :workspace 42 :driver driver)
                          :type 'user-error)
            (should-error (gptel-runner-start workflow :workspace "" :driver driver)
                          :type 'user-error)
            (should-error (gptel-runner-start workflow :workspace "/not/a/runner/workspace"
                                               :driver driver)
                          :type 'user-error))
        (delete-directory root-dir t)))))

(ert-deftest gptel-runner-background-admission-queues-and-releases-once ()
  (gptel-runner-background-core-test--isolated
    (gptel-runner-register-agent 'writer :preset nil :workspace-mode 'write)
    (let* ((driver (gptel-runner-fake-driver-create))
           (allowed nil) (held nil) (releases 0)
           (gptel-runner-call-admission-functions
            (list (lambda (call) (when allowed (setq held call)))))
           (gptel-runner-call-release-hook
            (list (lambda (call)
                    (when (eq call held) (setq held nil) (cl-incf releases))))))
      (gptel-runner-fake-queue driver 'writer '(:manual t :value "done"))
      (let* ((run (gptel-runner-start
                   (gptel-runner-agent-step :id 'write :agent 'writer :prompt "Write")
                   :allow-writes t :driver driver))
             (call (car (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-call-state call) 'ready))
        (should-not (gptel-runner-fake-driver-starts driver))
        (setq allowed t)
        (gptel-runner--drain-queue run)
        (should (eq held call))
        (gptel-runner-fake-release driver call)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should-not held)
        (should (= releases 1))
        (gptel-runner--deactivate-call call)
        (should (= releases 1))))))

(ert-deftest gptel-runner-background-admission-rolls-back-partial-acquisition ()
  (gptel-runner-background-core-test--isolated
    (let* (held
           (gptel-runner-call-admission-functions
            (list (lambda (call) (setq held call)) (lambda (_call) nil)))
           (gptel-runner-call-release-hook (list (lambda (_call) (setq held nil))))
           (call (gptel-runner-call-create)))
      (should-not (gptel-runner--admit-call call))
      (should-not held))))

(ert-deftest gptel-runner-background-release-on-pause-and-cancel ()
  (dolist (action '(pause cancel))
    (gptel-runner-background-core-test--isolated
      (gptel-runner-register-agent 'reader :preset nil)
      (let* ((driver (gptel-runner-fake-driver-create))
             held
             (gptel-runner-call-admission-functions (list (lambda (call) (setq held call))))
             (gptel-runner-call-release-hook (list (lambda (_call) (setq held nil)))))
        (gptel-runner-fake-queue driver 'reader '(:manual t))
        (let* ((run (gptel-runner-start
                     (gptel-runner-agent-step :id 'read :agent 'reader :prompt "Read")
                     :driver driver))
               (call (car (gptel-runner-run-calls run))))
          (should (eq held call))
          (if (eq action 'pause) (gptel-runner-pause-call call)
            (gptel-runner-abort-call call))
          (should-not held))))))

(ert-deftest gptel-runner-background-interaction-rounds-do-not-resurface ()
  (require 'gptel-runner-jobs)
  (let* ((call (gptel-runner-call-create
                :id "call-1" :generation 2 :state 'waiting-confirmation
                :buffer (current-buffer)
                :driver-data '(:pending-interaction
                               (:description (:id "current" :generation 2)))))
         (gptel-runner-jobs--run (gptel-runner-run-create :calls (list call)))
         (gptel-runner-jobs--invocation '(:epoch 3)))
    (should (gptel-runner-jobs--interaction-current-p
             '(:id "current" :call-id "call-1" :generation 2 :epoch 3)))
    (should-not (gptel-runner-jobs--interaction-current-p
                 '(:id "previous" :call-id "call-1" :generation 2 :epoch 3)))
    (should-not (gptel-runner-jobs--interaction-current-p
                 '(:id "current" :call-id "call-1" :generation 2 :epoch 1)))
    (setf (gptel-runner-call-driver-data call) nil)
    (should-not (gptel-runner-jobs--interaction-current-p
                 '(:id "current" :call-id "call-1" :generation 2 :epoch 3)))))

(provide 'gptel-runner-background-core-test)
;;; gptel-runner-background-core-test.el ends here
