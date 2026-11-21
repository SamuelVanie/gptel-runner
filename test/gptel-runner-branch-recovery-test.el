;;; gptel-runner-branch-recovery-test.el --- Branch checkpoints -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-branch-recovery-test--isolated (&rest body)
  "Run BODY with private registries and a temporary snapshot directory."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner-default-driver nil)
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner-snapshot-directory (make-temp-file "runner-branch-" t)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_id run) (gptel-runner-abort-run run))
                gptel-runner--runs)
       (maphash (lambda (run _coordinator) (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(defun gptel-runner-branch-recovery-test--increment (run key)
  "Increment RUN's counter at KEY and return its new value."
  (gptel-runner-put run key (1+ (gptel-runner-get run key 0))))

(defun gptel-runner-branch-recovery-test--workflow (arm finish)
  "Register a workflow selecting ARM before preparing and invoking FINISH.
Preparation changes the predicate's answer.  FINISH is an async Elisp function."
  (let* ((chosen
          (gptel-runner-sequence
           :id 'chosen
           (gptel-runner-elisp-step
            :id 'prepare :save-as 'prepared
            :function (lambda (run _node)
                        (gptel-runner-branch-recovery-test--increment run 'preparations)
                        t))
           (gptel-runner-elisp-step
            :id 'finish :async t :save-as 'finished :function finish)))
         (other
          (gptel-runner-elisp-step
           :id 'other :function (lambda (run _node)
                                  (gptel-runner-put run 'wrong-arm t))))
         (workflow
          (gptel-runner-workflow-create
           :name 'branch-recovery
           :root (gptel-runner-branch
                  :id 'choice
                  :predicate (lambda (run)
                               (gptel-runner-branch-recovery-test--increment
                                run 'predicates)
                               (if (eq arm :then)
                                   (not (gptel-runner-get run 'prepared))
                                 (gptel-runner-get run 'prepared)))
                  :then (if (eq arm :then) chosen other)
                  :else (if (eq arm :else) chosen other)))))
    (puthash 'branch-recovery workflow gptel-runner--workflows)
    workflow))

(defun gptel-runner-branch-recovery-test--resume (arm reload)
  "Check resuming ARM with completed preparation, reloading when RELOAD is set."
  (gptel-runner-branch-recovery-test--isolated
    (let (resolve)
      (let* ((workflow
              (gptel-runner-branch-recovery-test--workflow
               arm (lambda (run _node succeed _reject)
                     (gptel-runner-branch-recovery-test--increment run 'starts)
                     (setq resolve succeed)
                     nil)))
             (run (gptel-runner-start workflow))
             (old-resolve resolve))
        (should (eq (gptel-runner-run-state run) 'running))
        (gptel-runner-pause-run run)
        (when reload
          (gptel-runner-store-flush-pending)
          (remhash (gptel-runner-run-id run) gptel-runner--runs)
          (setq run (gptel-runner-load-run (gptel-runner-run-snapshot-file run))))
        (gptel-runner-resume-run run)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= (gptel-runner-get run 'starts) 2))
        (should (= (gptel-runner-get run 'preparations) 1))
        (should (= (gptel-runner-get run 'predicates) 1))
        (should-not (gptel-runner-get run 'wrong-arm))
        (should (eq (gptel-runner-node-state run 'other) 'skipped))
        ;; A late completion from the suspended attempt must not finish the run.
        (funcall old-resolve 'stale)
        (should (eq (gptel-runner-run-state run) 'running))
        (funcall resolve 'done)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (eq (gptel-runner-get run 'finished) 'done))
        (should (eq (plist-get (gptel-runner-node-outcome run 'finish) :value)
                    'done))))))

(ert-deftest gptel-runner-branch-recovery-pause-resume-keeps-entered-arm ()
  (dolist (arm '(:then :else))
    (gptel-runner-branch-recovery-test--resume arm nil)))

(ert-deftest gptel-runner-branch-recovery-snapshot-keeps-entered-arm ()
  (dolist (arm '(:then :else))
    (gptel-runner-branch-recovery-test--resume arm t)))

(ert-deftest gptel-runner-branch-recovery-retry-keeps-entered-arm ()
  (dolist (mode '(retry continue targeted))
    (gptel-runner-branch-recovery-test--isolated
      (let (resolve reject)
        (let ((run
               (gptel-runner-start
                (gptel-runner-branch-recovery-test--workflow
                 :then (lambda (run _node succeed fail)
                         (gptel-runner-branch-recovery-test--increment run 'starts)
                         (setq resolve succeed reject fail)
                         nil)))))
          (funcall reject 'failed)
          (should (eq (gptel-runner-run-state run) 'failed))
          (pcase mode
            ('retry (gptel-runner-retry run))
            ('continue (gptel-runner-continue run "Finish the prepared work"))
            ('targeted (gptel-runner-retry run :from-node 'finish)))
          (should (eq (gptel-runner-run-state run) 'running))
          (should (= (gptel-runner-get run 'starts) 2))
          (should (= (gptel-runner-get run 'preparations) 1))
          (should (= (gptel-runner-get run 'predicates) 1))
          (should-not (gptel-runner-get run 'wrong-arm))
          (funcall resolve 'done)
          (should (eq (gptel-runner-run-state run) 'succeeded)))))))

(ert-deftest gptel-runner-branch-recovery-repeat-resumes-before-first-success ()
  (gptel-runner-branch-recovery-test--isolated
    (let ((predicates 0) (starts 0) resolve)
      (gptel-runner-defworkflow branch-in-repeat nil
        (gptel-runner-repeat-until
         :id 'loop :max 1 :until (lambda (run) (gptel-runner-get run 'finished))
         :body (gptel-runner-branch
                :id 'choice
                :predicate (lambda (run)
                             (cl-incf predicates)
                             (not (gptel-runner-get run 'prepared)))
                :then (gptel-runner-elisp-step
                       :id 'finish :async t :save-as 'finished
                       :function (lambda (run _node succeed _reject)
                                   (cl-incf starts)
                                   (gptel-runner-put run 'prepared t)
                                   (setq resolve succeed)
                                   nil))
                :else (gptel-runner-elisp-step :id 'other :function #'ignore))))
      (let ((run (gptel-runner-start 'branch-in-repeat)))
        (gptel-runner-pause-run run)
        (gptel-runner-resume-run run)
        (should (eq (gptel-runner-run-state run) 'running))
        (should (= predicates 1))
        (should (= starts 2))
        (funcall resolve t)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-iteration run 'loop) 1))))))

(ert-deftest gptel-runner-branch-recovery-fresh-repeat-reevaluates ()
  (dolist (extend '(nil t))
    (gptel-runner-branch-recovery-test--isolated
      (let ((predicates 0) arms)
        (let ((run
               (gptel-runner-start
                (gptel-runner-repeat-until
                 :id 'loop :max (if extend 1 2)
                 :until (lambda (run) (= (gptel-runner-iteration run 'loop) 2))
                 :body (gptel-runner-branch
                        :id 'choice
                        :predicate (lambda (run)
                                     (cl-incf predicates)
                                     (zerop (gptel-runner-iteration run 'loop)))
                        :then (gptel-runner-elisp-step
                               :function (lambda (_run _node) (push :then arms)))
                        :else (gptel-runner-elisp-step
                               :function (lambda (_run _node) (push :else arms))))))))
          (when extend
            (should (eq (gptel-runner-run-state run) 'failed))
            (gptel-runner-extend-repeat run 'loop 1))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= predicates 2))
          (should (equal (reverse arms) '(:then :else))))))))

(ert-deftest gptel-runner-branch-recovery-new-cycle-reevaluates ()
  (dolist (mode '(continue rerun))
    (gptel-runner-branch-recovery-test--isolated
      (let ((selected t) (predicates 0) arms)
        (let ((run
               (gptel-runner-start
                (gptel-runner-branch
                 :predicate (lambda (_run) (cl-incf predicates) selected)
                 :then (gptel-runner-elisp-step
                        :function (lambda (_run _node) (push :then arms)))
                 :else (gptel-runner-elisp-step
                        :function (lambda (_run _node) (push :else arms)))))))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (setq selected nil)
          (if (eq mode 'continue)
              (gptel-runner-continue run "Use the other arm")
            (gptel-runner-rerun run))
          (should (eq (gptel-runner-run-state run) 'succeeded))
          (should (= predicates 2))
          (should (equal (reverse arms) '(:then :else))))))))

(ert-deftest gptel-runner-branch-recovery-targeted-retry-resets-later-branch ()
  (gptel-runner-branch-recovery-test--isolated
    (let ((preparations 0) (predicates 0) (attempts 0) arms)
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               (gptel-runner-elisp-step
                :id 'prepare :save-as 'first
                :function (lambda (_run _node) (= (cl-incf preparations) 1)))
               (gptel-runner-branch
                :predicate (lambda (run)
                             (cl-incf predicates)
                             (gptel-runner-get run 'first))
                :then (gptel-runner-elisp-step
                       :function (lambda (_run _node) (push :then arms)))
                :else (gptel-runner-elisp-step
                       :function (lambda (_run _node) (push :else arms))))
               (gptel-runner-elisp-step
                :function (lambda (_run _node)
                            (when (= (cl-incf attempts) 1) (error "Retry"))))))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (gptel-runner-retry run :from-node 'prepare)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= predicates 2))
        (should (equal (reverse arms) '(:then :else)))))))

(ert-deftest gptel-runner-branch-recovery-failed-predicate-can-be-retried ()
  (gptel-runner-branch-recovery-test--isolated
    (let ((predicates 0) completed)
      (let ((run
             (gptel-runner-start
              (gptel-runner-branch
               :predicate (lambda (_run)
                            (when (= (cl-incf predicates) 1) (error "Retry"))
                            t)
               :then (gptel-runner-elisp-step
                      :function (lambda (_run _node) (setq completed t)))))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (gptel-runner-retry run)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should completed)
        (should (= predicates 2))))))

(provide 'gptel-runner-branch-recovery-test)
;;; gptel-runner-branch-recovery-test.el ends here
