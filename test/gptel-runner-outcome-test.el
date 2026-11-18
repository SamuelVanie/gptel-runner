;;; gptel-runner-outcome-test.el --- Public node inspection tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)

(defmacro gptel-runner-outcome-test--isolated (&rest body)
  "Run BODY with isolated registries and snapshot storage."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner-snapshot-directory
          (make-temp-file "gptel-runner-outcomes-" t)))
     (unwind-protect (progn ,@body)
       (maphash (lambda (run _coordinator)
                  (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators)
       (delete-directory gptel-runner-snapshot-directory t))))

(defun gptel-runner-outcome-test--save (run)
  "Save RUN synchronously for testing and return its snapshot filename."
  (gptel-runner-save-run run)
  (gptel-runner-store-flush-pending)
  (should (eq (gptel-runner-store-save-status run) 'clean))
  (gptel-runner-run-snapshot-file run))

(ert-deftest gptel-runner-public-node-traversal ()
  (gptel-runner-outcome-test--isolated
    (gptel-runner-defworkflow inspectable ()
      (gptel-runner-sequence
       :id 'root
       (gptel-runner-branch
        :id 'choice :predicate #'ignore
        :then (gptel-runner-agent-step :id "agent" :agent 'unused :prompt "work")
        :else (gptel-runner-repeat-until
               :id 'loop :max 2
               :body (gptel-runner-elisp-step :id 'local :function #'ignore)))
       (gptel-runner-try
        :id 'guard
        :body (gptel-runner-parallel :id 'fan)
        :catch (gptel-runner-elisp-step :id 'catch :function #'ignore)
        :finally (gptel-runner-elisp-step :id 'cleanup :function #'ignore))))
    (let* ((workflow (gethash 'inspectable gptel-runner--workflows))
           (root (gptel-runner-workflow-root workflow))
           (run (gptel-runner-run-create :workflow workflow
                                         :node-states (make-hash-table)))
           (ids '(root choice "agent" loop local guard fan catch cleanup)))
      (dolist (source (list 'inspectable workflow root run))
        (should (equal (gptel-runner-map-nodes #'gptel-runner-node-id source)
                       ids))
        (should (equal (mapcar #'gptel-runner-node-id
                               (gptel-runner-list-nodes source)) ids))
        (should (eq (gptel-runner-find-node source 'root) root))
        (should (equal (gptel-runner-node-id
                        (gptel-runner-find-node source (copy-sequence "agent")))
                       "agent"))
        (should-not (gptel-runner-find-node source 'absent))
        (should-not (gptel-runner-node-path source 'absent))
        (should (equal (mapcar #'gptel-runner-node-id
                               (gptel-runner-node-path source 'local))
                       '(root choice loop local))))
      (should (equal (mapcar #'gptel-runner-node-id
                             (gptel-runner-list-nodes
                              run (lambda (node)
                                    (eq (gptel-runner-node-kind node) 'elisp))))
                     '(local catch cleanup)))
      (let ((branch (gptel-runner-find-node run 'choice)))
        (should (equal (gptel-runner-map-nodes #'gptel-runner-node-id branch)
                       '(choice "agent" loop local))))
      (should (equal (gptel-runner-node-path root root) (list root)))
      (should-error (gptel-runner-node-outcome run 'absent) :type 'user-error)
      (should-error (gptel-runner-node-state run 'absent) :type 'user-error)
      (should-error (gptel-runner-list-nodes 'unknown) :type 'user-error))))

(ert-deftest gptel-runner-outcomes-distinguish-pending-running-nil-and-failure ()
  (gptel-runner-outcome-test--isolated
    (let (resolve reject)
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               :id 'root
               (gptel-runner-elisp-step
                :id 'nil-step :async t
                :function (lambda (_run _node yes _no) (setq resolve yes) nil))
               (gptel-runner-elisp-step
                :id 'failure :async t
                :function (lambda (_run _node _yes no) (setq reject no) nil))
               (gptel-runner-elisp-step :id 'unreached :function #'ignore)))))
        (should (equal (gptel-runner-node-outcome run 'nil-step)
                       '(:node nil-step :state running)))
        (should (equal (gptel-runner-node-outcome run 'failure)
                       '(:node failure :state pending)))
        (should (eq (gptel-runner-node-result run 'nil-step 'missing) 'missing))
        (funcall resolve nil)
        (should (equal (gptel-runner-node-outcome run 'nil-step)
                       '(:node nil-step :state succeeded :value nil)))
        (should-not (gptel-runner-node-result run 'nil-step 'missing))
        (funcall reject '(:type rejected))
        (should (equal (gptel-runner-node-error run 'failure) '(:type elisp-error :error (:type rejected))))
        (should (eq (gptel-runner-node-result run 'failure 'missing) 'missing))
        (should (equal (gptel-runner-node-outcome run 'root)
                       '(:node root :state failed :value (:type elisp-error :error (:type rejected)))))
        (should (eq (gptel-runner-node-state run 'unreached) 'skipped))
        (should (eq (gptel-runner-node-error run 'unreached 'none) 'none))
        ;; Duplicate and late callbacks must not replace accepted outcomes.
        (funcall resolve 'late)
        (funcall reject 'late)
        (should-not (gptel-runner-node-result run 'nil-step 'missing))
        (should (equal (gptel-runner-node-error run 'failure) '(:type elisp-error :error (:type rejected))))))))

(ert-deftest gptel-runner-outcomes-use-accepted-agent-values ()
  (gptel-runner-outcome-test--isolated
    (gptel-runner-register-agent 'worker)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker
                               '(:value "42") '(:value "invalid"))
      (let ((run (gptel-runner-start
                  (gptel-runner-parallel
                   :id 'root :policy 'collect
                   (gptel-runner-agent-step
                    :id 'parsed :agent 'worker :prompt "work"
                    :parser #'string-to-number)
                   (gptel-runner-agent-step
                    :id 'invalid :agent 'worker :prompt "work"
                    :validator #'ignore))
                  :driver driver)))
        (should (= (gptel-runner-node-result run 'parsed) 42))
        (should (eq (plist-get (gptel-runner-node-error run 'invalid) :reason)
                    'validation))
        (should (equal (gptel-runner-call-result
                        (car (gptel-runner-run-calls run))) "42"))
        (should (equal (car (gptel-runner-node-result run 'root))
                       (gptel-runner-node-outcome run 'parsed)))))))

(ert-deftest gptel-runner-outcomes-survive-recovery-without-calls-or-save-as ()
  (gptel-runner-outcome-test--isolated
    (gptel-runner-register-agent 'worker)
    (gptel-runner-defworkflow durable-outcomes ()
      (gptel-runner-parallel
       :id 'root :policy 'collect
       (gptel-runner-parallel
        :id 'completed :policy 'collect
        (gptel-runner-agent-step
         :id 'agent :agent 'worker :prompt "work" :parser #'string-to-number)
        (gptel-runner-elisp-step :id 'nil-value :function #'ignore)
        (gptel-runner-sequence :id 'empty)
        (gptel-runner-branch
         :id 'choice :predicate (lambda (_run) t)
         :then (gptel-runner-elisp-step
                :id 'selected :function (lambda (_run _node) 17))
         :else (gptel-runner-elisp-step :id 'skipped :function #'ignore))
        (gptel-runner-repeat-until
         :id 'loop :max 1
         :until (lambda (run) (= (gptel-runner-iteration run 'loop) 1))
         :body (gptel-runner-elisp-step
                :id 'body :function (lambda (_run _node) 'loop-value)))
        (gptel-runner-try
         :id 'try
         :body (gptel-runner-elisp-step
                :id 'failed :async t
                :function (lambda (_run _node _resolve reject)
                            (funcall reject nil)))
         :catch (gptel-runner-elisp-step :id 'handler :function #'ignore)
         :finally (gptel-runner-elisp-step
                   :id 'cleanup :function (lambda (_run _node) 'cleaned))))
       (gptel-runner-agent-step :id 'wait :agent 'worker :prompt "work")))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "42") '(:manual t))
      (let* ((run (gptel-runner-start 'durable-outcomes :driver driver))
             (before (gptel-runner-map-nodes
                      (lambda (node) (gptel-runner-node-outcome run node)) run))
             (value (gptel-runner-node-result run 'completed)))
        (should (equal (gptel-runner-node-outcome run 'failed)
                       '(:node failed :state failed :value (:type elisp-error :error nil))))
        (should (equal (gptel-runner-node-error run 'failed 'missing)
                       '(:type elisp-error :error nil)))
        (should (equal (gptel-runner-node-result run 'choice) 17))
        (should (eq (gptel-runner-node-result run 'loop) 'loop-value))
        (should-not (gptel-runner-node-result run 'try 'missing))
        (should-not (gptel-runner-node-result run 'empty 'missing))
        (gptel-runner-pause-run run)
        (let ((file (gptel-runner-outcome-test--save run)))
          (remhash (gptel-runner-run-id run) gptel-runner--runs)
          (let* ((resumed-driver (gptel-runner-fake-driver-create))
                 (restored (gptel-runner-load-run file nil resumed-driver)))
            (should-not (gptel-runner-run-calls restored))
            (should (equal before
                           (gptel-runner-map-nodes
                            (lambda (node)
                              (gptel-runner-node-outcome restored node)) restored)))
            (gptel-runner-fake-queue resumed-driver 'worker '(:value "done"))
            (gptel-runner-resume-run restored)
            (should (eq (gptel-runner-run-state restored) 'succeeded))
            (should (= (length (gptel-runner-run-calls restored)) 1))
            (should (equal (plist-get (car (gptel-runner-node-result
                                           restored 'root)) :value) value))
            (should (equal (gptel-runner-node-result restored 'completed) value))
            ;; Saved failures inside the completed container remain reportable.
            (should (equal (gptel-runner-node-outcome restored 'failed)
                           '(:node failed :state failed :value (:type elisp-error :error nil))))))))))

(ert-deftest gptel-runner-outcomes-reset-for-repeat-retry-and-continuation ()
  (gptel-runner-outcome-test--isolated
    (let ((executions 0) fail)
      (gptel-runner-defworkflow reset-outcomes ()
        (gptel-runner-sequence
         :id 'root
         (gptel-runner-elisp-step
          :id 'before :function (lambda (_run _node) (cl-incf executions)))
         (gptel-runner-repeat-until
          :id 'loop :max 2
          :until (lambda (run) (= (gptel-runner-iteration run 'loop) 2))
          :body (gptel-runner-elisp-step
                 :id 'body
                 :function (lambda (run node)
                             (should (eq (gptel-runner-node-result run node 'none)
                                         'none))
                             (1+ (gptel-runner-iteration run 'loop)))))
         (gptel-runner-elisp-step
          :id 'last
          :function (lambda (run node)
                      (should-not (plist-member
                                   (gptel-runner-node-outcome run node) :value))
                      (if fail (error "retry me") 'last)))))
      (setq fail t)
      (let ((run (gptel-runner-start 'reset-outcomes)))
        (should (gptel-runner-node-error run 'last))
        (should (= (gptel-runner-node-result run 'body) 2))
        (setq fail nil)
        (gptel-runner-retry run)
        (should (= executions 1))
        (should (eq (gptel-runner-node-result run 'last) 'last))
        (should-not (gptel-runner-node-error run 'last))
        (gptel-runner-continue run "again")
        (should (= executions 2))
        (should (= (gptel-runner-node-result run 'body) 2))
        (should-not (gptel-runner-node-result run 'root 'missing))))))

(ert-deftest gptel-runner-outcomes-targeted-retry-preserves-independent-values ()
  (gptel-runner-outcome-test--isolated
    (let (resolve (fail t))
      (let ((run (gptel-runner-start
                  (gptel-runner-parallel
                   :id 'root
                   (gptel-runner-elisp-step
                    :id 'keep :function (lambda (_run _node) 'kept))
                   (gptel-runner-sequence
                    :id 'sequence
                    (gptel-runner-elisp-step
                     :id 'rerun :async t
                     :function (lambda (_run _node yes _no)
                                 (setq resolve yes) nil))
                    (gptel-runner-elisp-step
                     :id 'after :function (lambda (_run _node)
                                           (if fail (error "retry me") 'after))))))))
        (funcall resolve 'first)
        (setq fail nil)
        (gptel-runner-retry run :from-node 'rerun)
        (should (eq (gptel-runner-node-result run 'keep) 'kept))
        (dolist (id '(root sequence rerun after))
          (should-not (plist-member (gptel-runner-node-outcome run id) :value)))
        (should (eq (gptel-runner-node-state run 'after) 'pending))
        (funcall resolve 'second)
        (should (eq (gptel-runner-node-result run 'rerun) 'second))))))

(ert-deftest gptel-runner-outcomes-older-snapshots-report-unknown-values ()
  (gptel-runner-outcome-test--isolated
    (gptel-runner-defworkflow legacy-outcomes ()
      (gptel-runner-elisp-step :id 'nil-value :function #'ignore))
    (let* ((run (gptel-runner-start 'legacy-outcomes))
           (file (gptel-runner-outcome-test--save run))
           (snapshot (gptel-runner-store--read-file file)))
      (setf (plist-get snapshot :version) 2
            (plist-get (plist-get snapshot :run) :node-outcomes) nil)
      (with-temp-file file (prin1 snapshot (current-buffer)))
      (remhash (gptel-runner-run-id run) gptel-runner--runs)
      (let ((restored (gptel-runner-load-run file)))
        (should (equal (gptel-runner-node-outcome restored 'nil-value)
                       '(:node nil-value :state succeeded)))
        (should (eq (gptel-runner-node-result restored 'nil-value 'unknown) 'unknown))))))

(ert-deftest gptel-runner-outcomes-cancellation-terminalizes-entered-containers ()
  (gptel-runner-outcome-test--isolated
    (let (resolve)
      (let ((run (gptel-runner-start
                  (gptel-runner-try
                   :id 'guard
                   :body (gptel-runner-elisp-step
                          :id 'work :async t
                          :function (lambda (_run _node yes _no)
                                      (setq resolve yes) nil))
                   :finally (gptel-runner-elisp-step
                             :id 'cleanup :function #'ignore)))))
        (gptel-runner-abort-run run)
        (should (eq (gptel-runner-node-state run 'guard) 'cancelled))
        (should (eq (gptel-runner-node-error run 'guard) 'user))
        (should (eq (gptel-runner-node-state run 'work) 'cancelled))
        (should (equal (gptel-runner-node-outcome run 'cleanup)
                       '(:node cleanup :state pending)))
        (funcall resolve 'late)
        (should (eq (gptel-runner-node-state run 'work) 'cancelled))))))

(ert-deftest gptel-runner-outcomes-unreadable-values-preserve-checkpoint ()
  (gptel-runner-outcome-test--isolated
    (let ((value 'readable))
      (gptel-runner-defworkflow unreadable-outcomes ()
        (gptel-runner-elisp-step
         :id 'work :function (lambda (_run _node) value)))
      (let* ((run (gptel-runner-start 'unreadable-outcomes))
             (file (gptel-runner-outcome-test--save run)))
        (setq value (current-buffer))
        (gptel-runner-continue run "again")
        (gptel-runner-save-run run)
        (gptel-runner-store-flush-pending)
        (should (eq (gptel-runner-store-save-status run) 'error))
        (let* ((snapshot (gptel-runner-store--read-file file))
               (entry (assq 'work (plist-get (plist-get snapshot :run)
                                            :node-outcomes))))
          (should (eq (plist-get (cdr entry) :value) 'readable)))
        (let ((event (car (last (gptel-runner-run-events run)))))
          (should (eq (gptel-runner-event-type event) 'snapshot-error))
          (should (string-match-p "node-outcomes entry work"
                                  (prin1-to-string
                                   (gptel-runner-event-data event)))))))))

(provide 'gptel-runner-outcome-test)
;;; gptel-runner-outcome-test.el ends here
