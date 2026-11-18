;;; gptel-runner-jobs-test.el --- Background process integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Real batch Emacs processes, fake providers, and private temporary stores.

;;; Code:

(require 'ert)
(require 'gptel-runner)

(defvar gptel-runner-jobs-test--processes nil)

(defun gptel-runner-jobs-test--launch (invocation)
  "Launch INVOCATION in a real child Emacs for testing."
  (let ((process
         (make-process
          :name (concat "runner-test-" (plist-get invocation :id))
          :buffer (generate-new-buffer " *runner worker test*")
          :connection-type 'pipe :noquery t
          :command (list gptel-runner-job-emacs-program "-Q" "--batch"
                         "-L" gptel-runner-jobs--package-directory
                         "-l" "gptel-runner-jobs"
                         "-f" "gptel-runner-jobs-batch-main"
                         "--" "worker" (plist-get invocation :directory)))))
    (push process gptel-runner-jobs-test--processes)
    process))

(defmacro gptel-runner-jobs-test--isolated (&rest body)
  "Run BODY with a private durable job store and child process cleanup."
  (declare (indent 0) (debug t))
  `(let ((gptel-runner-job-directory (make-temp-file "runner-jobs-test-" t))
         (gptel-runner-job-launch-function #'gptel-runner-jobs-test--launch)
         (gptel-runner-job-preload-features nil)
         (gptel-runner-jobs-test--processes nil))
     (unwind-protect
         (progn ,@body)
       (dolist (process gptel-runner-jobs-test--processes)
         (when (process-live-p process) (delete-process process))
         (when (buffer-live-p (process-buffer process))
           (kill-buffer (process-buffer process))))
       (delete-directory gptel-runner-job-directory t))))

(defun gptel-runner-jobs-test--job (&optional workflow name init arguments)
  "Create a job for WORKFLOW with optional NAME, INIT, and ARGUMENTS."
  (gptel-runner-job-save
   (list :name (or name "Test pipeline")
         :workflow (or workflow 'background-example)
         :init-file (or init (expand-file-name "examples/background-jobs.el"
                                               gptel-runner-jobs--package-directory))
         :arguments (append arguments
                            (list :workspace gptel-runner-job-directory
                                  :goal "Exercise isolated pipelines"
                                  :inputs '((subject . "parallel processes")))))))

(defun gptel-runner-jobs-test--wait (invocation predicate &optional timeout)
  "Wait until PREDICATE accepts INVOCATION, within TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10))) record)
    (while (and (progn
                  (setq record (gptel-runner-invocation-get (plist-get invocation :id)))
                  (not (funcall predicate record)))
                (< (float-time) deadline))
      (accept-process-output nil 0.03))
    (unless (funcall predicate record)
      (ert-fail
       (list :last-manifest record
             :worker-output
             (mapcar (lambda (process)
                       (with-current-buffer (process-buffer process) (buffer-string)))
                     gptel-runner-jobs-test--processes))))
    record))

(defun gptel-runner-jobs-test--finished (invocation)
  "Wait for INVOCATION's worker to finish committing its history."
  (let ((record (gptel-runner-jobs-test--wait
                 invocation (lambda (record) (eq (plist-get record :phase) 'closed)))))
    (when (plist-get record :error)
      (message "Test worker error: %s" (plist-get record :error)))
    record))

(defun gptel-runner-jobs-test--input (invocation)
  "Wait for INVOCATION to have a call waiting for feedback."
  (gptel-runner-jobs-test--wait
   invocation (lambda (record)
                (cl-some (lambda (call) (eq (plist-get call :state) 'waiting-feedback))
                         (plist-get record :calls)))))

(defun gptel-runner-jobs-test--command (invocation operation &optional args)
  "Send OPERATION with ARGS to INVOCATION and await its receipt."
  (let* ((receipt (gptel-runner-job-command invocation operation args))
         (deadline (+ (float-time) 8)))
    (while (and (memq (plist-get receipt :status) '(queued processing))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (setq receipt (gptel-runner-job-command-status invocation (plist-get receipt :id))))
    (should (eq (plist-get receipt :status) 'applied))
    receipt))

(ert-deftest gptel-runner-jobs-independent-processes-retain-results ()
  (gptel-runner-jobs-test--isolated
   (let* ((local-count (hash-table-count gptel-runner--runs))
          (first (gptel-runner-job-run (gptel-runner-jobs-test--job nil "First")))
          (second (gptel-runner-job-run (gptel-runner-jobs-test--job nil "Second"))))
     (setq first (gptel-runner-jobs-test--finished first)
           second (gptel-runner-jobs-test--finished second))
     (should (eq (plist-get first :state) 'succeeded))
     (should (eq (plist-get second :state) 'succeeded))
     (should-not (equal (plist-get first :id) (plist-get second :id)))
     (should-not (equal (plist-get (plist-get first :worker) :pid)
                        (plist-get (plist-get second :worker) :pid)))
     ;; Loading history must not register executable runs in this Emacs.
     (should (= local-count (hash-table-count gptel-runner--runs)))
     (should (= 2 (length (gptel-runner-invocation-list))))
     (dolist (record (list first second))
       (let* ((calls (plist-get record :calls))
              (a (car calls)) (b (cadr calls))
              (outputs (gptel-runner-invocation-outputs record))
              (artifact (car (plist-get record :artifacts))))
         (should (= 3 (length calls)))
         (should (< (max (plist-get a :started-at) (plist-get b :started-at))
                    (min (plist-get a :finished-at) (plist-get b :finished-at))))
         (should (string-match-p "parallel processes" (alist-get 'report outputs)))
         (should (plist-get (gptel-runner-invocation-call record a) :result))
         (should (file-exists-p (expand-file-name (plist-get artifact :file)
                                                  (plist-get record :directory)))))))))

(ert-deftest gptel-runner-jobs-forget-preserves-history-and-reveals-new-generation ()
  (gptel-runner-jobs-test--isolated
   (let* ((job (gptel-runner-jobs-test--job))
          (run (gptel-runner-jobs-test--finished (gptel-runner-job-run job)))
          (id (plist-get run :id))
          (checkpoint (expand-file-name "checkpoint.el" (plist-get run :directory)))
          (outputs (gptel-runner-invocation-outputs run)))
     (gptel-runner-invocation-forget id)
     (should (gptel-runner-invocation-forgotten-p (gptel-runner-invocation-get id)))
     (should (= (length (gptel-runner-invocation-list)) 1))
     (should (file-exists-p checkpoint))
     (should (equal outputs (gptel-runner-invocation-outputs run)))
     (should (equal job (gptel-runner-job-get (plist-get job :id))))
     (gptel-runner-jobs-test--command run 'continue '(:observation "Check again"))
     (setq run (gptel-runner-jobs-test--finished run))
     (should-not (gptel-runner-invocation-forgotten-p run))
     (gptel-runner-invocation-forget run t)
     (should-not (file-exists-p checkpoint))
     (should (gptel-runner-invocation-outputs run))
     (should (gptel-runner-invocation-call run (car (plist-get run :calls))))
     (dolist (artifact (gptel-runner-artifacts run))
       (should (file-exists-p (gptel-runner-artifact-file run artifact)))))))

(ert-deftest gptel-runner-jobs-rerun-finished-invocation ()
  (gptel-runner-jobs-test--isolated
   (let* ((job (gptel-runner-jobs-test--job 'artifact-pipeline "Rerun report"
                (expand-file-name "examples/artifact-pipeline.el" gptel-runner-jobs--package-directory)))
          (first (gptel-runner-jobs-test--finished (gptel-runner-job-run job))))
     (should (eq (plist-get first :state) 'succeeded))
     (gptel-runner-jobs-test--command first 'rerun)
     (let ((second (gptel-runner-jobs-test--finished first)))
       (should (equal (plist-get first :id) (plist-get second :id)))
       (should (eq (plist-get second :state) 'succeeded))
       (should (= (plist-get second :epoch) (1+ (plist-get first :epoch))))
       (should (= (length (gptel-runner-artifacts second)) 2))
       (should (equal (gptel-runner-get first 'report-text)
                      (gptel-runner-get second 'report-text)))))))

(ert-deftest gptel-runner-jobs-forget-rechecks-state-and-generation-under-lock ()
  (let ((old '(:id "invocation-1" :epoch 1 :phase closed :root "/tmp/"))
        locked)
    (dolist (fresh '((:id "invocation-1" :epoch 2 :phase closed)
                     (:id "invocation-1" :epoch 1 :phase starting)
                     (:id "invocation-1" :epoch 1 :phase open :state paused)))
      (cl-letf (((symbol-function 'gptel-runner-jobs--with-control-lock)
                 (lambda (_run function)
                   (setq locked t)
                   (unwind-protect (funcall function) (setq locked nil))))
                ((symbol-function 'gptel-runner-invocation-get)
                 (lambda (_id) (should locked) fresh))
                ((symbol-function 'delete-file)
                 (lambda (&rest _) (ert-fail "Deleted an active checkpoint")))
                ((symbol-function 'gptel-runner-jobs--write)
                 (lambda (&rest _) (ert-fail "Replaced an active manifest"))))
        (should-error (gptel-runner-invocation-forget old t) :type 'user-error)))))

(ert-deftest gptel-runner-jobs-forget-requires-a-confirmed-stop ()
  (dolist (owner-state '(live dead unknown))
    (cl-letf (((symbol-function 'gptel-runner-jobs--lock-owner-state)
               (lambda (_owner) owner-state)))
      (dolist (phase '(starting open closing closed))
        (should (eq (and (gptel-runner-invocation-forgettable-p
                          (list :phase phase)) t)
                    (and (or (eq phase 'closed)
                             (and (memq phase '(open closing))
                                  (eq owner-state 'dead))) t)))))))

(ert-deftest gptel-runner-jobs-overlap-pause-feedback-and-restart ()
  (gptel-runner-jobs-test--isolated
   (let* ((job (gptel-runner-jobs-test--job 'background-example-feedback))
          (invocation (gptel-runner-job-run job)))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (should (eq (plist-get (gptel-runner-job-run job) :state) 'skipped))
     (gptel-runner-jobs-test--command invocation 'decision '(:text "Keep results" :reason "Test"))
     (gptel-runner-jobs-test--command invocation 'pause)
     (gptel-runner-jobs-test--wait invocation (lambda (r) (eq (plist-get r :state) 'paused)))
     (should (eq (plist-get (gptel-runner-job-run job) :state) 'skipped))
     (gptel-runner-jobs-test--command invocation 'resume '(:feedback "Revisit the report"))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (let ((call (car (last (plist-get invocation :calls)))))
       (gptel-runner-jobs-test--command
        invocation 'accept (list :call-id (plist-get call :id) :call-epoch 1 :result "Accepted report")))
     (setq invocation (gptel-runner-jobs-test--finished invocation))
     (should (eq (plist-get invocation :state) 'succeeded))
     (should (equal (alist-get 'report (gptel-runner-invocation-outputs invocation)) "Accepted report"))
     (let ((old-calls (length (plist-get invocation :calls))))
       (gptel-runner-jobs-test--command invocation 'continue
                                        '(:observation "Try a second approach" :reset-budget t))
       (setq invocation (gptel-runner-jobs-test--input invocation))
       (should (= 2 (plist-get invocation :epoch)))
       (should (> (length (plist-get invocation :calls)) old-calls))
       (should-error (gptel-runner-job-command invocation 'accept
                                               '(:call-id "call-2" :call-epoch 1 :result "Stale")))
       (gptel-runner-jobs-test--command invocation 'cancel)
       (should (eq (plist-get (gptel-runner-jobs-test--finished invocation) :state) 'cancelled))))))

(ert-deftest gptel-runner-jobs-crash-recovery-invalidates-commands ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-job-run (gptel-runner-jobs-test--job 'background-example-feedback))))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (let ((receipt-file (gptel-runner-jobs--receipt-file invocation "ambiguous")))
       (gptel-runner-jobs--write receipt-file '(:id "ambiguous" :status processing :epoch 1)))
     (delete-process (car gptel-runner-jobs-test--processes))
     (accept-process-output nil 0.1)
     (should (eq (plist-get (car (gptel-runner-invocation-list)) :state) 'interrupted))
     (gptel-runner-jobs-test--command invocation 'resume)
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (should (= 2 (plist-get invocation :epoch)))
     (should (eq (plist-get (gptel-runner-job-command-status invocation "ambiguous") :status)
                 'indeterminate))
     (gptel-runner-jobs-test--command invocation 'cancel)
     (gptel-runner-jobs-test--finished invocation))))

(ert-deftest gptel-runner-jobs-retry-after-terminal-worker-exit ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-job-run (gptel-runner-jobs-test--job 'background-example-feedback))))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (gptel-runner-jobs-test--command invocation 'cancel)
     (setq invocation (gptel-runner-jobs-test--finished invocation))
     (gptel-runner-jobs-test--command invocation 'retry '(:from-node summary :feedback "Try again"))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (should (= 2 (plist-get invocation :epoch)))
     (gptel-runner-jobs-test--command invocation 'cancel)
     (gptel-runner-jobs-test--finished invocation))))

(ert-deftest gptel-runner-jobs-durable-command-flush-failure-is-not-rejected ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (gptel-runner-jobs--invocation invocation)
          (id "storage-failure")
          (command (list :id id :invocation (plist-get invocation :id)
                         :epoch 1 :operation 'resume))
          applied)
     (gptel-runner-jobs--write
      (expand-file-name (concat "commands/" id ".el") (plist-get invocation :directory)) command)
     (cl-letf (((symbol-function 'gptel-runner-jobs--apply-command)
                (lambda (_command) (setq applied t)))
               ((symbol-function 'gptel-runner-jobs--flush)
                (lambda () (error "Disk is full"))))
       (should-error (gptel-runner-jobs--commands)))
     (should applied)
     (should (eq (plist-get (gptel-runner-job-command-status invocation id) :status) 'processing))
     (gptel-runner-jobs--abandoned-receipts)
     (should (eq (plist-get (gptel-runner-job-command-status invocation id) :status) 'indeterminate)))))

(ert-deftest gptel-runner-jobs-invalid-input-and-startup-failure ()
  (gptel-runner-jobs-test--isolated
   (should-error (gptel-runner-jobs-test--job nil nil nil '(:unknown t)))
   (let ((init (expand-file-name "broken-init.el" gptel-runner-job-directory)))
     (with-temp-file init (insert "(error \"Test initialization failure\")\n"))
     (let ((invocation (gptel-runner-job-run (gptel-runner-jobs-test--job nil nil init))))
       (setq invocation (gptel-runner-jobs-test--finished invocation))
       (should (eq (plist-get invocation :state) 'startup-error))
       (should (string-match-p "initialization failure" (plist-get invocation :error)))
       (should-not (file-exists-p (gptel-runner-jobs--job-lock (plist-get invocation :job-id))))))))

(ert-deftest gptel-runner-jobs-writers-serialize-across-processes ()
  (gptel-runner-jobs-test--isolated
   (let ((init (expand-file-name "writer-init.el" gptel-runner-job-directory)))
     (gptel-runner-jobs--write
      init
      '(progn
         (require 'gptel-runner)
         (gptel-runner-register-agent 'job-writer :workspace-mode 'write)
         (gptel-runner-defworkflow job-writer-workflow nil
				   (gptel-runner-agent-step :id 'write :agent 'job-writer :prompt "write" :save-as 'result))
         (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
         (gptel-runner-fake-queue gptel-runner-default-driver 'job-writer '(:delay 0.8 :value "written"))))
     (let* ((first (gptel-runner-job-run
                    (gptel-runner-jobs-test--job 'job-writer-workflow "Writer one" init '(:allow-writes t))))
            (second (gptel-runner-job-run
                     (gptel-runner-jobs-test--job 'job-writer-workflow "Writer two" init '(:allow-writes t))))
            (a (car (plist-get (gptel-runner-jobs-test--finished first) :calls)))
            (b (car (plist-get (gptel-runner-jobs-test--finished second) :calls))))
       (should (eq (plist-get a :state) 'succeeded))
       (should (eq (plist-get b :state) 'succeeded))
       (should (or (<= (plist-get a :finished-at) (plist-get b :started-at))
                   (<= (plist-get b :finished-at) (plist-get a :started-at))))))))

(ert-deftest gptel-runner-jobs-command-order-is-submission-order ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (gptel-runner-jobs--invocation invocation)
          (ids '("z-first" "a-second")) operations)
     (dolist (entry '(("z-first" 1 pause) ("a-second" 2 resume)))
       (gptel-runner-jobs--write
        (expand-file-name (concat "commands/" (car entry) ".el") (plist-get invocation :directory))
        (list :id (car entry) :sequence (cadr entry) :operation (caddr entry))))
     (cl-letf (((symbol-function 'gptel-runner-jobs--apply-command)
                (lambda (command) (push (plist-get command :operation) operations)))
               ((symbol-function 'gptel-runner-jobs--flush) #'ignore))
       (gptel-runner-jobs--commands))
     (should (equal (nreverse operations) '(pause resume)))
     (dolist (id ids)
       (should (eq (plist-get (gptel-runner-job-command-status invocation id) :status) 'applied))))))

(ert-deftest gptel-runner-jobs-restart-write-failure-releases-reservation ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id)))
          (write (symbol-function 'gptel-runner-jobs--write)))
     (gptel-runner-jobs--unlock lock (plist-get (plist-get invocation :worker) :token))
     (setq invocation (plist-put invocation :phase 'closed))
     (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)
     (gptel-runner-jobs--write (expand-file-name "checkpoint.el" (plist-get invocation :directory)) nil)
     (cl-letf (((symbol-function 'gptel-runner-jobs--write)
                (lambda (file data)
                  (if (string-match-p "/receipts/" file) (error "Injected write failure")
                    (funcall write file data)))))
       (should-error (gptel-runner-job-command invocation 'resume)))
     (should-not (file-exists-p lock)))))

(ert-deftest gptel-runner-jobs-synchronous-input-fails-without-hanging ()
  (gptel-runner-jobs-test--isolated
   (let ((init (expand-file-name "prompt-init.el" gptel-runner-job-directory)))
     (gptel-runner-jobs--write
      init
      '(progn
         (require 'gptel-runner)
         (when (or (bound-and-true-p native-comp-enable-subr-trampolines)
                   (bound-and-true-p native-comp-jit-compilation))
           (error "Worker must not depend on native compilation"))
         (gptel-runner-register-agent 'job-prompt)
         (gptel-runner-defworkflow job-prompt-workflow nil
				   (gptel-runner-agent-step :id 'ask :agent 'job-prompt :prompt "ask"))
         (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
         (gptel-runner-fake-queue gptel-runner-default-driver 'job-prompt
                                  (lambda (_call) (read-string "Unsupported prompt: ")))))
     (let* ((invocation (gptel-runner-job-run
                         (gptel-runner-jobs-test--job 'job-prompt-workflow nil init)))
            (record (gptel-runner-jobs-test--finished invocation)))
       (should (eq (plist-get record :state) 'failed))
       (should (string-match-p
                "asynchronous input adapter"
                (format "%S" (gptel-runner-invocation-call record (car (plist-get record :calls))))))))))

(ert-deftest gptel-runner-jobs-extend-budgets-and-repeat-after-exit ()
  (gptel-runner-jobs-test--isolated
   (let ((init (expand-file-name "budgets-init.el" gptel-runner-job-directory)))
     (gptel-runner-jobs--write
      init
      '(progn
         (require 'gptel-runner)
         (gptel-runner-register-agent 'job-budget-agent)
         (gptel-runner-defworkflow job-budget-workflow (:max-calls 1)
				   (gptel-runner-sequence
				    :id 'sequence
				    (gptel-runner-agent-step :id 'first :agent 'job-budget-agent :prompt "first")
				    (gptel-runner-agent-step :id 'second :agent 'job-budget-agent :prompt "second")))
         (defun job-test-repeat-done (run)
           (>= (gptel-runner-iteration run 'cycle) 2))
         (gptel-runner-defworkflow job-repeat-workflow nil
				   (gptel-runner-repeat-until
				    :id 'cycle :max 1 :until #'job-test-repeat-done
				    :body (gptel-runner-agent-step :id 'step :agent 'job-budget-agent :prompt "repeat")))
         (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
         (dotimes (_ 8)
           (gptel-runner-fake-queue gptel-runner-default-driver 'job-budget-agent '(:value "done")))))
     (dolist (spec '((job-budget-workflow extend (:additional-calls 1))
                     (job-repeat-workflow extend-repeat (:node cycle :additional-iterations 1))))
       (let* ((invocation (gptel-runner-job-run
                           (gptel-runner-jobs-test--job (car spec) nil init))))
         (setq invocation (gptel-runner-jobs-test--finished invocation))
         (should (eq (plist-get invocation :state) 'failed))
         (gptel-runner-jobs-test--command invocation (cadr spec) (caddr spec))
         (setq invocation (gptel-runner-jobs-test--finished invocation))
         (should (= 2 (plist-get invocation :epoch)))
         (should (eq (plist-get invocation :state) 'succeeded)))))))

(ert-deftest gptel-runner-jobs-call-pause-accept-and-cancel ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-job-run (gptel-runner-jobs-test--job 'background-example-feedback))))
     (setq invocation
           (gptel-runner-jobs-test--wait
            invocation (lambda (record) (cl-some (lambda (call) (eq (plist-get call :state) 'running))
                                                 (plist-get record :calls)))))
     (let* ((call (car (plist-get invocation :calls)))
            (args (list :call-id (plist-get call :id) :call-epoch 1)))
       (gptel-runner-jobs-test--command invocation 'pause-call args)
       (gptel-runner-jobs-test--command invocation 'accept (append args '(:result "Human analysis"))))
     (setq invocation (gptel-runner-jobs-test--input invocation))
     (let ((call (car (last (plist-get invocation :calls)))))
       (gptel-runner-jobs-test--command invocation 'cancel-call
                                        (list :call-id (plist-get call :id) :call-epoch 1)))
     (should (eq (plist-get (gptel-runner-jobs-test--finished invocation) :state) 'cancelled)))))

(ert-deftest gptel-runner-jobs-owner-publication-error-can-recover ()
  (gptel-runner-jobs-test--isolated
   (let* ((directory (expand-file-name "test-lock" gptel-runner-job-directory))
          (owner (gptel-runner-jobs--owner "test-token" "test-invocation"))
          (write (symbol-function 'gptel-runner-jobs--write)))
     (cl-letf (((symbol-function 'gptel-runner-jobs--write)
                (lambda (file data)
                  (if (string-suffix-p "/owner.el" file) (signal 'file-error '("Disk full"))
                    (funcall write file data)))))
       (should-error (gptel-runner-jobs--lock directory owner)))
     (should-not (file-exists-p directory))
     (should (gptel-runner-jobs--lock directory owner))
     (gptel-runner-jobs--unlock directory "test-token"))))

(ert-deftest gptel-runner-jobs-final-manifest-error-returns-nonzero ()
  (gptel-runner-jobs-test--isolated
   (let* ((gptel-runner--runs (make-hash-table :test #'equal))
          (gptel-runner--workflows (make-hash-table :test #'eq))
          (gptel-runner--agents (make-hash-table :test #'eq))
          (gptel-runner-default-driver nil)
          (invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (write (symbol-function 'gptel-runner-jobs--write))
          injected)
     (cl-letf (((symbol-function 'gptel-runner-jobs--write)
                (lambda (file data)
                  (if (and (not injected) (string-suffix-p "/manifest.el" file)
                           (eq (plist-get data :phase) 'closed))
                      (progn (setq injected t) (signal 'file-error '("Final commit failed")))
                    (funcall write file data)))))
       (should (= 2 (gptel-runner-jobs-worker (plist-get invocation :directory)))))
     (should injected)
     (should (eq (plist-get (gptel-runner-invocation-get (plist-get invocation :id)) :state)
                 'storage-error)))))

(ert-deftest gptel-runner-jobs-journal-error-retains-uncommitted-events ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (gptel-runner-jobs--invocation invocation)
          (gptel-runner-jobs--run
           (gptel-runner-run-create :id "test-run" :state 'running
                                    :workflow (gptel-runner-workflow-create
                                               :root (gptel-runner-sequence :id 'root))
                                    :node-states (make-hash-table :test #'equal)
                                    :blackboard (make-hash-table :test #'equal)))
          (gptel-runner-jobs--exports (make-hash-table :test #'equal))
          (gptel-runner-jobs--pending-events '((:type third) (:type second) (:type first)))
          (write (symbol-function 'write-region))
          (appends 0))
     (cl-letf (((symbol-function 'write-region)
                (lambda (start end filename &rest args)
                  (when (string-suffix-p "/events.el" filename)
                    (cl-incf appends)
                    (when (= appends 2) (signal 'file-error '("Journal full"))))
                  (apply write start end filename args))))
       (should-error (gptel-runner-jobs--publish)))
     (should (equal gptel-runner-jobs--pending-events '((:type third) (:type second))))
     (gptel-runner-jobs--publish)
     (should-not gptel-runner-jobs--pending-events)
     (with-temp-buffer
       (insert-file-contents (expand-file-name "events.el" (plist-get invocation :directory)))
       (goto-char (point-min))
       (should (equal (read (current-buffer)) '(:type first)))
       (should (equal (read (current-buffer)) '(:type second)))
       (should (equal (read (current-buffer)) '(:type third)))))))

(ert-deftest gptel-runner-jobs-uncertain-launch-keeps-reservation ()
  (gptel-runner-jobs-test--isolated
   (let* ((invocation (gptel-runner-jobs--new-invocation (gptel-runner-jobs-test--job) 'test))
          (lock (gptel-runner-jobs--job-lock (plist-get invocation :job-id))))
     (gptel-runner-jobs--launch-failed invocation '(gptel-runner-launchd-cleanup-error "Unconfirmed"))
     (setq invocation (gptel-runner-invocation-get (plist-get invocation :id)))
     (should (eq (plist-get invocation :state) 'launch-uncertain))
     (should (eq (gptel-runner-jobs--lock-owner-state
                  (gptel-runner-jobs--read (expand-file-name "owner.el" lock))) 'unknown))
     (should-not (gptel-runner-jobs--lock lock (gptel-runner-jobs--owner "other" "other")))
     (gptel-runner-jobs--confirm-stopped invocation)
     (should-not (file-exists-p lock))
     (should (eq (plist-get (gptel-runner-invocation-get (plist-get invocation :id)) :phase) 'closed)))))

(ert-deftest gptel-runner-jobs-command-channel-rejects-executable-arguments ()
  (let (called)
    (cl-letf (((symbol-function 'gptel-runner-extend)
               (lambda (&rest _arguments) (setq called t))))
      (should-error
       (gptel-runner-jobs--apply-command
        '(:operation extend :arguments (:additional-calls 1 :callback ignore))))
      (should-error
       (gptel-runner-job-command nil 'continue
                                 '(:observation "work" :callback (lambda (_run) (error "executed"))))))
    (should-not called)
    (should-error (gptel-runner-jobs--validate-command-arguments
                   'resume '(:feedback (lambda () "not data"))))))

(ert-deftest gptel-runner-jobs-worker-only-workspace-default-is-honored ()
  (gptel-runner-jobs-test--isolated
   (let* ((init (expand-file-name "workspace-init.el" gptel-runner-job-directory))
          (nested (expand-file-name "pipeline/nested" gptel-runner-job-directory))
          job invocation)
     (make-directory nested t)
     (should-not (gethash 'job-inherited-workspace gptel-runner--workflows))
     (gptel-runner-jobs--write
      init
      '(progn
         (require 'gptel-runner)
         (gptel-runner-register-agent 'job-workspace-agent)
         (gptel-runner-defworkflow job-inherited-workspace (:workspace "pipeline")
				   (gptel-runner-agent-step :id 'inspect :agent 'job-workspace-agent
							    :workspace "nested" :prompt "inspect" :save-as 'directory))
         (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
         (gptel-runner-fake-queue
          gptel-runner-default-driver 'job-workspace-agent
          (lambda (call) (list :value (gptel-runner-call-workspace call))))))
     (let ((default-directory (file-name-as-directory gptel-runner-job-directory)))
       (setq job (gptel-runner-job-save
                  (list :name "Worker-defined workspace" :workflow 'job-inherited-workspace
                        :init-file init))))
     (should-not (plist-member (plist-get job :arguments) :workspace))
     ;; Changing the launcher's directory must not affect either default.
     (let ((default-directory "/"))
       (setq invocation (gptel-runner-job-run job)))
     (setq invocation (gptel-runner-jobs-test--finished invocation))
     (should (eq (plist-get invocation :state) 'succeeded))
     (should (equal (alist-get 'directory (gptel-runner-invocation-outputs invocation))
                    (file-name-as-directory (file-truename nested))))
     (should-not (gethash 'job-inherited-workspace gptel-runner--workflows)))))

(ert-deftest gptel-runner-jobs-elisp-only-worker-retains-results-and-artifacts ()
  (gptel-runner-jobs-test--isolated
    (let ((init (expand-file-name "elisp-init.el" gptel-runner-job-directory)))
      (gptel-runner-jobs--write
       init
       '(progn
          (require 'gptel-runner)
          (setq gptel-runner-default-driver nil)
          (gptel-runner-defworkflow job-elisp-only nil
            (gptel-runner-sequence
             :id 'pipeline
             (gptel-runner-elisp-step
              :id 'prepare :save-as 'report
              :function (lambda (run _node)
                          (upcase (gptel-runner-get run 'subject))))
             (gptel-runner-elisp-step
              :id 'publish :save-as 'artifact :workspace-mode 'write
              :function (lambda (run _node)
                          (with-temp-file "report.txt"
                            (insert (gptel-runner-get run 'report)))
                          (gptel-runner-publish-artifact
                           run "report" "report.txt" "text/plain")))))))
      (let* ((job (gptel-runner-jobs-test--job
                   'job-elisp-only "Local Lisp" init '(:allow-writes t)))
             (invocation (gptel-runner-jobs-test--finished (gptel-runner-job-run job)))
             (calls (plist-get invocation :calls)))
        (should (eq (plist-get invocation :state) 'succeeded))
        (should (= (length calls) 2))
        (dolist (call calls)
          (should (eq (plist-get call :kind) 'elisp))
          (should-not (plist-get call :agent))
          (should (zerop (plist-get call :attempts))))
        (should (equal (plist-get (gptel-runner-invocation-call invocation (car calls))
                                  :result)
                       "PARALLEL PROCESSES"))
        (should (equal (alist-get 'report (gptel-runner-invocation-outputs invocation))
                       "PARALLEL PROCESSES"))
        (should (= (length (plist-get invocation :artifacts)) 1))))))

(ert-deftest gptel-runner-jobs-finalizer-publishes-after-duration-exhaustion ()
  (gptel-runner-jobs-test--isolated
    (let ((init (expand-file-name "finalize-init.el" gptel-runner-job-directory)))
      (gptel-runner-jobs--write
       init
       '(progn
          (require 'gptel-runner)
          (gptel-runner-defworkflow job-finalizer ()
            (gptel-runner-finalize
             :id 'run :max-duration 5 :max-calls 0 :max-requests 0
             :body (gptel-runner-sequence
                    :id 'body
                    (gptel-runner-elisp-step
                     :id 'prepare :save-as 'partial
                     :function (lambda (&rest _) "partial findings"))
                    (gptel-runner-elisp-step
                     :id 'wait :async t :function (lambda (&rest _) nil)))
             :finally
             (gptel-runner-elisp-step
              :id 'publish :workspace-mode 'write :save-as 'artifact
              :function (lambda (run _node)
                          (with-temp-file "report.txt"
                            (insert (gptel-runner-get run 'partial)))
                          (gptel-runner-publish-artifact
                           run "report" "report.txt" "text/plain")))))))
      (let* ((job (gptel-runner-jobs-test--job
                   'job-finalizer "Partial report" init
                   '(:allow-writes t :max-duration 0.1)))
             (invocation (gptel-runner-jobs-test--finished (gptel-runner-job-run job))))
        (should (eq (plist-get invocation :state) 'failed))
        (should (= (length (plist-get invocation :artifacts)) 1))
        (should (alist-get 'artifact (gptel-runner-invocation-outputs invocation)))
        (should (equal
                 (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name
                     (plist-get (car (plist-get invocation :artifacts)) :file)
                     (plist-get invocation :directory)))
                   (buffer-string))
                 "partial findings"))))))

(ert-deftest gptel-runner-jobs-bootstrap-captures-and-preserves-library-paths ()
  (gptel-runner-jobs-test--isolated
   (let* ((default-directory (file-name-as-directory gptel-runner-job-directory))
          (extra (expand-file-name "extra-libs" default-directory))
          (editor (expand-file-name "editor-libs" default-directory))
          (gptel-runner-job-load-path (list extra))
          (gptel-runner-job-preload-features '(gptel gptel-agent))
          (load-path (append (list nil "editor-libs" extra) load-path))
          job paths)
     (make-directory extra t)
     (make-directory editor t)
     (setq job (gptel-runner-jobs-test--job)
           paths (plist-get job :load-path))
     (should (equal (plist-get job :preload-features) '(gptel gptel-agent)))
     (should (equal (mapcar #'directory-file-name (cl-subseq paths 0 2))
                    (list extra editor)))
     (should (= 1 (cl-count extra paths :key #'directory-file-name :test #'equal)))
     (should (cl-every #'file-name-absolute-p paths))
     (should-not (cl-some (lambda (path)
                           (or (file-equal-p path lisp-directory)
                               (file-in-directory-p path lisp-directory)))
                         paths))
     ;; A scheduler may save state from a fresh Emacs with different defaults.
     (let ((gptel-runner-job-load-path nil)
           (gptel-runner-job-preload-features '(unrelated-package))
           (load-path (list lisp-directory)))
       (setq job (gptel-runner-job-save job))
       (should (equal paths (plist-get job :load-path)))
       (should (equal (plist-get job :preload-features) '(gptel gptel-agent)))
       ;; Partial edits retain the stored bootstrap too.
       (let ((edited (copy-sequence job)))
         (cl-remf edited :load-path)
         (cl-remf edited :preload-features)
         (setq job (gptel-runner-job-save edited)))
       (should (equal paths (plist-get job :load-path)))
       (should (equal (plist-get job :preload-features) '(gptel gptel-agent)))
       ;; Explicit nil means no preloads, not "use the defaults".
       (setq job (gptel-runner-job-save
                  (plist-put (plist-put job :preload-features nil) :load-path nil)))
       (should (plist-member job :preload-features))
       (should-not (plist-get job :preload-features))
       (should (plist-member job :load-path))
       (should-not (plist-get job :load-path))))))

(ert-deftest gptel-runner-jobs-bootstrap-loads-packages-before-every-init ()
  (gptel-runner-jobs-test--isolated
   (let* ((libraries (expand-file-name "libraries" gptel-runner-job-directory))
          (dependencies (expand-file-name "dependencies" gptel-runner-job-directory))
          (gptel-runner-job-load-path (list libraries))
          (gptel-runner-job-preload-features '(gptel gptel-agent))
          (load-path (cons dependencies load-path))
          (parent-features (mapcar #'featurep '(gptel gptel-agent)))
          invocations)
     (gptel-runner-jobs--write
      (expand-file-name "runner-test-bootstrap-dependency.el" dependencies)
      '(provide 'runner-test-bootstrap-dependency))
     (gptel-runner-jobs--write
      (expand-file-name "gptel.el" libraries)
      '(progn
         (require 'runner-test-bootstrap-dependency)
         (provide 'gptel)))
     (gptel-runner-jobs--write
      (expand-file-name "gptel-agent.el" libraries)
      '(progn
         (require 'gptel)
         (defvar gptel-agent-dirs '(bundled))
         (defvar runner-test-bootstrap-update nil)
         (defun gptel-agent-update ()
           (setq runner-test-bootstrap-update
                 (list gptel-agent-dirs (emacs-pid))))
         (provide 'gptel-agent)))
     (dolist (label '(first second))
       (let ((init (expand-file-name (format "%s-init.el" label)
                                     gptel-runner-job-directory)))
         (gptel-runner-jobs--write
          init
          `(progn
             (unless (and (featurep 'gptel) (featurep 'gptel-agent)
                          (featurep 'runner-test-bootstrap-dependency))
               (error "Worker packages were not loaded before init"))
             (unless (equal (car runner-test-bootstrap-update) '(bundled))
               (error "Agent definitions were not refreshed before init"))
             (setq gptel-agent-dirs ',label)
             (require 'gptel-runner-fake)
             (gptel-runner-register-agent 'bootstrap-inspector)
             (gptel-runner-defworkflow bootstrap-example ()
               (gptel-runner-agent-step :id 'inspect :agent 'bootstrap-inspector
                                        :prompt "inspect" :save-as 'bootstrap))
             (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
             (gptel-runner-fake-queue
              gptel-runner-default-driver 'bootstrap-inspector
              (lambda (_call)
                (list :value (prin1-to-string
                              (list runner-test-bootstrap-update gptel-agent-dirs)))))))
         (push (cons label
                     (gptel-runner-job-run
                      (gptel-runner-jobs-test--job 'bootstrap-example
                                                  (symbol-name label) init)))
               invocations)))
     (dolist (entry invocations)
       (let ((invocation (gptel-runner-jobs-test--finished (cdr entry))))
         (should (eq (plist-get invocation :state) 'succeeded))
         (let ((result (read (alist-get 'bootstrap
                                       (gptel-runner-invocation-outputs invocation)))))
           ;; Bootstrap refreshes once, leaving init customizations intact.
           (should (equal (caar result) '(bundled)))
           (should (eq (cadr result) (car entry)))
           (should (= (cadar result) (plist-get (plist-get invocation :worker) :pid)))
           (should-not (= (cadar result) (emacs-pid))))))
     (should (equal parent-features (mapcar #'featurep '(gptel gptel-agent))))
     (should-not (featurep 'runner-test-bootstrap-dependency))
     (should-not (boundp 'runner-test-bootstrap-update)))))

(ert-deftest gptel-runner-jobs-bootstrap-legacy-invocation-keeps-init-loading ()
  (gptel-runner-jobs-test--isolated
   (let* ((gptel-runner-job-preload-features '(gptel gptel-agent))
          (init (expand-file-name "legacy-init.el" gptel-runner-job-directory))
          job invocation)
     (gptel-runner-jobs--write
      init
      '(progn
         (when (or (featurep 'gptel) (featurep 'gptel-agent))
           (error "Legacy invocation unexpectedly preloaded packages"))
         (require 'gptel-runner-fake)
         (gptel-runner-register-agent 'legacy-inspector)
         (gptel-runner-defworkflow legacy-example ()
           (gptel-runner-agent-step :id 'inspect :agent 'legacy-inspector
                                    :prompt "inspect" :save-as 'report))
         (setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
         (gptel-runner-fake-queue gptel-runner-default-driver 'legacy-inspector
                                  '(:value "legacy init controlled loading"))))
     (setq job (gptel-runner-jobs-test--job 'legacy-example "Legacy" init)
           invocation (gptel-runner-jobs--new-invocation job 'test))
     ;; Model a stored invocation predating bootstrap metadata.
     (let ((legacy-job (copy-sequence (plist-get invocation :job))))
       (cl-remf legacy-job :load-path)
       (cl-remf legacy-job :preload-features)
       (setq invocation (plist-put invocation :job legacy-job)))
     (gptel-runner-jobs--write (gptel-runner-jobs--manifest-file invocation) invocation)
     (gptel-runner-jobs-test--launch invocation)
     (setq invocation (gptel-runner-jobs-test--finished invocation))
     (should (eq (plist-get invocation :state) 'succeeded))
     (should (equal (alist-get 'report (gptel-runner-invocation-outputs invocation))
                    "legacy init controlled loading")))))

(ert-deftest gptel-runner-jobs-bootstrap-missing-package-fails-before-init ()
  (gptel-runner-jobs-test--isolated
   (let* ((gptel-runner-job-preload-features '(runner-test-missing-bootstrap-package))
          (init (expand-file-name "must-not-run.el" gptel-runner-job-directory))
          (marker (expand-file-name "init-ran" gptel-runner-job-directory))
          job)
     (gptel-runner-jobs--write init `(with-temp-file ,marker (insert "unexpected")))
     (setq job (gptel-runner-jobs-test--job nil nil init))
     (dotimes (_ 2)
       (let ((invocation (gptel-runner-job-run job)))
         (should-not (eq (plist-get invocation :state) 'skipped))
         (setq invocation (gptel-runner-jobs-test--finished invocation))
         (should (eq (plist-get invocation :state) 'startup-error))
         (should (string-match-p "runner-test-missing-bootstrap-package"
                                 (plist-get invocation :error)))
         (should-not (file-exists-p marker))
         (should-not (file-exists-p (gptel-runner-jobs--job-lock
                                     (plist-get job :id)))))))))

(ert-deftest gptel-runner-jobs-public-output-contract-matches-local-and-restored ()
  (gptel-runner-jobs-test--isolated
    (let* ((init (expand-file-name "examples/artifact-pipeline.el"
                                   gptel-runner-jobs--package-directory))
           (gptel-runner-output-directory
            (expand-file-name "local/" gptel-runner-job-directory))
           (_loaded (load init nil t))
           (local (gptel-runner-start 'artifact-pipeline
                                      :inputs '((subject . "parallel processes"))))
           (invocation (gptel-runner-jobs-test--finished
                        (gptel-runner-job-run
                         (gptel-runner-jobs-test--job 'artifact-pipeline nil init))))
           (local-artifact (gptel-runner-get local 'report))
           (remote-artifact (gptel-runner-get invocation 'report)))
      (unwind-protect
          (progn
            (should (eq (plist-get invocation :state) 'succeeded))
            (should (equal (cl-loop for (key _value) on local-artifact by #'cddr collect key)
                           (cl-loop for (key _value) on remote-artifact by #'cddr collect key)))
            (dolist (source (list local invocation))
              (let ((artifact (gptel-runner-get source 'report)))
                (should (equal (gptel-runner-read-artifact source artifact)
                               "Report for parallel processes\n"))
                (should (equal (gptel-runner-artifacts source) (list artifact)))
                (should (equal (gptel-runner-node-result source 'report) artifact))
                (should (equal (gptel-runner-node-outcome source 'empty)
                               '(:node empty :state succeeded :value nil)))
                (should-not (gptel-runner-get source 'empty 'missing))
                (should (eq (gptel-runner-get source 'absent 'missing) 'missing))
                (should (eq (gptel-runner-node-state source 'read-report) 'succeeded))
                (should (eq (gptel-runner-node-error source 'report 'none) 'none))
                (should-error (gptel-runner-node-outcome source 'absent) :type 'user-error)))
            ;; Continuation starts a fresh worker from a checkpoint.  Keep the
            ;; original artifact and append a new version in the same storage.
            (gptel-runner-jobs-test--command invocation 'continue '(:observation "Again"))
            (setq invocation (gptel-runner-jobs-test--finished invocation))
            (should (= (plist-get invocation :epoch) 2))
            (should (= (length (gptel-runner-artifacts invocation)) 2))
            (should (equal (gptel-runner-read-artifact invocation remote-artifact)
                           "Report for parallel processes\n"))
            (should (= (plist-get (gptel-runner-get invocation 'report) :epoch) 2))
            (should (equal (gptel-runner-node-result invocation 'report)
                           (gptel-runner-get invocation 'report))))
        (gptel-runner-forget-run local)))))

(provide 'gptel-runner-jobs-test)
;;; gptel-runner-jobs-test.el ends here
