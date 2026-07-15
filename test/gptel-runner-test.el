;;; gptel-runner-test.el --- Tests for gptel-runner -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner)
(require 'gptel-runner-review)

(defmacro gptel-runner-test--isolated (&rest body)
  "Run BODY with fresh runner registries."
  (declare (indent 0))
  `(let ((gptel-runner--agents (make-hash-table :test #'eq))
         (gptel-runner--workflows (make-hash-table :test #'eq))
         (gptel-runner--runs (make-hash-table :test #'equal))
         (gptel-runner--next-id 0))
     ,@body))

(defun gptel-runner-test--wait (run &optional seconds)
  "Wait up to SECONDS for RUN and return its state."
  (let ((deadline (+ (float-time) (or seconds 1.0))))
    (while (and (not (gptel-runner--run-terminal-p run))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (gptel-runner-run-state run)))

(defun gptel-runner-test--event-count (run type)
  "Count TYPE events in RUN."
  (cl-count type (gptel-runner-run-events run)
            :key #'gptel-runner-event-type))

(defun gptel-runner-test--step (id agent &optional save)
  "Make a simple step with ID, AGENT, and SAVE key."
  (gptel-runner-agent-step :id id :agent agent :prompt "work" :save-as save))

(ert-deftest gptel-runner-registry-and-blackboard ()
  (gptel-runner-test--isolated
    (let ((agent (gptel-runner-register-agent 'reader :preset 'p)))
      (should (eq (gptel-runner-agent-workspace-mode agent) 'read))
      (should-error (gptel-runner-register-agent 'bad :workspace-mode 'root))
      (should (gptel-runner-unregister-agent 'reader))
      (should-not (gptel-runner-unregister-agent 'reader)))
    (let ((run (gptel-runner-run-create
                :blackboard (make-hash-table :test #'equal)
                :iterations (make-hash-table :test #'equal))))
      (should (eq (gptel-runner-put run 'answer 42) 42))
      (should (= (gptel-runner-get run 'answer) 42))
      (should (eq (gptel-runner-get run 'missing 'none) 'none))
      (should (zerop (gptel-runner-iteration run 'loop))))))

(ert-deftest gptel-runner-simple-stateless-success-and-callback-once ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create))
          (callbacks 0))
      (gptel-runner-fake-queue
       driver 'worker '(:status success :value "done" :duplicate t))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--step 'work 'worker 'report)
                  :goal "goal" :workspace default-directory :driver driver
                  :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'report) "done"))
        (should (= callbacks 1))
        (should (= (length (gptel-runner-run-calls run)) 1))
        (should (= (gptel-runner-test--event-count run 'run-completed) 1))))))

(ert-deftest gptel-runner-preflight-validation ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'writer :preset 'p :workspace-mode 'write)
    (gptel-runner-register-agent 'reader :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)))
      (should-error
       (gptel-runner-start (gptel-runner-test--step 'write 'writer 'x)
                           :driver driver :workspace default-directory))
      (let ((duplicate
             (gptel-runner-sequence
              (gptel-runner-test--step 'same 'reader 'a)
              (gptel-runner-test--step 'same 'reader 'b))))
        (should-error (gptel-runner-start duplicate :driver driver)))
      (let ((collision
             (gptel-runner-parallel
              (gptel-runner-test--step 'a 'reader 'same-key)
              (gptel-runner-test--step 'b 'reader 'same-key))))
        (should-error (gptel-runner-start collision :driver driver))))))

(ert-deftest gptel-runner-transient-request-retries-one-call ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent
     'worker :preset 'p
     :retry-policy (gptel-runner-retry-policy-create
                    :max-retries 2 :base-delay 0 :jitter 0))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'worker
       '(:status transient :value "busy" :metadata (:http-status 503))
       '(:status success :value "ok"))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--step 'work 'worker 'result)
                  :driver driver :max-requests 3)))
        (should (eq (gptel-runner-test--wait run) 'succeeded))
        (should (= (length (gptel-runner-run-calls run)) 1))
        (should (= (gptel-runner-budget-requests
                    (gptel-runner-run-budget run)) 2))
        (should (= (gptel-runner-call-request-attempt
                    (car (gptel-runner-run-calls run))) 2))))))

(ert-deftest gptel-runner-cancellation-during-backoff-stops-retry ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent
     'worker :preset 'p
     :retry-policy (gptel-runner-retry-policy-create
                    :max-retries 2 :base-delay 0.1 :jitter 0))
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (gptel-runner-fake-queue
       driver 'worker
       '(:status transient :metadata (:http-status 503))
       '(:status success :value "late retry"))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--step 'work 'worker)
                  :driver driver
                  :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-call-state
                     (car (gptel-runner-run-calls run))) 'retry-wait))
        (gptel-runner-abort-run run)
        (sleep-for 0.15)
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 1))
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-late-callback-after-cancellation-is-ignored ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (gptel-runner-fake-queue
       driver 'worker '(:status success :value "too late" :delay 0.05 :late t))
      (let* ((run (gptel-runner-start
                   (gptel-runner-test--step 'work 'worker 'result)
                   :driver driver :callback (lambda (_run) (cl-incf callbacks))))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-abort-call call)
        (sleep-for 0.08)
        (should (eq (gptel-runner-run-state run) 'cancelled))
        (should (eq (gptel-runner-call-state call) 'cancelled))
        (should-not (gptel-runner-get run 'result))
        (should (= callbacks 1))))))

(defun gptel-runner-test--review-flow ()
  "Return the standard implementation/review repeat used in tests."
  (gptel-runner-repeat-until
   :id 'cycle :max 5
   :until (lambda (run)
            (eq (plist-get (gptel-runner-get run 'review) :verdict) 'pass))
   :stop-when (lambda (run)
                (eq (plist-get (gptel-runner-get run 'review) :verdict)
                    'blocked))
   :progress-key #'gptel-runner-review-progress-key
   :body
   (gptel-runner-sequence
    (gptel-runner-test--step 'implement 'implementer 'implementation)
    (gptel-runner-agent-step :id 'review :agent 'reviewer :prompt "review"
                             :save-as 'review :repair-invalid t))))

(defun gptel-runner-test--register-review-agents ()
  "Register standard implementer and reviewer test agents."
  (gptel-runner-register-agent 'implementer :preset 'p :workspace-mode 'write)
  (gptel-runner-register-agent
   'reviewer :preset 'p :schema gptel-runner-review-schema
   :parser #'gptel-runner-parse-review :validator #'gptel-runner-valid-review-p))

(ert-deftest gptel-runner-review-revise-then-pass ()
  (gptel-runner-test--isolated
    (gptel-runner-test--register-review-agents)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'implementer
                               '(:value "first") '(:value "revision"))
      (gptel-runner-fake-queue
       driver 'reviewer
       '(:value "{\"verdict\":\"revise\",\"summary\":\"fix\",\"issues\":[{\"severity\":\"error\",\"message\":\"x\"}]}")
       '(:value "{\"verdict\":\"pass\",\"summary\":\"ok\",\"issues\":[]}"))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--review-flow) :driver driver
                  :workspace default-directory :allow-writes t)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-iteration run 'cycle) 2))
        (should (= (length (gptel-runner-run-calls run)) 4))
        (should (eq (plist-get (gptel-runner-get run 'review) :verdict)
                    'pass))))))

(ert-deftest gptel-runner-review-blocked-stops-before-reimplementation ()
  (gptel-runner-test--isolated
    (gptel-runner-test--register-review-agents)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'implementer '(:value "first"))
      (gptel-runner-fake-queue
       driver 'reviewer
       '(:value "{\"verdict\":\"blocked\",\"summary\":\"needs user\",\"issues\":[]}"))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--review-flow) :driver driver
                  :allow-writes t)))
        (should (eq (gptel-runner-run-state run) 'blocked))
        (should (= (length (gptel-runner-run-calls run)) 2))))))

(ert-deftest gptel-runner-review-repair-once ()
  (gptel-runner-test--isolated
    (gptel-runner-test--register-review-agents)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'implementer '(:value "first"))
      (gptel-runner-fake-queue
       driver 'reviewer '(:value "not json")
       '(:value "{\"verdict\":\"pass\",\"summary\":\"fixed\",\"issues\":[]}"))
      (let ((run (gptel-runner-start (gptel-runner-test--review-flow)
                                     :driver driver :allow-writes t)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (length (gptel-runner-run-calls run)) 3))
        (should (= (gptel-runner-test--event-count
                    run 'output-repair-started) 1))))))

(ert-deftest gptel-runner-review-second-malformed-fails-safe ()
  (gptel-runner-test--isolated
    (gptel-runner-test--register-review-agents)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'implementer '(:value "first"))
      (gptel-runner-fake-queue driver 'reviewer
                               '(:value "bad") '(:value "still bad"))
      (let ((run (gptel-runner-start (gptel-runner-test--review-flow)
                                     :driver driver :allow-writes t)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-not (eq (plist-get (gptel-runner-get run 'review) :verdict)
                        'pass))))))

(ert-deftest gptel-runner-identical-progress-stalls ()
  (gptel-runner-test--isolated
    (gptel-runner-test--register-review-agents)
    (let ((driver (gptel-runner-fake-driver-create))
          (review "{\"verdict\":\"revise\",\"summary\":\"same\",\"issues\":[{\"severity\":\"error\",\"message\":\"same\"}]}"))
      (gptel-runner-fake-queue driver 'implementer '(:value "a") '(:value "b"))
      (gptel-runner-fake-queue driver 'reviewer
                               (list :value review) (list :value review))
      (let ((run (gptel-runner-start (gptel-runner-test--review-flow)
                                     :driver driver :allow-writes t)))
        (should (eq (gptel-runner-run-state run) 'stalled))
        (should (= (gptel-runner-iteration run 'cycle) 2))))))

(ert-deftest gptel-runner-parallel-obeys-concurrency-and-synthesizes ()
  (gptel-runner-test--isolated
    (dolist (agent '(a b c synth)) (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create)))
      (dolist (agent '(a b c))
        (gptel-runner-fake-queue driver agent '(:manual t)))
      (gptel-runner-fake-queue driver 'synth '(:value "decision"))
      (let* ((root
              (gptel-runner-sequence
               (gptel-runner-parallel
                :id 'fanout
                (gptel-runner-test--step 'a 'a 'a)
                (gptel-runner-test--step 'b 'b 'b)
                (gptel-runner-test--step 'c 'c 'c))
               (gptel-runner-test--step 'synth 'synth 'decision)))
             (run (gptel-runner-start root :driver driver :max-concurrency 2)))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 2))
        (gptel-runner-fake-release driver (nth 0 (gptel-runner-run-calls run))
                                   '(:value "A"))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 3))
        (gptel-runner-fake-release driver (nth 1 (gptel-runner-run-calls run))
                                   '(:value "B"))
        (gptel-runner-fake-release driver (nth 2 (gptel-runner-run-calls run))
                                   '(:value "C"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'decision) "decision"))
        (should (= (gptel-runner-fake-driver-max-active driver) 2))))))

(ert-deftest gptel-runner-parallel-policies ()
  (gptel-runner-test--isolated
    (dolist (agent '(a b c)) (gptel-runner-register-agent agent :preset 'p))
    (dolist (case '((fail-fast nil failed) (collect nil succeeded)
                    (minimum-successes 2 succeeded)
                    (minimum-successes 3 failed)))
      (let ((driver (gptel-runner-fake-driver-create)))
        (gptel-runner-fake-queue driver 'a '(:value "a"))
        (gptel-runner-fake-queue driver 'b '(:status permanent :value "no"))
        (gptel-runner-fake-queue driver 'c '(:value "c"))
        (let* ((policy (nth 0 case)) (minimum (nth 1 case))
               (expected (nth 2 case))
               (args (append (list :id (intern (format "p-%s-%s" policy minimum))
                                   :policy policy)
                             (and minimum (list :minimum-successes minimum))
                             (list (gptel-runner-test--step
                                    (make-symbol "a") 'a 'a)
                                   (gptel-runner-test--step
                                    (make-symbol "b") 'b 'b)
                                   (gptel-runner-test--step
                                    (make-symbol "c") 'c 'c))))
               (run (gptel-runner-start
                     (apply #'gptel-runner-parallel args)
                     :driver driver :max-concurrency 3)))
          (should (eq (gptel-runner-run-state run) expected)))))))

(ert-deftest gptel-runner-shared-workspace-writers-never-overlap ()
  (gptel-runner-test--isolated
    (dolist (agent '(a b))
      (gptel-runner-register-agent agent :preset 'p :workspace-mode 'write))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'a '(:manual t))
      (gptel-runner-fake-queue driver 'b '(:manual t))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-parallel
                (gptel-runner-test--step 'a 'a 'a)
                (gptel-runner-test--step 'b 'b 'b))
               :driver driver :max-concurrency 2 :allow-writes t))
             (first (car (gptel-runner-run-calls run))))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 1))
        (gptel-runner-fake-release driver first '(:value "a"))
        (should (= (length (gptel-runner-fake-driver-starts driver)) 2))
        (gptel-runner-fake-release driver (cadr (gptel-runner-run-calls run))
                                   '(:value "b"))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-fake-driver-max-active driver) 1))))))

(ert-deftest gptel-runner-budget-failures-terminalize-once ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)) (callbacks 0))
      (gptel-runner-fake-queue driver 'worker '(:value "one"))
      (let ((run (gptel-runner-start
                  (gptel-runner-sequence
                   (gptel-runner-test--step 'one 'worker)
                   (gptel-runner-test--step 'two 'worker))
                  :driver driver :max-calls 1
                  :callback (lambda (_run) (cl-incf callbacks)))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (= callbacks 1))))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:manual t))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--step 'slow 'worker)
                  :driver driver :max-duration 0.02)))
        (should (eq (gptel-runner-test--wait run) 'failed))
        (should (= (gptel-runner-test--event-count run 'run-failed) 1))))
    (gptel-runner-register-agent
     'retrying :preset 'p
     :retry-policy (gptel-runner-retry-policy-create
                    :max-retries 2 :base-delay 0 :jitter 0))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'retrying '(:status transient :metadata (:http-status 503)))
      (let ((run (gptel-runner-start
                  (gptel-runner-test--step 'retry 'retrying)
                  :driver driver :max-requests 1)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (eq (plist-get
                     (gptel-runner-call-error
                      (car (gptel-runner-run-calls run))) :type)
                    'budget))))))

(ert-deftest gptel-runner-repeat-iteration-budget-is-distinct ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "a") '(:value "b"))
      (let ((run
             (gptel-runner-start
              (gptel-runner-repeat-until
               :id 'bounded :max 2 :until (lambda (_run) nil)
               :body (gptel-runner-test--step 'work 'worker))
              :driver driver :max-requests 10 :max-calls 10)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (= (gptel-runner-iteration run 'bounded) 2))
        (should (= (gptel-runner-budget-calls
                    (gptel-runner-run-budget run)) 2))))))

(ert-deftest gptel-runner-branch-selects-one-child ()
  (gptel-runner-test--isolated
    (dolist (agent '(yes no)) (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'yes '(:value "selected"))
      (let* ((no-node (gptel-runner-test--step 'no 'no 'no))
             (run (gptel-runner-start
                   (gptel-runner-branch
                    :id 'choice :predicate (lambda (_run) t)
                    :then (gptel-runner-test--step 'yes 'yes 'yes)
                    :else no-node)
                   :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'yes) "selected"))
        (should (eq (gethash 'no (gptel-runner-run-node-states run))
                    'skipped))))))

(ert-deftest gptel-runner-call-feedback-continues-original-workflow ()
  (gptel-runner-test--isolated
    (dolist (agent '(first second))
      (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'first '(:manual t))
      (gptel-runner-fake-queue driver 'second '(:value "second result"))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-sequence
                (gptel-runner-test--step 'first 'first 'first-result)
                (gptel-runner-test--step 'second 'second 'second-result))
               :driver driver))
             (call (car (gptel-runner-run-calls run))))
        (gptel-runner-pause-call call 'test-feedback)
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should (eq (gptel-runner-run-state run) 'running))
        (gptel-runner-complete-call-from-buffer "corrected result" call)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'first-result)
                       "corrected result"))
        (should (equal (gptel-runner-get run 'second-result)
                       "second result"))
        (should (= (length (gptel-runner-run-calls run)) 2))))))

(ert-deftest gptel-runner-snapshot-load-resume-next-session ()
  (gptel-runner-test--isolated
    (dolist (agent '(first second))
      (gptel-runner-register-agent agent :preset 'p))
    (let* ((snapshot-directory (make-temp-file "gptel-runner-snapshot-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create))
           (callbacks 0)
           observed-prompt)
      (unwind-protect
          (progn
            (gptel-runner-defworkflow persisted-handoff (:persist t)
              (gptel-runner-sequence
               :id 'handoff
               (gptel-runner-test--step 'first 'first 'first-result)
               (gptel-runner-test--step 'second 'second 'second-result)))
            (gptel-runner-fake-queue driver 'first '(:value "kept result"))
            (gptel-runner-fake-queue driver 'second '(:manual t))
            (let* ((run (gptel-runner-start
                         'persisted-handoff :goal "ship it" :driver driver
                         :max-calls 5 :max-requests 5
                         :callback (lambda (_run) (cl-incf callbacks))))
                   (file (progn (gptel-runner-pause-run run 'overnight)
                                (gptel-runner-run-snapshot-file run))))
              (should (eq (gptel-runner-run-state run) 'paused))
              (should (file-exists-p file))
              (should (= (file-modes file) #o600))
              (should (= callbacks 0))
              ;; Model a fresh Emacs session: definitions remain loaded, but
              ;; no runtime objects or callbacks survive.
              (setq gptel-runner--runs (make-hash-table :test #'equal))
              (let ((resume-driver (gptel-runner-fake-driver-create)))
                (gptel-runner-fake-queue
                 resume-driver 'second
                 (lambda (call)
                   (setq observed-prompt (gptel-runner-call-prompt call))
                   '(:value "resumed result")))
                (let ((restored
                       (gptel-runner-load-run
                        file (lambda (_run) (cl-incf callbacks)) resume-driver)))
                  (should (eq (gptel-runner-run-state restored) 'paused))
                  (should (equal (gptel-runner-get restored 'first-result)
                                 "kept result"))
                  (gptel-runner-resume-run restored "Use the smaller API")
                  (should (eq (gptel-runner-run-state restored) 'succeeded))
                  (should (string-match-p "Use the smaller API"
                                          observed-prompt))
                  (should (equal (gptel-runner-get restored 'second-result)
                                 "resumed result"))
                  (should (= callbacks 1))
                  (should (= (gptel-runner-budget-calls
                              (gptel-runner-run-budget restored)) 3))
                  (should (= (gptel-runner-budget-requests
                              (gptel-runner-run-budget restored)) 3))
                  (should (= (length (gptel-runner-run-calls restored)) 3))
                  (should (eq (gptel-runner-call-state
                               (nth 1 (gptel-runner-run-calls restored)))
                              'skipped))))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-persistence-requires-named-workflow ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (should-error
     (gptel-runner-start
      (gptel-runner-test--step 'work 'worker)
     :driver (gptel-runner-fake-driver-create) :persist t)
     :type 'user-error)))

(ert-deftest gptel-runner-restored-feedback-buffer-completes-node ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-feedback-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow persisted-feedback (:persist t)
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work" :save-as 'report))
            (gptel-runner-fake-queue driver 'worker '(:manual t))
            (let* ((run (gptel-runner-start 'persisted-feedback
                                            :driver driver))
                   (call (car (gptel-runner-run-calls run))))
              (gptel-runner-pause-call call 'feedback)
              (gptel-runner-pause-run run 'overnight)
              (setq gptel-runner--runs (make-hash-table :test #'equal))
              (let* ((restored (gptel-runner-load-run
                                (gptel-runner-run-snapshot-file run)
                                nil (gptel-runner-fake-driver-create)))
                     (restored-call (car (gptel-runner-run-calls restored))))
                (should (eq (gptel-runner-call-state restored-call)
                            'waiting-feedback))
                (gptel-runner-complete-call-from-buffer
                 "human-guided result" restored-call)
                (should (eq (gptel-runner-run-state restored) 'succeeded))
                (should (equal (gptel-runner-get restored 'report)
                               "human-guided result"))
                (should (= (length (gptel-runner-run-calls restored)) 1)))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-dashboard-groups-workflows-runs-and-calls ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (gptel-runner-defworkflow alpha-workflow ()
      (gptel-runner-agent-step
       :id 'alpha-step :agent 'worker :prompt "alpha"))
    (gptel-runner-defworkflow empty-workflow ()
      (gptel-runner-agent-step
       :id 'empty-step :agent 'worker :prompt "empty"))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "done"))
      (let* ((run (gptel-runner-start 'alpha-workflow :driver driver))
             (call (car (gptel-runner-run-calls run)))
             (ids (mapcar #'car (gptel-runner-ui--entries))))
        (should (equal ids
                       (list '(workflow alpha-workflow)
                             (list 'run (gptel-runner-run-id run))
                             (list 'call (gptel-runner-call-id call))
                             '(workflow empty-workflow))))
        (with-temp-buffer
          (gptel-runner-dashboard-mode)
          (tabulated-list-print t)
          (should (string-match-p "alpha-workflow" (buffer-string)))
          (should (string-match-p (gptel-runner-run-id run)
                                  (buffer-string)))
          (should (string-match-p "empty-workflow" (buffer-string))))))))

(ert-deftest gptel-runner-forget-run-and-workflow-clean-session-noise ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (gptel-runner-defworkflow disposable-workflow ()
      (gptel-runner-agent-step
       :id 'work :agent 'worker :prompt "work"))
    (let ((driver (gptel-runner-fake-driver-create))
          (snapshot (make-temp-file "gptel-runner-forget-")))
      (unwind-protect
          (progn
            (gptel-runner-fake-queue driver 'worker '(:value "done"))
            (let* ((run (gptel-runner-start 'disposable-workflow
                                            :driver driver))
                   (call (car (gptel-runner-run-calls run)))
                   (worker-buffer (generate-new-buffer " *runner-worker*"))
                   (events-buffer
                    (get-buffer-create
                     (format "*gptel-runner events:%s*"
                             (gptel-runner-run-id run)))))
              (setf (gptel-runner-call-buffer call) worker-buffer
                    (gptel-runner-run-snapshot-file run) snapshot)
              (gptel-runner-forget-workflow 'disposable-workflow)
              (should-not (gethash 'disposable-workflow
                                   gptel-runner--workflows))
              (should-not (gethash (gptel-runner-run-id run)
                                   gptel-runner--runs))
              (should-not (buffer-live-p worker-buffer))
              (should-not (buffer-live-p events-buffer))
              ;; Forgetting dashboard state preserves durable recovery data
              ;; unless snapshot deletion was explicitly requested.
              (should (file-exists-p snapshot)))
            (gptel-runner-defworkflow disposable-workflow ()
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work"))
            (gptel-runner-fake-queue driver 'worker '(:value "done again"))
            (let ((run (gptel-runner-start 'disposable-workflow
                                           :driver driver)))
              (setf (gptel-runner-run-snapshot-file run) snapshot)
              (gptel-runner-forget-run run t)
              (should-not (file-exists-p snapshot))))
        (when (file-exists-p snapshot) (delete-file snapshot))))
    (gptel-runner-defworkflow active-workflow ()
      (gptel-runner-agent-step
       :id 'active :agent 'worker :prompt "wait"))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:manual t))
      (let ((run (gptel-runner-start 'active-workflow :driver driver)))
        (should-error (gptel-runner-forget-run run) :type 'user-error)
        (should-error (gptel-runner-forget-workflow 'active-workflow)
                      :type 'user-error)
        (gptel-runner-abort-run run)
        (gptel-runner-forget-workflow 'active-workflow)
        (should-not (gethash 'active-workflow gptel-runner--workflows))))))

(provide 'gptel-runner-test)
;;; gptel-runner-test.el ends here
