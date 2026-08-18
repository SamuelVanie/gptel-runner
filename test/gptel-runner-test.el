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
         (gptel-runner-store--coordinators (make-hash-table :test #'eq))
         (gptel-runner--next-id 0))
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (run _coordinator)
                  (gptel-runner-store-cancel-save run))
                gptel-runner-store--coordinators))))

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

(defun gptel-runner-test--wait-for-snapshot (run &optional seconds)
  "Wait up to SECONDS for RUN's queued snapshot and return its status."
  (let ((deadline (+ (float-time) (or seconds 1.0))))
    (while (and (memq (gptel-runner-store-save-status run)
                      '(pending writing))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (gptel-runner-store-save-status run)))

(defun gptel-runner-test--step (id agent &optional save)
  "Make a simple step with ID, AGENT, and SAVE key."
  (gptel-runner-agent-step :id id :agent agent :prompt "work" :save-as save))

(ert-deftest gptel-runner-review-schema-is-json-serializable ()
  (let* ((encoded (json-serialize gptel-runner-review-schema))
         (decoded (json-parse-string encoded :object-type 'plist)))
    (should (equal (plist-get decoded :type) "object"))
    (should (equal
             (plist-get
              (plist-get
               (plist-get
                (plist-get decoded :properties) :issues)
               :items)
              :properties)
             '(:severity (:type "string")
               :file (:type ["string" "null"])
               :line (:type ["integer" "null"])
               :message (:type "string")
               :suggested_fix (:type ["string" "null"]))))))

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
      (should (zerop (gptel-runner-iteration run 'loop))))
    (let ((run (gptel-runner-run-create :id "run-42")))
      (puthash "run-42" run gptel-runner--runs)
      (should (eq (gptel-runner-find-run "run-42") run))
      (should-not (gptel-runner-find-run "missing"))
      (should-not (gptel-runner-find-run 'run-42)))))

(ert-deftest gptel-runner-records-ordered-decisions-with-provenance ()
  (gptel-runner-test--isolated
    (let* ((run (gptel-runner-run-create
                 :id "run-1" :state 'running :events nil :options nil
                 :blackboard (make-hash-table :test #'equal)))
           (node (gptel-runner-node-create :id 'implement))
           (call (gptel-runner-call-create
                  :id "call-1" :run run :node node :state 'running))
           (first (gptel-runner-record-decision
                   run "  Use SQLite for state.  "
                   "  It provides atomic updates.  " call))
           (second (gptel-runner-record-decision
                    run "Keep migrations reversible."))
           (decisions (gptel-runner-decisions run)))
      (should (gptel-runner-decision-memory-p run))
      (should (= (length decisions) 2))
      (should (equal (mapcar (lambda (entry) (plist-get entry :text))
                             decisions)
                     '("Use SQLite for state."
                       "Keep migrations reversible.")))
      (should (equal (plist-get first :rationale)
                     "It provides atomic updates."))
      (should (eq (plist-get first :node-id) 'implement))
      (should (equal (plist-get first :call-id) "call-1"))
      (should-not (plist-get second :node-id))
      (should (string-match-p "Use SQLite for state"
                              (gptel-runner-format-decisions run)))
      (should (string-match-p "Recorded by node: implement"
                              (gptel-runner-format-decisions run)))
      (should (= (gptel-runner-test--event-count
                  run 'decision-recorded) 2))
      (should-error (gptel-runner-record-decision run "   ")
                    :type 'user-error)
      (let* ((other-run (gptel-runner-run-create :id "other"))
             (other-call (gptel-runner-call-create :run other-run)))
        (should-error
         (gptel-runner-record-decision run "Wrong source" nil other-call)
         :type 'user-error)))))

(ert-deftest gptel-runner-decision-memory-defaults-on-and-propagates ()
  (gptel-runner-test--isolated
    (dolist (agent '(planner implementer))
      (gptel-runner-register-agent agent :preset 'p))
    (gptel-runner-defworkflow decision-memory-default ()
      (gptel-runner-sequence
       (gptel-runner-agent-step
        :id 'plan :agent 'planner :prompt "Create a plan")
       (gptel-runner-agent-step
        :id 'implement :agent 'implementer :prompt "Implement the plan")))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'planner
       (lambda (call)
         (gptel-runner-record-decision
          (gptel-runner-call-run call)
          "Use an append-only decision log."
          "It preserves provenance." call)
         '(:value "planned")))
      (gptel-runner-fake-queue
       driver 'implementer
       (lambda (call)
         (let ((prompt (gptel-runner-call-prompt call)))
           (should (string-match-p
                    "Workflow decisions recorded by earlier stages" prompt))
           (should (string-match-p
                    "Use an append-only decision log" prompt))
           (should (string-match-p "It preserves provenance" prompt)))
         '(:value "implemented")))
      (let ((run (gptel-runner-start
                  'decision-memory-default :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (eq (plist-get (gptel-runner-run-options run)
                               :decision-memory)
                    t))
        (should (gptel-runner-decision-memory-p run))
        (should (= (length (gptel-runner-decisions run)) 1))))))

(ert-deftest gptel-runner-decision-memory-can-be-disabled ()
  (gptel-runner-test--isolated
    (dolist (agent '(planner implementer))
      (gptel-runner-register-agent agent :preset 'p))
    (gptel-runner-defworkflow decision-memory-disabled
        (:decision-memory nil)
      (gptel-runner-sequence
       (gptel-runner-agent-step
        :id 'plan :agent 'planner :prompt "Create a plan")
       (gptel-runner-agent-step
        :id 'implement :agent 'implementer :prompt "Implement the plan")))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'planner
       (lambda (call)
         (gptel-runner-record-decision
          (gptel-runner-call-run call) "Do not propagate this." nil call)
         '(:value "planned")))
      (gptel-runner-fake-queue
       driver 'implementer
       (lambda (call)
         (should-not (string-match-p
                      "Do not propagate this"
                      (gptel-runner-call-prompt call)))
         '(:value "implemented")))
      (let ((run (gptel-runner-start
                  'decision-memory-disabled :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should-not (gptel-runner-decision-memory-p run))
        (should (= (length (gptel-runner-decisions run)) 1))))))

(ert-deftest gptel-runner-add-decision-uses-current-worker-call ()
  (gptel-runner-test--isolated
    (let* ((run (gptel-runner-run-create
                 :id "run" :state 'running :events nil
                 :blackboard (make-hash-table :test #'equal)))
           (node (gptel-runner-node-create :id 'review))
           (call (gptel-runner-call-create
                  :id "call" :run run :node node :state 'running))
           (answers '("Prefer the smaller API" "It is easier to maintain")))
      (with-temp-buffer
        (setq-local gptel-runner--call call)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _arguments) (pop answers)))
                  ((symbol-function 'message) #'ignore))
          (call-interactively #'gptel-runner-add-decision)))
      (let ((decision (car (gptel-runner-decisions run))))
        (should (equal (plist-get decision :text)
                       "Prefer the smaller API"))
        (should (equal (plist-get decision :rationale)
                       "It is easier to maintain"))
        (should (eq (plist-get decision :node-id) 'review))
        (should (equal (plist-get decision :call-id) "call"))))))

(ert-deftest gptel-runner-tool-observations-update-call-state ()
  (let* ((run (gptel-runner-run-create :id "run" :events nil))
         (node (gptel-runner-node-create :id 'work))
         (call (gptel-runner-call-create
                :id "call" :run run :node node :state 'running)))
    (gptel-runner--call-observe
     call 0 'waiting-tool '("AskUserQuestion"))
    (should (eq (gptel-runner-call-state call) 'waiting-tool))
    (should (equal (gptel-runner-call-tool-names call)
                   '("AskUserQuestion")))
    (gptel-runner--call-observe call 0 'waiting-confirmation nil)
    (should (eq (gptel-runner-call-state call) 'waiting-confirmation))
    (gptel-runner--call-observe call 0 'waiting-tool '("AskUserQuestion"))
    (gptel-runner--call-observe call 0 'tool-results nil)
    (should (eq (gptel-runner-call-state call) 'running))
    (should-not (gptel-runner-call-tool-names call))
    (should (equal (mapcar #'gptel-runner-event-type
                           (gptel-runner-run-events run))
                   '(waiting-tool waiting-confirmation waiting-tool
                     tool-results)))))

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

(ert-deftest gptel-runner-empty-output-is-repaired-before-handoff ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'planner :preset 'p)
    (gptel-runner-register-agent 'implementer :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue
       driver 'planner '(:value " \n\t") '(:value "repaired plan"))
      (gptel-runner-fake-queue
       driver 'implementer
       (lambda (call)
         (should (string-match-p "repaired plan"
                                 (gptel-runner-call-prompt call)))
         '(:value "implemented")))
      (let* ((root
              (gptel-runner-sequence
               (gptel-runner-agent-step
                :id 'plan :agent 'planner :prompt "Create the plan"
                :save-as 'plan)
               (gptel-runner-agent-step
                :id 'implement :agent 'implementer
                :prompt (lambda (run _node)
                          (format "Implement this: %s"
                                  (gptel-runner-get run 'plan))))))
             (run (gptel-runner-start root :goal "ship" :driver driver))
             (calls (gptel-runner-run-calls run)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'plan) "repaired plan"))
        (should (= (length calls) 3))
        (should (eq (gptel-runner-call-state (nth 0 calls)) 'failed))
        (should (gptel-runner--empty-output-error-p
                 (gptel-runner-call-error (nth 0 calls))))
        (should (gptel-runner-call-repair-p (nth 1 calls)))
        (should (string-match-p "Original task:\nCreate the plan"
                                (gptel-runner-call-prompt (nth 1 calls))))
        (should (= (gptel-runner-test--event-count
                    run 'output-repair-started) 1))))))

(ert-deftest gptel-runner-second-empty-output-fails-before-next-node ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'planner :preset 'p)
    (gptel-runner-register-agent 'implementer :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'planner '(:value nil) '(:value "  "))
      (gptel-runner-fake-queue driver 'implementer '(:value "must not run"))
      (let* ((root
              (gptel-runner-sequence
               (gptel-runner-agent-step
                :id 'plan :agent 'planner :prompt "Create the plan"
                :save-as 'plan :retries 3)
               (gptel-runner-test--step 'implement 'implementer)))
             (run (gptel-runner-start root :driver driver))
             (calls (gptel-runner-run-calls run)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-not (gptel-runner-get run 'plan))
        (should (= (length calls) 2))
        (should (cl-every (lambda (call)
                            (eq (gptel-runner-call-state call) 'failed))
                          calls))
        (should (eq (gethash 'implement
                             (gptel-runner-run-node-states run))
                    'skipped))
        (should (= (gptel-runner-test--event-count
                    run 'output-repair-started) 1))
        (should (= (gptel-runner-test--event-count
                    run 'agent-step-retry) 0))))))

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

(ert-deftest gptel-runner-repeat-collects-history-for-downstream-node ()
  (gptel-runner-test--isolated
    (dolist (agent '(writer reviewer summarizer))
      (gptel-runner-register-agent agent :preset 'p))
    (let* ((driver (gptel-runner-fake-driver-create))
           (expected
            '((:iteration 1
               :values ((implementation . "first draft")
                        (review . "revise")))
              (:iteration 2
               :values ((implementation . "revised draft")
                        (review . "pass")))))
           observed-history)
      (gptel-runner-fake-queue
       driver 'writer '(:value "first draft") '(:value "revised draft"))
      (gptel-runner-fake-queue
       driver 'reviewer '(:value "revise") '(:value "pass"))
      (gptel-runner-fake-queue driver 'summarizer '(:value "final document"))
      (let ((run
             (gptel-runner-start
              (gptel-runner-sequence
               (gptel-runner-repeat-until
                :id 'revision-cycle
                :max 3
                :until (lambda (current-run)
                         (equal (gptel-runner-get current-run 'review)
                                "pass"))
                :collect-keys '(implementation review)
                :save-history-as 'revision-history
                :body
                (gptel-runner-sequence
                 (gptel-runner-test--step
                  'write 'writer 'implementation)
                 (gptel-runner-test--step 'review 'reviewer 'review)))
               (gptel-runner-agent-step
                :id 'summarize
                :agent 'summarizer
                :prompt (lambda (current-run _node)
                          (setq observed-history
                                (gptel-runner-get
                                 current-run 'revision-history))
                          (format "Summarize: %S" observed-history))
                :save-as 'final-document))
              :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal observed-history expected))
        (should (equal (gptel-runner-get run 'revision-history) expected))
        (should (equal (gptel-runner-get run 'implementation)
                       "revised draft"))
        (should (equal (gptel-runner-get run 'review) "pass"))
        (should (equal (gptel-runner-get run 'final-document)
                       "final document"))))))

(ert-deftest gptel-runner-extend-repeat-preserves-and-appends-history ()
  (gptel-runner-test--isolated
    (dolist (agent '(worker summarizer))
      (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create))
          (initial-callbacks 0)
          (extension-callbacks 0)
          observed-history)
      (gptel-runner-fake-queue
       driver 'worker
       '(:value "first") '(:value "second")
       '(:value "third") '(:value "pass"))
      (gptel-runner-fake-queue driver 'summarizer '(:value "summary"))
      (let* ((repeat
              (gptel-runner-repeat-until
               :id 'review-cycle :max 2
               :until (lambda (run)
                        (equal (gptel-runner-get run 'result) "pass"))
               :collect-keys '(result) :save-history-as 'review-history
               :body (gptel-runner-test--step 'work 'worker 'result)))
             (root
              (gptel-runner-sequence
               :id 'workflow
               repeat
               (gptel-runner-agent-step
                :id 'summarize :agent 'summarizer
                :prompt (lambda (run _node)
                          (setq observed-history
                                (gptel-runner-get run 'review-history))
                          "summarize")
                :save-as 'summary)))
             (run
              (gptel-runner-start
               root :driver driver :max-calls 2 :max-requests 2
               :callback (lambda (_run) (cl-incf initial-callbacks)))))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should (= (gptel-runner-iteration run 'review-cycle) 2))
        (should (= initial-callbacks 1))
        (gptel-runner-extend-repeat
         (gptel-runner-run-id run) 'review-cycle 2
         :additional-calls 3 :additional-requests 3
         :callback (lambda (_run) (cl-incf extension-callbacks)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (= (gptel-runner-iteration run 'review-cycle) 4))
        (should (= (gptel-runner--repeat-limit run repeat) 4))
        (should (= (plist-get (gptel-runner-node-properties repeat) :max) 2))
        (should
         (equal
          (gptel-runner-get run 'review-history)
          '((:iteration 1 :values ((result . "first")))
            (:iteration 2 :values ((result . "second")))
            (:iteration 3 :values ((result . "third")))
            (:iteration 4 :values ((result . "pass"))))))
        (should (equal observed-history
                       (gptel-runner-get run 'review-history)))
        (should (equal (gptel-runner-get run 'summary) "summary"))
        (should (= (length (gptel-runner-run-calls run)) 5))
        (should (= (gptel-runner-budget-calls
                    (gptel-runner-run-budget run)) 5))
        (should (= (gptel-runner-budget-requests
                    (gptel-runner-run-budget run)) 5))
        (should (= initial-callbacks 1))
        (should (= extension-callbacks 1))
        (should (= (gptel-runner-test--event-count
                    run 'repeat-extended) 1))))))

(ert-deftest gptel-runner-extend-repeat-rejects-ineligible-runs ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'worker '(:value "only"))
      (let* ((root
              (gptel-runner-sequence
               :id 'workflow
               (gptel-runner-repeat-until
                :id 'cycle :max 1 :until (lambda (_run) nil)
                :body (gptel-runner-test--step 'work 'worker))))
             (run (gptel-runner-start root :driver driver)))
        (should (eq (gptel-runner-run-state run) 'failed))
        (should-error (gptel-runner-extend-repeat run 'cycle 0)
                      :type 'user-error)
        (should-error (gptel-runner-extend-repeat run 'cycle nil)
                      :type 'user-error)
        (should-error (gptel-runner-extend-repeat run 'missing 1)
                      :type 'user-error)
        (should-error (gptel-runner-extend-repeat run 'workflow 1)
                      :type 'user-error)
        (should-error (gptel-runner-extend run :additional-calls 1)
                      :type 'user-error)
        (should (= (gptel-runner-iteration run 'cycle) 1))
        (should (= (gptel-runner--repeat-limit
                    run (gptel-runner--find-node root 'cycle)) 1))))))

(ert-deftest gptel-runner-repeat-history-omits-stale-skipped-values ()
  (gptel-runner-test--isolated
    (dolist (agent '(optional reviewer))
      (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create)))
      (gptel-runner-fake-queue driver 'optional '(:value "first only"))
      (gptel-runner-fake-queue
       driver 'reviewer '(:value "revise") '(:value "pass"))
      (let ((run
             (gptel-runner-start
              (gptel-runner-repeat-until
               :id 'conditional-cycle
               :max 3
               :until (lambda (current-run)
                        (equal (gptel-runner-get current-run 'review)
                               "pass"))
               :collect-keys '(optional-result review)
               :save-history-as 'conditional-history
               :body
               (gptel-runner-sequence
                (gptel-runner-branch
                 :id 'optional-branch
                 :predicate
                 (lambda (current-run)
                   (zerop (gptel-runner-iteration
                           current-run 'conditional-cycle)))
                 :then
                 (gptel-runner-test--step
                  'optional-step 'optional 'optional-result))
                (gptel-runner-test--step 'review-step 'reviewer 'review)))
              :driver driver)))
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should
         (equal
          (gptel-runner-get run 'conditional-history)
          '((:iteration 1
             :values ((optional-result . "first only")
                      (review . "revise")))
            (:iteration 2 :values ((review . "pass"))))))))))

(ert-deftest gptel-runner-repeat-history-options-are-validated ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let ((driver (gptel-runner-fake-driver-create))
          (body (gptel-runner-test--step 'work 'worker 'result)))
      (dolist
          (node
           (list
            (gptel-runner-repeat-until
             :id 'missing-history-key :max 1
             :collect-keys '(result) :body body)
            (gptel-runner-repeat-until
             :id 'missing-collected-keys :max 1
             :save-history-as 'history :body body)
            (gptel-runner-repeat-until
             :id 'empty-collected-keys :max 1
             :collect-keys nil :save-history-as 'history :body body)
            (gptel-runner-repeat-until
             :id 'duplicate-collected-keys :max 1
             :collect-keys '(result result)
             :save-history-as 'history :body body)
            (gptel-runner-repeat-until
             :id 'history-key-collision :max 1
             :collect-keys '(result)
             :save-history-as 'result :body body)
            (gptel-runner-repeat-until
             :id 'unknown-body-key :max 1
             :collect-keys '(unknown)
             :save-history-as 'history :body body)))
        (should-error (gptel-runner-start node :driver driver))))))

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

(ert-deftest gptel-runner-pause-after-holds-result-before-next-node ()
  (gptel-runner-test--isolated
    (dolist (agent '(planner implementer))
      (gptel-runner-register-agent agent :preset 'p))
    (let ((driver (gptel-runner-fake-driver-create))
          (callbacks 0))
      (gptel-runner-fake-queue
       driver 'planner '(:value "draft plan" :duplicate t))
      (gptel-runner-fake-queue
       driver 'implementer
       (lambda (call)
         (should (string-match-p
                  "approved plan" (gptel-runner-call-prompt call)))
         '(:value "implemented")))
      (let* ((run
              (gptel-runner-start
               (gptel-runner-sequence
                (gptel-runner-agent-step
                 :id 'plan :agent 'planner :prompt "Create the plan"
                 :save-as 'plan :pause-after t)
                (gptel-runner-agent-step
                 :id 'implement :agent 'implementer
                 :prompt (lambda (current-run _node)
                           (format "Implement: %s"
                                   (gptel-runner-get current-run 'plan)))
                 :save-as 'report))
               :driver driver
               :callback (lambda (_run) (cl-incf callbacks))))
             (call (car (gptel-runner-run-calls run))))
        (should (eq (gptel-runner-run-state run) 'running))
        (should (eq (gptel-runner-call-state call) 'waiting-feedback))
        (should (eq (gethash 'plan (gptel-runner-run-node-states run))
                    'running))
        (should-not (gptel-runner-get run 'plan))
        (should (= (length (gptel-runner-run-calls run)) 1))
        (should (= (gptel-runner-test--event-count
                    run 'call-waiting-feedback) 1))
        (should (zerop callbacks))
        (gptel-runner-complete-call-from-buffer "approved plan" call)
        (should (eq (gptel-runner-run-state run) 'succeeded))
        (should (equal (gptel-runner-get run 'plan) "approved plan"))
        (should (equal (gptel-runner-get run 'report) "implemented"))
        (should (= (length (gptel-runner-run-calls run)) 2))
        (should (= callbacks 1))))))

(ert-deftest gptel-runner-pause-after-survives-whole-run-pause ()
  (gptel-runner-test--isolated
    (dolist (agent '(planner implementer))
      (gptel-runner-register-agent agent :preset 'p))
    (let* ((snapshot-directory
            (make-temp-file "gptel-runner-pause-after-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow gated-handoff (:persist t)
              (gptel-runner-sequence
               :id 'handoff
               (gptel-runner-agent-step
                :id 'plan :agent 'planner :prompt "Create the plan"
                :save-as 'plan :pause-after t)
               (gptel-runner-agent-step
                :id 'implement :agent 'implementer
                :prompt (lambda (run _node)
                          (format "Implement: %s"
                                  (gptel-runner-get run 'plan)))
                :save-as 'report)))
            (gptel-runner-fake-queue driver 'planner '(:value "draft plan"))
            (gptel-runner-fake-queue
             driver 'implementer
             (lambda (call)
               (should (string-match-p
                        "final plan" (gptel-runner-call-prompt call)))
               '(:value "implemented")))
            (let* ((run (gptel-runner-start 'gated-handoff :driver driver))
                   (call (car (gptel-runner-run-calls run))))
              (should (eq (gptel-runner-call-state call) 'waiting-feedback))
              (gptel-runner-pause-run run 'test-approval)
              (should (eq (gptel-runner-run-state run) 'paused))
              (gptel-runner-complete-call-from-buffer "final plan" call)
              (should (eq (gptel-runner-run-state run) 'succeeded))
              (should (equal (gptel-runner-get run 'plan) "final plan"))
              (should (equal (gptel-runner-get run 'report) "implemented"))))
        (delete-directory snapshot-directory t)))))

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
                   (file (progn
                           (gptel-runner-record-decision
                            run "Keep the public API small."
                            "It reduces downstream maintenance.")
                           (gptel-runner-pause-run run 'overnight)
                                (gptel-runner-run-snapshot-file run))))
              (should (eq (gptel-runner-run-state run) 'paused))
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
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
                  (should (equal
                           (mapcar (lambda (entry) (plist-get entry :text))
                                   (gptel-runner-decisions restored))
                           '("Keep the public API small.")))
                  (should-not (gptel-runner-run-calls restored))
                  (should (equal
                           (mapcar #'gptel-runner-event-type
                                   (gptel-runner-run-events restored))
                           '(snapshot-loaded)))
                  (gptel-runner-resume-run restored "Use the smaller API")
                  (should (eq (gptel-runner-run-state restored) 'succeeded))
                  (should (string-match-p "Use the smaller API"
                                          observed-prompt))
                  (should (string-match-p "Keep the public API small"
                                          observed-prompt))
                  (should (equal (gptel-runner-get restored 'second-result)
                                 "resumed result"))
                  (should (= callbacks 1))
                  (should (= (gptel-runner-budget-calls
                              (gptel-runner-run-budget restored)) 3))
                  (should (= (gptel-runner-budget-requests
                              (gptel-runner-run-budget restored)) 3))
                  (should (= (length (gptel-runner-run-calls restored)) 1))
                  (should (eq (gptel-runner-call-state
                               (car (gptel-runner-run-calls restored)))
                              'succeeded))))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-extended-repeat-limit-survives-snapshot-load ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-extend-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow persisted-extension (:persist t)
              (gptel-runner-repeat-until
               :id 'cycle :max 1 :until (lambda (_run) nil)
               :collect-keys '(result) :save-history-as 'history
               :body (gptel-runner-test--step 'work 'worker 'result)))
            (gptel-runner-fake-queue
             driver 'worker '(:value "first") '(:value "second"))
            (let ((run (gptel-runner-start
                        'persisted-extension :driver driver)))
              (gptel-runner-extend-repeat run 'cycle 1)
              (should (eq (gptel-runner-run-state run) 'failed))
              (should (= (gptel-runner-iteration run 'cycle) 2))
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (let ((file (gptel-runner-run-snapshot-file run)))
                (setq gptel-runner--runs (make-hash-table :test #'equal))
                (let ((restored-driver (gptel-runner-fake-driver-create)))
                  (gptel-runner-fake-queue
                   restored-driver 'worker '(:value "third"))
                  (let ((restored
                         (gptel-runner-load-run file nil restored-driver)))
                    (should (eq (gptel-runner-run-state restored) 'failed))
                    (should (= (gethash
                                'cycle
                                (gptel-runner-run-repeat-limits restored))
                               2))
                    (gptel-runner-extend-repeat restored 'cycle 1)
                    (should (= (gptel-runner-iteration restored 'cycle) 3))
                    (should
                     (equal
                      (gptel-runner-get restored 'history)
                      '((:iteration 1 :values ((result . "first")))
                        (:iteration 2 :values ((result . "second")))
                        (:iteration 3 :values ((result . "third"))))))
                    (should
                     (eq (gptel-runner-test--wait-for-snapshot restored)
                         'clean)))))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-run-budget-extension-survives-snapshot-load ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-budget-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow persisted-budget (:persist t)
              (gptel-runner-repeat-until
               :id 'review-cycle :max 5
               :until (lambda (run)
                        (equal (gptel-runner-get run 'result) "pass"))
               :collect-keys '(result) :save-history-as 'history
               :body (gptel-runner-test--step 'work 'worker 'result)))
            (gptel-runner-fake-queue driver 'worker '(:value "revise"))
            (let ((run (gptel-runner-start
                        'persisted-budget :driver driver :max-calls 1)))
              (should (eq (gptel-runner-run-state run) 'failed))
              (should (= (gptel-runner-iteration run 'review-cycle) 1))
              (should (equal (gptel-runner-run-terminal-data run)
                             '(:type budget :kind calls :limit 1
                               :requests 1 :calls 1)))
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (let ((file (gptel-runner-run-snapshot-file run)))
                (setq gptel-runner--runs (make-hash-table :test #'equal))
                (let ((restored-driver (gptel-runner-fake-driver-create)))
                  (gptel-runner-fake-queue
                   restored-driver 'worker '(:value "pass"))
                  (let ((restored
                         (gptel-runner-load-run file nil restored-driver)))
                    (should-not (gptel-runner-run-calls restored))
                    (should (equal (gptel-runner-run-terminal-data restored)
                                   '(:type budget :kind calls :limit 1
                                     :requests 1 :calls 1)))
                    ;; Snapshots written before terminal-data was added have
                    ;; to infer the exhausted finite budget from accounting.
                    (setf (gptel-runner-run-terminal-data restored) nil)
                    (gptel-runner-extend
                     (gptel-runner-run-id restored) :additional-calls 1)
                    (should (eq (gptel-runner-run-state restored) 'succeeded))
                    (should (= (gptel-runner-iteration
                                restored 'review-cycle) 2))
                    (should (= (length (gptel-runner-run-calls restored)) 1))
                    (should
                     (equal
                      (gptel-runner-get restored 'history)
                      '((:iteration 1 :values ((result . "revise")))
                        (:iteration 2 :values ((result . "pass"))))))
                    (should (= (gptel-runner-test--event-count
                                restored 'run-extended) 1))
                    (should
                     (eq (gptel-runner-test--wait-for-snapshot restored)
                         'clean)))))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-persistence-requires-named-workflow ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (should-error
     (gptel-runner-start
      (gptel-runner-test--step 'work 'worker)
     :driver (gptel-runner-fake-driver-create) :persist t)
     :type 'user-error)))

(ert-deftest gptel-runner-restored-unfinished-call-restarts-statelessly ()
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
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (setq gptel-runner--runs (make-hash-table :test #'equal))
              (let ((resume-driver (gptel-runner-fake-driver-create)))
                (gptel-runner-fake-queue
                 resume-driver 'worker '(:value "restarted result"))
                (let ((restored
                       (gptel-runner-load-run
                        (gptel-runner-run-snapshot-file run)
                        nil resume-driver)))
                  (should-not (gptel-runner-run-calls restored))
                  (gptel-runner-resume-run restored)
                  (should (eq (gptel-runner-run-state restored) 'succeeded))
                  (should (equal (gptel-runner-get restored 'report)
                                 "restarted result"))
                  (should (= (length (gptel-runner-run-calls restored)) 1))))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-checkpoints-are-coalesced-and-save-latest-state ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-coalesce-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (gptel-runner-checkpoint-delay 0.02)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow coalesced-save (:persist t)
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work" :save-as 'report))
            (gptel-runner-fake-queue driver 'worker '(:manual t))
            (let ((run (gptel-runner-start 'coalesced-save :driver driver)))
              (gptel-runner-put run 'progress "one")
              (gptel-runner-put run 'progress "two")
              (should (eq (gptel-runner-store-save-status run) 'pending))
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (let* ((snapshot
                      (gptel-runner-store--read-file
                       (gptel-runner-run-snapshot-file run)))
                     (blackboard
                      (plist-get (plist-get snapshot :run) :blackboard)))
                (should (equal (cdr (assq 'progress blackboard)) "two")))
              (should (= (gptel-runner-test--event-count
                          run 'snapshot-saved)
                         1))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-explicit-save-does-not-write-synchronously ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-queued-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow queued-save ()
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work"))
            (gptel-runner-fake-queue driver 'worker '(:value "done"))
            (let* ((run (gptel-runner-start 'queued-save :driver driver))
                   (file
                    (cl-letf (((symbol-function
                                'gptel-runner-store--begin)
                               (lambda (_coordinator)
                                 (ert-fail "save began synchronously"))))
                      (gptel-runner-save-run run))))
              (should (stringp file))
              (should-not (file-exists-p file))
              (should (eq (gptel-runner-store-save-status run) 'pending))
              (gptel-runner-store-flush-pending)
              (should (eq (gptel-runner-store-save-status run) 'clean))
              (should (file-exists-p file))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-v1-snapshot-is-rejected ()
  (let ((file (make-temp-file "gptel-runner-v1-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (prin1 '(:format gptel-runner-snapshot :version 1 :run nil)
                   (current-buffer)))
          (should-error (gptel-runner-store--read-file file)
                        :type 'user-error))
      (delete-file file))))

(ert-deftest gptel-runner-save-error-preserves-snapshot-and-can-retry ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-error-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow retry-save ()
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work"))
            (gptel-runner-fake-queue driver 'worker '(:value "done"))
            (let ((run (gptel-runner-start 'retry-save :driver driver)))
              (gptel-runner-put run 'progress "old")
              (gptel-runner-save-run run)
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (with-temp-buffer
                (gptel-runner-put run 'unreadable (current-buffer))
                (gptel-runner-save-run run)
                (should (eq (gptel-runner-test--wait-for-snapshot run)
                            'error)))
              (let* ((old-snapshot
                      (gptel-runner-store--read-file
                       (gptel-runner-run-snapshot-file run)))
                     (old-board
                      (plist-get (plist-get old-snapshot :run) :blackboard)))
                (should (equal (cdr (assq 'progress old-board)) "old"))
                (should-not (assq 'unreadable old-board)))
              (remhash 'unreadable (gptel-runner-run-blackboard run))
              (gptel-runner-put run 'progress "new")
              (gptel-runner-save-run run)
              (should (eq (gptel-runner-test--wait-for-snapshot run) 'clean))
              (let* ((new-snapshot
                      (gptel-runner-store--read-file
                       (gptel-runner-run-snapshot-file run)))
                     (new-board
                      (plist-get (plist-get new-snapshot :run) :blackboard)))
                (should (equal (cdr (assq 'progress new-board)) "new")))
              (should (= (gptel-runner-test--event-count
                          run 'snapshot-error)
                         1))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-state-changing-during-save-queues-new-generation ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-generation-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow generation-save ()
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work"))
            (gptel-runner-fake-queue driver 'worker '(:value "done"))
            (let* ((run (gptel-runner-start 'generation-save :driver driver))
                   (_old (gptel-runner-put run 'progress "old"))
                   (_file (gptel-runner-save-run run))
                   (coordinator
                    (gethash run gptel-runner-store--coordinators)))
              ;; Begin generation one, then change state before it commits.
              (gptel-runner-store--cancel-timer coordinator)
              (gptel-runner-store--begin coordinator)
              (gptel-runner-store--cancel-timer coordinator)
              (gptel-runner-put run 'progress "new")
              (gptel-runner-store--write-slice coordinator t)
              (should (eq (gptel-runner-store-save-status run) 'pending))
              (gptel-runner-store--cancel-timer coordinator)
              (gptel-runner-store--begin coordinator)
              (gptel-runner-store--cancel-timer coordinator)
              (gptel-runner-store--write-slice coordinator t)
              (should (eq (gptel-runner-store-save-status run) 'clean))
              (let* ((snapshot
                      (gptel-runner-store--read-file
                       (gptel-runner-run-snapshot-file run)))
                     (blackboard
                      (plist-get (plist-get snapshot :run) :blackboard)))
                (should (equal (cdr (assq 'progress blackboard)) "new")))
              (should (= (gptel-runner-test--event-count
                          run 'snapshot-saved)
                         2))))
        (delete-directory snapshot-directory t)))))

(ert-deftest gptel-runner-forget-cancels-a-queued-save ()
  (gptel-runner-test--isolated
    (gptel-runner-register-agent 'worker :preset 'p)
    (let* ((snapshot-directory (make-temp-file "gptel-runner-cancel-save-" t))
           (gptel-runner-snapshot-directory snapshot-directory)
           (driver (gptel-runner-fake-driver-create)))
      (unwind-protect
          (progn
            (gptel-runner-defworkflow cancelled-save ()
              (gptel-runner-agent-step
               :id 'work :agent 'worker :prompt "work"))
            (gptel-runner-fake-queue driver 'worker '(:value "done"))
            (let* ((run (gptel-runner-start 'cancelled-save :driver driver))
                   (file (gptel-runner-save-run run)))
              (should (eq (gptel-runner-store-save-status run) 'pending))
              (gptel-runner-forget-run run t)
              (accept-process-output nil 0.02)
              (should-not (file-exists-p file))
              (should-not
               (gethash run gptel-runner-store--coordinators))))
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
             (entries (gptel-runner-ui--entries))
             (ids (mapcar #'car entries)))
        (should (equal ids
                       (list '(workflow alpha-workflow)
                             (list 'run (gptel-runner-run-id run))
                             (list 'call (gptel-runner-call-id call))
                             '(workflow empty-workflow))))
        ;; Cell contents start at their column edge; tabulated-list itself
        ;; supplies all spacing between columns.
        (should (equal (aref (cadr (nth 1 entries)) 1)
                       (gptel-runner-run-id run)))
        (should (equal (aref (cadr (nth 2 entries)) 2) "alpha-step"))
        (with-temp-buffer
          (gptel-runner-dashboard-mode)
          (tabulated-list-print t)
          (should (string-match-p "alpha-workflow" (buffer-string)))
          (should (string-match-p (gptel-runner-run-id run)
                                  (buffer-string)))
          (should (string-match-p "empty-workflow" (buffer-string))))))))

(ert-deftest gptel-runner-dashboard-columns-are-configurable-as-a-set ()
  (let ((gptel-runner-dashboard-columns '(state workflow call)))
    (should (equal
             (mapcar #'car (append (gptel-runner-ui--format) nil))
             '("Workflow" "Call" "State")))
    (should (= (length
                (cadr (gptel-runner-ui--workflow-entry 'workflow 2)))
               3)))
  (let ((gptel-runner-dashboard-columns '(bogus workflow)))
    (should-error (gptel-runner-ui--format) :type 'user-error))
  (let ((gptel-runner-dashboard-columns nil))
    (should-error (gptel-runner-ui--format) :type 'user-error)))

(ert-deftest gptel-runner-dashboard-shows-active-tool-name ()
  (let* ((gptel-runner-dashboard-columns '(state))
         (run (gptel-runner-run-create
               :id "run" :iterations (make-hash-table :test #'equal)
               :budget (gptel-runner-budget-create)))
         (node (gptel-runner-node-create :id 'work))
         (call (gptel-runner-call-create
                :id "call" :run run :node node :state 'waiting-tool
                :tool-names '("AskUserQuestion")))
         (state-cell (aref (cadr (gptel-runner-ui--call-entry run call)) 0)))
    (should (equal (substring-no-properties state-cell)
                   "Calling AskUserQuestion..."))
    (should (eq (get-text-property 0 'face state-cell) 'warning))))

(ert-deftest gptel-runner-dashboard-long-values-remain-column-aligned ()
  (let ((gptel-runner-dashboard-columns '(workflow run node state)))
    (with-temp-buffer
      (gptel-runner-dashboard-mode)
      (setq tabulated-list-entries
            (list
             (list '(workflow long)
                   (gptel-runner-ui--row-vector
                    '((workflow . "this-is-a-very-long-workflow-name")
                      (state . "1 run"))))
             (list '(run "run-1")
                   (gptel-runner-ui--row-vector
                    '((run . "  run-1") (state . "succeeded"))))
             (list '(call "call-2")
                   (gptel-runner-ui--row-vector
                    '((node . "    this-is-a-very-long-node-name")
                      (state . "running"))))))
      (tabulated-list-print t)
      (let* ((lines (split-string (buffer-string) "\n" t))
             (workflow-column (string-match "1 run" (nth 0 lines)))
             (run-column (string-match "succeeded" (nth 1 lines)))
             (node-column (string-match "running" (nth 2 lines)))
             (workflow-cell (aref (cadr (nth 0 tabulated-list-entries)) 0)))
        (should (= workflow-column run-column node-column))
        (should (<= (string-width workflow-cell) 22))
        (should (string-suffix-p "…" workflow-cell))
        (should (equal (get-text-property 0 'help-echo workflow-cell)
                       "this-is-a-very-long-workflow-name"))))))

(ert-deftest gptel-runner-dashboard-configures-auto-refresh-timer ()
  (let ((gptel-runner-dashboard-refresh-interval 3.5)
        (timer (timer-create)) scheduled cancelled (refreshes 0))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (delay repeat function buffer)
                 (setq scheduled (list delay repeat function buffer))
                 timer))
              ((symbol-function 'cancel-timer)
               (lambda (candidate) (setq cancelled candidate))))
      (with-temp-buffer
        (gptel-runner-dashboard-mode)
        (should (equal (seq-take scheduled 3)
                       '(3.5 3.5
                         gptel-runner-dashboard--refresh-buffer)))
        (should (eq (nth 3 scheduled) (current-buffer)))
        (should (eq gptel-runner-dashboard--refresh-timer timer))
        (cl-letf (((symbol-function 'gptel-runner-dashboard-refresh)
                   (lambda () (cl-incf refreshes))))
          (funcall (nth 2 scheduled) (nth 3 scheduled))
          (should (= refreshes 1)))
        (fundamental-mode)
        (should (eq cancelled timer))
        (cl-letf (((symbol-function 'gptel-runner-dashboard-refresh)
                   (lambda () (cl-incf refreshes))))
          (funcall (nth 2 scheduled) (nth 3 scheduled))
          (should (= refreshes 1)))))))

(ert-deftest gptel-runner-dashboard-auto-refresh-can-be-disabled ()
  (let ((gptel-runner-dashboard-refresh-interval nil)
        scheduled)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest arguments) (setq scheduled arguments))))
      (with-temp-buffer
        (gptel-runner-dashboard-mode)
        (should-not scheduled)
        (should-not gptel-runner-dashboard--refresh-timer)))))

(ert-deftest gptel-runner-dashboard-rejects-invalid-refresh-interval ()
  (dolist (interval '(0 -1 invalid))
    (let ((gptel-runner-dashboard-refresh-interval interval))
      (with-temp-buffer
        (should-error (gptel-runner-dashboard-mode) :type 'user-error)))))

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
