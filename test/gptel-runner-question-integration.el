;;; gptel-runner-question-integration.el --- Background Agent questions -*- lexical-binding: t; -*-

(require 'gptel-runner-delegation-test)
(require 'gptel-runner-jobs-test)
(require 'gptel-runner-jobs-panel)

(defun gptel-runner-question-test--reply (fsm action)
  "Perform scripted provider ACTION on worker request FSM."
  (pcase action
    ('dataset
     (gptel-runner-delegation-test--tool
      fsm "AskUserQuestion"
      '(:action "add" :questions [(:question "Which dataset?"
                                   :choices [(:value "Small") (:value "Large")])])))
    ('label
     (gptel-runner-delegation-test--tool
      fsm "AskUserQuestion" '(:action "add" :questions [(:question "What label?")])))
    ('wait (gptel-runner-delegation-test--tool fsm "AskUserQuestion" '(:action "wait")))
    (_ (let* ((messages (map-nested-elt (gptel-fsm-info fsm) '(:data :messages)))
              (reply (aref messages (1- (length messages)))))
         ;; Surface the actual tool response seen by the next model turn.
         (gptel-runner-delegation-test--answer fsm (plist-get reply :content))))))

(defun gptel-runner-question-test--init (preset script)
  "Configure bundled PRESET and offline provider SCRIPT inside a batch worker."
  (setq gptel-backend (gptel-make-openai "question-test" :key "unused"
                                         :host "example.invalid" :models '(test))
        gptel-model 'test gptel-stream nil gptel-use-curl t
        gptel-runner-job-poll-interval 0.02)
  (let ((rounds (make-hash-table :test #'eq)))
    (advice-add 'gptel-curl-get-response :override
                (lambda (fsm)
                  (let ((round (gethash fsm rounds 0)))
                    (puthash fsm (1+ round) rounds)
                    (run-at-time 0 nil #'gptel-runner-question-test--reply
                                 fsm (nth round script))))))
  (gptel-runner-register-agent 'interviewer :preset preset)
  (gptel-runner-defworkflow runner-question-test ()
    (gptel-runner-agent-step :id 'choose :agent 'interviewer :prompt "Choose a dataset"
                             :completion 'response :save-as 'answer)))

(defun gptel-runner-question-test--start (preset script &optional max-requests)
  "Run a private job using PRESET, SCRIPT and optional MAX-REQUESTS."
  (let ((gptel-runner-job-preload-features '(gptel gptel-agent))
        (init (expand-file-name "init.el" gptel-runner-job-directory)))
    (with-temp-file init
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (prin1 '(require 'gptel-runner-question-integration) (current-buffer))
      (terpri (current-buffer))
      (prin1 `(gptel-runner-question-test--init ',preset ',script) (current-buffer)))
    (gptel-runner-job-run
     (gptel-runner-jobs-test--job 'runner-question-test nil init
                                  (when max-requests (list :max-requests max-requests))))))

(defun gptel-runner-question-test--wait (invocation &optional previous)
  "Wait for INVOCATION to finish or publish a batch other than PREVIOUS."
  (gptel-runner-jobs-test--wait
   invocation (lambda (run)
                (or (eq (plist-get run :phase) 'closed)
                    (when-let* ((batch (car (plist-get run :interactions))))
                      (not (equal (plist-get batch :id) (plist-get previous :id))))))))

(ert-deftest gptel-runner-delegation-job-panel-answers-bundled-agent-questions ()
  (dolist (preset '(gptel-agent gptel-plan ask))
    (ert-info ((format "Bundled preset %s" preset))
      (gptel-runner-jobs-test--isolated
        (let* ((invocation (gptel-runner-question-test--start preset '(dataset label wait)))
               (record (gptel-runner-question-test--wait invocation))
               (call (car (plist-get record :calls)))
               (batch (car (plist-get record :interactions)))
               (target (list 'background-call (plist-get record :id)
                             (plist-get call :epoch) (plist-get call :id))))
          (should (eq (plist-get batch :kind) 'question))
          (should (equal (mapcar (lambda (q) (plist-get q :question))
                                 (plist-get batch :questions))
                         '("Which dataset?" "What label?")))
          (save-window-excursion
            (let ((gptel-runner-background--target target)
                  (gptel-runner-dashboard-refresh-interval nil)
                  (answers (list "Small" "November")))
              (gptel-runner-background-visit)
              (unwind-protect
                  (progn
                    (should (derived-mode-p 'gptel-runner-panel-mode))
                    (should (string-search "Which dataset?" (buffer-string)))
                    (should (string-search "Small" (buffer-string)))
                    (should (string-search "What label?" (buffer-string)))
                    (goto-char (point-min))
                    (search-forward "Answer questions")
                    (cl-letf (((symbol-function 'completing-read)
                               (lambda (&rest _) (pop answers)))
                              ((symbol-function 'read-string)
                               (lambda (&rest _) (pop answers))))
                      (button-activate (button-at (1- (point)))))
                    (should-not answers))
                (when (derived-mode-p 'gptel-runner-panel-mode)
                  (kill-buffer (current-buffer))))))
          (let ((finished (gptel-runner-jobs-test--finished invocation)))
            (should (eq (plist-get finished :state) 'succeeded))
            (should-not (plist-get finished :interactions))
            (should (equal (alist-get 'answer (gptel-runner-invocation-outputs finished))
                           "Q1: Which dataset?\nR1: Small\n\nQ2: What label?\nR2: November"))
            (should (eq (plist-get (car (gptel-runner-invocation-commands finished)) :status)
                        'applied))))))))

(ert-deftest gptel-runner-delegation-question-resume-respects-request-budget ()
  (gptel-runner-jobs-test--isolated
    (let* ((job (gptel-runner-question-test--start 'ask '(dataset wait) 5))
           (first (car (plist-get (gptel-runner-question-test--wait job) :interactions))))
      (should first)
      (gptel-runner-jobs-test--command job 'pause)
      (gptel-runner-jobs-test--command job 'resume)
      (let* ((record (gptel-runner-question-test--wait job first))
             (fresh (car (plist-get record :interactions))))
        (should fresh)
        (should-not (equal (plist-get first :id) (plist-get fresh :id)))
        (cl-flet ((reply (batch text)
                    (list :answers (list text) :interaction-id (plist-get batch :id)
                          :call-id (plist-get batch :call-id) :call-epoch (plist-get batch :epoch)
                          :generation (plist-get batch :generation))))
          (let ((stale (gptel-runner-job-command job 'answer (reply first "Too late"))))
            (gptel-runner-jobs-test--command job 'answer (reply fresh "Small"))
            (should (eq 'rejected (plist-get (gptel-runner-job-command-status
                                              job (plist-get stale :id)) :status))))))
      (let* ((finished (gptel-runner-jobs-test--finished job))
             (snapshot (gptel-runner-store--read-file
                        (expand-file-name "checkpoint.el" (plist-get finished :directory)))))
        (should (eq (plist-get finished :state) 'succeeded))
        (should (equal (alist-get 'answer (gptel-runner-invocation-outputs finished))
                       "Q1: Which dataset?\nR1: Small"))
        ;; First add/wait, then a fresh add/wait and the synthesis send.
        (should (= 5 (map-nested-elt snapshot '(:run :budget :requests))))))))

(ert-deftest gptel-runner-delegation-questions-route-to-owning-child-once ()
  (let ((gptel-runner-delegation-test--tool-names
         '("Agent" "Discussion" "DiscussionControl" "AskUserQuestion")))
    (gptel-runner-delegation-test--with-run
      (let* ((agent (gptel-runner-delegation-test--agent fsm t))
             (child (gptel-agent--run-fsm agent))
             (chat (gptel-runner-delegation-test--discussion fsm))
             (discussion (gptel-agent-discussion--chat-fsm chat))
             (gptel-runner-jobs--run pipeline)
             (gptel-runner-jobs--invocation '(:id "children" :epoch 1)))
        (cl-flet ((ask (request question)
                    (gptel-runner-delegation-test--tool
                     request "AskUserQuestion"
                     (list :action "add" :questions (vector (list :question question))))
                    (gptel-runner-delegation-test--tool
                     request "AskUserQuestion" '(:action "wait")))
                  (answer (batch responses)
                    (gptel-runner-jobs--apply-command
                     (list :operation 'answer :invocation "children" :epoch 1
                           :arguments (list :call-id (gptel-runner-call-id call)
                                            :call-epoch 1 :generation (plist-get batch :generation)
                                            :interaction-id (plist-get batch :id)
                                            :answers responses)))))
          (ask child "Child question?")
          (ask discussion "Discussion question?")
          (let* ((batches (gptel-runner-gptel-pending-interactions call))
                 (first (cl-find (buffer-name (gptel-agent--run-child-buffer agent)) batches
                                 :test #'equal :key (lambda (b) (plist-get b :source-buffer))))
                 (second (cl-find (buffer-name (gptel-agent-discussion--chat-buffer chat)) batches
                                  :test #'equal :key (lambda (b) (plist-get b :source-buffer)))))
            (should (= (length batches) 2))
            (should first)
            (should second)
            (should-not (equal (plist-get first :id) (plist-get second :id)))
            (setq requests nil)
            (should-error (gptel-runner-gptel-approve-interactions
                           call (list (plist-get first :id) (plist-get second :id))))
            (should-error (answer second '("One" "Extra")))
            (should-not requests)
            (should (= 2 (length (gptel-runner-gptel-pending-interactions call))))
            (answer second '("For discussion only"))
            (should (equal requests (list discussion)))
            (should (string-search "R1: For discussion only"
                                   (prin1-to-string (plist-get (gptel-fsm-info discussion) :data))))
            (should (equal (gptel-runner-gptel-pending-interactions call) (list first)))
            (should-error (answer second '("Duplicate")))
            (setq requests nil)
            (gptel-runner-abort-call call)
            (should-not (gptel-runner-gptel-pending-interactions call))
            (should-error (answer first '("After cancellation")))
            (should-not requests)))))))

(ert-deftest gptel-runner-delegation-remote-cancel-resumes-implicit-question-wait ()
  (let ((gptel-runner-delegation-test--tool-names '("AskUserQuestion")))
    (gptel-runner-delegation-test--with-run
      (gptel-runner-delegation-test--tool
       fsm "AskUserQuestion" '(:questions [(:question "Anything to add?")]))
      ;; The bundled tool also collects outstanding questions when the
      ;; model replies before making its explicit wait call.
      (gptel-runner-delegation-test--answer fsm "Provisional reply")
      (let* ((pending (gptel-runner-gptel-pending-interactions call))
             (batch (car pending))
             (gptel-runner-jobs--run pipeline)
             (gptel-runner-jobs--invocation '(:id "cancel" :epoch 1))
             (command (list :operation 'cancel-questions :invocation "cancel" :epoch 1
                            :arguments (list :call-id (gptel-runner-call-id call)
                                             :call-epoch 1 :generation (plist-get batch :generation)
                                             :interaction-id (plist-get batch :id)))))
        (should (= 1 (length pending)))
        (should (eq 'question (plist-get batch :kind)))
        (setq requests nil)
        (gptel-runner-jobs--apply-command command)
        (gptel-runner-delegation-test--settle)
        (should (equal requests (list fsm)))
        (should (string-search "User cancelled interaction"
                               (prin1-to-string (plist-get (gptel-fsm-info fsm) :data))))
        (should-not (gptel-runner-gptel-pending-interactions call))
        (should-error (gptel-runner-jobs--apply-command command))
        (gptel-runner-delegation-test--answer fsm "Final after cancellation")
        (should (eq (gptel-runner-run-state pipeline) 'succeeded))
        (should (equal (gptel-runner-call-result call) "Final after cancellation"))))))

(provide 'gptel-runner-question-integration)
;;; gptel-runner-question-integration.el ends here
