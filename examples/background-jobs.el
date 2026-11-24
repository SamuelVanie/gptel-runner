;;; background-jobs.el --- Network-free background pipeline example -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this file as a job's :init-file.  It defines two pipelines:
;; background-example completes unattended; background-example-feedback waits
;; for acceptance of its final report.  Both run two agents in parallel,
;; retain their individual results, and publish a report file.
;; Save the job with :preload-features nil to run without gptel packages.
;;
;; New jobs preload gptel.  When gptel-agent is on the worker's search path
;; at registration, they also preload gptel-agent and discover its presets.
;; For live runs, the init file configures backends, registers runner agents
;; and workflows, and selects a gptel-runner-gptel-driver.

;;; Code:

(require 'gptel-runner)
(require 'gptel-runner-jobs)

(dolist (name '(background-example-a background-example-b background-example-summary))
  (gptel-runner-register-agent name :preset name :workspace-mode 'read))

(defun background-example-prompt (run node)
  "Build a prompt for RUN's NODE using a supplied subject input."
  (format "Goal: %s\nSubject: %s\nWorkspace: %s\nTask: %s"
          (gptel-runner-run-goal run)
          (or (gptel-runner-get run 'subject) "background jobs")
          (gptel-runner-node-workspace run node)
          (gptel-runner-node-id node)))

(defun background-example-analysis (call)
  "Return a delayed fake analysis outcome for CALL."
  (list :delay 1.0
        :value (format "%s examined %s."
                       (gptel-runner-agent-name (gptel-runner-call-agent call))
                       (or (gptel-runner-get (gptel-runner-call-run call) 'subject)
                           "background jobs"))))

(defun background-example-summary (call)
  "Publish and return a combined report for CALL."
  (let* ((run (gptel-runner-call-run call))
         (report (format "Parallel pipeline report\n\n%s\n%s\n"
                         (gptel-runner-get run 'analysis-a)
                         (gptel-runner-get run 'analysis-b))))
    (gptel-runner-put
     run 'report-artifact
     (gptel-runner-write-artifact run "report.txt" report "text/plain"))
    (list :value report)))

(defun background-example-workflow (pause-after)
  "Build the example workflow, optionally stopping at PAUSE-AFTER."
  (gptel-runner-sequence
   :id 'example-pipeline
   (gptel-runner-parallel
    :id 'analyses :policy 'collect
    (gptel-runner-agent-step
     :id 'analysis-a :agent 'background-example-a
     :prompt #'background-example-prompt :save-as 'analysis-a)
    (gptel-runner-agent-step
     :id 'analysis-b :agent 'background-example-b
     :prompt #'background-example-prompt :save-as 'analysis-b))
   (gptel-runner-agent-step
    :id 'summary :agent 'background-example-summary
    :prompt #'background-example-prompt :save-as 'report
    :pause-after pause-after)))

(gptel-runner-defworkflow background-example
    (:max-concurrency 2 :max-calls 20 :max-requests 20)
  (background-example-workflow nil))

(gptel-runner-defworkflow background-example-feedback
    (:max-concurrency 2 :max-calls 20 :max-requests 20)
  (background-example-workflow t))

(setq gptel-runner-default-driver (gptel-runner-fake-driver-create))
;; Enough scripts for manual retries/continuations in the same worker.
(dotimes (_ 20)
  (gptel-runner-fake-queue gptel-runner-default-driver 'background-example-a
                          #'background-example-analysis)
  (gptel-runner-fake-queue gptel-runner-default-driver 'background-example-b
                          #'background-example-analysis)
  (gptel-runner-fake-queue gptel-runner-default-driver 'background-example-summary
                          #'background-example-summary))

(provide 'background-jobs)
;;; background-jobs.el ends here
