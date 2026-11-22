;;; release-brief.el --- Twelve agents working toward one release brief -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file and run M-x gptel-runner-example-release-brief-demo to
;; watch a run with scripted replies.  To use your configured gptel model:
;;
;; (setq my-run (gptel-runner-start
;;               'release-brief :goal "Prepare a release brief for v2.0"
;;               :inputs '((changes . "Paste your release notes or diff here."))))
;;
;; After the run finishes, read (gptel-runner-get my-run 'release-brief).

;;; Code:

(require 'gptel-runner)

(defun release-brief-step (id task &optional sources save-as)
  "Create agent step ID for TASK using the values named in SOURCES.
Save the reply under SAVE-AS, or under ID when SAVE-AS is omitted."
  (let ((agent (intern (format "brief-%s" id))))
    (gptel-runner-register-agent agent)
    (gptel-runner-agent-step
     :id id :agent agent :save-as (or save-as id)
     :prompt
     (lambda (run _node)
       (format "%s\n\n%s\n\n%s" (gptel-runner-run-goal run) task
               (mapconcat (lambda (key)
                            (format "%s: %s" key (gptel-runner-get run key)))
                          (or sources '(changes)) "\n\n"))))))

(gptel-runner-defworkflow release-brief
    (:completion response :max-concurrency 6 :max-calls 12 :max-requests 24)
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-parallel
    :id 'research
    (release-brief-step 'scope "Which parts of the product changed?")
    (release-brief-step 'features "Identify new capabilities.")
    (release-brief-step 'fixes "Identify the bugs fixed.")
    (release-brief-step 'breaking "Which changes break existing usage?")
    (release-brief-step 'upgrades "What carries over when upgrading?")
    (release-brief-step 'docs "Find the documentation for this release."))
   (gptel-runner-parallel
    :id 'review
    (release-brief-step 'audience "Who needs this brief?" '(scope breaking))
    (release-brief-step 'impact "Explain the user impact." '(features fixes))
    (release-brief-step 'actions "List migration steps." '(breaking upgrades docs))
    (release-brief-step 'risks "Flag risks for existing users." '(breaking upgrades)))
   (release-brief-step 'draft "Draft a concise release brief." '(audience impact actions risks))
   (release-brief-step 'finalize "Check the draft against the changes and return the brief."
                       '(draft changes) 'release-brief)))

(defvar gptel-runner-example-release-brief-delay 1.0
  "Seconds each scripted agent takes to reply in the release brief demo.")

(defun gptel-runner-example-release-brief-demo ()
  "Run the release brief with scripted replies and return the run.
When called interactively, show its progress in the dashboard."
  (interactive)
  (let ((driver (gptel-runner-fake-driver-create))
        (changes (concat "Interrupted exports can now resume. Retrying used to duplicate rows. "
                         "The report export command replaces report dump. Saved schedules "
                         "carry over. The migration guide includes before-and-after examples.")))
    (dolist (reply
             '((brief-scope . "The reporting CLI and its export jobs changed.")
               (brief-features . "Interrupted exports can resume.")
               (brief-fixes . "A retry no longer duplicates rows.")
               (brief-breaking . "The report dump command was replaced by report export.")
               (brief-upgrades . "Saved schedules carry over.")
               (brief-docs . "The migration guide has before-and-after examples.")
               (brief-audience . "The brief is for CLI users and people maintaining export scripts.")
               (brief-impact . "Users can resume interrupted exports without duplicating rows.")
               (brief-actions . "Replace report dump with report export. See the migration guide.")
               (brief-risks . "Existing scripts using report dump will break. Schedules carry over.")
               (brief-draft . "Exports resume without duplicate rows. Update scripts to use report export.")
               (brief-finalize . "v2.0 resumes interrupted exports and fixes duplicate rows on retry.\nWhen upgrading, replace report dump with report export in scripts.\nSaved schedules carry over. See the migration guide for examples.")))
      (gptel-runner-fake-queue driver (car reply)
                               (list :value (cdr reply) :delay gptel-runner-example-release-brief-delay)))
    (let ((run (gptel-runner-start 'release-brief
                                   :goal "Prepare a release brief for v2.0"
                                   :inputs (list (cons 'changes changes)) :driver driver)))
      (when (called-interactively-p 'interactive)
        (gptel-runner-show-dashboard))
      run)))

(provide 'release-brief)
;;; release-brief.el ends here
