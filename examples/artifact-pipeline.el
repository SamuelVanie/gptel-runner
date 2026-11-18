;;; artifact-pipeline.el --- Launch-independent output example -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file and call (gptel-runner-start 'artifact-pipeline), or use it
;; as a background job's :init-file with :preload-features nil.  The same
;; workflow owns its report storage and returns the same artifact descriptor.

;;; Code:

(require 'gptel-runner)

(gptel-runner-defworkflow artifact-pipeline ()
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-elisp-step
    :id 'report :save-as 'report
    :function
    (lambda (run _node)
      (gptel-runner-write-artifact
       run "report.txt"
       (format "Report for %s\n" (gptel-runner-get run 'subject "the project")))))
   (gptel-runner-elisp-step
    :id 'read-report :save-as 'report-text
    :function
    (lambda (run _node)
      (gptel-runner-read-artifact run (gptel-runner-get run 'report))))
   (gptel-runner-elisp-step :id 'empty :save-as 'empty :function #'ignore)))

(provide 'artifact-pipeline)
;;; artifact-pipeline.el ends here
