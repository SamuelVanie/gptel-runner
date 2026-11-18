;;; mixed-elisp.el --- Mix local Lisp and agent steps -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file, configure the `summarizer' gptel preset, and start:
;; (gptel-runner-start 'mixed-elisp :workspace "/path/to/project/")
;; Read the result with (gptel-runner-get run 'report).
;; For a network-free demonstration, use the fake driver:
;; (let ((driver (gptel-runner-fake-driver-create)))
;;   (gptel-runner-fake-queue driver 'mixed-elisp-summarizer
;;                            '(:value "A small Emacs Lisp package."))
;;   (gptel-runner-start 'mixed-elisp :driver driver))

;;; Code:

(require 'gptel-runner)

(gptel-runner-register-agent 'mixed-elisp-summarizer :preset 'summarizer)

(gptel-runner-defworkflow mixed-elisp (:max-calls 1 :max-requests 3)
  (gptel-runner-sequence
   :id 'pipeline
   (gptel-runner-elisp-step
    :id 'list-files :save-as 'files
    :function (lambda (_run _node)
                (directory-files default-directory nil "\\.el\\'")))
   (gptel-runner-agent-step
    :id 'summarize :agent 'mixed-elisp-summarizer :save-as 'summary
    :prompt (lambda (run _node)
              (format "Summarize what these source filenames suggest:\n%s"
                      (mapconcat #'identity (gptel-runner-get run 'files) "\n"))))
   (gptel-runner-elisp-step
    :id 'build-report :save-as 'report
    :function (lambda (run _node)
                (list :file-count (length (gptel-runner-get run 'files))
                      :summary (gptel-runner-get run 'summary))))))

(provide 'mixed-elisp)
;;; mixed-elisp.el ends here
