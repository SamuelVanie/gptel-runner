;;; gptel-runner.el --- Deterministic stateless agent workflows -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.4"))
;; Keywords: tools, convenience

;;; Commentary:

;; gptel-runner schedules inspectable workflow ASTs over stateless agent calls.
;; It provides sequence, branch, bounded repeat, and parallel nodes together
;; with budgets, retries, cancellation, an event journal, and a dashboard.
;; See README.md and the examples directory for usage.

;;; Code:

(require 'gptel-runner-core)
(require 'gptel-runner-flow)
(require 'gptel-runner-fake)
(require 'gptel-runner-gptel)
(require 'gptel-runner-ui)

(provide 'gptel-runner)
;;; gptel-runner.el ends here
