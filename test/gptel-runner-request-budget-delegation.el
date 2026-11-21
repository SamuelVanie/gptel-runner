;;; gptel-runner-request-budget-delegation.el --- Supervised send budgets -*- lexical-binding: t; -*-

(require 'gptel-runner-delegation-test)

(ert-deftest gptel-runner-delegation-request-budget-stops-parent-and-descendants ()
  (dolist (background '(nil t))
    (let ((gptel-runner-delegation-test--start-arguments '(:max-requests 1)))
      (gptel-runner-delegation-test--with-run
        (should (eq (pop requests) fsm))
        (let* ((child (gptel-runner-delegation-test--agent fsm background))
               (worker (pop requests)))
          ;; Child requests have their own limits; the root may not send
          ;; another synthesis request when its own budget is exhausted.
          (should worker)
          (unless background
            (should (eq worker (gptel-agent--run-fsm child)))
            (gptel-runner-delegation-test--answer worker "Findings"))
          (gptel-runner-delegation-test--settle)
          (should-not requests)
          (should (eq (gptel-runner-run-state pipeline) 'failed))
          (should (eq (gptel-runner-call-state call) 'failed))
          (should (eq (plist-get (gptel-runner-call-error call) :kind) 'requests))
          (should (= (gptel-runner-budget-requests
                      (gptel-runner-run-budget pipeline)) 1))
          (should (zerop (gptel-runner-run-active-count pipeline)))
          (should-not (plist-get (gptel-runner-call-driver-data call) :agent-supervision))
          (when background
            (should-not (hash-table-values gptel-agent--runs))
            (should (eq (plist-get (car gptel-agent--run-history) :state)
                        'cancelled)))
          (gptel--fsm-transition fsm 'WAIT)
          (gptel-runner-delegation-test--settle)
          (should-not requests))))))

(provide 'gptel-runner-request-budget-delegation)
;;; gptel-runner-request-budget-delegation.el ends here
