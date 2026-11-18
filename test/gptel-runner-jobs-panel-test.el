;;; gptel-runner-jobs-panel-test.el --- Node panel tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-jobs-panel)

(defmacro gptel-runner-panel-test--with-panel (&rest body)
  "Render a panel with two current requests and evaluate BODY."
  (declare (indent 0))
  `(let* ((gptel-runner-dashboard-refresh-interval nil)
          (call (copy-tree '(:id "c1" :epoch 2 :generation 3 :node inspect :agent reader
                  :state waiting-confirmation :outside-access nil)))
          (first (copy-tree '(:id "i1" :call-id "c1" :epoch 2 :generation 3
                   :source-buffer "reader" :outside-access t
                   :tools ((:name "Read" :args (:file_path "/outside/a"))))))
          (second '(:id "i2" :call-id "c1" :epoch 2 :generation 3
                    :source-buffer "child" :tools ((:name "Shell" :args (:command "ls")))))
          (run (list :id "run1" :workflow 'demo :state 'running
                     :calls (list call)
                     :interactions (list first second
                                         '(:id "old" :call-id "c1" :epoch 1)
                                         '(:id "other" :call-id "c2" :epoch 2))))
          (status 'queued) sent)
     (with-temp-buffer
       (gptel-runner-panel-mode)
       (setq-local gptel-runner-background--target '(background-call "run1" 2 "c1"))
       (cl-letf (((symbol-function 'gptel-runner-invocation-get) (lambda (_) run))
                 ((symbol-function 'gptel-runner-invocation-call)
                  (lambda (&rest _) '(:workspace "/project/" :prompt "Inspect")))
                 ((symbol-function 'gptel-runner-job-command)
                  (lambda (_run operation arguments)
                    (push (cons operation arguments) sent)
                    '(:id "cmd1" :status queued)))
                 ((symbol-function 'gptel-runner-job-command-status)
                  (lambda (&rest _) (list :id "cmd1" :status status
                                          :error (when (eq status 'rejected) "Stale request"))))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (&rest _) (ert-fail "Decision opened another buffer"))))
         (gptel-runner-panel-refresh)
         ,@body))))

(ert-deftest gptel-runner-panel-approves-displayed-snapshot-once ()
  (gptel-runner-panel-test--with-panel
    (should (string-match-p "Needs your approval · 2 requests" (buffer-string)))
    (should (string-match-p "/outside/a" (buffer-string)))
    ;; A request arriving after render must not be added to this approval.
    (push '(:id "new" :call-id "c1" :epoch 2 :generation 3 :tools nil)
          (plist-get run :interactions))
    (let ((action (alist-get 'approve gptel-runner-panel--actions)))
      (funcall action)
      (should (equal sent '((approve-all :call-id "c1" :call-epoch 2 :generation 3
                                       :interaction-ids ("i1" "i2") :allow-outside nil))))
      (should-error (funcall action) :type 'user-error))
    (should (string-match-p "queued" (buffer-string)))
    (should (= (length sent) 1))))

(ert-deftest gptel-runner-panel-outside-option-is-explicit-and-revocable ()
  (gptel-runner-panel-test--with-panel
    (should (string-match-p "Other tool confirmations and file reviews still apply" (buffer-string)))
    (gptel-runner-panel-allow-outside)
    (should (plist-get (cdar sent) :allow-outside))
    (setf (plist-get call :outside-access) t)
    (gptel-runner-panel-refresh)
    (should (string-match-p "Revoke outside access" (buffer-string)))))

(ert-deftest gptel-runner-panel-rejected-receipt-is-visible-and-retryable ()
  (gptel-runner-panel-test--with-panel
    (gptel-runner-panel-approve-all)
    (should-not (alist-get 'approve gptel-runner-panel--actions))
    (setq status 'rejected)
    (gptel-runner-panel-refresh)
    (should (string-match-p "rejected — Stale request" (buffer-string)))
    (should (alist-get 'approve gptel-runner-panel--actions))))

(ert-deftest gptel-runner-panel-refresh-keeps-focused-action ()
  (gptel-runner-panel-test--with-panel
    (goto-char (point-min))
    (search-forward "Approve once")
    (backward-char 3)
    (let ((key (button-get (button-at (point)) 'runner-action)))
      (push '(:operation pause :receipt (:id "earlier" :status queued))
            gptel-runner-panel--receipts)
      (gptel-runner-panel-refresh)
      (should (equal (button-get (button-at (point)) 'runner-action) key)))))

(ert-deftest gptel-runner-panel-no-grant-option-for-ordinary-confirmations ()
  (gptel-runner-panel-test--with-panel
    (setf (plist-get first :outside-access) nil)
    (gptel-runner-panel-refresh)
    (should-not (string-match-p "allow outside paths" (buffer-string)))
    (should-error (gptel-runner-panel-allow-outside) :type 'user-error)))

(ert-deftest gptel-runner-panel-refresh-preserves-reading-position-and-removes-stale-focus ()
  (gptel-runner-panel-test--with-panel
    (goto-char (point-min))
    (search-forward "/outside/a")
    (let ((location (gptel-runner-panel--location (point))))
      (push '(:operation pause :receipt (:id "earlier" :status queued))
            gptel-runner-panel--receipts)
      (gptel-runner-panel-refresh)
      (should (equal (gptel-runner-panel--location (point)) location)))
    (search-forward "Approve once")
    (backward-char 2)
    (setf (plist-get run :interactions) (list second))
    (gptel-runner-panel-refresh)
    (should-not (button-at (point)))
    (should (= (point) (point-min)))))

(ert-deftest gptel-runner-dashboard-fold-preserves-other-runs ()
  (let ((gptel-runner-dashboard--collapsed '((background-run "a")))
        (gptel-runner-dashboard-entry-functions
         (list (lambda () '(((background-run "a") [])
                            ((background-call "a" 1 "c") [])
                            ((background-run "b") [])
                            ((background-call "b" 1 "d") []))))))
    (cl-letf (((symbol-function 'gptel-runner-ui--local-entries) #'ignore))
      (should (equal (mapcar #'car (gptel-runner-ui--entries))
                     '((background-run "a") (background-run "b")
                       (background-call "b" 1 "d")))))))

(ert-deftest gptel-runner-jobs-batch-command-schema-requires-exact-identities ()
  (let ((args '(:call-id "c1" :call-epoch 2 :generation 0
                :interaction-ids ("i1" "i2") :allow-outside t)))
    (gptel-runner-jobs--validate-command-arguments 'approve-all args)
    (dolist (invalid '((:interaction-ids nil) (:interaction-ids ("i1" "i1"))
                       (:interaction-ids (all)) (:allow-outside everything)))
      (should-error
       (gptel-runner-jobs--validate-command-arguments
        'approve-all (plist-put (copy-sequence args) (car invalid) (cadr invalid)))))
    (dolist (key '(:call-id :call-epoch :generation :interaction-ids))
      (let ((missing (copy-sequence args)))
        (cl-remf missing key)
        (should-error (gptel-runner-jobs--validate-command-arguments 'approve-all missing))))))

(ert-deftest gptel-runner-background-command-keeps-current-buffer ()
  (gptel-runner-panel-test--with-panel
    (let ((buffer (current-buffer)))
      (gptel-runner-background-command 'accept '(:result "Reviewed"))
      (should (eq buffer (current-buffer)))
      (should (eq (caar sent) 'accept)))))

(provide 'gptel-runner-jobs-panel-test)
;;; gptel-runner-jobs-panel-test.el ends here
