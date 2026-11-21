;;; gptel-runner-chat-tools-test.el --- Tool activity presentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Native fold, approval, and transcript boundaries of the tool renderer.

;;; Code:

(require 'ert)
(require 'org)
(require 'gptel-runner-chat-tools)

(defun gptel-runner-chat-tools-test--insert (&optional incomplete)
  "Insert a native Org tool result, returning renderer arguments.
INCOMPLETE omits the closing fence."
  (let ((beg (point)) opening-end body-beg close-beg)
    (insert "#+begin_tool (Bash \"pwd\")")
    (setq opening-end (point))
    (insert "\n")
    (setq body-beg (point))
    (insert (propertize "(:name \"Bash\" :args (\"pwd\"))\n\n/work/project\n"
                        'gptel '(tool . "call-1")))
    (unless incomplete
      (setq close-beg (point))
      (insert "#+end_tool"))
    (list beg opening-end body-beg close-beg (point) "(Bash \"pwd\")")))

(ert-deftest gptel-runner-chat-tools-preserve-transcript-and-role-properties ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-runner-chat-tools-test--insert))
           (before (buffer-string)) (undo buffer-undo-list) (point (point)))
      (set-buffer-modified-p nil)
      (let* ((overlays (apply #'gptel-runner-chat-tools-render-block arguments))
             (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-runner-chat-tool-header)) overlays)))
        (should (equal-including-properties before (buffer-string)))
        (should-not (buffer-modified-p))
        (should (equal undo buffer-undo-list))
        (should (= point (point)))
        (should header)
        (should (overlay-get header 'gptel-runner-chat-block-opening))
        (should (string-match-p "TOOL  ·  Bash.*pwd" (overlay-get header 'display)))
        (should-not (string-match-p "success\|complete\|passed" (overlay-get header 'display)))
        (should (cl-some (lambda (ov) (string-match-p "INPUT\npwd"
                                                     (or (overlay-get ov 'display) ""))) overlays))
        (should (cl-some (lambda (ov) (equal (substring-no-properties
                                            (or (overlay-get ov 'before-string) ""))
                                           "OUTPUT\n")) overlays))
        (mapc #'delete-overlay overlays)
        (should (equal-including-properties before (buffer-string)))))))

(ert-deftest gptel-runner-chat-tools-retain-native-folds-and-unhide-only-header ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-runner-chat-tools-test--insert))
           (beg (nth 0 arguments)) (body-beg (nth 2 arguments)))
      (goto-char beg)
      (org-cycle)
      (should (invisible-p body-beg))
      (add-to-invisibility-spec 'org-modern)
      (put-text-property beg (+ beg 12) 'invisible 'org-modern)
      (let ((before (buffer-string))
            (overlays (apply #'gptel-runner-chat-tools-render-block arguments)))
        (should-not (invisible-p beg))
        (should (invisible-p body-beg))
        (should (equal-including-properties before (buffer-string)))
        (mapc #'delete-overlay overlays)
        (should (invisible-p beg))
        (should (invisible-p body-beg))))))

(ert-deftest gptel-runner-chat-tools-preserve-spanning-native-approval-keymap ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-runner-chat-tools-test--insert))
           (native (make-overlay (point-min) (point-max)))
           (map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'ignore)
      (overlay-put native 'gptel-tool '(pending))
      (overlay-put native 'keymap map)
      (let* ((overlays (apply #'gptel-runner-chat-tools-render-block arguments))
             (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-runner-chat-tool-header)) overlays)))
        (goto-char (point-min))
        (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
        (should-not (overlay-get header 'keymap))
        (should (eq (overlay-get native 'keymap) map))
        (mapc #'delete-overlay overlays)
        (should (overlay-buffer native))))))

(ert-deftest gptel-runner-chat-tools-ignore-untyped-examples-and-pending-text ()
  (with-temp-buffer
    (org-mode)
    (let ((arguments (gptel-runner-chat-tools-test--insert)))
      (remove-text-properties (point-min) (point-max) '(gptel nil))
      (should-not (apply #'gptel-runner-chat-tools-render-block arguments)))))

(ert-deftest gptel-runner-chat-tools-incomplete-block-has-no-fold-action ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-runner-chat-tools-test--insert t))
           (overlays (apply #'gptel-runner-chat-tools-render-block arguments))
           (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-runner-chat-tool-header)) overlays)))
      (should header)
      (should-not (overlay-get header 'keymap)))))

(ert-deftest gptel-runner-chat-tools-use-native-org-fold-toggle ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-runner-chat-tools-test--insert))
           (body-beg (nth 2 arguments))
           (overlays (apply #'gptel-runner-chat-tools-render-block arguments)))
      (goto-char (point-min))
      (should-not (invisible-p body-beg))
      (gptel-runner-chat-tools-toggle)
      (should (invisible-p body-beg))
      (gptel-runner-chat-tools-toggle)
      (should-not (invisible-p body-beg))
      (mapc #'delete-overlay overlays))))

(ert-deftest gptel-runner-chat-tools-read-metadata-without-evaluation ()
  (with-temp-buffer
    (insert "(:name \"Bash\" :args ((progn (error \"must not run\"))))")
    (let ((metadata (gptel-runner-chat-tools--metadata (point-min) (point-max))))
      (should (equal (plist-get (car metadata) :name) "Bash"))
      (should (string-match-p "must not run"
                              (gptel-runner-chat-tools--input (plist-get (car metadata) :args)))))))

(ert-deftest gptel-runner-chat-tools-do-not-hide-headers-with-default-invisibility ()
  (with-temp-buffer
    (text-mode)
    (let ((arguments (gptel-runner-chat-tools-test--insert)))
      (setq-local buffer-invisibility-spec t)
      (apply #'gptel-runner-chat-tools-render-block arguments)
      (should-not (invisible-p (point-min))))))

(provide 'gptel-runner-chat-tools-test)
;;; gptel-runner-chat-tools-test.el ends here
