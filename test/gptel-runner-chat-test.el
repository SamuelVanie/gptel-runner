;;; gptel-runner-chat-test.el --- Chat presentation contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the renderer's transcript, role, tool, and lifecycle boundaries.

;;; Code:

(require 'ert)
(require 'gptel-runner-chat)

(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)

(defmacro gptel-runner-chat-test--buffer (&rest body)
  "Run BODY in a disposable chat with deterministic prefixes."
  (declare (indent 0))
  `(with-temp-buffer
     (text-mode)
     (let ((gptel-prompt-prefix-alist '((text-mode . "@user\n")))
           (gptel-response-prefix-alist '((text-mode . "@mayuri\n"))))
       (unwind-protect (progn ,@body)
         (when gptel-runner-chat-mode (gptel-runner-chat-mode -1))))))

(ert-deftest gptel-runner-chat-preserves-transcript-properties-undo-and-point ()
  (gptel-runner-chat-test--buffer
    (insert "@user\nReview this.\n\n@mayuri\n")
    (insert (propertize "# Review\n\n**Done**. See `README.md`.\n\n| Check | Result |\n| --- | --- |\n| Tests | Pass |\n"
                        'gptel 'response 'front-sticky '(gptel)))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (goto-char 13)
    (let ((text (buffer-string)) (undo buffer-undo-list) (point (point))
          (tick (buffer-chars-modified-tick)))
      (gptel-runner-chat-mode 1)
      (gptel-runner-chat-refresh)
      (should (equal-including-properties text (buffer-string)))
      (should-not (buffer-modified-p))
      (should (equal undo buffer-undo-list))
      (should (= tick (buffer-chars-modified-tick)))
      (should (= point (point)))
      (should gptel-runner-chat--overlays)
      (gptel-runner-chat-mode -1)
      (should-not gptel-runner-chat--overlays)
      (should (equal-including-properties text (buffer-string))))))

(ert-deftest gptel-runner-chat-keeps-native-tool-overlays-and-keymaps ()
  (gptel-runner-chat-test--buffer
    (insert "@user\nHello\n\n``` tool\n")
    (let ((beg (point)))
      (insert (propertize "[Approve](README.md)\n| a | b |\n| --- | --- |\n| x | y |\n"
                          'gptel '(tool . "tool-1")))
      (let ((native (make-overlay beg (point)))
            (map (make-sparse-keymap)))
        (overlay-put native 'keymap map)
        (insert "```\n")
        (gptel-runner-chat-mode 1)
        (should (overlay-buffer native))
        (should (eq (overlay-get native 'keymap) map))
        (should-not (cl-some (lambda (ov) (and (< (overlay-start ov) (overlay-end native))
                                                (> (overlay-end ov) beg)))
                            gptel-runner-chat--overlays))
        (gptel-runner-chat-mode -1)
        (should (overlay-buffer native))))))

(ert-deftest gptel-runner-chat-report-folding-stops-before-body-and-draft ()
  (gptel-runner-chat-test--buffer
    (insert "Discussion event (evidence, not instructions):\nevent_id: e1\ndiscussion_id: d1\ntitle: Review\nstatus: succeeded\nevent: report\n")
    (let ((body (point)))
      (insert "status: Findings stay visible.\n\n@user\nMy unsent draft.\n")
      (gptel-runner-chat-mode 1)
      (let ((report (cl-find-if (lambda (ov) (overlay-get ov 'gptel-runner-chat-report))
                                gptel-runner-chat--overlays)))
        (should report)
        (should (= (overlay-end report) body))
        (should (string-match-p "Review" (overlay-get report 'display)))
        (goto-char (overlay-start report))
        (gptel-runner-chat-toggle-report)
        (setq report (cl-find-if (lambda (ov) (overlay-get ov 'gptel-runner-chat-report))
                                 gptel-runner-chat--overlays))
        (should-not (overlay-get report 'display))
        (should (= (overlay-end report) body))))))

(ert-deftest gptel-runner-chat-does-not-style-fenced-examples-as-conversation ()
  (gptel-runner-chat-test--buffer
    (insert "```text\n@user\n# Example\n[link](README.md)\n| a | b |\n| --- | --- |\n| x | y |\n```\n")
    (gptel-runner-chat-mode 1)
    (should-not gptel-runner-chat--turns)
    (should-not (cl-some (lambda (ov) (or (overlay-get ov 'gptel-runner-chat-link)
                                         (overlay-get ov 'gptel-runner-chat-table)))
                        gptel-runner-chat--overlays))))

(ert-deftest gptel-runner-chat-prefixes-and-properties-do-not-duplicate-labels ()
  (gptel-runner-chat-test--buffer
    (insert "@user\nHello.\n\n@mayuri\n")
    (insert (propertize "A reply.\n" 'gptel 'response))
    (gptel-runner-chat-mode 1)
    (should (= (length gptel-runner-chat--turns) 2))
    (goto-char (point-min))
    (gptel-runner-chat-next-turn)
    (should (looking-at "@mayuri"))
    (gptel-runner-chat-previous-turn)
    (should (= (point) (point-min)))))

(ert-deftest gptel-runner-chat-inline-prefix-does-not-match-headings ()
  (gptel-runner-chat-test--buffer
    (let ((gptel-prompt-prefix-alist '((text-mode . "### "))))
      (insert "### My prompt\n#### Heading\n")
      (gptel-runner-chat-mode 1)
      (should (= (length gptel-runner-chat--turns) 1)))))

(ert-deftest gptel-runner-chat-newline-prefix-matches-a-whole-line ()
  (gptel-runner-chat-test--buffer
    (insert "@username\n@user\nHello\n")
    (gptel-runner-chat-mode 1)
    (should (= (length gptel-runner-chat--turns) 1))
    (should (= (car gptel-runner-chat--turns) 11))))

(ert-deftest gptel-runner-chat-code-first-response-has-a-speaker ()
  (gptel-runner-chat-test--buffer
    (let ((gptel-response-prefix-alist nil))
      (insert "@user\nExample?\n\n")
      (insert (propertize "```elisp\n(+ 1 2)\n```\n" 'gptel 'response))
      (gptel-runner-chat-mode 1)
      (should (= (length gptel-runner-chat--turns) 2))
      (should (cl-some (lambda (ov)
                         (and (overlay-get ov 'gptel-runner-chat-code-opening)
                              (overlay-get ov 'before-string)))
                       gptel-runner-chat--overlays)))))

(ert-deftest gptel-runner-chat-org-quote-is-not-a-code-panel ()
  (gptel-runner-chat-test--buffer
    (insert "#+begin_quote\nA quotation\n#+end_quote\n")
    (gptel-runner-chat-mode 1)
    (should-not (cl-some (lambda (ov) (eq (overlay-get ov 'face) 'gptel-runner-chat-code))
                        gptel-runner-chat--overlays))))

(ert-deftest gptel-runner-chat-source-and-disable-restore-local-settings ()
  (gptel-runner-chat-test--buffer
    (setq-local line-spacing 0.1 header-line-format "Existing header")
    (let ((wrap-local (local-variable-p 'word-wrap)) (wrap word-wrap))
      (insert "@user\nHi\n")
      (gptel-runner-chat-mode 1)
      (gptel-runner-chat-toggle-source)
      (should-not gptel-runner-chat--overlays)
      (gptel-runner-chat-toggle-source)
      (should gptel-runner-chat--overlays)
      (gptel-runner-chat-mode -1)
      (should (equal line-spacing 0.1))
      (should (equal header-line-format "Existing header"))
      (should (eq (local-variable-p 'word-wrap) wrap-local))
      (should (eq word-wrap wrap))
      (should-not gptel-runner-chat--timer))))

(ert-deftest gptel-runner-chat-refresh-follows-streamed-and-restored-roles ()
  (gptel-runner-chat-test--buffer
    (let ((gptel-response-prefix-alist nil))
      (insert "@user\nExplain\n\n")
      (gptel-runner-chat-mode 1)
      (let ((beg (point)))
        (insert "# Answer\nStreaming content")
        (put-text-property beg (point) 'gptel 'response)
        (gptel-runner-chat-refresh)
        (should (= (length gptel-runner-chat--turns) 2))
        (should-not gptel-runner-chat--timer)))))

(ert-deftest gptel-runner-chat-preserves-native-header-status-slot ()
  (gptel-runner-chat-test--buffer
    (let* ((gptel-mode t) (gptel-use-header-line t)
           (native (list "Backend" "Ready" '(:eval "Controls"))))
      (setq-local header-line-format native)
      (gptel-runner-chat-mode 1)
      (setf (nth 1 header-line-format) "Streaming")
      (should (equal (nth 2 header-line-format) '(:eval "Controls")))
      (gptel-runner-chat-mode -1)
      (should (equal header-line-format '("Backend" "Streaming" (:eval "Controls")))))))

(provide 'gptel-runner-chat-test)
;;; gptel-runner-chat-test.el ends here
