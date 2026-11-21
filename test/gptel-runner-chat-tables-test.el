;;; gptel-runner-chat-tables-test.el --- Chat table tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-chat-tables)

(ert-deftest gptel-runner-chat-tables-preserve-source-and-properties ()
  (with-temp-buffer
    (insert "| Name | Result |\n| --- | --- |\n| One | Done |\n")
    (put-text-property (point-min) (point-max) 'gptel 'response)
    (set-buffer-modified-p nil)
    (let* ((before (buffer-string))
           (undo buffer-undo-list)
           (gptel-runner-chat-table-width 80)
           (overlays (gptel-runner-chat-tables-render (point-min) (point-max))))
      (should (= (length overlays) 1))
      (should (equal-including-properties (buffer-string) before))
      (should (eq undo buffer-undo-list))
      (should-not (buffer-modified-p))
      (let ((display (overlay-get (car overlays) 'display)))
        (should (string-match-p "╭" display))
        (should (string-match-p "Done" display))
        (let ((widths (mapcar #'string-width (split-string display "\n" t))))
          (should (apply #'= widths)))))))

(ert-deftest gptel-runner-chat-tables-narrow-records-never-clip ()
  (with-temp-buffer
    (insert "| Component | Result |\n| --- | --- |\n| gptel-runner-chat-tables.el | A long explanation with wide 漢字 glyphs and a/path/without/breaks/to/verify |\n")
    (let* ((gptel-runner-chat-table-width 28)
           (overlay (car (gptel-runner-chat-tables-render (point-min) (point-max))))
           (display (overlay-get overlay 'display)))
      (should (string-prefix-p "─ 01" display))
      (should (string-match-p "Component" display))
      (dolist (line (split-string display "\n" t))
        (should (<= (string-width line) 28))))))

(ert-deftest gptel-runner-chat-tables-handle-org-rules-and-unbalanced-rows ()
  (with-temp-buffer
    (insert "| Name | Result |\n|------+--------|\n| 漢字 | yes | extra |\n| Short |\n|------+--------|\n")
    (let* ((gptel-runner-chat-table-width 80)
           (overlays (gptel-runner-chat-tables-render (point-min) (point-max)))
           (display (overlay-get (car overlays) 'display)))
      (should (= (length overlays) 1))
      (should (string-match-p "extra" display))
      (should (apply #'= (mapcar #'string-width (split-string display "\n" t)))))))

(ert-deftest gptel-runner-chat-tables-parse-escaped-and-code-pipes ()
  (should (equal (gptel-runner-chat-tables--cells "| a\\|b | `a|b` | ``x`|y`` |")
                 '("a|b" "`a|b`" "``x`|y``")))
  (should (equal (gptel-runner-chat-tables--cells " | first | second |  ")
                 '("first" "second")))
  (should (equal (gptel-runner-chat-tables--cells "| first | second")
                 '("first" "second"))))

(ert-deftest gptel-runner-chat-tables-ignore-nontables-and-skipped-ranges ()
  (with-temp-buffer
    (insert "| A shell pipeline | a value |\n| Another line | not a table |\n\n| A | B |\n| --- | --- |\n| 1 | 2 |\n")
    (should (= (length (gptel-runner-chat-tables-render (point-min) (point-max))) 1))
    (should-not (gptel-runner-chat-tables-render (point-min) (point-max)
                                                (lambda (_beg _end) t)))))

(ert-deftest gptel-runner-chat-tables-streaming-and-terminal-newline ()
  (with-temp-buffer
    (insert "| A | B |\n| --- | --- |\n| 1 | par")
    (let* ((gptel-runner-chat-table-width 80)
           (overlay (car (gptel-runner-chat-tables-render (point-min) (point-max))))
           (display (overlay-get overlay 'display)))
      (should (string-match-p "par" display))
      (should-not (string-suffix-p "\n" display)))))

(ert-deftest gptel-runner-chat-tables-display-links-remain-clickable ()
  (require 'gptel-runner-chat-links)
  (with-temp-buffer
    (insert "| File | Description |\n| --- | --- |\n| [Source](gptel-runner.el:12) | Read it |\n")
    (let* ((gptel-runner-chat-table-width 80)
           (overlay (car (gptel-runner-chat-tables-render (point-min) (point-max))))
           (display (overlay-get overlay 'display))
           (position (string-match "Source" display)))
      (should position)
      (should (equal (get-text-property position 'gptel-runner-chat-link display)
                     "gptel-runner.el:12"))
      (should (get-text-property position 'keymap display)))))

(provide 'gptel-runner-chat-tables-test)
;;; gptel-runner-chat-tables-test.el ends here
