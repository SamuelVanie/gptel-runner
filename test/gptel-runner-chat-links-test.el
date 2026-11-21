;;; gptel-runner-chat-links-test.el --- Chat link regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-runner-chat-links)

(ert-deftest gptel-runner-chat-links-render-preserves-transcript ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert (propertize "See [the source](src/main.el:12:3) and [[https://example.com][docs]]."
                        'gptel 'response 'front-sticky '(gptel)))
    (set-buffer-modified-p nil)
    (let ((text (buffer-string)) (undo buffer-undo-list)
          (overlays (gptel-runner-chat-links-render (point-min) (point-max))))
      (should (= 2 (length overlays)))
      (should (equal-including-properties text (buffer-string)))
      (should (equal undo buffer-undo-list))
      (should-not (buffer-modified-p))
      (should (equal (mapcar (lambda (ov) (overlay-get ov 'gptel-runner-chat-link)) overlays)
                     '("src/main.el:12:3" "https://example.com")))
      (mapc #'delete-overlay overlays)
      (should (equal-including-properties text (buffer-string))))))

(ert-deftest gptel-runner-chat-links-local-location-formats ()
  (dolist (target '("src/main.el:12:3" "src/main.el::12:3"
                    "file:src/main.el::12:3"))
    (let ((destination (gptel-runner-chat-links--destination target)))
      (should (eq (plist-get destination :type) 'file))
      (should (equal (plist-get destination :path) "src/main.el"))
      (should (= (plist-get destination :line) 12))
      (should (= (plist-get destination :column) 3))))
  (dolist (target '("src/main.el#L12-L19" "src/main.el:12-19"))
    (let ((destination (gptel-runner-chat-links--destination target)))
      (should (equal (plist-get destination :path) "src/main.el"))
      (should (= (plist-get destination :line) 12))))
  (should (equal (plist-get (gptel-runner-chat-links--destination
                            "file:///tmp/hello%20world.el") :path)
                 "/tmp/hello world.el"))
  (should (equal (plist-get (gptel-runner-chat-links--destination
                            "file://localhost/tmp/hello.el") :path)
                 "/tmp/hello.el")))

(ert-deftest gptel-runner-chat-links-rejects-special-protocols ()
  (dolist (target '("javascript:alert(1)" "elisp:(message hello)" "shell:ls"
                    "file://another-host/tmp/source.el" "/ssh:host:/tmp/source.el"
                    "file:%2Fssh:host:%2Ftmp%2Fsource.el" "#heading"))
    (should-not (gptel-runner-chat-links--destination target))))

(ert-deftest gptel-runner-chat-links-balanced-markdown-and-streaming ()
  (with-temp-buffer
    (insert "[one](https://example.com/a_(b)) [two](<some file.el:9> \"Title\")\n"
            "[unfinished](https://example.com/unfinished\n")
    (let ((overlays (gptel-runner-chat-links-render (point-min) (point-max))))
      (should (cl-find "https://example.com/a_(b)" overlays
                       :key (lambda (ov) (overlay-get ov 'gptel-runner-chat-link))
                       :test #'equal))
      (should (cl-find "some file.el:9" overlays
                       :key (lambda (ov) (overlay-get ov 'gptel-runner-chat-link))
                       :test #'equal))
      ;; A streamed unfinished link does not hide any of its Markdown syntax.
      (should-not (cl-find-if
                   (lambda (ov) (and (overlay-get ov 'display)
                                     (string-match-p "unfinished"
                                                     (overlay-get ov 'gptel-runner-chat-link))))
                   overlays)))))

(ert-deftest gptel-runner-chat-links-skips-excluded-regions ()
  (with-temp-buffer
    (insert "[code](source.el)\n[prose](source.el)\n")
    (let ((overlays (gptel-runner-chat-links-render
                     (point-min) (point-max)
                     (lambda (begin _end) (< begin 18)))))
      (should (= 1 (length overlays)))
      (should (equal (overlay-get (car overlays) 'display) "prose")))))

(ert-deftest gptel-runner-chat-links-resolves-when-activated ()
  (let ((directory (make-temp-file "gptel-chat-link-" t)))
    (unwind-protect
        (let ((file (expand-file-name "source.el" directory)) opened)
          (with-temp-file file (insert "first\nsecond\nthird\n"))
          (with-temp-buffer
            (insert "[source](source.el:2:3)")
            (let ((overlay (car (gptel-runner-chat-links-render (point-min) (point-max)))))
              ;; Workspace assignment happens after restoration and rendering.
              (setq default-directory directory)
              (cl-letf (((symbol-function 'find-file-other-window)
                         (lambda (path)
                           (setq opened path)
                           (erase-buffer)
                           (insert-file-contents path))))
                (gptel-runner-chat-links--visit
                 (overlay-get overlay 'gptel-runner-chat-link))
                (should (equal opened file))
                (should (= (line-number-at-pos) 2))
                (should (= (current-column) 2))))))
      (delete-directory directory t))))

(ert-deftest gptel-runner-chat-links-code-paths-must-exist ()
  (let ((directory (make-temp-file "gptel-chat-link-" t)))
    (unwind-protect
        (let ((file (expand-file-name "source.el" directory)))
          (with-temp-file file (insert "hello"))
          (with-temp-buffer
            (setq default-directory directory)
            (insert "`source.el:1` and `missing.el:1` and `ordinary code`.\n"
                    "=source.el:1= and ~source.el:1~")
            (let ((overlays (gptel-runner-chat-links-render (point-min) (point-max))))
              (should (= 3 (length overlays)))
              (should (equal (overlay-get (car overlays) 'gptel-runner-chat-link)
                             "source.el:1")))))
      (delete-directory directory t))))

(ert-deftest gptel-runner-chat-links-bare-url-retains-balanced-delimiters ()
  (with-temp-buffer
    (insert "See https://example.com/a_(b), then (https://example.com/c).")
    (should (equal (mapcar (lambda (ov) (overlay-get ov 'gptel-runner-chat-link))
                          (gptel-runner-chat-links-render (point-min) (point-max)))
                   '("https://example.com/a_(b)" "https://example.com/c")))))

(ert-deftest gptel-runner-chat-links-web-activation-and-missing-file ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq visited url))))
      (gptel-runner-chat-links--visit "https://example.com/source#L4")
      (should (equal visited "https://example.com/source#L4")))
    (should-error (gptel-runner-chat-links--visit "/this/chat/path/does/not/exist.el")
                  :type 'user-error)))

(ert-deftest gptel-runner-chat-links-keyboard-opens-sole-table-destination ()
  (with-temp-buffer
    (insert "| source | source |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))) opened)
      (overlay-put overlay 'gptel-runner-chat-table t)
      (overlay-put overlay 'display
                   (concat (gptel-runner-chat-links-string "source" "src/main.el:8")
                           " | "
                           (gptel-runner-chat-links-string "same source" "src/main.el:8")))
      (cl-letf (((symbol-function 'gptel-runner-chat-links--visit)
                 (lambda (target) (setq opened target)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Single destination should not prompt"))))
        (gptel-runner-chat-links-open)
        (should (equal opened "src/main.el:8"))))))

(ert-deftest gptel-runner-chat-links-keyboard-selects-labeled-table-destination ()
  (with-temp-buffer
    (insert "| one | two |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))) opened candidates)
      (overlay-put overlay 'gptel-runner-chat-table t)
      (overlay-put overlay 'display
                   (concat (gptel-runner-chat-links-string "Source" "src/main.el:8")
                           " | "
                           (gptel-runner-chat-links-string "Documentation" "https://example.com")))
      (cl-letf (((symbol-function 'gptel-runner-chat-links--visit)
                 (lambda (target) (setq opened target)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate require-match &rest _)
                   (should require-match)
                   (setq candidates collection)
                   (caar (cdr collection)))))
        (gptel-runner-chat-links-open)
        (should (= 2 (length candidates)))
        (should (string-match-p "Documentation.*https://example.com" (caadr candidates)))
        (should (equal opened "https://example.com"))))))

(ert-deftest gptel-runner-chat-links-blank-table-click-does-not-open-link ()
  (with-temp-buffer
    (insert "| source |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'gptel-runner-chat-table t)
      (overlay-put overlay 'display
                   (concat " | " (gptel-runner-chat-links-string "source" "source.el")))
      (cl-letf (((symbol-function 'event-start) (lambda (_) 'position))
                ((symbol-function 'posn-string) (lambda (_) (cons " | " 1)))
                ((symbol-function 'mouse-set-point) #'ignore)
                ((symbol-function 'gptel-runner-chat-links--visit)
                 (lambda (_) (ert-fail "Blank table clicks must not open links"))))
        (should-error (gptel-runner-chat-links-open 'mouse-event) :type 'user-error)))))

(ert-deftest gptel-runner-chat-links-table-without-links-reports-no-target ()
  (with-temp-buffer
    (insert "| ordinary text |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'gptel-runner-chat-table t)
      (overlay-put overlay 'display "ordinary text")
      (should-error (gptel-runner-chat-links-open) :type 'user-error))))

(provide 'gptel-runner-chat-links-test)
;;; gptel-runner-chat-links-test.el ends here
