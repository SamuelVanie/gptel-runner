;;; gptel-runner-chat-tools.el --- Quiet tool activity cards -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Style completed native gptel tool blocks without rewriting their contents,
;; taking ownership of their folding, or intercepting approval controls.
;; The chat renderer owns and removes the overlays returned by this module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup gptel-runner-chat-tools nil
  "Tool activity in gptel conversations."
  :group 'convenience)

(defface gptel-runner-chat-tool-header
  '((((class color) (background dark))
     (:inherit fixed-pitch :background "#14333e" :foreground "#83b9cf"
      :weight normal :slant normal :height 0.95 :extend t
      :underline nil :overline nil :box nil :inverse-video nil))
    (((class color) (background light))
     (:inherit fixed-pitch :background "#e8f0f4" :foreground "#376478"
      :weight normal :slant normal :height 0.95 :extend t
      :underline nil :overline nil :box nil :inverse-video nil))
    (t (:inherit fixed-pitch :weight bold)))
  "Compact activity row for a tool call."
  :group 'gptel-runner-chat-tools)

(defface gptel-runner-chat-tool-body
  '((((class color) (background dark))
     (:inherit fixed-pitch :background "#0c2e38" :weight normal
      :slant normal :height 0.95 :extend t :inverse-video nil
      :underline nil :overline nil :box nil))
    (((class color) (background light))
     (:inherit fixed-pitch :background "#f1f5f7" :weight normal
      :slant normal :height 0.95 :extend t :inverse-video nil
      :underline nil :overline nil :box nil))
    (t (:inherit fixed-pitch)))
  "Expanded tool details, distinct from quoted prose."
  :group 'gptel-runner-chat-tools)

(defface gptel-runner-chat-tool-label
  '((t (:inherit (gptel-runner-chat-tool-body shadow) :weight bold :height 0.85)))
  "Small INPUT and OUTPUT captions within tool details."
  :group 'gptel-runner-chat-tools)

(defvar gptel-tool-call-actions-map)
(declare-function org-cycle "org-cycle" (&optional arg))
(declare-function gptel-markdown-cycle-block "gptel" ())
(declare-function gptel-runner-chat-refresh "gptel-runner-chat" (&rest ignored))

(defun gptel-runner-chat-tools--native-p (beg end)
  "Whether BEG through END contains a native gptel tool result."
  (let ((pos beg) found)
    (while (and (< pos end) (not found))
      (setq found (eq (car-safe (get-text-property pos 'gptel)) 'tool)
            pos (next-single-property-change pos 'gptel nil end)))
    found))

(defun gptel-runner-chat-tools--approval-p (beg end)
  "Whether native approval controls overlap BEG through END."
  (cl-some (lambda (ov)
             (or (overlay-get ov 'gptel-tool)
                 (and (boundp 'gptel-tool-call-actions-map)
                      (eq (overlay-get ov 'keymap) gptel-tool-call-actions-map))))
           (overlays-in beg end)))

(defun gptel-runner-chat-tools--metadata (beg end)
  "Read the bounded native metadata at BEG, before END, without evaluation.
Return (PLIST . POSITION) where POSITION ends the serialized metadata."
  (condition-case nil
      (let* ((read-circle nil)
             (text (buffer-substring-no-properties beg (min end (+ beg 16384))))
             (parsed (read-from-string text))
             (value (car parsed)))
        (when (and (proper-list-p value) (eq (car value) :name)
                   (stringp (plist-get value :name)) (plist-member value :args))
          (cons value (+ beg (cdr parsed)))))
    (error nil)))

(defun gptel-runner-chat-tools--input (args)
  "Return a readable, bounded presentation of native ARGS."
  (let ((print-length 40) (print-level 8))
    (cond
     ((null args) "No arguments")
     ((stringp args) args)
     ((and (proper-list-p args) (keywordp (car args)))
      (let (lines)
        (while args
          (let ((key (pop args)) (value (pop args)))
            (push (format "%s: %s" (string-remove-prefix ":" (format "%s" key))
                          (if (stringp value) value (prin1-to-string value))) lines)))
        (string-join (nreverse lines) "\n")))
     ((proper-list-p args)
      (mapconcat (lambda (value) (if (stringp value) value (prin1-to-string value))) args "\n"))
     (t (prin1-to-string args)))))

(defun gptel-runner-chat-tools--summary (info metadata)
  "Return a (NAME . SUMMARY) pair from INFO and parsed METADATA."
  (let ((name (and metadata (plist-get (car metadata) :name)))
        (args (and metadata (plist-get (car metadata) :args))))
    (unless name
      (setq info (string-trim (string-remove-prefix "tool" (string-trim info))))
      (when (string-match "\\`(\\([^()[:space:]]+\\)[ \t]*\\(.*\\)" info)
        (setq name (match-string 1 info)
              args (string-remove-suffix ")" (match-string 2 info)))))
    (cons (or name "Tool")
          (if args
              (string-trim (replace-regexp-in-string
                            "[\n\r\t ]+" " "
                            (gptel-runner-chat-tools--input args)))
            ""))))

(defun gptel-runner-chat-tools-toggle (&optional event)
  "Toggle native tool details at point or mouse EVENT."
  (interactive (list (when (mouse-event-p last-input-event) last-input-event)))
  (when event (mouse-set-point event))
  (let ((header (cl-find-if
                 (lambda (ov) (overlay-get ov 'gptel-runner-chat-tool-header))
                 (overlays-at (point)))))
    (unless header (user-error "No tool activity header at point"))
    (when (gptel-runner-chat-tools--approval-p (overlay-start header) (overlay-end header))
      (user-error "Use the pending tool approval controls first"))
    (goto-char (overlay-start header))
    (cond ((derived-mode-p 'org-mode) (org-cycle))
          ((fboundp 'gptel-markdown-cycle-block) (gptel-markdown-cycle-block))
          (t (user-error "Native gptel block folding is unavailable")))
    (when (fboundp 'gptel-runner-chat-refresh) (gptel-runner-chat-refresh))))

(defvar gptel-runner-chat-tools-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-runner-chat-tools-toggle)
    (define-key map [mouse-1] #'gptel-runner-chat-tools-toggle)
    map)
  "Native-fold toggle bindings for tool result headers.")

(defun gptel-runner-chat-tools-render-block (beg opening-end body-beg close-beg end info)
  "Return display overlays for a native tool block from BEG to END.
OPENING-END ends the opening line, BODY-BEG starts its details, and CLOSE-BEG
starts the closing line, or is nil for an incomplete block.  INFO is the
opening fence's description.  Native text, role properties, fold state and
approval overlays remain untouched.  The caller owns the returned overlays."
  (save-excursion
    (save-match-data
      (let ((body-end (or close-beg end)) overlays)
        (when (gptel-runner-chat-tools--native-p body-beg body-end)
          (cl-labels ((decorate (start finish &rest properties)
                        (let ((ov (make-overlay start finish nil nil nil)))
                          (overlay-put ov 'gptel-runner-chat-tool t)
                          (overlay-put ov 'evaporate t)
                          (while properties
                            (overlay-put ov (pop properties) (pop properties)))
                          (push ov overlays)
                          ov)))
            (let* ((metadata (gptel-runner-chat-tools--metadata body-beg body-end))
                   (summary (gptel-runner-chat-tools--summary info metadata))
                   (folded (invisible-p (min body-beg (1- (point-max)))))
                   (prefix (concat "  " (if folded "▸" "▾") "  TOOL  ·  " (car summary)))
                   (windows (get-buffer-window-list (current-buffer) nil t))
                   (width (max 12 (- (if windows
                                            (apply #'min
                                                   (mapcar (lambda (window)
                                                             (/ (window-body-width window t)
                                                                (max 1 (window-font-width
                                                                        window 'gptel-runner-chat-tool-header))))
                                                           windows))
                                          (window-body-width)) 5)))
                   (caption (concat prefix
                                    (unless (string-empty-p (cdr summary))
                                      (concat "   " (truncate-string-to-width
                                                    (cdr summary) (max 12 (- width (string-width prefix)))
                                                    nil nil "…")))))
                   (header (decorate beg opening-end
                                     'display (propertize (truncate-string-to-width caption width nil nil "…")
                                                          'face 'gptel-runner-chat-tool-header)
                                     'face 'gptel-runner-chat-tool-header
                                     'invisible (unless (eq buffer-invisibility-spec t)
                                                  'gptel-runner-chat-visible)
                                     'priority 25
                                     'gptel-runner-chat-block-opening t
                                     'gptel-runner-chat-tool-header t)))
              ;; A pending approval can span earlier result blocks.  A new
              ;; keymap there would otherwise shadow the approval's C-c keys.
              (when (and close-beg (not (gptel-runner-chat-tools--approval-p beg opening-end)))
                (overlay-put header 'keymap gptel-runner-chat-tools-map)
                (overlay-put header 'mouse-face 'highlight)
                (overlay-put header 'help-echo
                             (concat (car summary) "  " (cdr summary)
                                     "\nRET or click: expand/collapse native tool details")))
              (when (< body-beg body-end)
                (decorate body-beg body-end 'face 'gptel-runner-chat-tool-body
                          'line-prefix "   │  " 'wrap-prefix "   │  " 'priority 2))
              (if metadata
                  (let ((input-end (cdr metadata)))
                    (decorate body-beg input-end 'priority 15
                              'display (concat
                                        (propertize "INPUT\n" 'face 'gptel-runner-chat-tool-label)
                                        (propertize (gptel-runner-chat-tools--input
                                                     (plist-get (car metadata) :args))
                                                    'face 'gptel-runner-chat-tool-body)))
                    (goto-char input-end)
                    (skip-chars-forward " \t\n" body-end)
                    (when (< (point) body-end)
                      (decorate (point) (1+ (point)) 'priority 15
                                'before-string (propertize "OUTPUT\n" 'face 'gptel-runner-chat-tool-label))))
                (when (< body-beg body-end)
                  (decorate body-beg (1+ body-beg) 'priority 15
                            'before-string (propertize "DETAILS\n" 'face 'gptel-runner-chat-tool-label))))
              (when close-beg
                (decorate close-beg end 'priority 25 'face 'gptel-runner-chat-tool-body
                          'display (propertize "   ╰────────────────────"
                                               'face 'gptel-runner-chat-tool-label))))))
        (nreverse overlays)))))

(provide 'gptel-runner-chat-tools)
;;; gptel-runner-chat-tools.el ends here
