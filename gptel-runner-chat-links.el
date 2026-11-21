;;; gptel-runner-chat-links.el --- Readable links in chats -*- lexical-binding: t; -*-

;;; Commentary:

;; Presentation-only links for chat transcripts.  Destinations are resolved
;; when activated, so restored conversations can acquire their workspace later.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'url-util)
(require 'project)

(defface gptel-runner-chat-link
  '((t (:inherit link :underline t)))
  "Face for actionable chat references."
  :group 'gptel-runner)

(defvar gptel-runner-chat-links-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-runner-chat-links-open)
    (define-key map [mouse-1] #'gptel-runner-chat-links-open)
    map)
  "Keymap on chat links.")

(defun gptel-runner-chat-links--destination (target)
  "Parse TARGET into a web or local file destination, or nil.
Only HTTP(S) URLs and local file references are supported."
  (let ((path (string-trim target)) line column)
    (cond
     ((string-match-p "\\`https?://[^[:space:]]+\\'" path)
      (list :type 'web :url path))
     (t
      (when (string-match
             "\\(?:#L\\|::?\\)\\([0-9]+\\)\\(?:[:.]\\([0-9]+\\)\\)?\\(?:-L?[0-9]+\\)?\\'"
             path)
        (setq line (string-to-number (match-string 1 path))
              column (and (match-string 2 path)
                          (string-to-number (match-string 2 path)))
              path (substring path 0 (match-beginning 0))))
      (when (string-prefix-p "file:" path)
        (setq path (substring path 5))
        (cond ((string-prefix-p "///" path) (setq path (substring path 2)))
              ((string-prefix-p "//localhost/" path)
               (setq path (substring path 11)))
              ((string-prefix-p "//" path) (setq path ""))))
      (setq path (decode-coding-string (url-unhex-string path) 'utf-8))
      (unless (or (string-empty-p path)
                  (string-prefix-p "#" path)
                  (string-match-p "[\n\r\0]" path)
                  (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" path)
                  (file-remote-p path))
        (list :type 'file :path path :line line :column column))))))

(defun gptel-runner-chat-links--resolve (path)
  "Resolve local PATH against the current chat or its project root."
  (let ((expanded (expand-file-name path default-directory)))
    (unless (file-remote-p expanded)
      (cond
       ((file-exists-p expanded) expanded)
       ((or (file-name-absolute-p path) (string-prefix-p "~" path)) expanded)
       (t
        (let* ((project (ignore-errors (project-current nil default-directory)))
               (root (and project (project-root project)))
               (candidate (and root (expand-file-name path root))))
          (if (and candidate (not (file-remote-p candidate))
                   (file-exists-p candidate))
              candidate
            expanded)))))))

(defun gptel-runner-chat-links--visit (target)
  "Visit TARGET, opening a local reference in another window."
  (let ((destination (gptel-runner-chat-links--destination target)))
    (pcase (plist-get destination :type)
      ('web (browse-url (plist-get destination :url)))
      ('file
       (let ((file (gptel-runner-chat-links--resolve
                    (plist-get destination :path))))
         (unless (and file (file-exists-p file))
           (user-error "File not found in this chat's workspace: %s" target))
         (find-file-other-window file)
         (when-let* ((line (plist-get destination :line)))
           (widen)
           (goto-char (point-min))
           (forward-line (max 0 (1- line)))
           (when-let* ((column (plist-get destination :column)))
             (move-to-column (max 0 (1- column)))))
         (when (fboundp 'pulse-momentary-highlight-one-line)
           (pulse-momentary-highlight-one-line (point)))))
      (_ (user-error "Unsupported chat link: %s" target)))))

(defun gptel-runner-chat-links--table-target ()
  "Select a destination from the rendered table at point, if any."
  (let (choices seen)
    (dolist (overlay (overlays-at (point)))
      (when (overlay-get overlay 'gptel-runner-chat-table)
        (let ((display (overlay-get overlay 'display)) (position 0))
          (when (stringp display)
            (while (< position (length display))
              (let* ((target (get-text-property position 'gptel-runner-chat-link display))
                     (next (next-single-property-change
                            position 'gptel-runner-chat-link display (length display))))
                (when (and (stringp target) (not (member target seen)))
                  (let ((label (string-trim
                                (replace-regexp-in-string
                                 "[[:space:]\n]+" " "
                                 (substring-no-properties display position next)))))
                    (push target seen)
                    (push (cons (format "%s  —  %s" label target) target) choices)))
                (setq position next)))))))
    (setq choices (nreverse choices))
    (cond ((null choices) nil)
          ((null (cdr choices)) (cdar choices))
          (t (cdr (assoc (completing-read "Open table link: " choices nil t)
                         choices))))))

(defun gptel-runner-chat-links-open (&optional event)
  "Open the chat reference at point or mouse EVENT.
From the keyboard, select among links when point is inside a rendered table."
  (interactive (list (when (mouse-event-p last-input-event) last-input-event)))
  (let* ((display (and event (posn-string (event-start event))))
         (display-target (and display
                              (get-text-property (cdr display)
                                                 'gptel-runner-chat-link
                                                 (car display)))))
    (when event (mouse-set-point event))
    (let ((target
         (or display-target
             (get-text-property (point) 'gptel-runner-chat-link)
             (cl-loop for overlay in (overlays-at (point))
                      thereis (overlay-get overlay 'gptel-runner-chat-link))
             (and (not event) (gptel-runner-chat-links--table-target)))))
      (unless target (user-error "No chat link at point"))
      (gptel-runner-chat-links--visit target))))

(defun gptel-runner-chat-links-string (label target)
  "Return clickable LABEL for TARGET for use in display strings."
  (propertize label
              'gptel-runner-chat-link target
              'face 'gptel-runner-chat-link
              'mouse-face 'highlight
              'keymap gptel-runner-chat-links-map
              'help-echo (concat target "\nRET or click to open")))

(defun gptel-runner-chat-links--markdown-target (text)
  "Extract the destination from Markdown TEXT, ignoring an optional title."
  (setq text (string-trim text))
  (cond ((string-match "\\`<\\([^>]+\\)>\\(?:[ \t]+.*\\)?\\'" text)
         (setq text (match-string 1 text)))
        ((string-match "\\`\\(.+?\\)[ \t]+\\(?:\"[^\"]*\"\\|'[^']*'\\)\\'" text)
         (setq text (match-string 1 text))))
  (replace-regexp-in-string "\\\\\\([][()<>\\\\]\\)" "\\1" text))

(defun gptel-runner-chat-links--closing-paren (limit)
  "Find a balanced closing parenthesis before LIMIT from point.
Point is initially just after the opening parenthesis."
  (let ((depth 1) done)
    (while (and (> depth 0) (< (point) limit) (not done))
      (pcase (char-after)
        (?\n (setq done t))
        (?\\ (forward-char) (when (< (point) limit) (forward-char)))
        (?\( (cl-incf depth) (forward-char))
        (?\) (cl-decf depth) (forward-char))
        (_ (forward-char))))
    (when (= depth 0) (point))))

(defun gptel-runner-chat-links--trim-bare (target)
  "Trim sentence punctuation and unmatched delimiters from bare TARGET."
  (setq target (string-trim-right target "[.,;:!?]+"))
  (while (and (> (length target) 0)
              (let* ((last (aref target (1- (length target))))
                     (opening (cdr (assq last '((?\) . ?\() (?\] . ?\[))))))
                (and opening
                     (> (cl-count last target) (cl-count opening target)))))
    (setq target (substring target 0 -1)))
  target)

(defun gptel-runner-chat-links-render (start end &optional skip-p)
  "Return link overlays for START through END without changing text.
SKIP-P, when non-nil, receives candidate start and end positions; a
non-nil result excludes the link.  The caller owns the returned overlays."
  (save-excursion
    (let (overlays)
      (cl-labels
          ((covered (begin finish)
             (or (and skip-p (funcall skip-p begin finish))
                 (cl-some (lambda (ov)
                            (and (< begin (overlay-end ov))
                                 (> finish (overlay-start ov))))
                          overlays)))
           (add-link (begin finish target &optional label existing-only)
             (unless (covered begin finish)
               (let* ((destination (gptel-runner-chat-links--destination target))
                      (file (and existing-only
                                 (eq (plist-get destination :type) 'file)
                                 (gptel-runner-chat-links--resolve
                                  (plist-get destination :path)))))
                 (when (and destination
                            (or (not existing-only)
                                (and file (file-exists-p file))))
                   (let ((ov (make-overlay begin finish nil nil nil)))
                     (overlay-put ov 'gptel-runner-chat-links t)
                     (overlay-put ov 'gptel-runner-chat-link target)
                     (overlay-put ov 'face 'gptel-runner-chat-link)
                     (overlay-put ov 'mouse-face 'highlight)
                     (overlay-put ov 'keymap gptel-runner-chat-links-map)
                     (overlay-put ov 'help-echo (concat target "\nRET or click to open"))
                     (overlay-put ov 'priority 30)
                     (overlay-put ov 'evaporate t)
                     (when label
                       (overlay-put ov 'display
                                    (gptel-runner-chat-links-string label target)))
                     (push ov overlays)))))))
        ;; Org links first so their nested brackets cannot become Markdown links.
        (goto-char start)
        (while (re-search-forward
                "\\[\\[\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]*\\)\\]\\)?\\]" end t)
          (add-link (match-beginning 0) (match-end 0)
                    (match-string-no-properties 1)
                    (or (match-string-no-properties 2)
                        (match-string-no-properties 1))))
        (goto-char start)
        (while (re-search-forward "\\[\\([^]\n]+\\)\\](" end t)
          (let ((begin (match-beginning 0))
                (label (match-string-no-properties 1))
                (destination-start (point)))
            (when-let* ((finish (gptel-runner-chat-links--closing-paren end)))
              (unless (eq (char-before begin) ?!)
                (add-link begin finish
                          (gptel-runner-chat-links--markdown-target
                           (buffer-substring-no-properties
                            destination-start (1- finish)))
                          (string-trim label "`+" "`+"))))))
        ;; Markdown and Org code-quoted paths, including slashless names.
        (dolist (pattern '("`\\([^`\n]+\\)`" "=\\([^=\n]+\\)="
                           "~\\([^~\n]+\\)~"))
          (goto-char start)
          (while (re-search-forward pattern end t)
            (let ((begin (match-beginning 0)) (finish (match-end 0))
                  (target (match-string-no-properties 1)))
              (add-link begin finish target target t))))
        (goto-char start)
        (while (re-search-forward "https?://[^[:space:]<>\"'`]+" end t)
          (let* ((begin (match-beginning 0))
                 (target (gptel-runner-chat-links--trim-bare
                          (match-string-no-properties 0))))
            (add-link begin (+ begin (length target)) target)))
        (goto-char start)
        (while (re-search-forward
                "\\(?:^\\|[[:space:](]\\)\\(\\(?:/\\|~/\\)[^[:space:]<>\"'`]+\\)" end t)
          (let* ((begin (match-beginning 1))
                 (target (gptel-runner-chat-links--trim-bare
                          (match-string-no-properties 1))))
            (add-link begin (+ begin (length target)) target nil t))))
      (sort overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(provide 'gptel-runner-chat-links)
;;; gptel-runner-chat-links.el ends here
