;;; gptel-runner-chat-tables.el --- Readable chat tables -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Render Markdown and Org pipe tables without rewriting their source.  Wide
;; tables become a stack of labeled records in narrow conversation windows.
;; The caller owns the returned overlays and refreshes them after edits/resizes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function gptel-runner-chat-links-render "gptel-runner-chat-links"
                  (start end &optional skip-p))
(declare-function gptel-runner-chat-links-string "gptel-runner-chat-links"
                  (label target))

(defgroup gptel-runner-chat-tables nil
  "Readable tables in gptel conversations."
  :group 'applications)

(defcustom gptel-runner-chat-table-width nil
  "Maximum displayed table width, or nil to fit the conversation window.
This is a display-column limit, measured using the fixed-pitch table font."
  :type '(choice (const :tag "Fit window" nil) (integer :tag "Columns"))
  :group 'gptel-runner-chat-tables)

(defface gptel-runner-chat-table
  '((t (:inherit fixed-pitch :height 1.0 :weight normal :slant normal)))
  "Face for table cells and their padding."
  :group 'gptel-runner-chat-tables)

(defface gptel-runner-chat-table-header
  '((t (:inherit (gptel-runner-chat-table font-lock-keyword-face) :weight bold)))
  "Face for table headings and record labels."
  :group 'gptel-runner-chat-tables)

(defface gptel-runner-chat-table-border
  '((t (:inherit (gptel-runner-chat-table shadow))))
  "Face for the quiet rules around tables."
  :group 'gptel-runner-chat-tables)

(defun gptel-runner-chat-tables--cells (line)
  "Split pipe table LINE, respecting escapes and inline code spans."
  (let ((index (if (string-match "\\`[ \t]*|" line) (match-end 0) 0))
        (limit (length line)) (code-run 0) cells chars last-delimiter)
    (while (< index limit)
      (let ((char (aref line index)))
        (cond
         ((and (= char ?\\) (< (1+ index) limit))
          (unless (= (aref line (1+ index)) ?|) (push char chars))
          (push (aref line (1+ index)) chars)
          (setq index (+ index 2) last-delimiter nil))
         ((= char ?`)
          (let ((run 1))
            (while (and (< (+ index run) limit)
                        (= (aref line (+ index run)) ?`))
              (cl-incf run))
            (cond ((zerop code-run) (setq code-run run))
                  ((= run code-run) (setq code-run 0)))
            (dotimes (_ run) (push ?` chars))
            (cl-incf index run)
            (setq last-delimiter nil)))
         ((and (= char ?|) (zerop code-run))
          (push (string-trim (apply #'string (nreverse chars))) cells)
          (setq chars nil last-delimiter t)
          (cl-incf index))
         (t (push char chars) (cl-incf index)
            (unless (memq char '(?\s ?\t)) (setq last-delimiter nil))))))
    (unless (and last-delimiter
                 (string-empty-p (string-trim (apply #'string (reverse chars)))))
      (push (string-trim (apply #'string (nreverse chars))) cells))
    (nreverse cells)))

(defun gptel-runner-chat-tables--separator (line)
  "Return alignment symbols if LINE is a table rule; otherwise nil."
  (let ((cells (gptel-runner-chat-tables--cells
                (replace-regexp-in-string "+" "|" line t t))))
    (when (and cells (cl-every (lambda (cell)
                                (string-match-p "\\`:?-+:?\\'" cell))
                              cells))
      (mapcar (lambda (cell)
                (cond ((and (string-prefix-p ":" cell)
                            (string-suffix-p ":" cell)) 'center)
                      ((string-suffix-p ":" cell) 'right)
                      (t 'left)))
              cells))))

(defun gptel-runner-chat-tables--width ()
  "Return the width available for a table in this buffer."
  (max 12
       (or gptel-runner-chat-table-width
           (let ((windows (get-buffer-window-list (current-buffer) nil t)))
             (- (if windows
                    (apply #'min
                           (mapcar (lambda (window)
                                     (/ (window-body-width window t)
                                        (max 1 (window-font-width
                                                window 'gptel-runner-chat-table))))
                                   windows))
                  (window-body-width))
                4)))))

(defun gptel-runner-chat-tables--cell (text)
  "Return display TEXT with unobtrusive inline markup."
  (let ((text (replace-regexp-in-string "<br[ \t]*/?>" " " text t t)))
    (when (fboundp 'gptel-runner-chat-links-render)
      (setq text
            (with-temp-buffer
              (insert text)
              (let ((position (point-min)) pieces)
                (dolist (overlay
                         (sort (gptel-runner-chat-links-render (point-min) (point-max))
                               (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                  (push (buffer-substring-no-properties position (overlay-start overlay)) pieces)
                  (push (or (overlay-get overlay 'display)
                            (gptel-runner-chat-links-string
                             (buffer-substring-no-properties (overlay-start overlay)
                                                             (overlay-end overlay))
                             (overlay-get overlay 'gptel-runner-chat-link))) pieces)
                  (setq position (overlay-end overlay)))
                (push (buffer-substring-no-properties position (point-max)) pieces)
                (apply #'concat (nreverse pieces))))))
    (setq text (replace-regexp-in-string "`\\([^`\n]+\\)`" "\\1" text))
    (setq text (replace-regexp-in-string "\\*\\*\\([^*\n]+\\)\\*\\*" "\\1" text))
    text))

(defun gptel-runner-chat-tables--face (text face)
  "Return TEXT with FACE, preserving any more specific face properties."
  (setq text (copy-sequence text))
  (add-face-text-property 0 (length text) face t text)
  text)

(defun gptel-runner-chat-tables--pad (text width alignment face)
  "Pad TEXT to WIDTH according to ALIGNMENT, applying FACE."
  (let* ((space (max 0 (- width (string-width text))))
         (left (pcase alignment ('right space) ('center (/ space 2)) (_ 0))))
    (gptel-runner-chat-tables--face
     (concat (make-string left ?\s) text (make-string (- space left) ?\s)) face)))

(defun gptel-runner-chat-tables--rule (widths left middle right)
  "Make a table rule for WIDTHS with LEFT, MIDDLE and RIGHT characters."
  (gptel-runner-chat-tables--face
   (concat left (mapconcat (lambda (width) (make-string (+ width 2) ?─))
                          widths middle) right "\n")
   'gptel-runner-chat-table-border))

(defun gptel-runner-chat-tables--row (cells widths alignments face)
  "Render CELLS using WIDTHS, ALIGNMENTS and FACE."
  (let ((border (gptel-runner-chat-tables--face "│" 'gptel-runner-chat-table-border)))
    (concat border
            (mapconcat
             #'identity
             (cl-loop for width in widths for column from 0
                      collect (concat
                               (gptel-runner-chat-tables--face " " face)
                               (gptel-runner-chat-tables--pad
                                (or (nth column cells) "") width
                                (nth column alignments) face)
                               (gptel-runner-chat-tables--face " " face)))
             border)
            border "\n")))

(defun gptel-runner-chat-tables--wrap (text width)
  "Wrap TEXT to display WIDTH, breaking long words without losing content."
  (let (lines)
    (while (> (string-width text) width)
      (let* ((prefix (truncate-string-to-width text width))
             (cut (length prefix)))
        ;; Prefer a word boundary, but retain all characters in long paths.
        (when (string-match "[ \t]+[^ \t]*\\'" prefix)
          (when (> (match-beginning 0) (/ width 3))
            (setq cut (match-beginning 0))))
        (when (zerop cut) (setq cut 1))
        (push (substring text 0 cut) lines)
        (setq text (string-trim-left (substring text cut)))))
    (nreverse (cons text lines))))

(defun gptel-runner-chat-tables--cards (headers rows width)
  "Render HEADERS and ROWS as labeled records fitting WIDTH."
  (let ((column-count (apply #'max (length headers) (mapcar #'length rows)))
        (number 0) parts)
    (dolist (row rows)
      (cl-incf number)
      (push (gptel-runner-chat-tables--face
             (concat (format "─ %02d " number) (make-string (max 0 (- width 5)) ?─) "\n")
             'gptel-runner-chat-table-border) parts)
      (dotimes (column column-count)
        (let ((label (or (nth column headers) (format "Column %d" (1+ column))))
              (value (or (nth column row) "—")))
          (dolist (line (gptel-runner-chat-tables--wrap label (- width 2)))
            (push (gptel-runner-chat-tables--face
                   (concat "  " line "\n") 'gptel-runner-chat-table-header) parts))
          (dolist (line (gptel-runner-chat-tables--wrap value (- width 4)))
            (push (gptel-runner-chat-tables--face
                   (concat "    " line "\n") 'gptel-runner-chat-table) parts)))))
    (apply #'concat (nreverse parts))))

(defun gptel-runner-chat-tables--display (headers rows alignments width)
  "Render HEADERS and ROWS with ALIGNMENTS within WIDTH."
  (setq headers (mapcar #'gptel-runner-chat-tables--cell headers)
        rows (mapcar (lambda (row) (mapcar #'gptel-runner-chat-tables--cell row)) rows))
  (let* ((count (apply #'max (length headers) (mapcar #'length rows)))
         (widths (cl-loop for column below count
                          collect (apply #'max 1
                                         (mapcar (lambda (row)
                                                   (string-width (or (nth column row) "")))
                                                 (cons headers rows)))))
         (natural (+ 1 (* 3 count) (apply #'+ widths))))
    (if (> natural width)
        (gptel-runner-chat-tables--cards headers (or rows (list headers)) width)
      (concat
       (gptel-runner-chat-tables--rule widths "╭" "┬" "╮")
       (gptel-runner-chat-tables--row headers widths nil 'gptel-runner-chat-table-header)
       (gptel-runner-chat-tables--rule widths "├" "┼" "┤")
       (mapconcat (lambda (row)
                    (gptel-runner-chat-tables--row row widths alignments
                                                 'gptel-runner-chat-table))
                  rows "")
       (gptel-runner-chat-tables--rule widths "╰" "┴" "╯")))))

(defun gptel-runner-chat-tables-render (start end &optional skip-p)
  "Return overlays rendering pipe tables between START and END.
SKIP-P, when non-nil, receives a table's start and end and excludes it on a
non-nil return.  At least a header and a separator are required.  The caller
owns these overlays; source text and its text properties remain untouched."
  (save-excursion
    (save-match-data
      (goto-char start)
      (let ((width (gptel-runner-chat-tables--width)) overlays)
        (while (re-search-forward "^[ \t]*|" end t)
          (let ((beg (line-beginning-position)) lines finish)
            (goto-char beg)
            (while (and (< (point) end) (looking-at "[ \t]*|"))
              (push (buffer-substring-no-properties
                     (line-beginning-position) (min end (line-end-position))) lines)
              (forward-line 1))
            (setq finish (min end (point)) lines (nreverse lines))
            (let* ((leading-rule (gptel-runner-chat-tables--separator (car lines)))
                   (data (if leading-rule (cdr lines) lines))
                   (alignment (and (cdr data)
                                   (gptel-runner-chat-tables--separator (cadr data)))))
              (when (and alignment (not (and skip-p (funcall skip-p beg finish))))
                (let* ((headers (gptel-runner-chat-tables--cells (car data)))
                       (rows (cl-loop for line in (cddr data)
                                      unless (gptel-runner-chat-tables--separator line)
                                      collect (gptel-runner-chat-tables--cells line)))
                       (display (gptel-runner-chat-tables--display headers rows alignment width))
                       (overlay (make-overlay beg finish nil nil t)))
                  ;; Avoid adding a line when the source has no final newline.
                  (when (and (> finish beg) (not (eq (char-before finish) ?\n)))
                    (setq display (string-remove-suffix "\n" display)))
                  (overlay-put overlay 'gptel-runner-chat-table t)
                  (overlay-put overlay 'gptel-runner-chat-tables t)
                  (overlay-put overlay 'priority 30)
                  ;; Org Modern inverts the source's first pipe character.
                  ;; Replacement strings inherit that character unless reset.
                  (overlay-put overlay 'face
                               '(:inherit default :inverse-video nil :underline nil
                                 :overline nil :strike-through nil :box nil :extend nil))
                  (overlay-put overlay 'display display)
                  (overlay-put overlay 'line-prefix nil)
                  (overlay-put overlay 'wrap-prefix nil)
                  (overlay-put overlay 'help-echo
                               "Table · C-c C-v to view or edit the original source")
                  (push overlay overlays))))))
        (nreverse overlays)))))

(provide 'gptel-runner-chat-tables)
;;; gptel-runner-chat-tables.el ends here
