;;; gptel-runner-chat.el --- A quiet, readable gptel workspace -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Optional presentation for ordinary gptel chats, in Org or Markdown.
;; Enable `gptel-runner-chat-global-mode' to dress existing and future chats.
;; All decorations are overlays: transcripts, role properties, and undo remain
;; untouched.  C-c C-v toggles source, C-c C-n / C-c C-p move between turns.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'face-remap)
(require 'gptel-runner-chat-links)
(require 'gptel-runner-chat-tables)
(require 'gptel-runner-chat-tools)

(defgroup gptel-runner-chat nil
  "Readable conversations without changing their contents."
  :group 'convenience :prefix "gptel-runner-chat-")

(defcustom gptel-runner-chat-width 100
  "Preferred reading width, in columns, including the side padding."
  :type 'integer :group 'gptel-runner-chat)

(defcustom gptel-runner-chat-refresh-delay 0.18
  "Seconds between display updates while a response is streaming."
  :type 'number :group 'gptel-runner-chat)

(defcustom gptel-runner-chat-assistant-name "Assistant"
  "Fallback name for responses without a named response prefix."
  :type 'string :group 'gptel-runner-chat)

(defface gptel-runner-chat-user
  '((t (:inherit font-lock-keyword-face :weight bold :height 0.95)))
  "Speaker label for the user's messages." :group 'gptel-runner-chat)
(defface gptel-runner-chat-assistant
  '((t (:inherit font-lock-function-name-face :weight bold :height 0.95)))
  "Speaker label for assistant responses." :group 'gptel-runner-chat)
(defface gptel-runner-chat-heading
  '((t (:inherit default :weight bold :height 1.18)))
  "Section heading." :group 'gptel-runner-chat)
(defface gptel-runner-chat-subheading
  '((t (:inherit default :weight semi-bold :height 1.06)))
  "Smaller section heading." :group 'gptel-runner-chat)
(defface gptel-runner-chat-muted
  '((t (:inherit shadow :height 0.9)))
  "Secondary information and structural punctuation." :group 'gptel-runner-chat)
(defface gptel-runner-chat-code
  '((((class color) (background dark))
     (:inherit fixed-pitch :background "#12333b" :extend t :weight normal :slant normal))
    (((class color) (background light))
     (:inherit fixed-pitch :background "#edf2f3" :extend t :weight normal :slant normal))
    (t (:inherit fixed-pitch)))
  "Code panel, keeping syntax highlighting from the major mode."
  :group 'gptel-runner-chat)
(defface gptel-runner-chat-code-label
  '((t (:inherit (gptel-runner-chat-code shadow) :height 0.9 :weight bold)))
  "Language caption on a code panel." :group 'gptel-runner-chat)
(defface gptel-runner-chat-inline-code
  '((t (:inherit (fixed-pitch font-lock-constant-face))))
  "Inline code." :group 'gptel-runner-chat)
(defface gptel-runner-chat-report
  '((t (:inherit font-lock-type-face :weight bold)))
  "Discussion report caption." :group 'gptel-runner-chat)
(defface gptel-runner-chat-quote
  '((t (:inherit font-lock-doc-face :slant italic)))
  "Quoted prose." :group 'gptel-runner-chat)

(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-use-header-line)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar gptel-runner-chat-mode)
(declare-function gptel-highlight-mode "gptel" (&optional arg))

(defvar-local gptel-runner-chat--overlays nil)
(defvar-local gptel-runner-chat--timer nil)
(defvar-local gptel-runner-chat--saved nil)
(defvar-local gptel-runner-chat--face-cookie nil)
(defvar-local gptel-runner-chat--window-margins nil)
(defvar-local gptel-runner-chat--source nil)
(defvar-local gptel-runner-chat--expanded-reports nil)
(defvar-local gptel-runner-chat--excluded nil)
(defvar-local gptel-runner-chat--indexed-ranges nil)
(defvar-local gptel-runner-chat--range-index [])
(defvar-local gptel-runner-chat--turns nil)
(defvar-local gptel-runner-chat--tool-starts nil)
(defvar-local gptel-runner-chat--tool-response-starts nil)
(defvar-local gptel-runner-chat--highlight-was-on nil)

(defun gptel-runner-chat--overlay (beg end &rest properties)
  "Create an owned overlay from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end nil nil nil)))
    (overlay-put ov 'gptel-runner-chat t)
    (overlay-put ov 'evaporate t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    (push ov gptel-runner-chat--overlays)
    ov))

(defun gptel-runner-chat--clear ()
  "Remove only overlays owned by this mode."
  (mapc #'delete-overlay gptel-runner-chat--overlays)
  (setq gptel-runner-chat--overlays nil
        gptel-runner-chat--turns nil
        gptel-runner-chat--tool-starts nil
        gptel-runner-chat--tool-response-starts nil
        gptel-runner-chat--excluded nil
        gptel-runner-chat--indexed-ranges nil
        gptel-runner-chat--range-index []))

(defun gptel-runner-chat--excluded-p (beg end)
  "Find an excluded range overlapping BEG through END in logarithmic time."
  (unless (eq gptel-runner-chat--indexed-ranges gptel-runner-chat--excluded)
    (setq gptel-runner-chat--indexed-ranges gptel-runner-chat--excluded
          gptel-runner-chat--range-index
          (vconcat (sort (copy-sequence gptel-runner-chat--excluded)
                         (lambda (a b) (< (car a) (car b)))))))
  (let ((lo 0) (hi (length gptel-runner-chat--range-index)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (cdr (aref gptel-runner-chat--range-index mid)) beg)
            (setq lo (1+ mid))
          (setq hi mid))))
    (and (< lo (length gptel-runner-chat--range-index))
         (< (car (aref gptel-runner-chat--range-index lo)) end))))

(defun gptel-runner-chat--skip-p (beg end)
  "Whether BEG through END overlaps code or native tool content."
  (or (let ((pos beg) found)
        (while (and (< pos end) (not found))
          (let ((role (get-text-property pos 'gptel)))
            (setq found (and role (not (eq role 'response)))))
          (setq pos (next-single-property-change pos 'gptel nil end)))
        found)
      (gptel-runner-chat--excluded-p beg end)))

(defun gptel-runner-chat--blocks ()
  "Give code, quotations and native tools distinct visual treatments."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward
            "^[ \t]*\\(```+\\|~~~+\\|#\\+begin_\\([[:alnum:]_]+\\)\\)\\([^\n]*\\)$" nil t)
      (let* ((beg (line-beginning-position))
             (opening-end (line-end-position))
             (fence (match-string-no-properties 1))
             (kind (match-string-no-properties 2))
             (info (string-trim (match-string-no-properties 3)))
             (tool (or (equal (downcase (or kind "")) "tool")
                       (string-match-p "\\`tool\\b" info)))
             (closing (if kind
                          (concat "^[ \t]*#\\+end_" (regexp-quote kind) "[ \t]*$")
                        (format "^[ \t]*%s\\{%d,\\}[ \t]*$"
                                (regexp-quote (substring fence 0 1)) (length fence))))
             (body-beg (min (point-max) (1+ opening-end)))
             close-beg end)
        (forward-line 1)
        (if (re-search-forward closing nil t)
            (setq close-beg (line-beginning-position) end (line-end-position))
          (setq end (point-max)))
        (push (cons beg end) gptel-runner-chat--excluded)
        (when tool
          (let ((decorations (gptel-runner-chat-tools-render-block
                              beg opening-end body-beg close-beg end info)))
            (when decorations
              (push beg gptel-runner-chat--tool-starts)
              (setq gptel-runner-chat--overlays
                    (nconc decorations gptel-runner-chat--overlays)))))
        (when (equal (downcase (or kind "")) "quote")
          (gptel-runner-chat--overlay
           beg opening-end 'display (propertize "❝" 'face 'gptel-runner-chat-quote)
           'face '(:inherit default :underline nil :overline nil :box nil)
           'invisible (unless (eq buffer-invisibility-spec t) 'gptel-runner-chat-visible)
           'priority 20 'gptel-runner-chat-block-opening t)
          (when close-beg
            (gptel-runner-chat--overlay
             close-beg end 'display (propertize "❞" 'face 'gptel-runner-chat-muted)
             'face '(:inherit default :underline nil :overline nil :box nil)
             'priority 20))
          (when (< body-beg (or close-beg end))
            (let ((rail (propertize "  │  " 'face 'gptel-runner-chat-muted)))
              (gptel-runner-chat--overlay
               body-beg (or close-beg end) 'face 'gptel-runner-chat-quote
               'line-prefix rail 'wrap-prefix rail 'priority 2
               'gptel-runner-chat-quotation t))))
        (when (and (not tool) (or (null kind) (member (downcase kind) '("src" "example"))))
          (gptel-runner-chat--overlay beg end 'face 'gptel-runner-chat-code 'priority 1)
          (gptel-runner-chat--overlay
           beg opening-end 'face
           '(:inherit gptel-runner-chat-code-label :underline nil :overline nil :box nil)
           'display
           (propertize (concat "  ┌  " (if (string-empty-p info) (or kind "code") info))
                       'face 'gptel-runner-chat-code-label) 'priority 20
           'gptel-runner-chat-code-opening t)
          (when close-beg
            (gptel-runner-chat--overlay
             close-beg end 'face
             '(:inherit gptel-runner-chat-code-label :underline nil :overline nil :box nil)
             'display (propertize "  └────────────────────"
				  'face 'gptel-runner-chat-code-label)
             'priority 20))
          (when (< body-beg (or close-beg end))
            (gptel-runner-chat--overlay
             body-beg (or close-beg end) 'line-prefix "  " 'wrap-prefix "  ")))
        (goto-char end)))))

(defun gptel-runner-chat--speaker (beg end label assistant &optional prefix)
  "Label a turn at BEG through END with LABEL and ASSISTANT role.
PREFIX means an existing prefix is replaced, otherwise insert a caption."
  (let* ((face (if assistant 'gptel-runner-chat-assistant 'gptel-runner-chat-user))
         (caption (propertize (concat (if assistant "●  " "○  ") (upcase label))
                              'face face)))
    (push beg gptel-runner-chat--turns)
    (if prefix
        (gptel-runner-chat--overlay beg end 'display caption 'priority 30)
      (if-let* ((opening (cl-find-if
                          (lambda (ov) (or (overlay-get ov 'gptel-runner-chat-code-opening)
                                           (overlay-get ov 'gptel-runner-chat-block-opening)))
                          (overlays-at beg))))
          (overlay-put opening 'before-string (concat caption "\n\n"))
        (gptel-runner-chat--overlay
         beg (min (point-max) (1+ beg))
         'before-string (concat caption "\n\n") 'priority 30)))))

(defun gptel-runner-chat--prefix (response)
  "Return this major mode's configured RESPONSE or prompt prefix."
  (let ((var (if response 'gptel-response-prefix-alist 'gptel-prompt-prefix-alist)))
    (and (boundp var) (alist-get major-mode (symbol-value var)))))

(defun gptel-runner-chat--roles ()
  "Label each assistant turn once, including its tools and final response."
  (let* ((prompt (gptel-runner-chat--prefix nil))
         (response (gptel-runner-chat--prefix t))
         (name (if (and response (string-match "@[[:alnum:]_-]+" response))
                   (substring (match-string 0 response) 1)
                 gptel-runner-chat-assistant-name))
         (pos (point-min)) events assistant-active tool-pending)
    ;; Gather boundaries first, then read them in transcript order.  A tool
    ;; result interrupts the response text property, not the speaker's turn.
    (dolist (spec (list (cons prompt nil) (cons response t)))
      (when (and (stringp (car spec)) (not (string-empty-p (car spec))))
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^" (regexp-quote (string-remove-suffix "\n" (car spec)))
                        (when (string-suffix-p "\n" (car spec)) "$")) nil t)
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (unless (or (gptel-runner-chat--skip-p beg end)
                        (and (not (cdr spec)) (eq (get-text-property beg 'gptel) 'response)))
              (push (list beg end (if (cdr spec) 'assistant-prefix 'user-prefix)
                          (string-suffix-p "\n" (car spec))) events))))))
    (dolist (beg gptel-runner-chat--tool-starts)
      (push (list beg beg 'tool) events))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'gptel nil (point-max))))
        (when (eq (get-text-property pos 'gptel) 'response)
          (save-excursion
            (goto-char pos)
            (skip-chars-forward " \t\n" next)
            (let ((beg (point)))
              (when (< beg next)
                (push (list beg beg 'assistant) events)))))
        ;; Empty prompt prefixes are supported by gptel too.  In that case
        ;; ordinary untagged prose is the only available user boundary.
        (when (and (or (null prompt) (string-empty-p prompt))
                   (null (get-text-property pos 'gptel)))
          (save-excursion
            (goto-char pos)
            (skip-chars-forward " \t\n" next)
            (when (and (< (point) next)
                       (not (cl-some (lambda (event)
                                       (and (eq (nth 2 event) 'assistant-prefix)
                                            (= (car event) (point)))) events))
                       (not (gptel-runner-chat--excluded-p (point) (1+ (point))))
                       (not (cl-some (lambda (ov) (overlay-get ov 'gptel-tool))
                                     (overlays-at (point)))))
              (push (list (point) (point) 'user) events))))
        (setq pos next)))
    (setq events
          (sort events (lambda (a b)
                         (if (= (car a) (car b))
                             (and (memq (nth 2 a) '(user-prefix assistant-prefix))
                                  (not (memq (nth 2 b) '(user-prefix assistant-prefix))))
                           (< (car a) (car b))))))
    (dolist (event events)
      (pcase-let ((`(,beg ,end ,kind . ,rest) event))
        (cond
         ((memq kind '(user-prefix user))
          (setq assistant-active nil tool-pending nil)
          (gptel-runner-chat--speaker beg end "You" nil (eq kind 'user-prefix))
          (when (and (eq kind 'user-prefix) (not (car rest)))
            (overlay-put (car gptel-runner-chat--overlays) 'after-string "\n\n")))
         (assistant-active
          ;; A literal repeated prefix is presentation noise as well.  Hide
          ;; only that prefix; keep every byte and role property in the buffer.
          (when (eq kind 'assistant-prefix)
            (gptel-runner-chat--overlay
             beg (if (and (car rest) (< end (point-max))) (1+ end) end)
             'display "" 'priority 30 'gptel-runner-chat-continuation t)))
         (t
          (setq assistant-active t)
          (gptel-runner-chat--speaker beg end name t (eq kind 'assistant-prefix))
          (when (and (eq kind 'assistant-prefix) (not (car rest)))
            (overlay-put (car gptel-runner-chat--overlays) 'after-string "\n\n"))))
        (when (and (eq kind 'assistant) tool-pending)
          (push beg gptel-runner-chat--tool-response-starts)
          (setq tool-pending nil))
        (when (eq kind 'tool) (setq tool-pending t))))))

(defun gptel-runner-chat--tool-response-spacing ()
  "Add two display-only blank lines before prose following tool activity."
  (dolist (beg gptel-runner-chat--tool-response-starts)
    ;; Attach to an existing replacement when the answer starts with a
    ;; heading, table, link or code block, so the gap stays visible too.
    (let ((ov (or (cl-find-if
                   (lambda (item) (and (= (overlay-start item) beg)
                                       (overlay-get item 'display)))
                   gptel-runner-chat--overlays)
                  (gptel-runner-chat--overlay beg (min (point-max) (1+ beg))
                                              'priority 30))))
      (overlay-put ov 'before-string (concat "\n\n" (overlay-get ov 'before-string)))
      (overlay-put ov 'gptel-runner-chat-tool-gap t))))

(defun gptel-runner-chat--headings ()
  "Style headings, list bullets, quotations, and inline emphasis."
  (goto-char (point-min))
  (while (re-search-forward
          (if (derived-mode-p 'org-mode) "^\\(\\*+\\) +\\(.+\\)$" "^\\(#+\\) +\\(.+\\)$") nil t)
    (let ((beg (match-beginning 0)) (end (match-end 0))
          (body (match-beginning 2)) (depth (length (match-string 1))))
      (unless (or (gptel-runner-chat--skip-p beg end) (memq beg gptel-runner-chat--turns))
        (gptel-runner-chat--overlay beg body 'display "" 'priority 10)
        (gptel-runner-chat--overlay body end 'face
                                    (if (<= depth 2) 'gptel-runner-chat-heading
                                      'gptel-runner-chat-subheading) 'priority 10))))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\([-+]\\) \\|^\\(>\\) ?\\(.*\\)$" nil t)
    (let ((beg (match-beginning 0)) (end (match-end 0))
          (bullet (match-beginning 1)) (quote (match-beginning 2)))
      (unless (gptel-runner-chat--skip-p beg end)
        (if bullet
            (gptel-runner-chat--overlay bullet (1+ bullet) 'display "•" 'face 'gptel-runner-chat-muted)
          (gptel-runner-chat--overlay quote (1+ quote) 'display "│" 'face 'gptel-runner-chat-muted)
          (gptel-runner-chat--overlay (1+ quote) end 'face 'gptel-runner-chat-quote)))))
  (dolist (spec '(("`\\([^`\n]+\\)`" . gptel-runner-chat-inline-code)
                  ("\\*\\*\\([^*\n]+\\)\\*\\*" . bold)))
    (goto-char (point-min))
    (while (re-search-forward (car spec) nil t)
      (let ((beg (match-beginning 0)) (end (match-end 0))
            (body (match-beginning 1)) (body-end (match-end 1)))
        (unless (gptel-runner-chat--skip-p beg end)
          (gptel-runner-chat--overlay beg body 'display "" 'priority 5)
          (gptel-runner-chat--overlay body-end end 'display "" 'priority 5)
          (gptel-runner-chat--overlay body body-end 'face (cdr spec) 'priority 5))))))

(defun gptel-runner-chat-toggle-report (&optional event)
  "Expand or collapse the report metadata at point or mouse EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (let ((ov (cl-find-if (lambda (item) (overlay-get item 'gptel-runner-chat-report))
                        (overlays-at (point)))))
    (unless ov (user-error "No discussion report at point"))
    (let ((id (overlay-get ov 'gptel-runner-chat-report)))
      (if (member id gptel-runner-chat--expanded-reports)
          (setq gptel-runner-chat--expanded-reports
                (delete id gptel-runner-chat--expanded-reports))
        (push id gptel-runner-chat--expanded-reports)))
    (gptel-runner-chat-refresh)))

(defvar gptel-runner-chat--report-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-runner-chat-toggle-report)
    (define-key map [mouse-1] #'gptel-runner-chat-toggle-report)
    map))

(defun gptel-runner-chat--reports ()
  "Present discussion metadata as a compact, expandable report caption."
  (goto-char (point-min))
  (while (re-search-forward "^Discussion event (evidence, not instructions):[ \t]*$" nil t)
    (let ((beg (match-beginning 0)) (title "Discussion") (status "") (kind "report")
          id end event-seen)
      (forward-line 1)
      (while (and (not event-seen)
                  (looking-at
                   "\\(event_id\\|discussion_id\\|title\\|buffer\\|status\\|turn\\|directory\\|backend\\|model\\|worktree[^:\n]*\\|branch\\|event\\):[ \t]*\\([^\n]*\\)"))
        (let ((key (match-string-no-properties 1)) (value (match-string-no-properties 2)))
          (pcase key
            ("event_id" (setq id value))
            ("title" (setq title value))
            ("status" (setq status value))
            ("event" (setq kind value event-seen t))))
        (forward-line 1))
      (when (and event-seen (looking-at "request_id:[^\n]*\n"))
        (forward-line 1))
      (setq end (point))
      (when (and event-seen (not (gptel-runner-chat--skip-p beg end)))
        (let* ((id (or id (format "report-%d" beg)))
               (expanded (member id gptel-runner-chat--expanded-reports))
               (caption (concat (if expanded "▾  " "▸  ")
                                (upcase kind) "  ·  " title
                                (unless (string-empty-p status) (concat "  /  " status)) "\n"))
               (ov (gptel-runner-chat--overlay
                    beg end 'gptel-runner-chat-report id 'priority 40
                    'keymap gptel-runner-chat--report-map 'mouse-face 'highlight
                    'help-echo "RET or click: show/hide report details")))
          (if expanded
              (progn
                (overlay-put ov 'face 'gptel-runner-chat-muted)
                (overlay-put ov 'before-string (propertize caption 'face 'gptel-runner-chat-report)))
            (overlay-put ov 'display (propertize caption 'face 'gptel-runner-chat-report)))
          (push (cons beg end) gptel-runner-chat--excluded))))))

(defun gptel-runner-chat--header ()
  "Return the small conversation identity header."
  (concat (propertize "  ◉  CHAT" 'face 'gptel-runner-chat-assistant)
          (propertize (concat "    " (file-name-nondirectory (directory-file-name default-directory)))
                      'face 'gptel-runner-chat-muted)
          (when (and (boundp 'gptel-model) (not (bound-and-true-p gptel-use-header-line)))
            (propertize (format "   /   %s" gptel-model) 'face 'gptel-runner-chat-muted))
          (propertize (cond (gptel-runner-chat--source "   SOURCE")
                            ((bound-and-true-p gptel-use-header-line) "")
                            (t "     C-c C-v  source")) 'face 'gptel-runner-chat-muted)
          "  "))

(defun gptel-runner-chat--margins ()
  "Give every window displaying the current buffer a comfortable measure."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (unless (assq window gptel-runner-chat--window-margins)
      (push (cons window (window-margins window)) gptel-runner-chat--window-margins))
    (let ((margin (max 2 (/ (- (window-total-width window) gptel-runner-chat-width) 2))))
      (unless (equal (window-margins window) (cons margin margin))
        (set-window-margins window margin margin)))))

;;;###autoload
(defun gptel-runner-chat-refresh (&rest _)
  "Refresh the current chat's display without modifying its transcript."
  (interactive)
  (when gptel-runner-chat-mode
    (when (timerp gptel-runner-chat--timer) (cancel-timer gptel-runner-chat--timer))
    (setq gptel-runner-chat--timer nil)
    (save-excursion
      (save-restriction
        (widen)
        (gptel-runner-chat--clear)
        (gptel-runner-chat--margins)
        (unless gptel-runner-chat--source
          (gptel-runner-chat--blocks)
          (gptel-runner-chat--reports)
          (gptel-runner-chat--roles)
          (gptel-runner-chat--headings)
          (let ((tables (gptel-runner-chat-tables-render
                         (point-min) (point-max) #'gptel-runner-chat--skip-p)))
            (dolist (ov tables)
              (push (cons (overlay-start ov) (overlay-end ov)) gptel-runner-chat--excluded))
            (setq gptel-runner-chat--overlays (nconc tables gptel-runner-chat--overlays)))
          (setq gptel-runner-chat--overlays
                (nconc (gptel-runner-chat-links-render
                        (point-min) (point-max) #'gptel-runner-chat--skip-p)
                       gptel-runner-chat--overlays))
          (gptel-runner-chat--tool-response-spacing)
          (setq gptel-runner-chat--turns (sort (delete-dups gptel-runner-chat--turns) #'<)))))))

(defun gptel-runner-chat--schedule (&rest _)
  "Coalesce streaming and editing changes into one display refresh."
  (when (and gptel-runner-chat-mode (not (timerp gptel-runner-chat--timer)))
    (setq gptel-runner-chat--timer
          (run-at-time gptel-runner-chat-refresh-delay nil
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer (gptel-runner-chat-refresh))))
                       (current-buffer)))))

(defun gptel-runner-chat--window-change (&optional frame)
  "Schedule a responsive table layout in windows of FRAME."
  (dolist (window (window-list (and (framep frame) frame) 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when gptel-runner-chat-mode (gptel-runner-chat--schedule)))))

(defun gptel-runner-chat-toggle-source ()
  "Toggle the untouched source text for editing or inspection."
  (interactive)
  (setq gptel-runner-chat--source (not gptel-runner-chat--source))
  (gptel-runner-chat-refresh)
  (force-mode-line-update))

(defun gptel-runner-chat-next-turn (&optional previous)
  "Move to the next turn, or PREVIOUS turn when non-nil."
  (interactive)
  (let ((destination (if previous
                         (car (last (cl-remove-if-not (lambda (pos) (< pos (point)))
                                                      gptel-runner-chat--turns)))
                       (cl-find-if (lambda (pos) (> pos (point))) gptel-runner-chat--turns))))
    (if destination (goto-char destination) (user-error "No %s turn" (if previous "previous" "next")))))

(defun gptel-runner-chat-previous-turn ()
  "Move to the previous conversation turn."
  (interactive)
  (gptel-runner-chat-next-turn t))

(defvar gptel-runner-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'gptel-runner-chat-toggle-source)
    (define-key map (kbd "C-c C-n") #'gptel-runner-chat-next-turn)
    (define-key map (kbd "C-c C-p") #'gptel-runner-chat-previous-turn)
    (define-key map (kbd "C-c C-o") #'gptel-runner-chat-links-open)
    map))

(defun gptel-runner-chat--cleanup ()
  "Cancel the pending renderer and remove decorations."
  (when (timerp gptel-runner-chat--timer) (cancel-timer gptel-runner-chat--timer))
  (setq gptel-runner-chat--timer nil)
  (gptel-runner-chat--clear)
  (unless (cl-some (lambda (buffer)
                     (and (not (eq buffer (current-buffer)))
                          (buffer-local-value 'gptel-runner-chat-mode buffer)))
                   (buffer-list))
    (remove-hook 'window-size-change-functions #'gptel-runner-chat--window-change)
    (remove-hook 'window-configuration-change-hook #'gptel-runner-chat--window-change)))

;;;###autoload
(define-minor-mode gptel-runner-chat-mode
  "A spacious, navigable reading view for Org and Markdown chats.
All display changes are reversible.
Use `gptel-runner-chat-toggle-source' to view or edit the raw source."
  :lighter " Atelier" :keymap gptel-runner-chat-mode-map
  (if gptel-runner-chat-mode
      (unless gptel-runner-chat--saved
        (dolist (var '(line-spacing word-wrap truncate-lines header-line-format
                                    display-line-numbers))
          (push (list var (local-variable-p var) (symbol-value var)) gptel-runner-chat--saved))
        (setq-local line-spacing 0.22 word-wrap t truncate-lines nil display-line-numbers nil)
        (setq-local header-line-format
                    (if (and (bound-and-true-p gptel-mode)
                             (bound-and-true-p gptel-use-header-line)
                             (consp header-line-format))
                        ;; gptel updates slot 1 in place.  Keep that tail shared
                        ;; with the saved header so status and controls survive.
                        (cons (list '(:eval (gptel-runner-chat--header))
                                    (car header-line-format))
                              (cdr header-line-format))
                      (list '(:eval (gptel-runner-chat--header)) header-line-format)))
        (setq gptel-runner-chat--face-cookie
              (face-remap-add-relative 'default '(:weight normal :slant normal) 'variable-pitch))
        (setq gptel-runner-chat--highlight-was-on (bound-and-true-p gptel-highlight-mode))
        (when gptel-runner-chat--highlight-was-on (gptel-highlight-mode -1))
        (add-hook 'after-change-functions #'gptel-runner-chat--schedule nil t)
        (add-hook 'gptel-post-stream-hook #'gptel-runner-chat--schedule nil t)
        (add-hook 'gptel-post-response-functions #'gptel-runner-chat--schedule nil t)
        (add-hook 'kill-buffer-hook #'gptel-runner-chat--cleanup nil t)
        (add-hook 'change-major-mode-hook #'gptel-runner-chat--disable nil t)
        (add-hook 'window-size-change-functions #'gptel-runner-chat--window-change)
        (add-hook 'window-configuration-change-hook #'gptel-runner-chat--window-change)
        (gptel-runner-chat-refresh)
        ;; Restored chats receive role properties after gptel-mode-hook.
        (gptel-runner-chat--schedule))
    (gptel-runner-chat--cleanup)
    (remove-hook 'after-change-functions #'gptel-runner-chat--schedule t)
    (remove-hook 'gptel-post-stream-hook #'gptel-runner-chat--schedule t)
    (remove-hook 'gptel-post-response-functions #'gptel-runner-chat--schedule t)
    (remove-hook 'kill-buffer-hook #'gptel-runner-chat--cleanup t)
    (remove-hook 'change-major-mode-hook #'gptel-runner-chat--disable t)
    (when gptel-runner-chat--face-cookie
      (face-remap-remove-relative gptel-runner-chat--face-cookie)
      (setq gptel-runner-chat--face-cookie nil))
    (dolist (saved gptel-runner-chat--saved)
      (if (nth 1 saved) (set (make-local-variable (car saved)) (nth 2 saved))
        (kill-local-variable (car saved))))
    (dolist (entry gptel-runner-chat--window-margins)
      (when (and (window-live-p (car entry)) (eq (window-buffer (car entry)) (current-buffer)))
        (set-window-margins (car entry) (cadr entry) (cddr entry))))
    (setq gptel-runner-chat--saved nil gptel-runner-chat--window-margins nil)
    (when (and gptel-runner-chat--highlight-was-on (bound-and-true-p gptel-mode))
      (gptel-highlight-mode 1))
    (setq gptel-runner-chat--highlight-was-on nil)))

(defun gptel-runner-chat--disable ()
  "Disable the chat view before a major mode change."
  (gptel-runner-chat-mode -1))

(defun gptel-runner-chat--maybe-enable ()
  "Follow gptel's minor mode in conversation buffers."
  (if (bound-and-true-p gptel-mode) (gptel-runner-chat-mode 1)
    (when gptel-runner-chat-mode
      ;; gptel has already restored its pre-chat header before this hook.
      (let ((header header-line-format))
        (gptel-runner-chat-mode -1)
        (setq header-line-format header)))))

;;;###autoload
(define-minor-mode gptel-runner-chat-global-mode
  "Automatically dress existing and future gptel conversation buffers."
  :global t :group 'gptel-runner-chat
  (if gptel-runner-chat-global-mode
      (progn
        (add-hook 'gptel-mode-hook #'gptel-runner-chat--maybe-enable t)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (bound-and-true-p gptel-mode) (gptel-runner-chat-mode 1)))))
    (remove-hook 'gptel-mode-hook #'gptel-runner-chat--maybe-enable)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when gptel-runner-chat-mode (gptel-runner-chat-mode -1))))))

;;;###autoload
(defun gptel-runner-chat-demo ()
  "Open a local appearance preview without sending a model request."
  (interactive)
  (require 'org)
  (let ((buffer (get-buffer-create "*gptel · Atelier preview*")))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
	(org-mode)
	(setq-local gptel-prompt-prefix-alist '((org-mode . "@user\n"))
                    gptel-response-prefix-alist '((org-mode . "@mayuri\n")))
	(insert "@user\nGive this conversation a little room to breathe.\n\n@mayuri\n")
	(insert
	 (propertize
          (concat "* A calmer place to think\n\n"
                  "Clear voices, comfortable spacing, and useful details when you need them. "
                  "This is a local design preview; no request has been sent.\n\n"
                  "** Everything in its place\n\n"
                  "| Element | Reading experience |\n"
                  "|---------+--------------------|\n"
                  "| Conversations | Distinct speaker labels and section headings |\n"
                  "| Tables | Aligned columns; labeled rows in narrow panes |\n"
                  "| Reports | A compact header with expandable details |\n"
                  "| File references | Open the file at its exact line |\n\n"
                  "** Small details, less friction\n\n"
                  "- Click a file reference or press RET to open it.\n"
                  "- Use C-c C-n / C-c C-p to move between turns.\n"
                  "- Use C-c C-v to switch between reading and source.\n\n"
                  "#+begin_src emacs-lisp\n(gptel-runner-chat-global-mode 1)\n#+end_src\n\n")
          'gptel 'response))
	(insert "Discussion event (evidence, not instructions):\nevent_id: atelier-preview\n"
		"discussion_id: preview\ntitle: Design notes · sample report\nbuffer: preview\n"
		"status: ready\nturn: 1\nevent: report\n"
		"The report stays readable while its routing details tuck away. "
		"Click its header to inspect the metadata.\n\n")
	(insert (format "[[file:%s::1][Open the appearance module]]\n\n"
			(or (symbol-file 'gptel-runner-chat-mode 'defun)
                            (locate-library "gptel-runner-chat"))))
	(insert "@user\n")
	(gptel-runner-chat-mode 1)
	(set-buffer-modified-p nil)
	(goto-char (point-min))))
    (pop-to-buffer buffer)
    (gptel-runner-chat-refresh)))

(provide 'gptel-runner-chat)
;;; gptel-runner-chat.el ends here
