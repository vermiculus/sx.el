;;; sx-question-mode.el --- Creating the buffer that displays questions  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
(require 'markdown-mode)
(eval-when-compile
  (require 'rx))

(require 'sx)
(require 'sx-question)

(defgroup sx-question-mode nil
  "Customization group for sx-question-mode."
  :prefix "sx-question-mode-"
  :group 'sx)

(defgroup sx-question-mode-faces nil
  "Customization group for the faces of `sx-question-mode'."
  :prefix "sx-question-mode-"
  :group 'sx-question-mode)


;;; Displaying a question
(defvar sx-question-mode--window nil
  "Window where the content of questions is displayed.")

(defvar sx-question-mode--buffer nil
  "Buffer being used to display questions.")

(defvar sx-question-mode--data nil
  "The data of the question being displayed.")

(defun sx-question-mode--display (data &optional window)
  "Display question given by DATA on WINDOW.
If WINDOW is nil, use selected one.

Returns the question buffer."
  (with-current-buffer
      (sx-question-mode--display-buffer window)
    (sx-question-mode--erase-and-print-question data)))

(defun sx-question-mode--erase-and-print-question (data)
  "Erase contents of buffer and print question given by DATA."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sx-question-mode)
    (sx-question-mode--print-question data)
    (current-buffer)))

(defun sx-question-mode--display-buffer (window)
  "Display and return the buffer used for displaying a question.
Create `sx-question-mode--buffer' if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p sx-question-mode--buffer)
    (setq sx-question-mode--buffer
          (generate-new-buffer "*stack-question*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window sx-question-mode--buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window sx-question-mode--buffer 'visible))
   ;; Neither, so we create the window.
   (t (switch-to-buffer sx-question-mode--buffer)))
  sx-question-mode--buffer)


;;; Printing a question's content
;;;; Faces and Variables

(defface sx-question-mode-header
  '((t :inherit font-lock-variable-name-face))
  "Face used on the question headers in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-title
  '((t :height 1.3 :weight bold :inherit default))
  "Face used on the question title in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-title-comments
  '((t :height 1.1 :inherit sx-question-mode-title))
  "Face used on the question title in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-title "\n"
  "String used before the question title at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-author
  '((t :inherit font-lock-string-face))
  "Face used on the question author in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-author "\nAuthor:   "
  "String used before the question author at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-date
  '((t :inherit font-lock-string-face))
  "Face used on the question date in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-date "\nAsked on: "
  "String used before the question date at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-tags
  '((t :underline nil :inherit font-lock-function-name-face))
  "Face used on the question tags in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-author
  '((t :inherit font-lock-variable-name-face))
  "Face used for author names in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-score
  '((t))
  "Face used for the score in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-score-downvoted
  '((t :inherit (font-lock-warning-face sx-question-mode-score)))
  "Face used for downvoted score in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-score-upvoted
  '((t :weight bold
       :inherit (font-lock-function-name-face sx-question-mode-score)))
  "Face used for downvoted score in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-tags "\nTags:     "
  "String used before the question tags at the header."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-header-score "\nScore:    "
  "String used before the question score at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-content-face
  '((((background dark)) :background "#090909")
    (((background light)) :background "#f4f4f4"))
  "Face used on the question body in the question buffer.
This shouldn't have a foreground, or this will interfere with
font-locking."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-last-edit-format " (edited %s ago by %s)"
  "Format used to describe last edit date in the header.
First \"%s\" is replaced with the date and the second \"%s\" with
the editor's name."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-separator
  (concat (make-string 80 ?_) "\n")
  "Separator used between header and body."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-answer-title "Answer"
  "Title used at the start of \"Answer\" sections."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-comments-title " Comments"
  "Title used at the start of \"Comments\" sections."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-comments-format "%s: %s\n"
  "Format used to display comments.
First \"%s\" is replaced with user name.  Second \"%s\" is
replaced with the comment."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-pretty-links t
  "If non-nil, markdown links are displayed in a compact form."
  :type 'boolean
  :group 'sx-question-mode)


;;; Printing a question's content
;;;; Functions
(defun sx-question-mode--print-question (question)
  "Print a buffer describing QUESTION.
QUESTION must be a data structure returned by `json-read'."
  (setq sx-question-mode--data question)
  ;; Clear the overlays
  (mapc #'delete-overlay sx--overlays)
  (setq sx--overlays nil)
  ;; Print everything
  (sx-question-mode--print-section question)
  (sx-assoc-let question
    (mapc #'sx-question-mode--print-section .answers))
  (goto-char (point-min))
  (sx-question-mode-next-section))

(defvar sx-question-mode--section-help-echo
  (format
   (propertize "%s to hide/display content" 'face 'minibuffer-prompt)
   (propertize "RET" 'face 'font-lock-function-name-face))
  "Help echoed in the minibuffer when point is on a section.")

(defvar sx-question-mode--title-properties
  `(face sx-question-mode-title
         action sx-question-mode-hide-show-section
         help-echo ,sx-question-mode--section-help-echo
         button t
         follow-link t)
  "Title properties.")

(defun sx-question-mode--print-section (data)
  "Print a section corresponding to DATA.
DATA can represent a question or an answer."
  ;; This makes `data' accessible through `sx--data-here'.
  (sx-assoc-let data
    (sx--wrap-in-text-property
        (list 'sx--data-here data)
      (insert sx-question-mode-header-title
              (apply
               #'propertize
               ;; Questions have title
               (or .title
                   ;; Answers don't
                   sx-question-mode-answer-title)
               ;; Section level
               'sx-question-mode--section (if .title 1 2)
               ;; face, action and help-echo
               sx-question-mode--title-properties))
      ;; Sections can be hidden with overlays
      (sx--wrap-in-overlay
          '(sx-question-mode--section-content t)
        (sx-question-mode--insert-header
         ;; Author
         sx-question-mode-header-author
         (sx-question-mode--propertize-display-name .owner)
         'sx-question-mode-author
         ;; Date
         sx-question-mode-header-date
         (concat
          (sx-time-seconds-to-date .creation_date)
          (when .last_edit_date
            (format sx-question-mode-last-edit-format
                    (sx-time-since .last_edit_date)
                    (sx-question-mode--propertize-display-name .last_editor))))
         'sx-question-mode-date)
        (sx-question-mode--insert-header
         sx-question-mode-header-score
         (format "%s" .score)
         (cond
          ((eq .upvoted t) 'sx-question-mode-score-upvoted)
          ((eq .downvoted t) 'sx-question-mode-score-downvoted)
          (t 'sx-question-mode-score)))
        (when .title
          ;; Tags
          (sx-question-mode--insert-header
           sx-question-mode-header-tags
           (mapconcat #'sx-question--tag-format .tags " ")
           'sx-question-mode-tags))
        ;; Body
        (insert "\n"
                (propertize sx-question-mode-separator
                            'face 'sx-question-mode-header
                            'sx-question-mode--section 4))
        (sx--wrap-in-overlay
            '(face sx-question-mode-content-face)
          (insert "\n"
                  (sx-question-mode--fill-and-fontify
                   .body_markdown)
                  "\n"
                  (propertize sx-question-mode-separator
                              'face 'sx-question-mode-header)))))
    ;; Comments have their own `sx--data-here' property (so they can
    ;; be upvoted too).
    (when .comments
      (insert "\n"
              (apply #'propertize
                     sx-question-mode-comments-title
                     'face 'sx-question-mode-title-comments
                     'sx-question-mode--section 3
                     sx-question-mode--title-properties))
      (sx--wrap-in-overlay
          '(sx-question-mode--section-content t)
        (insert "\n")
        (sx--wrap-in-overlay
            '(face sx-question-mode-content-face)
          (mapc #'sx-question-mode--print-comment .comments))))))

(defun sx-question-mode--propertize-display-name (author)
  "Return display_name of AUTHOR with `sx-question-mode-author' face."
  (sx-assoc-let author
    (propertize .display_name
                'face 'sx-question-mode-author)))

(defun sx-question-mode--print-comment (comment-data)
  "Print the comment described by alist COMMENT-DATA.
The comment is indented, filled, and then printed according to
`sx-question-mode-comments-format'."
  (sx--wrap-in-text-property
      (list 'sx--data-here comment-data)
    (sx-assoc-let comment-data
      (insert
       (format
        sx-question-mode-comments-format
        (sx-question-mode--propertize-display-name .owner)
        (substring
         ;; We fill with three spaces at the start, so the comment is
         ;; slightly indented.
         (sx-question-mode--fill-and-fontify
          (concat "   " .body_markdown))
         ;; Then we remove the spaces from the first line, since we'll
         ;; add the username there anyway.
         3))))))

(defun sx-question-mode--insert-header (&rest args)
  "Insert propertized ARGS.
ARGS is a list of repeating values -- `header', `value', and
`face'.  `header' is given `sx-question-mode-header' as a face,
where `value' is given `face' as its face.

\(fn HEADER VALUE FACE [HEADER VALUE FACE] [HEADER VALUE FACE] ...)"
  (while args
    (insert
     (propertize (pop args) 'face 'sx-question-mode-header)
     (propertize (pop args) 'face (pop args)))))


;;;;; Font-locking the content
(defvar sx-question-mode-bullet-appearance
  (propertize (if (char-displayable-p ?•) "  •" "  *")
              'face 'markdown-list-face)
  "String to be displayed as the bullet of markdown list items.")

(defun sx-question-mode--fill-and-fontify (text)
  "Return TEXT filled according to `markdown-mode'."
  (with-temp-buffer
    (insert text)
    (markdown-mode)
    (font-lock-mode -1)
    (when sx-question-mode-bullet-appearance
      (font-lock-add-keywords ;; Bullet items.
       nil
       `((,(rx line-start (0+ blank) (group-n 1 (any "*+-")) blank)
          1 '(face nil display ,sx-question-mode-bullet-appearance) prepend))))
    (font-lock-add-keywords ;; Highlight usernames.
     nil
     `((,(rx (or blank line-start)
             (group-n 1 (and "@" (1+ (or (syntax word) (syntax symbol)))))
             symbol-end)
        1 font-lock-builtin-face)))
    ;; Everything.
    (font-lock-fontify-region (point-min) (point-max))
    ;; Compact links.
    (sx-question-mode--process-links-in-buffer)
    ;; And now the filling
    (goto-char (point-min))
    (while (null (eobp))
      ;; Don't fill pre blocks.
      (unless (sx-question-mode--move-over-pre)
        (skip-chars-forward "\r\n[:blank:]")
        (fill-paragraph)
        (forward-paragraph)))
    (buffer-string)))

(defvar sx-question-mode--link-regexp
  ;; Done at compile time.
  (rx "[" (group-n 1 (1+ (not (any "]")))) "]"
      (or (and "(" (group-n 2 (1+ (not (any ")")))) ")")
          (and "[" (group-n 3 (1+ (not (any "]")))) "]")))
  "Regexp matching markdown links.")

(defun sx-question-mode--process-links-in-buffer ()
  "Turn all markdown links in this buffer into compact format."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp sx-question-mode--link-regexp nil t)
      (let* ((text (match-string-no-properties 1))
             (url (or (match-string-no-properties 2)
                      (sx-question-mode-find-reference
                       (match-string-no-properties 3)
                       text))))
        (replace-match
         (sx-question-mode--propertize-link
          (if sx-question-mode-pretty-links
              text
            (match-string-no-properties 0))
          url)
         :fixedcase :literal nil 0)))))

(defun sx-question-mode--propertize-link (text url)
  "Return a link propertized version of string TEXT.
URL is used as 'help-echo and 'url properties."
  (propertize
   text
   ;; Mouse-over
   'help-echo   (format
                 (propertize "URL: %s, %s to visit" 'face 'minibuffer-prompt)
                 (propertize url 'face 'default)
                 (propertize "RET" 'face 'font-lock-function-name-face))
   ;; In case we need it.
   'url         url
   ;; Decoration
   'face        'link
   'mouse-face  'highlight
   ;; So RET works
   'button      t
   ;; So mouse works
   'follow-link t
   ;; What RET calls
   'action      #'sx-question-mode-follow-link))

(defun sx-question-mode-follow-link (&optional pos)
  "Follow link at POS.  If POS is nil, use `point'."
  (interactive)
  (browse-url
   (or (get-text-property (or pos (point)) 'url)
       (user-error "No url under point: %s" (or pos (point))))))

(defun sx-question-mode-find-reference (id &optional fallback-id)
  "Find url identified by reference ID in current buffer.
If ID is nil, use FALLBACK-ID instead."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (search-forward-regexp
             (format (rx line-start (0+ blank) "[%s]:" (0+ blank)
                         (group-n 1 (1+ (not blank))))
                     (or id fallback-id))
             nil t)
        (match-string-no-properties 1)))))

(defun sx-question-mode--move-over-pre ()
  "Non-nil if paragraph at point can be filled."
  (markdown-match-pre-blocks
   (save-excursion
     (skip-chars-forward "\r\n[:blank:]")
     (point))))


;;; Movement commands
;; Sections are headers placed above a question's content or an
;; answer's content, or above the list of comments. They are
;; identified with the `sx-question-mode--section' text property.
;; To move between sections, just search for the property. The value
;; of the text-property is the depth of the section (1 for contents, 2
;; for comments).
(defcustom sx-question-mode-recenter-line 1
  "Screen line to which we recenter after moving between sections.
This is used as an argument to `recenter', only used if the end
of section is outside the window.

If nil, no recentering is performed."
  :type '(choice (const :tag "Don't recenter" nil)
                 integer)
  :group 'sx-question-mode)

(defun sx-question-mode-next-section (&optional n)
  "Move down to next section (question or answer) of this buffer.
Prefix argument N moves N sections down or up."
  (interactive "p")
  (let ((count (if n (abs n) 1)))
    (while (> count 0)
      ;; This will either move us to the next section, or move out of
      ;; the current one.
      (unless (sx-question-mode--goto-property-change 'section n)
        ;; If all we did was move out the current one, then move again
        ;; and we're guaranteed to reach the next section.
        (sx-question-mode--goto-property-change 'section n))
      (let ((ov (car-safe (sx-question-mode--section-overlays-at (point)))))
        (unless (and (overlayp ov)
                     (overlay-get ov 'invisible))
          (cl-decf count)))))
  (when (equal (selected-window) (get-buffer-window))
    (when sx-question-mode-recenter-line
      (let ((ov (car-safe (sx-question-mode--section-overlays-at (line-end-position)))))
        (when (and (overlayp ov) (> (overlay-end ov) (window-end)))
          (recenter sx-question-mode-recenter-line))))
    (sx-message-help-echo)))

(defun sx-question-mode-previous-section (&optional n)
  "Move down to previous section (question or answer) of this buffer.
Prefix argument moves N sections up or down."
  (interactive "p")
  (sx-question-mode-next-section (- (or n 1))))

(defun sx-question-mode--goto-property-change (prop &optional direction)
  "Move forward to the next change of text-property sx-question-mode--PROP.
Return the new value of PROP at point.

If DIRECTION is negative, move backwards instead."
  (let ((prop (intern (format "sx-question-mode--%s" prop)))
        (func (if (and (numberp direction)
                       (< direction 0))
                  #'previous-single-property-change
                #'next-single-property-change))
        (limit (if (and (numberp direction)
                        (< direction 0))
                   (point-min) (point-max))))
    (goto-char (funcall func (point) prop nil limit))
    (get-text-property (point) prop)))

(defun sx-question-mode-hide-show-section (&optional _)
  "Hide or show section under point.
Optional argument _ is for `push-button'."
  (interactive)
  (let ((ov (car (or (sx-question-mode--section-overlays-at (point))
                     (sx-question-mode--section-overlays-at
                      (line-end-position))))))
    (goto-char (overlay-start ov))
    (forward-line 0)
    (overlay-put
     ov 'invisible
     (null (overlay-get ov 'invisible)))))

(defun sx-question-mode--section-overlays-at (pos)
  "Return a list of `sx-question-mode--section-content' overlays at POS."
  (cl-remove-if (lambda (x) (null (overlay-get x 'sx-question-mode--section-content)))
                (overlays-at pos)))


;;; Major-mode
(define-derived-mode sx-question-mode special-mode "Question"
  "Major mode to display and navigate a question and its answers.
Letters do not insert themselves; instead, they are commands.

\\<sx-question-mode>
\\{sx-question-mode}"
  ;; Determine how to close this window.
  (unless (window-parameter nil 'quit-restore)
    (set-window-parameter
     nil 'quit-restore
     `(other window nil ,(current-buffer))))
  ;; We call font-lock-region manually. See `sx-question-mode--fill-and-fontify'
  (font-lock-mode -1)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t)
  (remove-hook 'window-configuration-change-hook
               'markdown-fontify-buffer-wiki-links t)
  (read-only-mode))

(mapc
 (lambda (x) (define-key sx-question-mode-map
          (car x) (cadr x)))
 `(("n" sx-question-mode-next-section)
   ("p" sx-question-mode-previous-section)
   ("g" sx-question-mode-refresh)
   ("c" sx-comment)
   ("v" sx-visit)
   ("u" sx-toggle-upvote)
   ("d" sx-toggle-downvote)
   ("q" quit-window)
   (" " scroll-up-command)
   (,(kbd "S-SPC") scroll-down-command)
   ([backspace] scroll-down-command)
   ([tab] forward-button)
   (,(kbd "<S-iso-lefttab>") backward-button)
   (,(kbd "<S-tab>") backward-button)
   (,(kbd "<backtab>") backward-button)
   ([return] push-button)))

(defun sx-question-mode-refresh (&optional no-update)
  "Refresh currently displayed question.
Queries the API for any changes to the question or its answers or
comments, and redisplays it.

With non-nil prefix argument NO-UPDATE, just redisplay, don't
query the api."
  (interactive "P")
  (sx-question-mode--ensure-mode)
  (let ((point (point))
        (line (count-screen-lines
               (window-start) (point))))
    (sx-question-mode--erase-and-print-question
     (if no-update
         sx-question-mode--data
       (sx-assoc-let sx-question-mode--data
         (sx-question-get-question .site .question_id))))
    (goto-char point)
    (when (equal (selected-window)
                 (get-buffer-window (current-buffer)))
      (recenter line))))

(defun sx-question-mode--ensure-mode ()
  "Ensures we are in question mode, erroring otherwise."
  (unless (derived-mode-p 'sx-question-mode)
    (error "Not in `sx-question-mode'")))

(provide 'sx-question-mode)
;;; sx-question-mode.el ends here
