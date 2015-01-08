;;; sx-question-print.el --- populating the question-mode buffer with content  -*- lexical-binding: t; -*-

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
(require 'sx-button)
(require 'sx)
(require 'sx-question)
(require 'sx-babel)

(defgroup sx-question-mode nil
  "Customization group for sx-question-mode."
  :prefix "sx-question-mode-"
  :tag "SX Question Mode"
  :group 'sx)

(defgroup sx-question-mode-faces nil
  "Customization group for the faces of `sx-question-mode'."
  :prefix "sx-question-mode-"
  :tag "SX Question Mode Faces"
  :group 'sx-question-mode)


;;; Faces and Variables
(defcustom sx-question-mode-deleted-user
  '((display_name . "(deleted user)"))
  "The structure used to represent a deleted account."
  :type '(alist :options ((display_name string)))
  :group 'sx-question-mode)

(defface sx-question-mode-header
  '((t :inherit font-lock-variable-name-face))
  "Face used on the question headers in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-title
  '((t :weight bold :inherit default))
  "Face used on the question title in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-title-comments
  '((t :inherit sx-question-mode-title))
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


;;; Functions
;;;; Printing the general structure
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
  (insert "\n\n                       ")
  (insert-text-button "Write an Answer" :type 'sx-button-answer)
  ;; Go up
  (goto-char (point-min))
  (sx-question-mode-next-section))

(defun sx-question-mode--print-section (data)
  "Print a section corresponding to DATA.
DATA can represent a question or an answer."
  ;; This makes `data' accessible through `sx--data-here'.
  (sx-assoc-let data
    (sx--wrap-in-overlay
        (list 'sx--data-here data)
      (insert sx-question-mode-header-title)
      (insert-text-button
       ;; Questions have title, Answers don't
       (or .title sx-question-mode-answer-title)
       ;; Section level
       'sx-question-mode--section (if .title 1 2)
       'sx-button-copy .share_link
       :type 'sx-question-mode-title)
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
              (sx-question-mode--propertize-display-name
               (or .last_editor sx-question-mode-deleted-user)))))
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
                            'face 'sx-question-mode-header))
        (sx--wrap-in-overlay
            '(face sx-question-mode-content-face)
          (insert "\n"
                  (sx-question-mode--fill-and-fontify
                   .body_markdown)
                  "\n"
                  (propertize sx-question-mode-separator
                              'face 'sx-question-mode-header)))
        ;; Comments have their own `sx--data-here' property (so they can
        ;; be upvoted too).
        (when .comments
          (insert "\n")
          (insert-text-button
           sx-question-mode-comments-title
           'face 'sx-question-mode-title-comments
           'sx-question-mode--section 3
           'sx-button-copy .share_link
           :type 'sx-question-mode-title)
          (sx--wrap-in-overlay
              '(sx-question-mode--section-content t)
            (insert "\n")
            (sx--wrap-in-overlay
                '(face sx-question-mode-content-face)
              (mapc #'sx-question-mode--print-comment .comments))
            ;; If there are comments, we want part of this margin to go
            ;; inside them, so the button get's placed beside the
            ;; "Comments" header when you hide them.
            (insert "         ")))
        ;; If there are no comments, we have to add this margin here.
        (unless .comments
          (insert "         "))
        (insert "               ")
        ;; This is where the "add a comment" button is printed.
        (insert-text-button "Add a Comment"
                            :type 'sx-button-comment)
        (insert "\n")))))

(defun sx-question-mode--propertize-display-name (author)
  "Return display_name of AUTHOR with `sx-question-mode-author' face."
  (sx-assoc-let author
    (propertize (or .display_name "??")
                'face 'sx-question-mode-author)))

(defun sx-question-mode--print-comment (comment-data)
  "Print the comment described by alist COMMENT-DATA.
The comment is indented, filled, and then printed according to
`sx-question-mode-comments-format'."
  (sx--wrap-in-overlay
      (list 'sx--data-here comment-data)
    (sx-assoc-let comment-data
      (when (and (numberp .score) (> .score 0))
        (insert (number-to-string .score)
                (if (eq .upvoted t) "^" "")
                " "))
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


;;;; Printing and Font-locking the content (body)
(defvar sx-question-mode-bullet-appearance
  (propertize (if (char-displayable-p ?•) "  •" "  *")
              'face 'markdown-list-face)
  "String to be displayed as the bullet of markdown list items.")

(defvar sx-question-mode--reference-regexp
  (rx line-start (0+ blank) "[%s]:" (0+ blank)
      (group-n 1 (1+ (not blank))))
  "Regexp used to find the url of labeled links.
E.g.:
   [1]: https://...")

(defvar sx-question-mode--link-regexp
  ;; Done at compile time.
  (rx "[" (group-n 1 (1+ (not (any "]")))) "]"
      (or (and "(" (group-n 2 (1+ (not (any ")")))) ")")
          (and "[" (group-n 3 (1+ (not (any "]")))) "]")))
  "Regexp matching markdown links.")

(defun sx-question-mode--fill-and-fontify (text)
  "Return TEXT filled according to `markdown-mode'."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (markdown-mode))
    (font-lock-mode -1)
    (when sx-question-mode-bullet-appearance
      (font-lock-add-keywords ;; Bullet items.
       nil
       `((,(rx line-start (0+ blank) (group-n 1 (any "*+-")) blank)
          1 '(face nil display ,sx-question-mode-bullet-appearance) prepend))))
    (font-lock-add-keywords ;; Highlight usernames.
     nil
     `((,(rx (or blank line-start)
             (group-n 1 (and "@" (1+ (not space))))
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
      (unless (sx-question-mode--dont-fill-here)
        (let ((beg (point)))
          (skip-chars-forward "\r\n[:blank:]")
          (forward-paragraph)
          (fill-region beg (point)))))
    (replace-regexp-in-string "[[:blank:]]+\\'" "" (buffer-string))))


;;; Handling links
(defun sx-question-mode--process-links-in-buffer ()
  "Turn all markdown links in this buffer into compact format."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp sx-question-mode--link-regexp nil t)
      (let* ((text (match-string-no-properties 1))
             (url (or (match-string-no-properties 2)
                      (sx-question-mode-find-reference
                       (match-string-no-properties 3)
                       text)))
             (full-text (match-string-no-properties 0)))
        (when (stringp url)
          (replace-match "")
          (sx-question-mode--insert-link
           (if sx-question-mode-pretty-links text full-text)
           url))))))

(defun sx-question-mode--insert-link (text url)
  "Return a link propertized version of string TEXT.
URL is used as 'help-echo and 'url properties."
  (insert-text-button
   text
   ;; Mouse-over
   'help-echo
   (format sx-button--link-help-echo
     (propertize (sx--shorten-url url)
                 'face 'font-lock-function-name-face))
   ;; For visiting and stuff.
   'sx-button-url url
   'sx-button-copy url
   :type 'sx-button-link))

(defun sx-question-mode-find-reference (id &optional fallback-id)
  "Find url identified by reference ID in current buffer.
If ID is nil, use FALLBACK-ID instead."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (search-forward-regexp
             (format sx-question-mode--reference-regexp
               (or id fallback-id))
             nil t)
        (match-string-no-properties 1)))))


;;; Things we don't fill
(defun sx-question-mode--dont-fill-here ()
  "If text shouldn't be filled here, return t and skip over it."
  (catch 'sx-question-mode-done
    (let ((before (point)))
      (skip-chars-forward "\r\n[:blank:]")
      (let ((first-non-blank (point)))
        (dolist (it '(sx-question-mode--skip-and-fontify-pre
                      sx-question-mode--skip-headline
                      sx-question-mode--skip-references
                      sx-question-mode--skip-comments))
          ;; If something worked, keep point where it is and return t.
          (if (funcall it) (throw 'sx-question-mode-done t)
            ;; Before calling each new function. Go back to the first
            ;; non-blank char.
            (goto-char first-non-blank)))
        ;; If nothing matched, go back to the very beginning.
        (goto-char before)
        ;; And return nil
        nil))))

(defun sx-question-mode--skip-and-fontify-pre ()
  "If there's a pre block ahead, handle it, skip it and return t.
Handling means to turn it into a button and remove erroneous
font-locking."
  (let ((beg (line-beginning-position)))
    ;; To identify code-blocks we need to be at start of line.
    (goto-char beg)
    (when (markdown-match-pre-blocks (line-end-position))
      (sx-babel--make-pre-button beg (point))
      t)))

(defun sx-question-mode--skip-comments ()
  "If there's an html comment ahead, skip it and return t."
  ;; @TODO: Handle the comment.
  ;; "Handling means to store any relevant metadata it might be holding."
  (markdown-match-comments (line-end-position)))

(defun sx-question-mode--skip-headline ()
  "If there's a headline ahead, skip it and return non-nil."
  (when (or (looking-at-p "^#+ ")
            (progn (forward-line 1) (looking-at-p "===\\|---")))
    ;; Returns non-nil.
    (forward-line 1)))

(defun sx-question-mode--skip-references ()
  "If there's a reference ahead, skip it and return non-nil."
  (while (looking-at-p (format sx-question-mode--reference-regexp ".+"))
    ;; Returns non-nil
    (forward-line 1)))

(provide 'sx-question-print)
;;; sx-question-print.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
