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
(require 'sx-user)

(defvar sx-question-mode--data)

(defgroup sx-question-mode nil
  "Customization group for sx-question-mode."
  :prefix "sx-question-mode-"
  :tag "SX Question Mode"
  :group 'sx)

(defgroup sx-question-mode-faces '((sx-user custom-group))
  "Customization group for the faces of `sx-question-mode'.
Some faces of this mode might be defined in the `sx-user' group."
  :prefix "sx-question-mode-"
  :tag "SX Question Mode Faces"
  :group 'sx-question-mode)


;;; Faces and Variables
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

(defcustom sx-question-mode-header-author-format "\nAuthor:    %d %r"
  "String used to display the question author at the header.
% constructs have special meaning here.  See `sx-user--format'."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-date
  '((t :inherit font-lock-string-face))
  "Face used on the question date in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-date "\nPosted on: "
  "String used before the question date at the header."
  :type 'string
  :group 'sx-question-mode)

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

(defcustom sx-question-mode-header-tags "\nTags:      "
  "String used before the question tags at the header."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-header-score "\nScore:     "
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
  (concat (propertize (make-string 72 ?\s)
                      'face '(underline sx-question-mode-header))
          "\n")
  "Separator used between header and body."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-answer-title "Answer"
  "Title used at the start of \"Answer\" sections."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-accepted
  '((((background dark)) :foreground "LimeGreen"
     :height 1.3 :inherit sx-question-mode-title)
    (((background light)) :foreground "ForestGreen"
     :height 1.3 :inherit sx-question-mode-title))
  "Face used for accepted answers in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-closed
  '((t :box 2 :inherit font-lock-warning-face))
  "Face used for closed question header in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-closed-reason
  `((t :box (:line-width 2 :color ,(face-attribute 'sx-question-mode-closed
                                                   :foreground nil t))
       :inherit sx-question-mode-title))
  "Face used for closed question header in the question buffer.
Aesthetically, it's important that the color of this face's :box
attribute match the color of the face `sx-question-mode-closed'."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-answer-accepted-title "Accepted Answer"
  "Title used at the start of accepted \"Answer\" section."
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

(defconst sx-question-mode--sort-methods
  (let ((methods
         '(("Higher-scoring" . sx-answer-higher-score-p)
           ("Newer"          . sx-answer-newer-p)
           ("More active"    . sx-answer-more-active-p))))
    (append (mapcar (lambda (x) (cons (concat (car x) " first") (cdr x)))
                    methods)
            (mapcar (lambda (x) (cons (concat (car x) " last")
                                      (sx--invert-predicate (cdr x))))
                    methods))))

(defcustom sx-question-mode-answer-sort-function
  #'sx-answer-higher-score-p
  "Function used to sort answers in the question buffer."
  :type
  (cons 'choice
        (mapcar (lambda (x) `(const :tag ,(car x) ,(cdr x)))
                sx-question-mode--sort-methods))
  :group 'sx-question-mode)

(defcustom sx-question-mode-use-images (image-type-available-p 'imagemagick)
  "Non-nil if SX should download and display images.
By default, this is `t' if the `imagemagick' image type is
available (checked with `image-type-available-p').  If this image
type is not available, images won't work."
  :type 'boolean
  :group 'sx-question-mode)

(defcustom sx-question-mode-image-max-width 550
  "Maximum width, in pixels, of images in the question buffer."
  :type 'integer
  :group 'sx-question-mode)


;;; Functions
;;;; Printing the general structure
(defconst sx-question-mode--closed-mode-line-string
  '(:propertize "  [CLOSED]  " face font-lock-warning-face)
  "String indicating closed questions in the mode-line.")

(defun sx-question-mode--print-question (question)
  "Print a buffer describing QUESTION.
QUESTION must be a data structure returned by `json-read'."
  (when (sx--deleted-p question)
    (sx-user-error "This is a deleted question"))
  (setq sx-question-mode--data question)
  ;; Clear the overlays
  (mapc #'delete-overlay sx--overlays)
  (setq sx--overlays nil)
  ;; Print everything
  (sx-assoc-let question
    (when .closed_reason
      (add-to-list 'mode-line-format sx-question-mode--closed-mode-line-string)
      (sx-question-mode--print-close-reason .closed_reason .closed_date .closed_details))
    (sx-question-mode--print-section question)
    (mapc #'sx-question-mode--print-section
          (cl-remove-if
           #'sx--deleted-p
           (cl-sort .answers sx-question-mode-answer-sort-function))))
  (insert "\n\n                       ")
  (insert-text-button "Write an Answer" :type 'sx-button-answer)
  ;; Go up
  (goto-char (point-min))
  (sx-question-mode-next-section))

(defun sx-question-mode--print-close-reason (reason date &optional details)
  "Print a header explaining REASON and DATE.
DATE is an integer.

DETAILS, when given is an alist further describing the close."
  (let ((l (point)))
    (let-alist details
      (insert "\n    "
              (propertize (format " %s as %s, %s ago. "
                                  (if .on_hold "Put on hold" "Closed")
                                  reason
                                  (sx-time-since date))
                          'face 'sx-question-mode-closed)
              "\n")
      (when .description
        (insert (replace-regexp-in-string "<[^>]+>" "" .description)
                "\n")))
    (save-excursion
      (goto-char l)
      (search-forward " as " nil 'noerror)
      (setq l (point))
      (skip-chars-forward "^,")
      (let ((ov (make-overlay l (point))))
        (overlay-put ov 'face 'sx-question-mode-closed-reason)
        (push ov sx--overlays)))))

(defun sx-question-mode--maybe-print-accept-button ()
  "Print accept button if you own this question."
  (when (sx-assoc-let sx-question-mode--data
          (ignore-errors
            (= .owner.user_id
               (cdr (assq 'user_id (sx-network-user .site_par))))))
    (insert "     ")
    (insert-text-button "Accept" :type 'sx-button-accept)))

(defun sx-question-mode--print-section (data)
  "Print a section corresponding to DATA.
DATA can represent a question or an answer."
  ;; This makes `data' accessible through `sx--data-here'.
  (sx--wrap-in-overlay
      (list 'sx--data-here data)
    (sx-assoc-let data
      (insert sx-question-mode-header-title)
      (insert-text-button
       ;; Questions have title, Answers don't
       (cond (.title)
             (.is_accepted sx-question-mode-answer-accepted-title)
             (t sx-question-mode-answer-title))
       ;; Section level
       'sx-question-mode--section (if .title 1 2)
       'sx-button-copy .share_link
       'face (if .is_accepted 'sx-question-mode-accepted
               'sx-question-mode-title)
       :type 'sx-question-mode-title)
      (when (not (or .title .is_accepted))
        (sx-question-mode--maybe-print-accept-button))

      ;; Sections can be hidden with overlays
      (sx--wrap-in-overlay
          '(sx-question-mode--section-content t)

        ;; Author
        (insert
         (sx-user--format
          (propertize sx-question-mode-header-author-format
                      'face 'sx-question-mode-header)
          .owner))

        ;; Date
        (sx-question-mode--insert-header
         sx-question-mode-header-date
         (concat
          (sx-time-seconds-to-date .creation_date)
          (when .last_edit_date
            (format sx-question-mode-last-edit-format
                    (sx-time-since .last_edit_date)
                    (sx-user--format "%d" .last_editor))))
         'sx-question-mode-date)

        ;; Score and upvoted/downvoted status.
        (sx-question-mode--insert-header
         sx-question-mode-header-score
         (format "%s%s" .score
                 (cond (.upvoted "↑") (.downvoted "↓") (t "")))
         (cond (.upvoted 'sx-question-mode-score-upvoted)
               (.downvoted 'sx-question-mode-score-downvoted)
               (t 'sx-question-mode-score)))

        ;; Tags
        (when .title
          ;; Tags
          (sx-question-mode--insert-header
           sx-question-mode-header-tags
           (sx-tag--format-tags .tags .site_par)
           nil))
        ;; Body
        (insert "\n" sx-question-mode-separator)
        (sx--wrap-in-overlay
            '(face sx-question-mode-content-face)
          (insert "\n")
          (sx-question-mode--insert-markdown .body_markdown)
          (insert "\n" sx-question-mode-separator))
        ;; Clean up commments manually deleted.  The `append' call is
        ;; to ensure `comments' is a list and not a vector.
        (let ((comments (cl-remove-if #'sx--deleted-p .comments)))
          (when comments
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
                ;; Comments have their own `sx--data-here' property (so they can
                ;; be upvoted too).
                (mapc #'sx-question-mode--print-comment comments))
              ;; If there are comments, we want part of this margin to go
              ;; inside them, so the button get's placed beside the
              ;; "Comments" header when you hide them.
              (insert "         ")))
          ;; If there are no comments, we have to add this margin here.
          (unless comments
            (insert "         ")))
        (insert "               ")
        ;; This is where the "add a comment" button is printed.
        (insert-text-button "Add a Comment"
                            :type 'sx-button-comment)
        (insert "\n")))))

(defun sx-question-mode--print-comment (comment-data)
  "Print the comment described by alist COMMENT-DATA.
The comment is indented, filled, and then printed according to
`sx-question-mode-comments-format'."
  (sx--wrap-in-overlay
      (list 'sx--data-here comment-data)
    (sx-assoc-let comment-data
      (when (and (numberp .score) (> .score 0))
        (insert (number-to-string .score)
                (if .upvoted "^" "")
                " "))
      (insert
       (format sx-question-mode-comments-format
               (sx-user--format "%d" .owner)
               (substring
                ;; We use temp buffer, so that image overlays don't get
                ;; inserted with the comment.
                (with-temp-buffer
                  ;; We fill with three spaces at the start, so the comment is
                  ;; slightly indented.
                  (sx-question-mode--insert-markdown (concat "   " (sx--squash-whitespace .body_markdown)))
                  (buffer-string))
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
     (let ((header (pop args))
           (face (pop args)))
       (if face (propertize header 'face face)
         header)))))


;;;; Printing and Font-locking the content (body)
(defvar sx-question-mode-bullet-appearance
  (propertize (if (char-displayable-p ?•) "•" "*")
              'face 'markdown-list-face)
  "String to be displayed as the bullet of markdown list items.")

(defconst sx-question-mode--reference-regexp
  (rx line-start (0+ blank) "[%s]:" (0+ blank)
      (group-n 1 (1+ (not (any blank "\n\r")))))
  "Regexp used to find the url of labeled links.
E.g.:
   [1]: https://...")

(defconst sx-question-mode--link-regexp
  ;; Done at compile time.
  (rx (or (and "[" (optional (group-n 6 "meta-")) "tag:"
               (group-n 5 (+ (not (any " ]")))) "]")
          (and (opt "!") "[" (group-n 1 (1+ (not (any "[]")))) "]"
               (or (and "(" (group-n 2 (1+ (not (any ")")))) ")")
                   (and "[" (group-n 3 (1+ (not (any "]")))) "]")))
          (group-n 4 (and "http" (opt "s") "://"
                          (>= 2 (any lower numeric "_%"))
                          "."
                          (>= 2 (any lower numeric "_%"))
                          (* (any lower numeric "-/._%&#?=;"))))))
  "Regexp matching markdown links.")

(defun sx-question-mode--process-line-breaks (beg end-marker)
  "Process Markdown line breaks between BEG and END-MARKER.
Double space at the end of a line becomes an invisible \"\\n\".
Consecutive blank lines beyond the first are consensed.
Assumes `marker-insertion-type' of END-MARKER is t."
  (goto-char beg)
  (while (search-forward-regexp
          (rx line-start (* blank) "\n"
              (group-n 1 (+ (any blank "\n"))))
          end-marker 'noerror)
    ;; An invisible newline ensures the previous text
    ;; will get filled as a separate paragraph.
    (replace-match "" nil nil nil 1))
  (goto-char beg)
  (while (search-forward-regexp "  $" end-marker 'noerror)
    ;; An invisible newline ensures the previous text
    ;; will get filled as a separate paragraph.
    (replace-match (propertize "\n" 'invisible t))))

(defun sx-question-mode--process-markdown-in-region (beg end)
  "Process Markdown text between BEG and END.
This does not do Markdown font-locking.  Instead, it fills text,
propertizes links, inserts images, cleans up html comments, and
font-locks code-blocks according to mode."
  ;; Paragraph filling
  (let ((paragraph-start
         "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.[ \t]\\|[ \t]*: ")
        (paragraph-separate "\\(?:[ \t\f]*\\|.*  \\)$")
        (adaptive-fill-first-line-regexp "\\`[ \t]*>[ \t]*?\\'")
        (adaptive-fill-function #'markdown-adaptive-fill-function)) 
    (save-restriction
      (narrow-to-region beg end)
      ;; html tags can span many paragraphs, so we handle them
      ;; globally first.
      (sx-question-mode--process-html-tags (point-min) (copy-marker (point-max)))
      ;; And now the filling and other handlings.
      (goto-char (point-min))
      (while (null (eobp))
        ;; Don't fill pre blocks.
        (unless (sx-question-mode--dont-fill-here)
          (let ((beg (point)))
            (skip-chars-forward "\r\n[:blank:]")
            (forward-paragraph)
            (let ((end (point-marker)))
              (set-marker-insertion-type end t)
              ;; Turn markdown linebreaks into their final form
              (sx-question-mode--process-line-breaks beg end)
              ;; Compactify links by paragraph, so we don't linkify
              ;; inside code-blocks. This will still linkify inside
              ;; code tags, unfortunately.
              (sx-question-mode--process-links beg end)
              ;; Filling is done after all of the above, since those
              ;; steps change the length of text.
              (fill-region beg end)
              (goto-char end)))))
      (goto-char (point-max)))))

(defconst sx-question-mode-hr
  (propertize (make-string 72 ?―)
              'face 'markdown-header-rule-face))

(defun sx-question-mode--insert-markdown (text)
  "Return TEXT fontified according to `markdown-mode'."
  (let ((beg (point)))
    (insert
     ;; Font-locking needs to be done in a temp buffer, because it
     ;; affects the entire buffer even if we narrow.
     (with-temp-buffer
       (insert text)
       ;; Trim whitespace
       (goto-char (point-max))
       (skip-chars-backward "\r\n[:blank:]")
       (delete-region (point) (point-max))
       (goto-char (point-min))
       (skip-chars-forward "\r\n[:blank:]")
       (forward-line 0)
       (delete-region (point-min) (point))
       ;; Font lock
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
           1 font-lock-builtin-face)
          ("^---+$" 0 '(face nil display ,sx-question-mode-hr))))
       ;; Everything.
       (font-lock-fontify-region (point-min) (point-max))
       (replace-regexp-in-string "[[:blank:]]+\\'" "" (buffer-string))))
    ;; This part can and should be done in place, this way it can
    ;; create overlays.
    (sx-question-mode--process-markdown-in-region beg (point))))


;;; HTML tags
(defconst sx-question-mode--html-tag-regexp
  (rx "<" (group-n 1 "%s") (* (not (any ">"))) ">"))

(defface sx-question-mode-sub-sup-tag
  '((t :height 0.7))
  "Face used on <sub> and <sup> tags."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-kbd-tag
  '((((background dark))
     :height 0.9
     :weight semi-bold
     :box (:line-width 3 :style released-button :color "gray30"))
    (((background light))
     :height 0.9
     :weight semi-bold
     :box (:line-width 3 :style released-button :color "gray70")))
  "Face used on <kbd> tags."
  :group 'sx-question-mode-faces)

(defun sx-question-mode--inside-code-p ()
  "Return non-nil if point is inside code.
This can be inline Markdown code or a Markdown code-block."
  (save-match-data
    (or (markdown-code-at-point-p)
        (save-excursion
          (sx-question-mode--skip-and-fontify-pre 'dont-fontify)))))

(defun sx-question-mode--standalone-tag-p (string)
  "Return non-nil if STRING ends in \"/>\"."
  (string-match "/[[:blank:]]*>\\'" string))

(defun sx-question-mode--next-tag (tag &optional closing end)
  "Move point to the next occurrence of html TAG, or return nil.
Don't move past END.
If CLOSING is non-nil, find a closing tag."
  (search-forward-regexp
   (format sx-question-mode--html-tag-regexp
           (if closing
               (concat "/[[:blank:]]*" tag)
             tag))
   end 'noerror))

(defun sx-question-mode--process-html-tags (beg end-marker)
  "Hide all html tags between BEG and END and possibly interpret them.
END-MARKER should be a marker."
  ;; This code understands nested html, but not if the same tag is
  ;; nested in itself (e.g., <kbd><kbd></kbd></kbd>).
  (set-marker-insertion-type end-marker t)
  (goto-char beg)
  (while (sx-question-mode--next-tag "[[:alpha:]]+" nil end-marker)
    (unless (sx-question-mode--inside-code-p)
      (let ((tag (match-string 1))
            (full (match-string 0))
            (l   (match-beginning 0)))
        (replace-match "")
        (pcase tag
          (`"hr"
           (unless (looking-at-p "^") (insert "\n"))
           (insert (propertize "---" 'display sx-question-mode-hr))
           (unless (eq (char-after) ?\n) (insert "\n")))
          (`"br" (insert "\n  ")))
        (when (and (not (sx-question-mode--standalone-tag-p full))
                   (sx-question-mode--next-tag tag 'closing))
          (let ((r (copy-marker (match-beginning 0))))
            ;; The code tag is special, because it quotes everything inside.
            (if (string= tag "code")
                (progn (replace-match "`")
                       (save-excursion (goto-char l) (insert "`")))
              (replace-match "")
              ;; Handle stuff between the two tags.
              (save-match-data (sx-question-mode--process-html-tags l r))
              (pcase tag
                (`"kbd"
                 (add-text-properties l r '(face sx-question-mode-kbd-tag))
                 (when (looking-at-p
                        (format sx-question-mode--html-tag-regexp "kbd"))
                   (insert " ")))
                (`"sub"
                 (add-text-properties
                  l r '(face sx-question-mode-sub-sup-tag display (raise -0.3))))
                (`"sup"
                 (add-text-properties
                  l r '(face sx-question-mode-sub-sup-tag display (raise +0.3))))))))))))


;;; Handling links
(defun sx-question-mode--process-links (beg end-marker)
  "Turn all markdown links between BEG and ENG into compact format.
Image links are downloaded and displayed, if
`sx-question-mode-use-images' is non-nil.
Assumes `marker-insertion-type' of END-MARKER is t."
  (goto-char beg)
  (while (search-forward-regexp sx-question-mode--link-regexp end-marker t)
    ;; Tags are tag-buttons.
    (let ((tag (match-string-no-properties 5)))
      (if (and tag (> (length tag) 0))
          (progn (replace-match "")
                 (sx-tag--insert tag))
        ;; Other links are link-buttons.
        (let* ((text (match-string-no-properties 1))
               (url (or (match-string-no-properties 2)
                        (match-string-no-properties 4)
                        (sx-question-mode-find-reference
                         (match-string-no-properties 3)
                         text)))
               (full-text (match-string-no-properties 0))
               (image-p (and sx-question-mode-use-images
                             (eq ?! (elt full-text 0)))))
          (when (stringp url)
            (replace-match "")
            (sx-question-mode--insert-link
             (cond (image-p (sx-question-mode--create-image url))
                   ((and sx-question-mode-pretty-links text))
                   ((not text) (sx--shorten-url url))
                   (t full-text))
             url)))))))

(defun sx-question-mode--create-image (url)
  "Get and create an image from URL and insert it at POINT.
The image will take the place of the character at POINT.
Its size is bound by `sx-question-mode-image-max-width' and
`window-body-width'."
  (let* ((ov (make-overlay (point) (point) (current-buffer) t nil))
         (callback
          (lambda (data)
            (let* ((image (create-image data 'imagemagick t))
                   (image-width (car (image-size image 'pixels))))
              (overlay-put
               ov 'display
               (append image
                       (list :width (min sx-question-mode-image-max-width
                                         (window-body-width nil 'pixel)
                                         image-width))))))))
    (sx-request-get-url url callback)
    (overlay-put ov 'face 'default)
    ov))

(defun sx-question-mode--insert-link (text url)
  "Return a link propertized version of TEXT-OR-IMAGE.
URL is used as 'help-echo and 'url properties."
  ;; Try to handle an image/link inside another link.
  (when (eq (char-before) ?\[)
    (insert "a")
    (forward-char -2)
    (if (looking-at sx-question-mode--link-regexp)
        (progn (setq url (or (match-string-no-properties 2)
                             (match-string-no-properties 4)
                             (sx-question-mode-find-reference
                              (match-string-no-properties 3)
                              (if (stringp text) text "¶"))
                             url))
               (replace-match ""))
      (forward-char 1)
      (delete-char 1)))
  (unless (stringp text)
    ;; Images need to be at the start of a line.
    (unless (looking-at-p "^") (insert "\n"))
    ;; And need an empty line above so they don't get wrapped into
    ;; text when we do filling.
    (insert (propertize "\n" 'display "")))
  ;; Insert the link button.
  (insert-text-button (if (stringp text) text "¶")
                      ;; Mouse-over
                      'help-echo
                      (format sx-button--link-help-echo
                              ;; If TEXT is a shortened url, we don't shorten URL.
                              (propertize (if (and (stringp text)
                                                   (string-match "^https?:" text))
                                              url (sx--shorten-url url))
                                          'face 'font-lock-function-name-face))
                      ;; For visiting and stuff.
                      'sx-button-url url
                      'sx-button-copy url
                      :type 'sx-button-link)
  ;; Images need to be at the end of a line too.
  (unless (stringp text)
    (move-overlay text (1- (point)) (point) (current-buffer))
    (insert (propertize "\n\n" 'display "\n"))))

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

(defun sx-question-mode--skip-and-fontify-pre (&optional dont-fontify)
  "If there's a pre block ahead, handle it, skip it and return t.
Handling means to turn it into a button and remove erroneous
font-locking.

If DONT-FONTIFY is non-nil, just return the result and possibly
move point, don't create the code-block button."
  (let ((beg (line-beginning-position)))
    ;; To identify code-blocks we need to be at start of line.
    (goto-char beg)
    (when (fboundp 'markdown-syntax-propertize)
      (markdown-syntax-propertize (point) (point-max)))
    (when (markdown-match-pre-blocks (line-end-position))
      (unless dont-fontify
        (sx-babel--make-pre-button beg (point)))
      t)))

(defun sx-question-mode--skip-comments ()
  "If there's an html comment ahead, skip it and return t."
  ;; @TODO: Handle the comment.
  ;; "Handling means to store any relevant metadata it might be holding."
  (let ((end (save-excursion
               (when (markdown-match-comments (line-end-position))
                 (point)))))
    (when end
      (delete-region (point) end)
      (skip-chars-backward "[:blank:]")
      (when (looking-at "^[:blank:]*\n")
        (replace-match ""))
      t)))

(defun sx-question-mode--skip-headline ()
  "If there's a headline ahead, skip it and return non-nil."
  (when (or (looking-at-p "^#+ ")
            (progn (forward-line 1) (looking-at-p "===\\|---")))
    ;; Returns non-nil.
    (forward-line 1)))

(defun sx-question-mode--skip-references ()
  "If there's a reference ahead, skip it and return non-nil."
  (forward-line 0)
  (when (looking-at-p (format sx-question-mode--reference-regexp ".+"))
    ;; Returns non-nil
    (forward-paragraph 1)
    t))

(provide 'sx-question-print)
;;; sx-question-print.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
