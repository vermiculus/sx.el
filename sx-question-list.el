;;; sx-question-list.el --- major-mode for navigating questions list  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Artur Malabarba

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

;; Provides question list logic (as used in e.g. `sx-tab-frontpage').

;;; Code:
(require 'tabulated-list)
(require 'cl-lib)

(require 'sx)
(require 'sx-switchto)
(require 'sx-time)
(require 'sx-tag)
(require 'sx-site)
(require 'sx-question)
(require 'sx-question-mode)
(require 'sx-favorites)

(defgroup sx-question-list nil
  "Customization group for sx-question-list."
  :prefix "sx-question-list-"
  :tag "SX Question List"
  :group 'sx)

(defgroup sx-question-list-faces nil
  "Customization group for the faces of `sx-question-list'."
  :prefix "sx-question-list-"
  :tag "SX Question List Faces"
  :group 'sx-question-list)


;;; Customization
(defcustom sx-question-list-height 12
  "Height, in lines, of SX's *question-list* buffer."
  :type 'integer
  :group 'sx-question-list)

(defcustom sx-question-list-excluded-tags nil
  "List of tags (strings) to be excluded from the question list."
  :type '(repeat string)
  :group 'sx-question-list)

(defface sx-question-list-parent
  '((t :inherit default))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-answers
  '((((background light)) :foreground "SeaGreen4"
     :height 1.0 :inherit sx-question-list-parent)
    (((background dark))  :foreground "#D1FA71"
     :height 1.0 :inherit sx-question-list-parent)
    (t :inherit sx-question-list-parent))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-answers-accepted
  '((t :box 1 :inherit sx-question-list-answers))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-score
  '((t :height 1.0 :inherit sx-question-list-parent))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-score-upvoted
  '((t :weight bold
       :inherit sx-question-list-score))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-date
  '((t :inherit font-lock-comment-face))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-read-question
  '((t :height 1.0 :inherit sx-question-list-parent))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-unread-question
  '((t :weight bold :inherit sx-question-list-read-question))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-favorite
  '((t :inherit sx-question-list-score-upvoted))
  ""
  :group 'sx-question-list-faces)

(defface sx-question-list-bounty
  '((t :inherit font-lock-warning-face))
  ""
  :group 'sx-question-list-faces)


;;; Backend variables

(defvar sx-question-list--site nil
  "Site being displayed in the *question-list* buffer.")
(make-variable-buffer-local 'sx-question-list--site)

(defvar sx-question-list--order nil
  "Order being displayed in the *question-list* buffer.
This is also affected by `sx-question-list--descending'.")
(make-variable-buffer-local 'sx-question-list--order)

(defvar sx-question-list--descending t
  "In which direction should `sx-question-list--order' be sorted.
If non-nil (default), descending.
If nil, ascending.")
(make-variable-buffer-local 'sx-question-list--order)

(defvar sx-question-list--print-function #'sx-question-list--print-info
  "Function to convert a question alist into a tabulated-list entry.
Used by `sx-question-list-mode', the default value is
`sx-question-list--print-info'.

If this is set to a different value, it may be necessary to
change `tabulated-list-format' accordingly.")
(make-variable-buffer-local 'sx-question-list--print-function)

(defcustom sx-question-list-ago-string " ago"
  "String appended to descriptions of the time since something happened.
Used in the questions list to indicate a question was updated
\"4d ago\"."
  :type 'string
  :group 'sx-question-list)

(defun sx-question-list--print-info (question-data)
  "Convert `json-read' QUESTION-DATA into tabulated-list format.

This is the default printer used by `sx-question-list'. It
assumes QUESTION-DATA is an alist containing (at least) the
elements:
 `question_id', `site_par', `score', `upvoted', `answer_count',
 `title', `bounty_amount', `bounty_amount', `bounty_amount',
 `last_activity_date', `tags', `owner'.

Also see `sx-question-list-refresh'."
  (sx-assoc-let question-data
    (let ((favorite (if (member .question_id
                                (assoc .site_par
                                       sx-favorites--user-favorite-list))
                        (if (char-displayable-p ?\x2b26) "\x2b26" "*") " ")))
      (list
       question-data
       (vector
        (list (int-to-string .score)
              'face (if .upvoted 'sx-question-list-score-upvoted
                      'sx-question-list-score))
        (list (int-to-string .answer_count)
              'face (if (sx-question--accepted-answer-id question-data)
                        'sx-question-list-answers-accepted
                      'sx-question-list-answers))
        (concat
         ;; First line
         (propertize
          .title
          'face (if (sx-question--read-p question-data)
                    'sx-question-list-read-question
                  'sx-question-list-unread-question))
         (propertize " " 'display "\n   ")
         ;; Second line
         (propertize favorite 'face 'sx-question-list-favorite)
         (if (and (numberp .bounty_amount) (> .bounty_amount 0))
             (propertize (format "%4d" .bounty_amount)
                         'face 'sx-question-list-bounty)
           "    ")
         " "
         (propertize (format "%3s%s"
                       (sx-time-since .last_activity_date)
                       sx-question-list-ago-string)
                     'face 'sx-question-list-date)
         " "
         ;; @TODO: Make this width customizable. (Or maybe just make
         ;; the whole thing customizable)
         (format "%-40s" (sx-tag--format-tags .tags sx-question-list--site))
         " "
         (sx-user--format "%15d %4r" .owner)
         (propertize " " 'display "\n")))))))

(defvar sx-question-list--pages-so-far 0
  "Number of pages currently being displayed.
This variable gets reset to 0 before every refresh.
It should be used by `sx-question-list--next-page-function'.")
(make-variable-buffer-local 'sx-question-list--pages-so-far)

(defvar sx-question-list--refresh-function nil
  "Function used to refresh the list of questions to be displayed.
Used by `sx-question-list-mode', this is a function, called with
no arguments, which returns a list questions to be displayed,
like the one returned by `sx-question-get-questions'.

If this is not set, the value of `sx-question-list--dataset' is
used, and the list is simply redisplayed.")
(make-variable-buffer-local 'sx-question-list--refresh-function)

(defvar sx-question-list--next-page-function nil
  "Function used to fetch the next page of questions to be displayed.
Used by `sx-question-list-mode'. This is a function, called with
no arguments, which returns a list questions to be displayed,
like the one returned by `sx-question-get-questions'.

This function will be called when the user presses \\<sx-question-list-mode-map>\\[sx-question-list-next] at the end
of the question list. It should either return nil (indicating
\"no more questions\") or return a list of questions which will
appended to the currently displayed list.

If this is not set, it's the same as a function which always
returns nil.")
(make-variable-buffer-local 'sx-question-list--next-page-function)

(defvar sx-question-list--dataset nil
  "The logical data behind the displayed list of questions.
This dataset contains even questions that are hidden by the user,
and thus not displayed in the list of questions.

This is ignored if `sx-question-list--refresh-function' is set.")
(make-variable-buffer-local 'sx-question-list--dataset)

(defconst sx-question-list--key-definitions
  '(
    ;; S-down and S-up would collide with `windmove'.
    ("<down>" sx-question-list-next)
    ("<up>" sx-question-list-previous)
    ("RET" sx-display "Display")
    ("n" sx-question-list-next "Navigate")
    ("p" sx-question-list-previous "Navigate")
    ("j" sx-question-list-view-next "Navigate")
    ("k" sx-question-list-view-previous "Navigate")
    ("N" sx-question-list-next-far)
    ("P" sx-question-list-previous-far)
    ("J" sx-question-list-next-far)
    ("K" sx-question-list-previous-far)
    ("g" sx-question-list-refresh)
    ("t" sx-tab-switch "tab")
    ("a" sx-ask "ask")
    ("S" sx-search "Search")
    ("s" sx-switchto-map "switch-to")
    ("v" sx-visit-externally "visit")
    ("u" sx-upvote)
    ("d" sx-downvote)
    ("h" sx-question-list-hide "hide")
    ("m" sx-question-list-mark-read "mark-read")
    ("*" sx-favorite)
    )
  "List of key definitions for `sx-question-list-mode'.
This list must follow the form described in
`sx--key-definitions-to-header-line'.")

(defconst sx-question-list--header-line
  (sx--key-definitions-to-header-line
   sx-question-list--key-definitions)
  "Header-line used on the question list.")

(defvar sx-question-list--order-methods
  '(("Recent Activity" . activity)
    ("Creation Date"   . creation)
    ("Most Voted"      . votes)
    ("Score"           . votes))
  "Alist of possible values to be passed to the `sort' keyword.")
(make-variable-buffer-local 'sx-question-list--order-methods)

(defun sx-question-list--interactive-order-prompt (&optional prompt)
  "Interactively prompt for a sorting order.
PROMPT is displayed to the user.  If it is omitted, a default one
is used."
  (let ((order (sx-completing-read
                (or prompt "Order questions by: ")
                (mapcar #'car sx-question-list--order-methods))))
    (cdr-safe (assoc-string order sx-question-list--order-methods))))

(defun sx-question-list--make-pager (method &optional submethod)
  "Return a function suitable for use as a question list pager.
Meant to be used as `sx-question-list--next-page-function'."
  (lambda (page)
    (sx-method-call method
      :keywords `((page . ,page)
                  ,@(when sx-question-list--order
                      `((order . ,(if sx-question-list--descending 'desc 'asc))
                        (sort . ,sx-question-list--order))))
      :site sx-question-list--site
      :auth t
      :submethod submethod
      :filter sx-browse-filter)))


;;; Mode Definition

(defvar sx-question-list--current-tab "Latest"
  ;; @TODO Other values (once we implement them) are "Top Voted",
  ;; "Unanswered", etc.
  "Variable describing current tab being viewed.")

(defconst sx-question-list--mode-line-format
  '("   "
    (:propertize
     (:eval (sx--pretty-site-parameter sx-question-list--site))
     face mode-line-buffer-id)
    " " mode-name ": "
    (:propertize sx-question-list--current-tab
                 face mode-line-buffer-id)
    " ["
    "Unread: "
    (:propertize
     (:eval (sx-question-list--unread-count))
     face mode-line-buffer-id)
    ", "
    "Total: "
    (:propertize
     (:eval (int-to-string (length tabulated-list-entries)))
     face mode-line-buffer-id)
    "] ")
  "Mode-line construct to use in question-list buffers.")

(define-derived-mode sx-question-list-mode
  tabulated-list-mode "Question List"
  "Major mode for browsing a list of questions from StackExchange.
Letters do not insert themselves; instead, they are commands.

The recommended way of using this mode is to activate it and then
set `sx-question-list--next-page-function'. The return value of
this function is mapped with `sx-question-list--print-function',
so you may need to customize the latter if the former does not
return a list of questions.

The full list of variables which can be set is:
 1. `sx-question-list--site'
      Set this to the name of the site if that makes sense. If it
      doesn't leave it as nil.
 2. `sx-question-list--print-function'
      Change this if the data you're dealing with is not strictly a
      list of questions (see the doc for details).
 3. `sx-question-list--refresh-function'
      This is used to populate the initial list. It is only necessary
      if item 4 does not fit your needs.
 4. `sx-question-list--next-page-function'
      This is used to fetch further questions. If item 3 is nil, it is
      also used to populate the initial list.
 5. `sx-question-list--dataset'
      This is only used if both 3 and 4 are nil. It can be used to
      display a static list.
 6. `sx-question-list--order'
      Set this to the `sort' method that should be used when
      requesting the list, if that makes sense. If it doesn't
      leave it as nil.
\\<sx-question-list-mode-map>
If none of these is configured, the behaviour is that of a
\"Frontpage\", for the site given by
`sx-question-list--site'.

Item 2 is mandatory, but it also has a sane default which is
usually enough.

As long as one of 3, 4, or 5 is provided, the other two are
entirely optional. Populating or refreshing the list of questions
is done in the following way:
 - Set `sx-question-list--pages-so-far' to 1.
 - Call function 2.
 - If function 2 is not given, call function 3 with argument 1.
 - If 3 is not given use the value of 4.

Adding further questions to the bottom of the list is done by:
 - Increment `sx-question-list--pages-so-far'.
 - Call function 3 with argument `sx-question-list--pages-so-far'.
 - If it returns anything, append to the dataset and refresh the
   display; otherwise, decrement `sx-question-list--pages-so-far'.

If `sx-question-list--site' is given, items 3 and 4 should take it
into consideration.  The same holds for `sx-question-list--order'.

\\{sx-question-list-mode-map}"
  (hl-line-mode 1)
  (setq mode-line-format
        sx-question-list--mode-line-format)
  (setq sx-question-list--pages-so-far 0)
  (setq tabulated-list-format
        [("  V" 3 t :right-align t)
         ("  A" 3 t :right-align t)
         ("Title" 0 sx-question-list--date-more-recent-p)])
  (setq tabulated-list-padding 1)
  ;; Sorting by title actually sorts by date. It's what we want, but
  ;; it's not terribly intuitive.
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
            #'sx-question-list-refresh nil t)
  ;; This is the default value, but we'll error out if the user has
  ;; set it to nil.
  (setq tabulated-list-use-header-line t)
  (tabulated-list-init-header)
  (setq header-line-format sx-question-list--header-line))

(defcustom sx-question-list-date-sort-method 'last_activity_date
  "Parameter which controls date sorting."
  ;; This should be made into a (choice ...) of constants.
  :type 'symbol
  ;; Add a setter to protect the value.
  :group 'sx-question-list)

(sx--create-comparator sx-question-list--date-more-recent-p
  "Non-nil if tabulated-entry A is newer than B."
  #'> (lambda (x)
        (cdr (assq sx-question-list-date-sort-method (car x)))))


;;; Keybinds
;; We need this quote+eval combo because `kbd' was a macro in 24.2.
(mapc (lambda (x) (eval `(define-key sx-question-list-mode-map
                      (kbd ,(car x)) #',(cadr x))))
  sx-question-list--key-definitions)

(sx--define-conditional-key sx-question-list-mode-map "O" #'sx-question-list-order-by
  (and (boundp 'sx-question-list--order) sx-question-list--order))

(defun sx-question-list-hide (data)
  "Hide question under point.
Non-interactively, DATA is a question alist."
  (interactive
   (list (if (derived-mode-p 'sx-question-list-mode)
             (tabulated-list-get-id)
           (sx-user-error "Not in `sx-question-list-mode'"))))
  (sx-question--mark-hidden data)
  ;; The current entry will not be present after the list is
  ;; redisplayed. To avoid `tabulated-list-mode' getting lost (and
  ;; sending us to the top) we move to the next entry before
  ;; redisplaying.
  (forward-line 1)
  (when (called-interactively-p 'any)
    (sx-question-list-refresh 'redisplay 'noupdate)))

(defun sx-question-list-mark-read (data)
  "Mark as read question under point.
Non-interactively, DATA is a question alist."
  (interactive
   (list (if (derived-mode-p 'sx-question-list-mode)
             (tabulated-list-get-id)
           (sx-user-error "Not in `sx-question-list-mode'"))))
  (sx-question--mark-read data)
  (sx-question-list-next 1)
  (when (called-interactively-p 'any)
    (sx-question-list-refresh 'redisplay 'noupdate)))

(defun sx-question-list--unread-count ()
  "Number of unread questions in current dataset, as a string."
  (int-to-string
   (cl-count-if-not
    #'sx-question--read-p sx-question-list--dataset)))

(defun sx-question-list--remove-excluded-tags (question-list)
  "Return QUESTION-LIST, with some questions removed.
Removes all questions hidden by the user, as well as those
containing a tag in `sx-question-list-excluded-tags'."
  (cl-remove-if (lambda (q)
                  (or (sx-question--hidden-p q)
                      (cl-intersection (let-alist q .tags)
                                       sx-question-list-excluded-tags
                                       :test #'string=)))
                question-list))

(defun sx-question-list-refresh (&optional redisplay no-update)
  "Update the list of questions.
If REDISPLAY is non-nil (or if interactive), also call `tabulated-list-print'.
If the prefix argument NO-UPDATE is nil, query StackExchange for
a new list before redisplaying."
  (interactive "p\nP")
  ;; Reset the mode-line unread count (we rebuild it here).
  (unless no-update
    (setq sx-question-list--pages-so-far 1))
  (let* ((question-list
          (or (and no-update sx-question-list--dataset)
              (and (functionp sx-question-list--refresh-function)
                   (funcall sx-question-list--refresh-function))
              (and (functionp sx-question-list--next-page-function)
                   (funcall sx-question-list--next-page-function 1))
              sx-question-list--dataset))
         ;; Preserve window positioning.
         (window (get-buffer-window (current-buffer)))
         (old-start (when window (window-start window))))
    ;; The dataset contains everything. Hiding and filtering is done
    ;; on the `tabulated-list-entries' below.
    (setq sx-question-list--dataset question-list)
    ;; Print the result.
    (setq tabulated-list-entries
          (mapcar sx-question-list--print-function
                  (sx-question-list--remove-excluded-tags
                   sx-question-list--dataset)))
    (when redisplay
      (tabulated-list-print 'remember))
    (when window
      (set-window-start window old-start)))
  (sx-message "Done."))

(defun sx-question-list-view-previous (n)
  "Move cursor up N questions up and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (sx-question-list-view-next (- n)))

(defun sx-question-list-view-next (n)
  "Move cursor down N questions and display this question.
Displayed in `sx-question-mode--window', replacing any question
that may currently be there."
  (interactive "p")
  (sx-question-list-next n)
  (sx-question-mode--display
   (tabulated-list-get-id)
   (sx-question-list--create-question-window)))

(defun sx-question-list--create-question-window ()
  "Create or find a window where a question can be displayed.

If any current window displays a question, that window is
returned. If none do, a new one is created such that the
question-list window remains `sx-question-list-height' lines
high (if possible)."
  (or (sx-question-mode--get-window)
      ;; Create a proper window.
      (let ((window
             (condition-case er
                 (split-window (selected-window) sx-question-list-height 'below)
               (error
                ;; If the window is too small to split, use any one.
                (if (string-match
                     "Window #<window .*> too small for splitting"
                     (car (cdr-safe er)))
                    (next-window)
                  (error (cdr er)))))))
        ;; Configure the window to be closed on `q'.
        (set-window-prev-buffers window nil)
        (set-window-parameter
         window 'quit-restore
         ;; See (info "(elisp) Window Parameters")
         `(window window ,(selected-window) ,sx-question-mode--buffer))
        window)))

(defvar sx-question-list--last-refresh (current-time)
  "Time of the latest refresh.")

(defun sx-question-list-next (n)
  "Move cursor down N questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (if (and (< n 0) (bobp))
      (when (> (time-to-seconds
                (time-subtract (current-time) sx-question-list--last-refresh))
               1)
        (sx-question-list-refresh 'redisplay)
        (setq sx-question-list--last-refresh (current-time)))
    (forward-line n)
    ;; If we were trying to move forward, but we hit the end.
    (when (eobp)
      ;; Try to get more questions.
      (sx-question-list-next-page))
    (sx-question-list--ensure-line-good-line-position)))

(defun sx-question-list--ensure-line-good-line-position ()
  "Scroll window such that current line is a good place.
Check if we're at least 6 lines from the bottom. Scroll up if
we're not. Do the same for 3 lines from the top."
  ;; At least one entry below us.
  (let ((lines-to-bottom (count-screen-lines (point) (window-end))))
    (unless (>= lines-to-bottom 6)
      (recenter (- 6))))
  ;; At least one entry above us.
  (let ((lines-to-top (count-screen-lines (point) (window-start))))
    (unless (>= lines-to-top 3)
      (recenter 3))))

(defun sx-question-list-next-page ()
  "Fetch and display the next page of questions."
  (interactive)
  ;; Stay at the last line.
  (goto-char (point-max))
  (forward-line -1)
  (when (functionp sx-question-list--next-page-function)
    ;; Try to get more questions
    (let ((list
           (funcall sx-question-list--next-page-function
             (1+ sx-question-list--pages-so-far))))
      (if (null list)
          (message "No further questions.")
        ;; If it worked, increment the variable.
        (cl-incf sx-question-list--pages-so-far)
        ;; And update the dataset.
        ;; @TODO: Check for duplicates.
        (setq sx-question-list--dataset
              (append sx-question-list--dataset list))
        (sx-question-list-refresh 'redisplay 'no-update)
        (forward-line 1)))))

(defun sx-question-list-previous (n)
  "Move cursor up N questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (sx-question-list-next (- n)))

(defcustom sx-question-list-far-step-size 5
  "How many questions `sx-question-list-next-far' skips."
  :type 'integer
  :group 'sx-question-list
  :package-version '(sx-question-list . ""))

(defun sx-question-list-next-far (n)
  "Move cursor up N*`sx-question-list-far-step-size' questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (sx-question-list-next (* n sx-question-list-far-step-size)))

(defun sx-question-list-previous-far (n)
  "Move cursor up N questions.
This does not update `sx-question-mode--window'."
  (interactive "p")
  (sx-question-list-next-far (- n)))

(defun sx-question-list-switch-site (site)
  "Switch the current site to SITE and display its questions.
Retrieve completions from `sx-site-get-api-tokens'.
Sets `sx-question-list--site' and then call
`sx-question-list-refresh' with `redisplay'."
  (interactive
   (list (sx-completing-read
           "Switch to site: " (sx-site-get-api-tokens)
           (lambda (site) (not (equal site sx-question-list--site)))
           t)))
  (when (and (stringp site) (> (length site) 0))
    (setq sx-question-list--site site)
    (sx-question-list-refresh 'redisplay)))

(defun sx-question-list-order-by (sort &optional ascend)
  "Order questions in the current list by the method SORT.
Sets `sx-question-list--order' and then calls
`sx-question-list-refresh' with `redisplay'.

With a prefix argument or a non-nil ASCEND, invert the sorting
order."
  (interactive
   (list (when sx-question-list--order
           (sx-question-list--interactive-order-prompt))
         current-prefix-arg))
  (unless sx-question-list--order
    (sx-user-error "This list can't be reordered"))
  (when (and sort (symbolp sort))
    (setq sx-question-list--order sort)
    (setq sx-question-list--descending (not ascend))
    (sx-question-list-refresh 'redisplay)))

(provide 'sx-question-list)
;;; sx-question-list.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
