;;; sx-question-list.el --- Major-mode for navigating questions list.  -*- lexical-binding: t; -*-

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
(require 'tabulated-list)
(require 'cl-lib)

(require 'sx)
(require 'sx-time)
(require 'sx-site)
(require 'sx-question)
(require 'sx-question-mode)


;;; Customization
(defcustom sx-question-list-height 12
  "Height, in lines, of stack-mode's *question-list* buffer."
  :type 'integer
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
  '((t :underline t :overline t :inherit sx-question-list-answers))
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

(defface sx-question-list-tags
  '((t :inherit sx-question-mode-tags))
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


;;; Mode Definition
(define-derived-mode sx-question-list-mode tabulated-list-mode "Question List"
  "Major mode for browsing a list of questions from StackExchange.
Letters do not insert themselves; instead, they are commands.
\\<sx-question-list>
\\{sx-question-list}"
  (hl-line-mode 1)
  (sx-question-list--update-mode-line)
  (setq tabulated-list-format
        [("  V" 3 t :right-align t)
         ("  A" 3 t :right-align t)
         ("Title" 0 sx-question-list--date-more-recent-p)])
  (setq tabulated-list-padding 1)
  ;; Sorting by title actually sorts by date. It's what we want, but
  ;; it's not terribly intuitive.
  (setq tabulated-list-sort-key '("Title" . nil))
  (add-hook 'tabulated-list-revert-hook
            #'sx-question-list-refresh nil t)
  (add-hook 'tabulated-list-revert-hook
            #'sx-question-list--update-mode-line nil t)
  (tabulated-list-init-header))

(defcustom sx-question-list-date-sort-method 'last_activity_date
  "Parameter which controls date sorting."
  ;; This should be made into a (choice ...) of constants.
  :type 'symbol
  ;; Add a setter to protect the value.
  :group 'sx-question-list)

(defun sx-question-list--date-more-recent-p (x y)
  "Non-nil if tabulated-entry X is newer than Y."
  (sx--<
   sx-question-list-date-sort-method
   (car x) (car y) #'>))

(mapc
 (lambda (x) (define-key sx-question-list-mode-map
          (car x) (cadr x)))
 '(("n" sx-question-list-next)
   ("p" sx-question-list-previous)
   ("j" sx-question-list-view-next)
   ("k" sx-question-list-view-previous)
   ("g" sx-question-list-refresh)
   ([down] sx-question-list-view-next)
   ([up] sx-question-list-view-previous)
   (":" sx-question-list-switch-site)
   ("v" sx-question-list-visit)
   ([?\r] sx-question-list-display-question)))

(defvar sx-question-list--current-page "Latest"
  ;; Other values (once we implement them) are "Top Voted",
  ;; "Unanswered", etc.
  "Variable describing current page being viewed.")

(defvar sx-question-list--unread-count 0
  "Holds the number of unread questions in the current buffer.")
(make-variable-buffer-local 'sx-question-list--unread-count)

(defvar sx-question-list--total-count 0
  "Holds the total number of questions in the current buffer.")
(make-variable-buffer-local 'sx-question-list--total-count)

(defconst sx-question-list--mode-line-format
  '("  "
    mode-name
    " "
    (:propertize sx-question-list--current-page
                 face mode-line-buffer-id)
    " ["
    "Unread: "
    (:propertize
     (:eval (int-to-string sx-question-list--unread-count))
     face mode-line-buffer-id)
    ", "
    "Total: "
    (:propertize
     (:eval (int-to-string sx-question-list--total-count))
     face mode-line-buffer-id)
    "] ")
  "Mode-line construct to use in question-list buffers.")

(defun sx-question-list--update-mode-line ()
  "Fill the mode-line with useful information."
  ;; All the data we need is right in the buffer.
  (when (derived-mode-p 'sx-question-list-mode)
    (setq mode-line-format
          sx-question-list--mode-line-format)
    (setq sx-question-list--total-count
          (length tabulated-list-entries))))

(defvar sx-question-list--current-site "emacs"
  "Site being displayed in the *question-list* buffer.")

(defvar sx-question-list--current-dataset nil
  "")

(defun sx-question-list-refresh (&optional redisplay no-update)
  "Update the list of questions.
If REDISPLAY is non-nil (or if interactive), also call `tabulated-list-print'.
If the prefix argument NO-UPDATE is nil, query StackExchange for
a new list before redisplaying."
  (interactive "p\nP")
  ;; Reset the mode-line unread count (we rebuild it here).
  (setq sx-question-list--unread-count 0)
  (let ((question-list
         (if (and no-update sx-question-list--current-dataset)
             sx-question-list--current-dataset
           (sx-question-get-questions
            sx-question-list--current-site))))
    (setq sx-question-list--current-dataset question-list)
    ;; Print the result.
    (setq tabulated-list-entries
          (mapcar #'sx-question-list--print-info question-list)))
  (when redisplay (tabulated-list-print 'remember)))

(defun sx-question-list-visit (&optional data)
  "Visits question under point (or from DATA) using `browse-url'."
  (interactive)
  (unless data (setq data (tabulated-list-get-id)))
  (unless data (error "No question here!"))
  (sx-assoc-let data
    (browse-url .link))
  (sx-question--mark-read data)
  (sx-question-list-refresh 'redisplay 'no-update))

(defcustom sx-question-list-ago-string " ago"
  "String appended to descriptions of the time since something happened.
Used in the questions list to indicate a question was updated \"4d ago\"."
  :type 'string
  :group 'sx-question-list)

(defun sx-question-list--print-info (data)
  "Convert `json-read' DATA into tabulated-list format."
  (sx-assoc-let data
    (list
     data
     (vector
      (list (int-to-string .score)
            'face (if .upvoted 'sx-question-list-score-upvoted
                    'sx-question-list-score))
      (list (int-to-string .answer_count)
            'face (if (sx-question--accepted-answer-id data)
                      'sx-question-list-answers-accepted
                    'sx-question-list-answers))
      (concat
       (propertize
        .title
        'face (if (sx-question--read-p data)
                  'sx-question-list-read-question
                ;; Increment `sx-question-list--unread-count' for the mode-line.
                (cl-incf sx-question-list--unread-count)
                'sx-question-list-unread-question))
       (propertize " " 'display "\n         ")
       (propertize (concat (sx-time-since .last_activity_date)
                           sx-question-list-ago-string)
                   'face 'sx-question-list-date)
       " "
       (propertize (mapconcat #'sx-question--tag-format .tags " ")
                   'face 'sx-question-list-tags)
       (propertize " " 'display "\n"))))))

(defun sx-question-list-view-previous (n)
  "Hide this question, move to previous one, display it."
  (interactive "p")
  (sx-question-list-view-next (- n)))

(defun sx-question-list-view-next (n)
  "Hide this question, move to next one, display it."
  (interactive "p")
  (sx-question-list-next n)
  (sx-question-list-display-question))

(defun sx-question-list-next (n)
  "Move to the next entry."
  (interactive "p")
  (forward-line n))

(defun sx-question-list-previous (n)
  "Move to the previous entry."
  (interactive "p")
  (sx-question-list-next (- n)))

(defun sx-question-list-display-question (&optional data focus)
  "Display question given by DATA.
If called interactively (or with DATA being nil), display
question under point.
Also when called interactively (or when FOCUS is non-nil), also
focus the relevant window."
  (interactive '(nil t))
  (unless data (setq data (tabulated-list-get-id)))
  (unless data (error "No question here!"))
  (unless (sx-question--read-p data)
    (cl-decf sx-question-list--unread-count)
    (sx-question--mark-read data)
    (sx-question-list-refresh 'redisplay 'no-update))
  (unless (and (window-live-p sx-question-mode--window)
               (null (equal sx-question-mode--window (selected-window))))
    (setq sx-question-mode--window
          (condition-case er
              (split-window (selected-window) sx-question-list-height 'below)
            (error
             ;; If the window is too small to split, use current one.
             (if (string-match
                  "Window #<window .*> too small for splitting"
                  (car (cdr-safe er)))
                 nil
               (error (cdr er)))))))
  ;; Display the question.
  (sx-question-mode--display data sx-question-mode--window)
  ;; Configure the window to be closed on `q'.
  (set-window-prev-buffers sx-question-mode--window nil)
  (set-window-parameter
   sx-question-mode--window
   'quit-restore
   ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Parameters.html#Window-Parameters
   `(window window ,(selected-window) ,sx-question-mode--buffer))
  (when focus
    (if sx-question-mode--window
        (select-window sx-question-mode--window)
      (switch-to-buffer sx-question-mode--buffer))))

(defun sx-question-list-switch-site (site)
  "Switch the current site to SITE and display its questions"
  (interactive
   (list (funcall (if ido-mode #'ido-completing-read #'completing-read)
          "Switch to site: " (sx-site-get-api-tokens)
          (lambda (site)
            (not (equal site sx-question-list--current-site)))
          t)))
  (setq sx-question-list--current-site site)
  (sx-question-list-refresh 'redisplay))

(defvar sx-question-list--buffer nil
  "Buffer where the list of questions is displayed.")

(defun list-questions (no-update)
  "Display a list of StackExchange questions."
  (interactive "P")
  (unless (buffer-live-p sx-question-list--buffer)
    (setq sx-question-list--buffer
          (generate-new-buffer "*question-list*")))
  (with-current-buffer sx-question-list--buffer
    (sx-question-list-mode)
    (sx-question-list-refresh 'redisplay no-update))
  (switch-to-buffer sx-question-list--buffer))

(defalias 'sx-list-questions #'list-questions)

(provide 'sx-question-list)
;;; sx-question-list.el ends here
