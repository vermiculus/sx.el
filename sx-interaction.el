;;; sx-interaction.el --- Voting, commenting, and otherwise interacting with questions.  -*- lexical-binding: t; -*-

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

(require 'sx)
(require 'sx-question)
(require 'sx-question-mode)
(require 'sx-question-list)


;;; Using data in buffer
(defun sx--data-here (&optional noerror)
  "Get data for the question or other object under point.
If NOERROR is non-nil, don't throw an error on failure.

This looks at the text property `sx--data-here'. If it's not set,
it looks at a few other reasonable variables. If those fail too,
it throws an error."
  (or (get-text-property (point) 'sx--data-here)
      (and (derived-mode-p 'sx-question-list-mode)
           (tabulated-list-get-id))
      (and (derived-mode-p 'sx-question-mode)
           sx-question-mode--data)
      (and (null noerror)
           (error "No question data found here"))))

(defun sx--maybe-update-display ()
  "Refresh the question list if we're inside it."
  (cond
   ((derived-mode-p 'sx-question-list-mode)
    (sx-question-list-refresh 'redisplay 'no-update))
   ((derived-mode-p 'sx-question-mode)
    (sx-question-mode-refresh 'no-update))))

(defun sx--copy-data (from to)
  "Copy all fields of alist FORM onto TO.
Only fields contained in TO are copied."
  (setcar to (car from))
  (setcdr to (cdr from)))


;;; Visiting
(defun sx-visit (data &optional copy-as-kill)
  "Visit DATA in a web browser.
DATA can be a question, answer, or comment. Interactively, it is
derived from point position.

If copy-as-kill is non-nil, do not call `browse-url'.
Instead, copy the link as a new kill with `kill-new'.
Interactively, this is specified with a prefix argument.

If DATA is a question, also mark it as read."
  (interactive (list (sx--data-here) current-prefix-arg))
  (sx-assoc-let data
    (let ((link
           (when (stringp .link)
             (funcall (if copy-as-kill #'kill-new #'browse-url)
                      .link))))
      (when (and (called-interactively-p 'any) copy-as-kill)
        (message "Copied: %S" link)))
    (when (and .title (not copy-as-kill))
      (sx-question--mark-read data)
      (sx--maybe-update-display))))


;;; Displaying
(defun sx-display-question (&optional data focus window)
  "Display question given by DATA, on WINDOW.
When DATA is nil, display question under point. When FOCUS is
non-nil (the default when called interactively), also focus the
relevant window. 

If WINDOW nil, the window is decided by
`sx-question-mode-display-buffer-function'."
  (interactive (list (sx--data-here) t))
  (when (sx-question--mark-read data)
    (sx--maybe-update-display))
  ;; Display the question.
  (setq window
        (get-buffer-window
         (sx-question-mode--display data window)))
  (when focus
    (if (window-live-p window)
        (select-window window)
      (switch-to-buffer sx-question-mode--buffer))))


;;; Voting
(defun sx-toggle-upvote (data)
  "Apply or remove upvote from DATA.
DATA can be a question, answer, or comment. Interactively, it is
guessed from context at point."
  (interactive (list (sx--data-here)))
  (sx-assoc-let data
    (sx-set-vote data "upvote" (null (eq .upvoted t)))))

(defun sx-toggle-downvote (data)
  "Apply or remove downvote from DATA.
DATA can be a question or an answer. Interactively, it is guessed
from context at point."
  (interactive (list (sx--data-here)))
  (sx-assoc-let data
    (sx-set-vote data "downvote" (null (eq .downvoted t)))))

(defun sx-set-vote (data type status)
  "Set the DATA's vote TYPE to STATUS.
DATA can be a question, answer, or comment. TYPE can be
\"upvote\" or \"downvote\". STATUS is a boolean.

Besides posting to the api, DATA is also altered to reflect the
changes."
  (let ((result
         (sx-assoc-let data
           (sx-method-call
               (cond
                (.comment_id "comments")
                (.answer_id "answers")
                (.question_id "questions"))
             :id (or .comment_id .answer_id .question_id)
             :submethod (concat type (unless status "/undo"))
             :auth 'warn
             :url-method "POST"
             :filter sx-browse-filter
             :site .site))))
    ;; The api returns the new DATA.
    (when (> (length result) 0)
      (sx--copy-data (elt result 0) data)
      ;; Display the changes in `data'.
      (sx--maybe-update-display))))


;;; Commenting
(defun sx-comment (data &optional text)
  "Post a comment on DATA given by TEXT.
DATA can be a question, an answer, or a comment. Interactively,
it is guessed from context at point.
If DATA is a comment, the comment is posted as a reply to it.

TEXT is a string. Interactively, it is read from the minibufer."
  (interactive (list (sx--data-here) 'query))
  ;; When clicking the "Add a Comment" button, first arg is a marker.
  (when (markerp data)
    (setq data (sx--data-here))
    (setq text 'query))
  (sx-assoc-let data
    ;; Get the comment text
    (when (eq text 'query)
      (setq text (read-string
                  "Comment text: "
                  (when .comment_id
                    (concat (sx--user-@name .owner) " "))))
      (while (< (string-width text) 15)
        (setq text (read-string "Comment text (at least 15 characters): " text))))
    ;; If non-interactive, `text' could be anything.
    (unless (stringp text)
      (error "Comment body must be a string"))
    ;; And post
    (let ((result
           (sx-method-call 'posts
             :id (or .post_id .answer_id .question_id)
             :submethod "comments/add"
             :auth 'warn
             :url-method "POST"
             :filter sx-browse-filter
             :site .site
             :keywords `((body ,text)))))
      ;; The api returns the new DATA.
      (when (> (length result) 0)
        (sx--add-comment-to-object
         (elt result 0)
         (if .post_id
             (sx--get-post .post_type .site .post_id)
           data))
        ;; Display the changes in `data'.
        (sx--maybe-update-display)))))

(defun sx--get-post (type site id)
  "Find in the database a post identified by TYPE, SITE and ID.
TYPE is `question' or `answer'. 
SITE is a string.
ID is an integer."
  (let ((db (cons sx-question-mode--data
                  sx-question-list--dataset)))
    (setq db
          (cond 
           ((string= type "question") db)
           ((string= type "answer")
            (apply #'cl-map 'list #'identity
                   (mapcar (lambda (x) (cdr (assoc 'answers x))) db)))))
    (car (cl-member-if
          (lambda (x) (sx-assoc-let x
                   (and (equal (or .answer_id .question_id) id)
                        (equal .site site))))
          db))))

(defun sx--add-comment-to-object (comment object)
  "Add COMMENT to OBJECT's `comments' property.
OBJECT can be a question or an answer."
  (let ((com-cell (assoc 'comments object)))
    (if com-cell
        (progn
          (setcdr
           com-cell
           (apply #'vector
                  (append
                   (cl-map 'list #'identity
                           (cdr com-cell))
                   (list comment)))))
      ;; No previous comments, add it manually.
      (setcdr object (cons (car object) (cdr object)))
      (setcar object `(comments . [,comment])))))

(provide 'sx-interaction)
;;; sx-interaction.el ends here
