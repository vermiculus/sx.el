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
(defun sx--data-here ()
  "Get the text property `sx--data-here'."
  (or (get-text-property (point) 'sx--data-here)
      (and (derived-mode-p 'sx-question-list-mode)
           (tabulated-list-get-id))
      (or (derived-mode-p 'sx-question-mode)
          sx-question-mode--data)))

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

(defun sx-visit (data)
  "Visit DATA in a web browser.
DATA can be a question, answer, or comment. Interactively, it is
derived from point position.
If DATA is a question, also mark it as read."
  (interactive (list (sx--data-here)))
  (sx-assoc-let data
    (when (stringp .link)
      (browse-url .link))
    (when .title
      (sx-question--mark-read data)
      (sx--maybe-update-display))))

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

(provide 'sx-interaction)
;;; sx-interaction.el ends here
