;;; sx-question.el --- question logic for stack-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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

;;


;;; Code:

(require 'sx)
(require 'sx-filter)
(require 'sx-method)

(defvar sx-question-browse-filter
  '((question.body_markdown
     question.comments
     question.answers
     question.last_editor
     question.accepted_answer_id
     user.display_name
     comment.owner
     comment.body_markdown
     comment.body
     answer.last_editor
     answer.owner
     answer.body_markdown
     answer.comments)
    (user.profile_image shallow_user.profile_image)))

(defun sx-question-get-questions (site &optional page)
  "Get the page PAGE of questions from SITE."
  (mapcar
   (lambda (question) (cons (cons 'site site) question))
   (sx-method-call
    "questions"
    `((site . ,site)
      (page . ,page))
    sx-question-browse-filter)))

(defun sx-question-get-question (site id)
  "Get the question ID from SITE."
  (let ((res (sx-method-call
              (format "questions/%s" id)
              `((site . ,site))
              sx-question-browse-filter)))
    (if (vectorp res)
        (elt res 0)
      (error "Couldn't find question %S in %S" id site))))


;;; Question Properties
(defvar sx-question--user-read-list nil 
  "Alist of questions read by the user.
Each element has the form (SITE . QUESTION-LIST).
And each element in QUESTION-LIST has the form (QUESTION_ID . LAST-VIEWED-DATE).")

(defun sx-question--ensure-read-list ()
  "Ensure the `sx-question--user-read-list' has been read from cache."
  (unless sx-question--user-read-list
    (setq sx-question--user-read-list
          (sx-cache-get 'read-questions))))

(defun sx-question--read-p (question)
  "Non-nil if QUESTION has been read since last updated."
  (sx-question--ensure-read-list)
  (sx-assoc-let question
    (let ((ql (cdr (assoc .site sx-question--user-read-list))))
      (and ql
           (>= (or (cdr (assoc .question_id ql)) 0)
               .last_activity_date)))))

(defun sx-question--mark-read (question)
  "Mark QUESTION as being read, until it is updated again."
  (sx-question--ensure-read-list)
  (sx-assoc-let question
    (let ((site-cell (assoc .site sx-question--user-read-list))
          (q-cell (cons .question_id .last_activity_date))
          cell)
      (cond
       ;; First question from this site.
       ((null site-cell)
        (push (list .site q-cell) sx-question--user-read-list))
       ;; Question already has an older time.
       ((setq cell (assoc .question_id site-cell))
        (setcdr cell .last_activity_date))
       ;; Question wasn't present.
       (t 
        (setcdr site-cell (cons q-cell (cdr site-cell)))))))
  ;; Save the results.
  (sx-cache-set 'read-questions sx-question--user-read-list))

(defun sx-question--accepted-answer-id (question)
  "Return accepted answer in QUESTION, or nil if none."
  (sx-assoc-let question
    (and (integerp .accepted_answer_id)
         .accepted_answer_id)))

(defun sx-question--tag-format (tag)
  "Formats TAG for display"
  (concat "[" tag "]"))

(provide 'sx-question)
;;; sx-question.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
