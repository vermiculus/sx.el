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
(require 'sx-lto)
(require 'sx-request)

;; I don't know why this is here, but it was causing an API request on require.
(defvar sx-question-browse-filter nil
  ;; (stack-filter-compile
  ;;  nil
  ;;  '(user.profile_image shallow_user.profile_image))
  )

;; (stack-filter-store 'question-browse sx-question-browse-filter)

(defun sx-question-get-questions (site &optional page)
  "Get the page PAGE of questions from SITE."
  (sx-request-make
   "questions"
   `((site . ,site)
     (page . ,page))
   sx-question-browse-filter))

(defun sx-question-get-question (site id)
  "Get the question ID from SITE."
  (let ((res (sx-request-make
              (format "questions/%s" id)
              `((site . ,site))
              sx-question-browse-filter)))
    (if (vectorp res)
        (elt res 0)
      (error "Couldn't find question %s in %s" id site))))


;;; Question Properties
(defun sx-question--read-p (question)
  "Non-nil if QUESTION has been read since last updated."
  ;; @TODO:
  (cl-evenp (random)))

(defun sx-question--accepted-answer (question)
  "Return accepted answer in QUESTION, or nil if none."
  ;; @TODO:
  (cl-evenp (random)))

(defun sx-question--mark-read (question)
  "Mark QUESTION as being read, until it is updated again."
  nil)

(defun sx-question--< (property x y &optional pred)
  "Non-nil if PROPERTY attribute of question X is less than that of Y.
With optional argument predicate, use it instead of `<'."
  (funcall (or pred #'<)
           (cdr (assoc property x))
           (cdr (assoc property y))))

(provide 'sx-question)
;;; sx-question.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
