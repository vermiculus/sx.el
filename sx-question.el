;;; sx-question.el --- question logic                -*- lexical-binding: t; -*-

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

;; This file provides an API for retrieving questions and defines
;; additional logic for marking questions as read or hidden.


;;; Code:

(require 'sx)
(require 'sx-filter)
(require 'sx-method)

(defun sx-question-get-questions (site &optional page keywords submethod)
  "Get SITE questions.  Return page PAGE (the first if nil).
Return a list of question.  Each question is an alist of
properties returned by the API with an added (site SITE)
property.

KEYWORDS are added to the method call along with PAGE.

`sx-method-call' is used with `sx-browse-filter'."
  (sx-method-call 'questions
    :keywords `((page . ,page) ,@keywords)
    :site site
    :auth t
    :submethod submethod
    :filter sx-browse-filter))

(defun sx-question-get-question (site question-id)
  "Query SITE for a QUESTION-ID and return it.
If QUESTION-ID doesn't exist on SITE, raise an error."
  (let ((res (sx-method-call 'questions
               :id question-id
               :site site
               :auth t
               :filter sx-browse-filter)))
    (if res (elt res 0)
      (error "Couldn't find question %S in %S"
             question-id site))))

(defun sx-question-get-from-answer (site answer-id)
  "Get question from SITE to which ANSWER-ID belongs.
If ANSWER-ID doesn't exist on SITE, raise an error."
  (let ((res (sx-method-call 'answers
               :id answer-id
               :site site
               :submethod 'questions
               :auth t
               :filter sx-browse-filter)))
    (if res (elt res 0)
      (error "Couldn't find answer %S in %S"
             answer-id site))))

(defun sx-question-get-from-comment (site comment-id)
  "Get question from SITE to which COMMENT-ID belongs.
If COMMENT-ID doesn't exist on SITE, raise an error.

Note this requires two API requests.  One for the comment and one
for the post."
  (let ((res (sx-method-call 'comments
               :id comment-id
               :site site
               :auth t
               :filter sx-browse-filter)))
    (unless res
      (error "Couldn't find comment %S in %S" comment-id site))
    (sx-assoc-let (elt res 0)
      (funcall (if (string= .post_type "answer")
                   #'sx-question-get-from-answer
                 #'sx-question-get-question)
        .site_par
        .post_id))))


;;; Question Properties

;;;; Read/unread
(defvar sx-question--user-read-list nil
  "Alist of questions read by the user.

Each element has the form

    (SITE . QUESTION-LIST)

where each element in QUESTION-LIST has the form

    (QUESTION_ID . LAST-VIEWED-DATE).")

(defun sx-question--ensure-read-list (site)
  "Ensure `sx-question--user-read-list' has been read from cache.
If no cache exists for it, initialize one with SITE."
  (unless sx-question--user-read-list
    (setq sx-question--user-read-list
          (sx-cache-get 'read-questions `'((,site))))))

(defun sx-question--read-p (question)
  "Non-nil if QUESTION has been read since last updated.
See `sx-question--user-read-list'."
  (sx-assoc-let question
    (sx-question--ensure-read-list .site_par)
    (let ((ql (cdr (assoc .site_par sx-question--user-read-list))))
      (and ql
           (>= (or (cdr (assoc .question_id ql)) 0)
               .last_activity_date)))))

(defmacro sx-sorted-insert-skip-first (newelt list &optional predicate)
  "Inserted NEWELT into LIST sorted by PREDICATE.
This is designed for the (site id id ...) lists.  So the first car
is intentionally skipped."
  `(let ((tail ,list)
         (x ,newelt))
     (while (and ;; We're not at the end.
             (cdr-safe tail)
             ;; We're not at the right place.
             (funcall (or #',predicate #'<) x (cadr tail)))
       (setq tail (cdr tail)))
     (setcdr tail (cons x (cdr tail)))))

(defun sx-question--mark-read (question)
  "Mark QUESTION as being read until it is updated again.
Returns nil if question (in its current state) was already marked
read, i.e., if it was `sx-question--read-p'.
See `sx-question--user-read-list'."
  (prog1
      (sx-assoc-let question
        (sx-question--ensure-read-list .site_par)
        (let ((site-cell (assoc .site_par sx-question--user-read-list))
              (q-cell (cons .question_id .last_activity_date))
              cell)
          (cond
           ;; First question from this site.
           ((null site-cell)
            (push (list .site_par q-cell) sx-question--user-read-list))
           ;; Question already present.
           ((setq cell (assoc .question_id site-cell))
            ;; Current version is newer than cached version.
            (when (or (not (numberp (cdr cell)))
                      (> .last_activity_date (cdr cell)))
              (setcdr cell .last_activity_date)))
           ;; Question wasn't present.
           (t
            (sx-sorted-insert-skip-first
             q-cell site-cell
             (lambda (x y) (> (or (car x) -1) (or (car y) -1))))))))
    ;; Save the results.
    ;; @TODO This causes a small lag on `j' and `k' as the list gets
    ;; large.  Should we do this on a timer?
    (sx-cache-set 'read-questions sx-question--user-read-list)))


;;;; Hidden
(defvar sx-question--user-hidden-list nil
  "Alist of questions hidden by the user.

Each element has the form

  (SITE QUESTION_ID QUESTION_ID ...)")

(defun sx-question--ensure-hidden-list (site)
  "Ensure the `sx-question--user-hidden-list' has been read from cache.

If no cache exists for it, initialize one with SITE."
  (unless sx-question--user-hidden-list
    (setq sx-question--user-hidden-list
          (sx-cache-get 'hidden-questions `'((,site))))))

(defun sx-question--hidden-p (question)
  "Non-nil if QUESTION has been hidden."
  (sx-assoc-let question
    (sx-question--ensure-hidden-list .site_par)
    (let ((ql (cdr (assoc .site_par sx-question--user-hidden-list))))
      (and ql (memq .question_id ql)))))

(defun sx-question--mark-hidden (question)
  "Mark QUESTION as being hidden."
  (sx-assoc-let question
    (let ((site-cell (assoc .site_par sx-question--user-hidden-list)))
      ;; If question already hidden, do nothing.
      (unless (memq .question_id site-cell)
        (if (null site-cell)
            ;; First question from this site.
            (push (list .site_par .question_id) sx-question--user-hidden-list)
          ;; Not first question and question wasn't present.
          ;; Add it in, but make sure it's sorted (just in case we
          ;; decide to rely on it later).
          (sx-sorted-insert-skip-first .question_id site-cell >))
        ;; Save the results.
        (sx-cache-set 'hidden-questions sx-question--user-hidden-list)))))


;;;; Other data
(defun sx-question--accepted-answer-id (question)
  "Return accepted answer in QUESTION or nil if none exists."
  (sx-assoc-let question
    (and (integerp .accepted_answer_id)
         .accepted_answer_id)))


;;; Question Mode Answer-Sorting Functions
(sx--create-comparator sx-answer-higher-score-p
  "Return t if answer A has a higher score than answer B."
  #'> (lambda (x) (cdr (assq 'score x))))

(sx--create-comparator sx-answer-newer-p
  "Return t if answer A was posted later than answer B."
  #'> (lambda (x) (cdr (assq 'creation_date x))))

(sx--create-comparator sx-answer-more-active-p
  "Return t if answer A was updated after answer B."
  #'> (lambda (x) (cdr (assq 'last_activity_date x))))

(provide 'sx-question)
;;; sx-question.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
