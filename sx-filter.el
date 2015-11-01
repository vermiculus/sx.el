;;; sx-filter.el --- handles retrieval of filters    -*- lexical-binding: t; -*-

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

;; This file manages filters and provides an API to compile filters
;; and retrieve them from the cache.  See `sx-filter-compile' and
;; `sx-filter-get-var', respectively.

;;; Code:


;;; Dependencies

(require 'sx)
(require 'sx-cache)
(require 'sx-request)


;;; Customizations

(defvar sx--filter-alist
  nil
  "An alist of known filters.  See `sx-filter-compile'.
Structure:

    (((INCLUDE  EXCLUDE  BASE ) . \"compiled filter \")
     ((INCLUDE2 EXCLUDE2 BASE2) . \"compiled filter2\")
     ...)")


;;; Creation
(defmacro sx-filter-from-nil (included)
  "Create a filter data structure with INCLUDED fields.
All wrapper fields are included by default."
  `(quote
    ((,@(sx--tree-expand
         (lambda (path)
           (intern (mapconcat #'symbol-name path ".")))
         included)
      .backoff
      .error_id
      .error_message
      .error_name
      .has_more
      .items
      .page
      .page_size
      .quota_max
      .quota_remaining
      )
     nil nil)))

;;; @TODO allow BASE to be a precompiled filter name
(defun sx-filter-compile (&optional include exclude base)
  "Compile INCLUDE and EXCLUDE into a filter derived from BASE.
INCLUDE and EXCLUDE must both be lists; BASE should be a symbol.

Returns the compiled filter as a string."
  (let ((keyword-arguments
         `((include . ,(if include (sx--thing-as-string include)))
           (exclude . ,(if exclude (sx--thing-as-string exclude)))
           (base    . ,(if base base)))))
    (let ((result (elt (sx-request-make "filter/create" keyword-arguments) 0)))
      (sx-assoc-let result
        .filter))))


;;; Storage and Retrieval

(defun sx-filter-get-var (filter-variable)
  "Return the string representation of FILTER-VARIABLE."
  (apply #'sx-filter-get filter-variable))

(defun sx-filter-get (&optional include exclude base)
  "Return the string representation of the given filter.

If the filter data exists in `sx--filter-alist', that value will
be returned.  Otherwise, compile INCLUDE, EXCLUDE, and BASE into
a filter with `sx-filter-compile' and push the association onto
`sx--filter-alist'.  Re-cache the alist with `sx-cache-set' and
return the compiled filter."
  (unless sx--filter-alist
    (setq sx--filter-alist (sx-cache-get 'filter)))
  (or (cdr (assoc (list include exclude base) sx--filter-alist))
      (let ((filter (sx-filter-compile include exclude base)))
        (when filter
          (push (cons (list include exclude base) filter) sx--filter-alist)
          (sx-cache-set 'filter sx--filter-alist)
          filter))))


;;; Browsing filter
(defconst sx-browse-filter
  (sx-filter-from-nil
   ((question body_markdown
              bounty_amount
              comments
              creation_date
              closed_reason
              closed_date
              closed_details
              answers
              answer_count
              score
              title
              owner
              tags
              last_editor
              last_activity_date
              accepted_answer_id
              link
              upvoted
              downvoted
              question_id
              share_link)
    (user display_name
          link
          accept_rate
          reputation)
    (shallow_user display_name
                  link
                  accept_rate
                  reputation)
    (comment owner
             body_markdown
             body
             link
             edited
             creation_date
             upvoted
             score
             post_type
             post_id
             comment_id)
    (answer answer_id
            creation_date
            last_editor
            last_activity_date
            link
            share_link
            score
            owner
            body_markdown
            upvoted
            downvoted
            comments)))
  "The filter applied when retrieving question data.
See `sx-question-get-questions' and `sx-question-get-question'.")

(provide 'sx-filter)
;;; sx-filter.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
