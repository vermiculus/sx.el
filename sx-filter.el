;;; sx-filter.el --- filters                         -*- lexical-binding: t; -*-

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


;;; Dependencies

(require 'sx)
(require 'sx-cache)


;;; Customizations

(defconst sx-filter-cache-file
  "filters.el")

(defvar sx-filter
  'default
  "The current filter.
To customize the filter for the next call to `sx-request-make',
let-bind this variable to the output of a call to
`sx-filter-compile'.  Be careful!  If you're going to be using
this new filter a lot, create a variable for it.  Creation
requests count against `sx-request-remaining-api-requests'!")


;;; Compilation

;;; TODO allow BASE to be a precompiled filter name
(defun sx-filter-compile (&optional include exclude base)
  "Compile INCLUDE and EXCLUDE into a filter derived from BASE.

INCLUDE and EXCLUDE must both be lists; BASE should be a symbol
or string."
  (let ((keyword-arguments
         `((include . ,(if include (sx--thing-as-string include)))
           (exclude . ,(if exclude (sx--thing-as-string exclude)))
           (base    . ,(if base base)))))
    (let ((response (sx-request-make
                     "filter/create"
                     keyword-arguments)))
      (url-hexify-string
       (cdr (assoc 'filter
                   (elt response 0)))))))


;;; Storage and Retrieval

(defun sx-filter-get (filter)
  "Retrieve named FILTER from `sx-filter-cache-file'."
  (cdr (assoc filter (sx-cache-get sx-filter-cache-file))))

(defun sx-filter-store (name &optional filter)
  "Store NAME as FILTER in `sx-filter-cache-file'.

NAME should be a symbol and FILTER is a string as compiled by
`sx-filter-compile'.

If NAME is a cons cell, (car NAME) is taken to be the actual NAME
and (cdr NAME) is taken to be the actual FILTER.  In this case,
the second argument is simply ignored."
  (let ((name   (if (consp name) (car name) name))
        (filter (if (consp name) (cdr name) filter)))
    (unless (symbolp name)
      (error "Name must be a symbol: %S" name))
    (let* ((dict (sx-cache-get sx-filter-cache-file))
           (entry (assoc name dict)))
      (if entry (setcdr entry filter)
        (setq dict (cons (cons name filter) dict)))

      (sx-cache-set sx-filter-cache-file dict))))

(defun sx-filter-store-all (name-filter-alist)
  (mapc #'sx-filter-store name-filter-alist))

(provide 'sx-filter)
;;; sx-filter.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
