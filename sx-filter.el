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
(require 'sx-request)


;;; Customizations

(defvar sx--filter-alist
  (sx-cache-get 'filter)
  "")


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
      (sx-assoc-let (elt response 0)
        (url-hexify-string filter)))))


;;; Storage and Retrieval

(defun sx-filter-get-var (filter-variable)
  "Return the string representation of FILTER-VARIABLE."
  (apply #'sx-filter-get filter-variable))

(defun sx-filter-get (&optional include exclude base)
  "Return the string representation of the given filter."
  ;; Maybe we alreay have this filter
  (or (cdr (assoc (list include exclude base) sx--filter-alist))
      ;; If we don't, build it, save it, and return it.
      (let ((filter (sx-filter-compile include exclude base)))
        (when filter
          (push (cons (list include exclude base) filter) sx--filter-alist)
          (sx-cache-set 'filter sx--filter-alist)
          filter))))

(provide 'sx-filter)
;;; sx-filter.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
