;;; sx-method.el --- method calls

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

;;; This file is effectively a common-use wrapper for
;;; `sx-request-make'.  It provides higher-level handling such as
;;; (authentication, filters, ...) that `sx-request-make' doesn't need
;;; to handle.

;;; Code:
(require 'json)
(require 'url)
(require 'sx)
(require 'sx-request)
(require 'sx-filter)

(defun sx-method-call
    (method &optional keyword-arguments filter)
  "Call METHOD with KEYWORD-ARGUMENTS using FILTER.

Return the response content as a complex alist.

See `sx-request-make' and `sx-filter-get-var'."
  (sx-request-make
   method
   (cons (cons 'filter
               (sx-filter-get-var
                (cond (filter filter)
                      ((boundp 'stack-filter) stack-filter))))
         keyword-arguments)))

(provide 'sx-method)
;;; sx-method.el ends here
