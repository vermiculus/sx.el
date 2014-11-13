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

;;

;;; Code:
(require 'json)
(require 'url)
(require 'sx)
(require 'sx-request)
(require 'sx-filter)

(defun sx-method-call
    (method &optional keyword-arguments filter silent)
  "Call METHOD with KEYWORD-ARGUMENTS using FILTER.

If SILENT is non-nil, no messages will be printed.

Return the entire response as a complex alist."
  (sx-request-make
   method
   (cons (cons 'filter
               (sx-filter-get-var
                (cond (filter filter)
                      ((boundp 'stack-filter) stack-filter))))
         keyword-arguments)))

(provide 'sx-method)
;;; sx-method.el ends here
