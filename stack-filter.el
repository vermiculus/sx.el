;;; stack-filter.el --- filters for stack-mode       -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: help, hypermedia, mail, news, tools

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

(if (boundp 'production)
    (require 'stack-core)
  (setq load-path (cons "." load-path))
  (load "stack-core.el"))


;;; Customizations

(defcustom stack-core-filter
  (stack-core-compile-filter
   nil					;don't include anything extra
   '(user.profile_image			;don't include pictures (yet)
     shallow_user.profile_image)	;
   'withbody)				;we want the body!
  "The current filter.  To customize the filter for the next call
to `stack-core-make-request', let-bind this variable to the
output of a call to `stack-core-compile-filter'.  Be careful!  If
you're going to be using this new filter a lot, create a variable
for it.  Creation requests count against
`stack-core-remaining-api-requests'!")


;;; Filter compilation

(defun stack-core-compile-filter (&optional include exclude base)
  "Compile a StackExchange filter including fields from INCLUDE,
excluding those from EXCLUDE, using BASE as a base filter.

INCLUDE and EXCLUDE must both be lists; BASE should be a symbol
or string."
  (let ((keyword-arguments
	 `((include . ,(if include (mapconcat
				    #'stack-core-thing-as-string
				    include ";")))
	   (exclude . ,(if exclude (mapconcat
				    #'stack-core-thing-as-string
				    exclude ";")))
	   (base    . ,(if base base)))))
    (let ((response (stack-core-make-request "filter/create" keyword-arguments)))
      (url-hexify-string
       (cdr (assoc 'filter (elt (cdr (assoc 'items response)) 0)))))))

(provide 'stack-filter)
;;; stack-filter.el ends here
