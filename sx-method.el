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
(require 'sx-auth)
(require 'sx-request)
(require 'sx-filter)

(cl-defun sx-method-call (method &key id
                                      submethod
                                      keywords
                                      (filter 'none)
                                      auth
                                      (url-method "GET")
                                      (site sx-site-default))
    "Call METHOD with additional keys.

:ID is the id associated with a question, answer, comment, post or
user.
:SUBMETHOD is the additional segments of the method.
:KEYWORDS are the api parameters.
:FILTER is the set of filters to control the returned information
:AUTH defines how to act if the method or filters require
authentication.
:URL-METHOD is either \"POST\" or \"GET\"
:SITE is the api parameter specifying the site.

Return the entire response as a complex alist."
    (let ((access-token (sx-cache-get 'auth))
          (method-auth (sx-auth--method-p method submethod))
          (filter-auth (sx-auth--filter-p filter))
          (full-method (concat (format "%s" method)
                               (when id
                                 (format "/%s" id))
                               (when submethod
                                 (format "/%s" submethod))))
          (call 'sx-request-make))
      (lwarn "sx-call-method" :warning "A: %S T: %S. M: %S,%s. F: %S" (equal 'warn auth)
	     access-token method-auth full-method filter-auth)
      (unless access-token
        (cond
         ;; 1. Need auth and warn user (interactive use)
         ((and method-auth (equal 'warn auth))
          (user-error
           "This request requires authentication.  Please run `M-x sx-auth-authenticate' and try again."))
         ;; 2. Need auth to populate UI, cannot provide subset
         ((and method-auth auth)
          (setq call 'sx-request-fallback))
         ;; 3. Need auth for type.  Use auth-less filter.
         ((and filter-auth auth)
          (setq filter filter-auth))
         ;; 4. Requires auth but no value set for auth
         ((and (or filter-auth method-auth) (not auth))
          (error "This request requires authentication."))))
      ;; Concatenate all parameters now that filter is ensured.
      (setq parameters
            (cons `(site . ,site)
                  (cons (cons 'filter
                              (sx-filter-get-var filter))
                        keywords)))
      (funcall call
               full-method
               parameters
               url-method)))

(provide 'sx-method)
;;; sx-method.el ends here
