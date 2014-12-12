;;; sx-method.el --- Main interface for API method calls. -*- lexical-binding: t; -*-

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
(require 'sx-auth)
(require 'sx-request)
(require 'sx-filter)

(cl-defun sx-method-call (method &key id
                                      submethod
                                      keywords
                                      (filter '(()))
                                      auth
                                      (url-method "GET")
                                      site)
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

When AUTH is nil, it is assumed that no auth-requiring filters or
methods will be used.  If they are an error will be signaled.  This is
to ensure awareness of where auth is needed.

When AUTH Is t, filters will automatically use a non-auth subset if
no `access_token' is available.  Methods requiring auth will instead
use `sx-request-fallback' rather than have a failed API response.
This is meant to allow for UI pages where portions may require AUTH
but could still be used without authenticating (i.e a launch/home page).

When AUTH is 'warn, methods will signal a `user-error'.  This is meant
for interactive commands that absolutely require authentication
\(submitting questions/answers, reading inbox, etc).  Filters will
treat 'warn as equivalent to t.

Return the entire response as a complex alist."
  (declare (indent 1))
  (let ((access-token (sx-cache-get 'auth))
        (method-auth (sx-auth--method-p method submethod))
        (filter-auth (sx-auth--filter-p filter))
        (full-method (concat (format "%s" method)
                             (when id
                               (format "/%s" id))
                             (when submethod
                               (format "/%s" submethod))
                             ;; On GET methods site is buggy, so we
                             ;; need to provide it as a url argument.
                             (when (and site (string= url-method "GET"))
                               (prog1
                                   (format "?site=%s" site)
                                 (setq site nil)))))
        (call #'sx-request-make)
        parameters)
    (lwarn "sx-call-method" :debug "A: %S T: %S. M: %S,%s. F: %S" (equal 'warn auth)
           access-token method-auth full-method filter-auth)
    (unless access-token
      (cond
       ;; 1. Need auth and warn user (interactive use)
       ((and method-auth (equal 'warn auth))
        (user-error
         "This request requires authentication.  Please run `M-x sx-authenticate' and try again."))
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
          (cons (cons 'filter (sx-filter-get-var filter))
                keywords))
    (when site
      (setq parameters (cons (cons 'site site) parameters)))
    (funcall call
             full-method
             parameters
             url-method)))

(provide 'sx-method)
;;; sx-method.el ends here
