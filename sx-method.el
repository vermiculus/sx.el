;;; sx-method.el --- method calls                    -*- lexical-binding: t; -*-

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
                                      page
                                      (pagesize 100)
                                      (filter '(()))
                                      auth
                                      (url-method 'get)
                                      get-all
                                      (process-function
                                       #'sx-request-response-get-items)
                                      callback
                                      site)
  "Call METHOD with additional keys.

ID is the id associated with a question, answer, comment, post or
user.
SUBMETHOD is the additional segments of the method.
KEYWORDS are the api parameters.  Some parameters have their own
keywords too, for convenience.  The KEYWORDS argument overrides
parameter specific keywords.
FILTER is the set of filters to control the returned information
AUTH defines how to act if the method or filters require
authentication.
URL-METHOD is either `post' or `get'
SITE is the api parameter specifying the site.
GET-ALL is nil or non-nil
PROCESS-FUNCTION is a response-processing function
PAGE is the page number which will be requested
PAGESIZE is the number of items to retrieve per request, default
100
CALLBACK is a function to be called if the request succeeds.  It
is given the returned result as an argument.

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

If GET-ALL is nil, this method will only return the first (or
specified) page available from this method call.  If t, all pages
will be retrieved (`sx-request-all-stop-when-no-more') .
Otherwise, it is a function STOP-WHEN for `sx-request-all-items'.

If PROCESS-FUNCTION is nil, only the items of the response will
be returned (`sx-request-response-get-items').  Otherwise, it is
a function that processes the entire response (as returned by
`json-read').

See `sx-request-make' and `sx-request-all-items'.

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
                             (when (and site (eq url-method 'get))
                               (prog1
                                   (format "?site=%s" site)
                                 (setq site nil)))))
        (call (if get-all #'sx-request-all-items #'sx-request-make))
        (get-all
         (cond
          ((eq get-all t) #'sx-request-all-stop-when-no-more)
          (t get-all))))
    (lwarn "sx-call-method" :debug "A: %S T: %S. M: %S,%s. F: %S" (equal 'warn auth)
           access-token method-auth full-method filter-auth)
    (unless access-token
      (cond
       ;; 1. Need auth and warn user (interactive use)
       ((and method-auth (equal 'warn auth))
        (sx-user-error
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
    (push `(filter . ,(sx-filter-get-var filter)) keywords)
    (unless (assq 'page keywords)
      (push `(page . ,page) keywords))
    (unless (assq 'pagesize keywords)
      (push `(pagesize . ,pagesize) keywords))
    (when site
      (push `(site . ,site) keywords))
    (let ((result (funcall call
                    full-method
                    keywords
                    url-method
                    (or get-all process-function))))
      (when callback
        (funcall callback result))
      result)))

(defun sx-method-post-from-data (data &rest keys)
  "Make a POST `sx-method-call', deriving parameters from DATA.
KEYS are [KEYWORD VALUE] pairs passed to `sx-method-call', except
the following which are decided by this function:

    METHOD :site and :id are derived from DATA, where METHOD is
           either \"answers\", \"comments\", or \"questions\".
    :url-method is post.
    :filter is `sx-browse-filter'.
    :auth is warn.

As a special exception, if the car of KEYS is not a keyword, it
is assumed to be the :submethod argument."
  (declare (indent 1))
  (sx-assoc-let data
    (apply #'sx-method-call
      (cond (.comment_id "comments")
            (.answer_id "answers")
            (.question_id "questions"))
      :id (or .comment_id .answer_id .question_id)
      :auth 'warn
      :url-method 'post
      :filter sx-browse-filter
      :site .site_par
      (if (keywordp (car keys))
          keys
        (cons :submethod keys)))))

(provide 'sx-method)
;;; sx-method.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
