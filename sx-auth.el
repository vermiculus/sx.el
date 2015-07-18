;;; sx-auth.el --- user authentication               -*- lexical-binding: t; -*-

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

;; This file handles logic related to authentication.  This includes
;; determining if a certain filter requires authentication (via the
;; variable `sx-auth-filter-auth' and function `sx-auth--filter-p'),
;; determining if a method requires authentication (via the variable
;; `sx-auth-method-auth' and function `sx-auth--method-p'), and
;; actually authenticating the user (with `sx-auth-authenticate').

;;; Code:

(require 'sx)
(require 'sx-request)
(require 'sx-cache)

(defconst sx-auth-root
  "https://stackexchange.com/oauth/dialog")
(defconst sx-auth-redirect-uri
  "http://seanallred.com/sx.el/auth/auth.htm")
(defconst sx-auth-client-id
  "3291")
(defvar sx-auth-access-token
  nil
  "Your access token.
This is needed to use your account to write questions, make
comments, and read your inbox.  Do not alter this unless you know
what you are doing!

This variable is set with `sx-auth-authenticate'.")

(defconst sx-auth-method-auth
  '((me . t)
    (inbox . t)
    (notifications . t)
    (events . t)
    (posts (comments add))
    (comments delete
              edit
              flags
              upvote)
    (answers accept
             delete
             downvote
             edit
             flags
             upvote)
    (questions answers
               add
               close
               delete
               downvote
               edit
               favorite
               flags
               render
               upvote
               (unanswered my-tags)))
  "List of methods that require auth.
Methods are of the form \(METHOD . SUBMETHODS) where SUBMETHODS
  is \(METHOD METHOD METHOD ...).

If all SUBMETHODS require auth or there are no submethods, form
will be \(METHOD . t)")

(defconst sx-auth-filter-auth
  '(question.upvoted
    question.downvoted
    answer.upvoted
    answer.downvoted
    comment.upvoted)
  "List of filter types that require auth.
Keywords are of the form \(OBJECT TYPES) where TYPES is \(FILTER
FILTER FILTER).")

;;;###autoload
(defun sx-authenticate ()
  "Authenticate this application.
Authentication is required to read your personal data (such as
notifications) and to write with the API (asking and answering
questions).

When this function is called, `browse-url' is used to send the
user to an authorization page managed by StackExchange.  The
following privileges are requested:

* read_inbox
    use SX to manage and visit items in your inbox

* write_acesss
    write comments, ask questions, and post answers on your
    behalf

* no_expiry
    do not pester you to reauthorize again

After authorization with StackExchange, the user is then
redirected to a website managed by SX.  The access token required
to use authenticated methods is included in the hash (which is
parsed and displayed prominently on the page)."
  (interactive)
  (setq
   sx-auth-access-token
   (let ((url (concat
               sx-auth-root
               "?"
               (sx-request--build-keyword-arguments
                `((client_id . ,sx-auth-client-id)
                  (scope . (read_inbox
                            no_expiry
                            private_info
                            write_access))
                  (redirect_uri . ,(url-hexify-string
                                    sx-auth-redirect-uri)))
                ","))))
     (browse-url url)
     (read-string "Enter the access token displayed on the webpage: ")))
  (if (string-equal "" sx-auth-access-token)
      (progn (setq sx-auth-access-token nil)
             (error "You must enter this code to use this client fully"))
    (sx-cache-set 'auth `((access_token . ,sx-auth-access-token)))))

(defun sx-auth--method-p (method &optional submethod)
  "Check if METHOD is one that may require authentication.
If it has `auth-required' SUBMETHODs, or no submethod, return t."
  (let ((method-auth (cdr (assoc method sx-auth-method-auth)))
        ;; If the submethod has additional options, they may all be
        ;; eligible, in which case we only need to check the `car'.
        (sub-head (if (listp submethod)
                      (car submethod))))
    (lwarn " sx-auth method" :debug "Method %s requires auth" method-auth)
    (and method-auth
         (or
          ;; All submethods require auth.
          (eq t method-auth)
          ;; All sub-submethods require auth.
          (member sub-head method-auth)
          ;; Specific submethod requires auth.
          (member submethod method-auth)))))

;; Temporary solution.  When we switch to pre-defined filters we will
;; have to change the logic to match against specific filters.
(defun sx-auth--filter-p (filter)
  "Check if FILTER contains properties that require authentication.
If it has `auth-required' properties, return a filter that has
removed those properties."
  (let* ((incl-filter (if (listp filter) (car filter)))
         (rest-filter (if incl-filter (cdr filter)))
         (auth-filters (remove nil
                               ;; Only retrieve the elements that
                               ;; are issues.
                               (mapcar (lambda (prop)
                                         (car
                                          (member prop
                                                  sx-auth-filter-auth)))
                                       (or incl-filter filter))))
         clean-filter out-filter)
    (lwarn "sx-auth filter" :debug "Filter: %S" filter)
    ;; Auth-filters is the filters that are issues
    (when auth-filters
      (setq clean-filter
            (cl-remove-if (lambda (prop)
                            (member prop auth-filters))
                          (or incl-filter filter))))
    (if (and incl-filter clean-filter)
        (setq out-filter
              (cons clean-filter rest-filter))
      (setq out-filter clean-filter))
    (lwarn "sx-auth filter2" :debug "Filter property %s requires auth. %S"
           auth-filters out-filter)
    out-filter))

(provide 'sx-auth)
;;; sx-auth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
