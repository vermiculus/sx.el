;;; stack-auth.el --- user authentication for stack-mode  -*- lexical-binding: t; -*-

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

(require 'stack-core)

(defconst stack-auth-root
  "https://stackexchange.com/oauth/dialog")
(defconst stack-auth--redirect-uri
  "http://vermiculus.github.io/stack-mode/auth/auth.htm")
(defconst stack-auth--client-id
  "3291")
(defvar stack-auth-access-token
  nil
  "Your access token.

This is needed to use your account to write questions, make
comments, and read your inbox.  Do not alter this unless you know
what you are doing!")

(defun stack-authenticate ()
  "Authenticate this application.

Authentication is required to read your personal data (such as
notifications) and to write with the API (asking and answering
questions)."
  (interactive)
  (setq
   stack-auth-access-token
   (when (browse-url
          (let ((stack-core-api-root stack-auth-root)
                (stack-core-api-batch-request-separator ","))
            (stack-core-build-request
             nil
             `((client_id . ,stack-auth--client-id)
               (scope . (read_inbox
                         no_expiry
                         write_access))
               (redirect_uri . ,(url-hexify-string
                                 stack-auth--redirect-uri))))))
     (read-string "Enter the access token displayed on the webpage: ")))
  (if (string-equal "" stack-auth-access-token)
      (progn (setq stack-auth-access-token nil)
             (error "You must enter this code to use this client fully"))
    (stack-cache-set "auth.el" `((access-token . ,stack-auth-access-token)))))

(provide 'stack-auth)
;;; stack-auth.el ends here
