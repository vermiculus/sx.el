;;; sx-auth.el --- user authentication  -*- lexical-binding: t; -*-

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

(require 'sx)
(require 'sx-request)
(require 'sx-cache)

(defconst sx-auth-root
  "https://stackexchange.com/oauth/dialog")
(defconst sx-auth-redirect-uri
  "http://vermiculus.github.io/stack-mode/auth/auth.htm")
(defconst sx-auth-client-id
  "3291")
(defvar sx-auth-access-token
  nil
  "Your access token.

This is needed to use your account to write questions, make
comments, and read your inbox.  Do not alter this unless you know
what you are doing!")

(defun sx-auth-authenticate ()
  "Authenticate this application.

Authentication is required to read your personal data (such as
notifications) and to write with the API (asking and answering
questions)."
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

(provide 'sx-auth)
;;; sx-auth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
