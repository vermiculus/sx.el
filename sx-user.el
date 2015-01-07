;;; sx-user.el --- handling and printing user information  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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


;;; Code:
(require 'sx)
(require 'sx-button)

(defgroup sx-user nil
  "Customization group for sx-question-mode."
  :prefix "sx-user-"
  :tag "SX Question Mode"
  :group 'sx)

(defvar sx--user-format-property-alist
  '((?d face sx-question-mode-author)
    (?r face sx-question-mode-reputation)
    (?a face sx-question-mode-accept-rate))
  "Alist relating % constructs with text properties.
See `sx--user-format'.")

(defun sx--user-format (format-string user)
  "Use FORMAT-STRING to format the user object USER.
The value is a copy of FORMAT-STRING, but with certain constructs
replaced by text that describes the specified USER:

%d is the display name.
%l is the link to the profile.
%r is the reputation.
%a is the accept rate.

The returned string is additionally propertized as a button with
the `sx-button-user' category."
  (sx-assoc-let (append user sx-question-mode-fallback-user)
    (let* ((text (sx-format-replacements
                  format-string
                  `((?d . ,.display_name)
                    (?l . ,.link)
                    (?r . ,.reputation)
                    (?a . ,.accept_rate))
                  sx--format-user-property-alist)))
      (if link
          (insert-text-button text
                              ;; For visiting and stuff.
                              'sx-button-url  link
                              'sx-button-copy link
                              :type 'sx-button-user)
        text))))

(provide 'sx-user)
;;; sx-user.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
