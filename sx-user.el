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
  "How users are displayed by SX."
  :prefix "sx-user-"
  :tag "SX User"
  :group 'sx)

(defcustom sx-question-mode-fallback-user
  '(
    (about_me                  . "")
    (accept_rate               . -1)
    (account_id                . -1)
    (age                       . -1)
    (answer_count              . -1)
    (badge_counts              . ((bronze . -1) (silver . -1) (gold . -1)))
    (creation_date             . -1)
    (display_name              . "(unknown user)")
    (down_vote_count           . -1)
    (is_employee               . nil)
    (last_access_date          . -1)
    (last_modified_date        . -1)
    (link                      . "")
    (location                  . "")
    (profile_image             . ":(")
    (question_count            . -1)
    (reputation                . -1)
    (reputation_change_day     . -1)
    (reputation_change_month   . -1)
    (reputation_change_quarter . -1)
    (reputation_change_week    . -1)
    (reputation_change_year    . -1)
    (timed_penalty_date        . -1)
    (up_vote_count             . -1)
    (user_id                   . -1)
    (user_type                 . does_not_exist)
    (view_count                . -1)
    (website_url               . "")
    )
  "The structure used to represent missing user information.
NOOTE: SX relies on this variable containing all necessary user
information.  You may edit any of its fields, but you'll run into
errors if you remove them."
  :type '(alist :options ((about_me string)
                          (accept_rate integer)
                          (account_id integer)
                          (age integer)
                          (answer_count integer)
                          (badge_counts alist)
                          (creation_date integer)
                          (display_name string)
                          (down_vote_count integer)
                          (is_employee boolean)
                          (last_access_date integer)
                          (last_modified_date integer)
                          (link string)
                          (location string)
                          (profile_image string)
                          (question_count integer)
                          (reputation integer)
                          (reputation_change_day integer)
                          (reputation_change_month integer)
                          (reputation_change_quarter integer)
                          (reputation_change_week integer)
                          (reputation_change_year integer)
                          (timed_penalty_date integer)
                          (up_vote_count integer)
                          (user_id integer)
                          (user_type symbol)
                          (view_count integer)
                          (website_url string)))
  :group 'sx-user)


;;; Text properties
(defface sx-user-name
  '((t :inherit font-lock-builtin-face))
  "Face used for user names."
  :group 'sx-user)

(defface sx-user-reputation
  '((t :inherit font-lock-comment-face))
  "Face used for user reputations."
  :group 'sx-user)

(defface sx-user-accept-rate
  '((t))
  "Face used for user accept-rates."
  :group 'sx-user)

(defvar sx-user--format-property-alist
  `((?d button ,(list t) category ,(button-category-symbol 'sx-button-user) face sx-user-name)
    (?n button ,(list t) category ,(button-category-symbol 'sx-button-user) face sx-user-name)
    (?@ button ,(list t) category ,(button-category-symbol 'sx-button-user) face sx-user-name)
    (?r face sx-user-reputation)
    (?a face sx-user-accept-rate))
  "Alist relating % constructs with text properties.
See `sx-user--format'.")


;;; Formatting function
(defun sx-user--format (format-string user)
  "Use FORMAT-STRING to format the user object USER.
The value is a copy of FORMAT-STRING, but with certain constructs
replaced by text that describes the specified USER:

%d is the display name.
%@ is the display name in a format suitable for @mentions.
%l is the link to the profile.
%r is the reputation.
%a is the accept rate.

The string replaced in each of these construct is also given the
text-properties specified in `sx-user--format-property-alist'.
Specially, %d and %@ are turned into buttons with the
`sx-button-user' category."
  (sx-assoc-let (append user sx-question-mode-fallback-user)
    (let* ((text (sx-format-replacements
                  format-string
                  `((?d . ,\.display_name)
                    (?n . ,\.display_name)
                    (?l . ,\.link)
                    (?r . ,\.reputation)
                    (?a . ,\.accept_rate)
                    (?@ . ,(when (string-match "%@" format-string)
                             (sx-user--@name .display_name)))
                    )
                  sx-user--format-property-alist)))
      (if (< 0 (string-width .link))
          (propertize text
                      ;; For visiting and stuff.
                      'sx-button-url  .link
                      'sx-button-copy .link)
        text))))


;;; @name conversion
(defconst sx-user--ascii-replacement-list
  '(("[:space:]"  . "")
    ("àåáâäãåą"   .  "a")
    ("èéêëę"      .  "e")
    ("ìíîïı"      .  "i")
    ("òóôõöøőð"   .  "o")
    ("ùúûüŭů"     .  "u")
    ("çćčĉ"       .  "c")
    ("żźž"        .  "z")
    ("śşšŝ"       .  "s")
    ("ñń"         .  "n")
    ("ýÿ"         .  "y")
    ("ğĝ"         .  "g")
    ("ř"          . "r")
    ("ł"          . "l")
    ("đ"          . "d")
    ("ß"          . "ss")
    ("Þ"          . "th")
    ("ĥ"          . "h")
    ("ĵ"          . "j")
    ("^[:ascii:]" . ""))
  "List of replacements to use for non-ascii characters.
Used to convert user names into @mentions.")

(defun sx-user--@name (display-name)
  "Convert DISPLAY-NAME into an @mention.
In order to correctly @mention the user, all whitespace is
removed from DISPLAY-NAME and a series of unicode conversions are
performed before it is returned.
See `sx-user--ascii-replacement-list'.

If all you need is the @name, this is very slightly faster than
using `sx-user--format', but it doesn't do any sanity checking."
  (concat "@" (sx--recursive-replace
               sx-user--ascii-replacement-list display-name)))

(provide 'sx-user)
;;; sx-user.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
