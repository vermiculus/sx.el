;;; sx-inbox.el --- Base inbox logic. -*- lexical-binding: t; -*-

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
(require 'sx-filter)
(require 'sx-method)

(defvar sx-inbox-filter 
  '((inbox.answer_id
     inbox.body
     inbox.comment_id
     inbox.creation_date
     inbox.is_unread
     inbox.item_type
     inbox.link
     inbox.question_id
     inbox.site
     inbox.title)
    (site.logo_url
     site.audience
     site.icon_url
     site.high_resolution_icon_url
     site.site_state
     site.launch_date
     site.markdown_extensions
     site.related_sites))
  "Filter used when retrieving inbox items.")

(defun sx-inbox-get (&optional notifications page keywords)
  "Get an array of inbox items for the current user.
If NOTIFICATIONS is non-nil, query from `notifications' method,
otherwise use `inbox' method.

Return an array of items.  Each item is an alist of properties
returned by the API.
See https://api.stackexchange.com/docs/types/inbox-item

KEYWORDS are added to the method call along with PAGE.

`sx-method-call' is used with `sx-inbox-filter'."
  (sx-method-call (if notifications 'notifications 'inbox)
    :keywords keywords
    :filter sx-inbox-filter))


;;; Major-mode
(defvar sx-inbox--unread-inbox nil
  "List of inbox items still unread.")

(defvar sx-inbox--unread-notifications nil
  "List of notifications items still unread.")

(defvar sx-inbox--read-inbox nil
  "List of inbox items which are read.
These are identified by their links.")

(defvar sx-inbox--read-notifications nil
  "List of notification items which are read.
These are identified by their links.")

(defvar sx-inbox--header-line
  '("    "
    (:propertize "n p j k" face mode-line-buffer-id)
    ": Navigate"
    "    "
    (:propertize "RET" face mode-line-buffer-id)
    ": View"
    "    "
    (:propertize "v" face mode-line-buffer-id)
    ": Visit externally"
    "    "
    (:propertize "q" face mode-line-buffer-id)
    ": Quit")
  "Header-line used on the question list.")

(define-derived-mode sx-inbox-mode
  sx-question-list-mode "Question List"
  "Mode used to list inbox and notification items."
  (setq sx-question-list--print-function
        #'sx-inbox--print-info)
  (setq sx-question-list--dataset sx-inbox--unread-inbox)
  (setq tabulated-list-format
        [("Type" 30 t :right-align t)
         ("Date" 10 t :right-align t)
         ("Title" 0 sx-inbox--date-more-recent-p)])
  (setq header-line-format sx-inbox--header-line))


;;; Keybinds
(mapc
    (lambda (x) (define-key sx-inbox-mode-map
             (car x) (cadr x)))
  '(
    ("t" nil)
    ("a" nil)
    ("u" nil)
    ("d" nil)
    ("h" nil)
    ("m" sx-inbox-mark-read)
    ([?\r] sx-display)
    ))


;;; print-info
(defun sx-inbox--print-info (data)
  "Convert `json-read' DATA into tabulated-list format.

This is the default printer used by `sx-inbox'. It assumes DATA
is an alist containing the elements:
 `answer_id', `body', `comment_id', `creation_date', `is_unread',
 `item_type', `link', `question_id', `site', `title'.

Also see `sx-question-list-refresh'."
  (list
   data
   (sx-assoc-let data
     (vector
      (list
       (concat (capitalize (replace-regexp-in-string "_" " " .item_type))
               (cond
                (.answer_id " on Answer at:")
                (.question_id " on:"))))
      (list (propertize (concat (sx-time-since .last_activity_date)
                                sx-question-list-ago-string)
                        'face 'sx-question-list-date))
      (list
       (concat
        (propertize " " 'display "\n")
        .title
        (propertize " " 'display "\n")
        .body))))))

(provide 'sx-inbox)
;;; sx-inbox.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
