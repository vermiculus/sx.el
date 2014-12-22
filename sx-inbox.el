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
(require 'sx-question-list)


;;; API
(defvar sx-inbox-filter
  '((inbox_item.answer_id
     inbox_item.body
     inbox_item.comment_id
     inbox_item.creation_date
     inbox_item.is_unread
     inbox_item.item_type
     inbox_item.link
     inbox_item.question_id
     inbox_item.site
     inbox_item.title)
    (site.logo_url
     site.audience
     site.icon_url
     site.high_resolution_icon_url
     site.site_state
     site.launch_date
     site.markdown_extensions
     site.related_sites
     site.styling))
  "Filter used when retrieving inbox items.")

(defcustom sx-inbox-fill-column 40
  "`fill-column' used in `sx-inbox-mode'."
  :type 'integer
  :group 'sx)

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
(defvar sx-inbox--notification-p nil
  "If non-nil, current buffer lists notifications, not inbox.")
(make-variable-buffer-local 'sx-inbox--notification-p)

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
  (toggle-truncate-lines 1)
  (setq fill-column sx-inbox-fill-column)
  (setq sx-question-list--print-function #'sx-inbox--print-info)
  (setq sx-question-list--next-page-function
        (lambda (page) (sx-inbox-get sx-inbox--notification-p page)))
  (setq tabulated-list-format
        [("Type" 30 t nil t) ("Date" 10 t :right-align t) ("Title" 0)])
  (setq header-line-format sx-inbox--header-line)
  (tabulated-list-revert))


;;; Keybinds
(mapc (lambda (x) (define-key sx-inbox-mode-map (car x) (cadr x)))
  '(
    ("t" nil)
    ("a" nil)
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
 `item_type', `link', `question_id', `site', `title'."
  (list
   data
   (sx-assoc-let data
     (vector
      (list
       (concat (capitalize (replace-regexp-in-string "_" " " .item_type))
               (cond (.answer_id " on Answer at:")
                     (.question_id " on:")))
       'face 'font-lock-keyword-face)
      (list
       (concat (sx-time-since .creation_date)
               sx-question-list-ago-string)
       'face 'sx-question-list-date)
      (list
       (propertize
        " " 'display
        (concat "\n  " .title "\n"
                (let ((col fill-column))
                  (with-temp-buffer
                    (setq fill-column col)
                    (insert "  " .body)
                    (fill-region (point-min) (point-max))
                    (propertize (buffer-string)
                                'face 'font-lock-function-name-face))))
        'face 'default))))))

(provide 'sx-inbox)
;;; sx-inbox.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
