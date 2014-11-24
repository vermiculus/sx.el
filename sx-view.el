;;; sx-view.el --- User-level functions for viewing frontpages.       -*- lexical-binding: t; -*-

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

;;


;;; Code:

(require 'sx)
(require 'sx-question-list)

(defcustom sx-view-default-site "emacs"
  "Name of the site to use by default when listing questions."
  :type 'string 
  :group 'stack-exchange)

(defmacro sx-view--define-page (page)
  "Define a stack-exchange page called PAGE.
Page is a capitalized string.

This defines a command `sx-view-PAGE' for displaying the page,
and a variable `sx-view--PAGE-buffer' for holding the bufer."
  (declare (indent 1) (debug t))
  (let ((name (downcase page))
        (buffer-variable
         (intern (concat "sx-view--" name "-buffer"))))
    `(progn
       (defvar ,buffer-variable nil
         ,(format "Buffer where the %s questions are displayed."
                  page))
       (defun
           ,(intern (concat "sx-view-" name))
           (&optional no-update site)
         ,(format "Display a list of %s questions for SITE.

NO-UPDATE (the prefix arg) is passed to `sx-question-list-refresh'.
If SITE is nil, use `sx-view-default-site'."
                  page)
         (interactive
          (list current-prefix-arg
                (funcall (if ido-mode #'ido-completing-read #'completing-read)
                  (format "Site (%s): " sx-view-default-site)
                  (sx-site-get-api-tokens) nil t nil
                  sx-view-default-site)))
         (sx-initialize)
         (unless site (setq site sx-view-default-site))
         ;; Create the buffer
         (unless (buffer-live-p ,buffer-variable)
           (setq ,buffer-variable
                 (generate-new-buffer "*question-list*")))
         ;; Fill the buffer with content.
         (with-current-buffer ,buffer-variable
           (sx-question-list-mode)
           (setq sx-question-list--current-site site)
           (setq sx-question-list--current-page ,page)
           (sx-question-list-refresh 'redisplay no-update))
         (switch-to-buffer ,buffer-variable)))))


;;; FrontPage
(sx-view--define-page "FrontPage")

(provide 'sx-view)
;;; sx-view.el ends here
