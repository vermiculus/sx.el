;;; sx-tab.el --- functions for viewing different tabs  -*- lexical-binding: t; -*-

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

;; This file provides a single macro to define 'tabs' to view lists of
;; questions.

;;; Tabs:

;; - FrontPage          :: The standard front page
;; - Newest             :: Newest questions
;; - TopVoted           :: Top-voted questions
;; - Hot                :: Hot questions recently
;; - Week               :: Hot questions for the week
;; - Month              :: Hot questions for the month
;; - Unanswered         :: Unanswered questions
;; - Unanswered My-tags :: Unanswered questions (subscribed tags)
;; - Featured           :: Featured questions
;; - Starred            :: Favorite questions

;;; Code:

(require 'sx)
(require 'sx-question-list)
(require 'sx-interaction)

(defvar sx-tab--list nil
  "List of the names of all defined tabs.")

(defun sx-tab-switch (tab)
  "Switch to another question-list tab."
  (interactive
   (list (sx-completing-read
          "Switch to tab: " sx-tab--list
          (lambda (tab) (not (equal tab sx-question-list--current-tab)))
          t)))
  (funcall (intern (format "sx-tab-%s" (downcase tab)))))

(defconst sx-tab--order-methods
  `(,@(default-value 'sx-question-list--order-methods)
    ("Hottest Now"     . hot)
    ("Weekly Hottest"  . week)
    ("Monthly Hottest" . month))
  "Alist of possible values to be passed to the `sort' keyword.")

(defcustom sx-tab-default-order 'activity
  "Default ordering method used on `sx-tab-questions' and the likes.
Possible values are the cdrs of `sx-tab--order-methods'."
  :type (cons 'choice
              (mapcar (lambda (c) `(const :tag ,(car c) ,(cdr c)))
                (cl-remove-duplicates
                 sx-tab--order-methods
                 :key #'cdr))))

(defconst sx-tab--docstring-format
  "Display a list of %s questions for SITE.
The variable `sx-tab-default-order' can be used to customize the
sorting of the resulting list.

NO-UPDATE (the prefix arg) is passed to `sx-question-list-refresh'.
If SITE is nil, use `sx-default-site'.")


;;; The main macro
(defmacro sx-tab--define (tab pager &optional printer refresher obsolete
                              &rest body)
  "Define a StackExchange tab called TAB.
TAB is a capitalized string.

This defines a command `sx-tab-TAB' for displaying the tab,
and a variable `sx-tab--TAB-buffer' for holding the bufer.

The arguments PAGER, PRINTER, and REFRESHER, if non-nil, are
respectively used to set the value of the variables
`sx-question-list--print-function',
`sx-question-list--refresh-function', and
`sx-question-list--next-page-function'.

BODY is evaluated after activating the mode and setting these
variables, but before refreshing the display.

If OBSOLETE is non-nil, it should be a string indicating the tab
to use instead."
  (declare (indent 1) (debug t))
  (let* ((name (downcase tab))
         (buffer-variable
          (intern (format "sx-tab--%s-buffer"
                    (if obsolete (downcase obsolete)
                      name))))
         (function-name
          (intern (concat "sx-tab-" name)))
         (use-instead
          (when obsolete (intern (concat "sx-tab-" (downcase obsolete))))))
    `(progn
       ,(unless obsolete
          `(defvar ,buffer-variable nil
             ,(format "Buffer where the %s questions are displayed." tab)))
       (defun ,function-name (&optional no-update site)
         ,(format sx-tab--docstring-format tab)
         (interactive
          (list current-prefix-arg
                (sx--interactive-site-prompt)))
         (sx-initialize)
         (unless site (setq site sx-default-site))
         ;; Create the buffer
         (unless (buffer-live-p ,buffer-variable)
           (setq ,buffer-variable
                 (generate-new-buffer
                  ,(format "*question-list: %s *" (or obsolete tab)))))
         ;; Fill the buffer with content.
         (with-current-buffer ,buffer-variable
           (sx-question-list-mode)
           (when ,printer (setq sx-question-list--print-function ,printer))
           (when ,refresher (setq sx-question-list--refresh-function ,refresher))
           (setq sx-question-list--next-page-function ,pager)
           (setq sx-question-list--site site)
           (setq sx-question-list--order 'activity)
           (setq sx-question-list--current-tab ,(or obsolete tab))
           ,@body
           (sx-question-list-refresh 'redisplay no-update))
         (switch-to-buffer ,buffer-variable))
       ,(when obsolete
          `(make-obsolete ',function-name ',use-instead nil))
       ;; Add this tab to the list of existing tabs. So we can prompt
       ;; the user with completion and stuff.
       (unless ,obsolete
         (add-to-list 'sx-tab--list ,tab)))))


;;; FrontPage
(sx-tab--define "FrontPage"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . activity)))))
;;;###autoload
(autoload 'sx-tab-frontpage
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Newest
(sx-tab--define "Newest"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . creation)))))
;;;###autoload
(autoload 'sx-tab-newest
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)



;;; TopVoted
(sx-tab--define "TopVoted"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . votes)))))
;;;###autoload
(autoload 'sx-tab-topvoted
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)



;;; Hot
(sx-tab--define "Hot"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . hot)))))
;;;###autoload
(autoload 'sx-tab-hot
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)



;;; Week
(sx-tab--define "Week"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . week)))))
;;;###autoload
(autoload 'sx-tab-week
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)



;;; Month
(sx-tab--define "Month"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page '((sort . month)))))
;;;###autoload
(autoload 'sx-tab-month
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Unanswered
(sx-tab--define "Unanswered"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page nil 'unanswered)))
;;;###autoload
(autoload 'sx-tab-unanswered
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Unanswered My-tags
(sx-tab--define "Unanswered-my-tags"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page nil 'unanswered/my-tags)))
;;;###autoload
(autoload 'sx-tab-unanswered-my-tags
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Featured
(sx-tab--define "Featured"
  (lambda (page)
    (sx-question-get-questions
     sx-question-list--site page nil 'featured)))
;;;###autoload
(autoload 'sx-tab-featured
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Starred
(sx-tab--define "Starred"
  (lambda (page)
    (sx-method-call 'me
      :page page
      :site sx-question-list--site
      :auth t
      :submethod 'favorites
      :filter sx-browse-filter)))
;;;###autoload
(autoload 'sx-tab-featured
  (expand-file-name
   "sx-tab"
   (when load-file-name
     (file-name-directory load-file-name)))
  nil t)


;;; Inter-modes navigation
(defun sx-tab-meta-or-main ()
  "Switch to the meta version of a main site, or vice-versa.
Inside a question, go to the frontpage of the site this question
belongs to."
  (interactive)
  (if (and (derived-mode-p 'sx-question-list-mode)
           sx-question-list--site)
      (sx-question-list-switch-site
       (if (string-match "\\`meta\\." sx-question-list--site)
           (replace-match "" :fixedcase nil sx-question-list--site)
         (concat "meta." sx-question-list--site)))
    (sx-tab-frontpage nil (sx--site (sx--data-here 'question)))))

(provide 'sx-tab)
;;; sx-tab.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
