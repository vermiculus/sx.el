;;; stack-navigation-mode.el --- Major-mode for navigating questions list.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: help, hypermedia, mail, news, tools


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


;;; Requirements
(define-derived-mode stack-navigation-mode tabulated-list-mode "Question List")

(mapc
 (lambda (x) (define-key stack-navigation-mode-map
          (car x) (cadr x)))
 '(("j" stack-navigation-mode-next-question)
   ("k" stack-navigation-mode-previous-question)
   ("n" outline-next-visible-heading)
   ("p" outline-previous-visible-heading)
   ))

(defun stack-navigation-mode-previous-question (n)
  "Hide this question, move to previous one, display it."
  (interactive "p")
  (stack-navigation-mode-next-question (- n)))

(defun stack-navigation-mode-next-question (n)
  "Hide this question, move to next one, display it."
  (interactive "p")
  (org-global-cycle 1)
  (org-forward-heading-same-level n 'invis-ok)
  (org-show-subtree))

(provide 'stack-navigation-mode)
;;; stack-navigation-mode.el ends here
