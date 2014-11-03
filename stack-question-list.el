;;; stack-question-list.el --- Major-mode for navigating questions list.  -*- lexical-binding: t; -*-

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
(require 'stack-question)
(require 'tabulated-list)


;;; Mode Definition
(define-derived-mode stack-question-list-mode tabulated-list-mode "Question List"
  "Major mode for browsing a list of questions from stack-exchange.
Letters do not insert themselves; instead, they are commands.
\\<stack-question-list>
\\{stack-question-list}"
  (hl-line-mode 1)
  (stack-question-list--update-mode-line)
  (setq tabulated-list-format
        `[("Vote" 4 nil) ("Answ" 4 nil) ("Date" 4 nil) ("Title" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" nil))
  (add-hook 'tabulated-list-revert-hook
            #'stack-question-list--refresh-question-list nil t)
  (add-hook 'tabulated-list-revert-hook
            #'stack-question-list--update-mode-line nil t)
  (tabulated-list-init-header))

(mapc
 (lambda (x) (define-key stack-question-list-mode-map
          (car x) (cadr x)))
 '(("j" stack-question-list-next)
   ("k" stack-question-list-previous)
   ([RET] stack-question-list-display-question)))

(defun stack-question-list--update-mode-line ()
  "Fill the mode-line with useful information."
  nil)

(defun stack-question-list--refresh-question-list ()
  ""
  ;; Obviously this needs to be changed.
  (let ((question-list ))
    ;; Print the result.
    (setq tabulated-list-entries
          (mapcar #'stack-question-list--print-info question-list))))

(defun stack-question-list--print-info (data)
  "Convert `json-read' DATA into tabulated-list format."
  )

(defun stack-question-list-previous (n)
  "Hide this question, move to previous one, display it."
  (interactive "p")
  (stack-question-list-next (- n)))

(defun stack-question-list-next (n)
  "Hide this question, move to next one, display it."
  (interactive "p")
  (next-line n)
  (stack-question-list-display-question))

(defcustom stack-question-list-height 10
  "Height, in lines, of stack-mode's *question-list* buffer."
  :type 'integer
  :group 'stack-question-list)

(defun stack-question-list-display-question (&optional data focus)
  "Display question given by DATA.
If called interactively (or with DATA being nil), display
question under point.
Also when called interactively (or when FOCUS is non-nil), also
focus the relevant window."
  (interactive '(nil t))
  (unless data (setq data (tabulated-list-get-id)))
  (unless data (error "No question here!"))
  (let ((window stack-question--window))
    (unless (window-live-p window)
      (setq window
            (split-window-below stack-question-list-height)))
    (stack-question--display data window)
    (when focus
      (select-window window))))

(provide 'stack-question-list)
;;; stack-question-list.el ends here
