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
(require 'cl-lib)
(load "test/tests.el")


;;; Customization
(defcustom stack-question-list-height 15
  "Height, in lines, of stack-mode's *question-list* buffer."
  :type 'integer
  :group 'stack-question-list)

(defface stack-question-list-parent
  '((t :inherit default))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-answers
  '((((background light)) :foreground "SeaGreen4"
     :height 1.0 :inherit stack-question-list-parent)
    (((background dark))  :foreground "#D1FA71"
     :height 1.0 :inherit stack-question-list-parent)
    (t :inherit stack-question-list-parent))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-answers-accepted
  '((((background light)) :background "YellowGreen"
     :inherit stack-question-list-answers)
    (((background dark)) :background "DarkOliveGreen"
     :inherit stack-question-list-answers)
    (t :inherit stack-question-list-answers))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-score
  '((t :height 1.0 :inherit stack-question-list-parent))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-score-upvoted
  '((t :weight bold
       :inherit stack-question-list-score))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-tags
  '((t :inherit font-lock-function-name-face))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-date
  '((t :inherit font-lock-comment-face))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-read-question
  '((t :height 1.0 :inherit stack-question-list-parent))
  ""
  :group 'stack-question-list-faces)

(defface stack-question-list-unread-question
  '((t :weight bold :inherit stack-question-list-read-question))
  ""
  :group 'stack-question-list-faces)


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
            #'stack-question-list-refresh nil t)
  (add-hook 'tabulated-list-revert-hook
            #'stack-question-list--update-mode-line nil t)
  (tabulated-list-init-header))

(mapc
 (lambda (x) (define-key stack-question-list-mode-map
          (car x) (cadr x)))
 '(("j" stack-question-list-next)
   ("k" stack-question-list-previous)
   ("g" stack-question-list-refresh)
   ([?\r] stack-question-list-display-question)))

(defun stack-question-list--update-mode-line ()
  "Fill the mode-line with useful information."
  nil)

(defun stack-question-list-refresh (&optional redisplay)
  "Update the list of questions.
If REDISPLAY is non-nil, also call `tabulated-list-print'."
  (interactive '(t))
  ;; Obviously this needs to be changed.
  (let ((question-list (stack-test-sample-data "questions")))
    ;; Print the result.
    (setq tabulated-list-entries
          (mapcar #'stack-question-list--print-info question-list)))
  (when redisplay (tabulated-list-print 'remember)))

;; (stack-question-list--print-info sample-question-unauthenticated)

(defun stack-question-list--print-info (data)
  "Convert `json-read' DATA into tabulated-list format."
  (cl-flet ((ca (x) (cdr (assoc x data))))
    (list
     data
     (vector (int-to-string (ca 'score))
             (int-to-string (ca 'answer_count))
             (int-to-string (ca 'last_activity_date))
             (ca 'title)))))

(defun stack-question-list-previous (n)
  "Hide this question, move to previous one, display it."
  (interactive "p")
  (stack-question-list-next (- n)))

(defun stack-question-list-next (n)
  "Hide this question, move to next one, display it."
  (interactive "p")
  (next-line n)
  (stack-question-list-display-question))

(defun stack-question-list-display-question (&optional data focus)
  "Display question given by DATA.
If called interactively (or with DATA being nil), display
question under point.
Also when called interactively (or when FOCUS is non-nil), also
focus the relevant window."
  (interactive '(nil t))
  (unless data (setq data (tabulated-list-get-id)))
  (unless data (error "No question here!"))
  (unless (window-live-p stack-question--window)
    (setq stack-question--window
          (split-window-below stack-question-list-height)))
  (stack-question--display data stack-question--window)
  (when focus
    (select-window stack-question--window)))

(defvar stack-question-list--buffer nil
  "Buffer where the list of questions is displayed.")

(defun list-questions (no-update)
  "Display a list of stack-exchange questions."
  (interactive "P")
  (unless (buffer-live-p stack-question-list--buffer)
    (setq stack-question-list--buffer
          (generate-new-buffer "*question-list*")))
  (with-current-buffer stack-question-list--buffer
    (stack-question-list-mode)
    (stack-question-list-refresh 'redisplay))
  ;; The package menu buffer has keybindings.  If the user types
  ;; `M-x list-packages', that suggests it should become current.
  (switch-to-buffer stack-question-list--buffer))

(provide 'stack-question-list)
;;; stack-question-list.el ends here
