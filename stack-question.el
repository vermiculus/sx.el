;;; stack-question.el --- question logic for stack-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
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

;; 


;;; Code:

(require 'stack-core)
(require 'stack-filter)

(defvar stack-question-browse-filter
  (stack-filter-compile
   nil
   '(user.profile_image shallow_user.profile_image)))

(stack-filter-store 'question-browse stack-question-browse-filter)

(defun stack-question-get-questions (site &optional page)
  "Get the page PAGE of questions from SITE."
  (stack-core-make-request
   "questions"
   `((site . ,site)
     (page . ,page))
   stack-question-browse-filter))


;;; Displaying a question
(defvar stack-question--window nil
  "Window where the content of questions is displayed.")

(defvar stack-question--buffer nil
  "Buffer being used to display questions.")

(defcustom stack-question-use-html t
  "If nil, markdown is used for the body."
  :type 'boolean
  :group 'stack-question)

(defun stack-question--display (data window)
  "Display question given by DATA on WINDOW.
If WINDOW is nil, use selected one."
  (let ((stack-lto--body-src-block
         (if stack-question-use-html nil
           stack-lto--body-src-block))
        (inhibit-read-only t))
    (with-current-buffer
        (stack-question--display-buffer window)
      (erase-buffer)
      (insert
       (org-element-interpret-data
        (stack-lto--question data)))
      (org-mode)
      (show-all)
      (view-mode)
      (current-buffer))))

(defun stack-question--display-buffer (window)
  "Display and return the buffer used for displaying a question.
Create the buffer if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p stack-question--buffer)
    (setq stack-question--buffer
          (generate-new-buffer "*stack-question*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window stack-question--buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window stack-question--buffer 'visible))
   ;; Neither, so we create the window.
   (t (switch-to-buffer stack-question--buffer)))
  stack-question--buffer)

(provide 'stack-question)
;;; stack-question.el ends here
