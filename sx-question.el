;;; sx-question.el --- question logic for stack-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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
(require 'sx-filter)
(require 'sx-lto)
(require 'sx-method)

(defvar sx-question-browse-filter
  '(nil (user.profile_image shallow_user.profile_image)))

(defun sx-question-get-questions (site &optional page)
  "Get the page PAGE of questions from SITE."
  (sx-method-call
   "questions"
   `((site . ,site)
     (page . ,page))
   sx-question-browse-filter))


;;; Question Properties
(defun sx-question--read-p (question)
  "Non-nil if QUESTION has been read since last updated."
  ;; @TODO:
  (cl-evenp (random)))

(defun sx-question--accepted-answer (question)
  "Return accepted answer in QUESTION, or nil if none."
  ;; @TODO:
  (cl-evenp (random)))

(defun sx-question--mark-read (question)
  "Mark QUESTION as being read, until it is updated again."
  nil)


;;; Displaying a question
(defvar sx-question--window nil
  "Window where the content of questions is displayed.")

(defvar sx-question--buffer nil
  "Buffer being used to display questions.")

(defcustom sx-question-use-html t
  "If nil, markdown is used for the body."
  :type 'boolean
  :group 'sx-question)

(defun sx-question--display (data &optional window)
  "Display question given by DATA on WINDOW.
If WINDOW is nil, use selected one."
  (let ((sx-lto--body-src-block
         (if sx-question-use-html nil
           sx-lto--body-src-block))
        (inhibit-read-only t))
    (with-current-buffer
        (sx-question--display-buffer window)
      (erase-buffer)
      (insert
       (org-element-interpret-data
        (sx-lto--question data)))
      (org-mode)
      (show-all)
      (view-mode)
      (current-buffer))))

(defun sx-question--display-buffer (window)
  "Display and return the buffer used for displaying a question.
Create the buffer if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p sx-question--buffer)
    (setq sx-question--buffer
          (generate-new-buffer "*sx-question*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window sx-question--buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window sx-question--buffer 'visible))
   ;; Neither, so we create the window.
   (t (switch-to-buffer sx-question--buffer)))
  sx-question--buffer)

(provide 'sx-question)
;;; sx-question.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
