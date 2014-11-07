;;; sx-lto.el --- lisp-to-org conversion functions  -*- lexical-binding: t; -*-

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


;;; Requirements
(require 'sx)
(require 'org)

(defun sx-lto--question (data)
  "Return question DATA in a format acceptable by `org-element-interpret-data'.
DATA is a list of cons cells representing a question, as received
by the API and read by `json-read'."
  `(headline (:title ,(cdr (assoc 'title data))
                     :level 1
                     :tags ,(mapcar #'identity (cdr (assoc 'tags data))))
             ,(sx-lto--question-answer data)
             ,@(mapcar #'sx-lto--answer (cdr (assoc 'answers data)))))

(defun sx-lto--answer (data)
  "Return answer DATA in a format acceptable by `org-element-interpret-data'.
DATA is a list of cons cells representing a question, as received
by the API and read by `json-read'."
  ;; Right now this doesn't do anything special. But it should check
  ;; whether the answer is accepted. How do we display that?
  `(headline (:title "Answer" :level 2)
             ,(sx-lto--question-answer data)))

(defun sx-lto--question-answer (data)
  "Process and return the elements of DATA which questions and answers have in common."
  (let ((comments
         (mapcar #'sx-lto--comment (cdr (assoc 'comments data)))))
    `(;; Body as a src block (really NOT nice).
      (src-block (:value ,(sx-lto--body data)
                         . ,sx-lto--body-src-block))
      ;; Comments as descriptive lists. If there are no comments, an
      ;; empty list would throw an error.
      ,@(when comments `((plain-list (:type descriptive) ,comments))))))


;;; Body rendering
(defvar sx-lto--body-src-block
  '(:language "markdown" :switches nil :parameters nil :hiddenp nil)
  "Properties used on the markdown src-block which represents the body.")

(defface sx-lto-body
  '((((background light)) :background "Grey90")
    (((background dark)) :background "Grey10"))
  "Face used on the body content of questions and answers."
  :group 'stack-mode-faces)

;;; This is not used ATM since we got rid of HTML. But it can be used
;;; once we start extending markdown mode.
(defcustom sx-lto-bullet (if (char-displayable-p ?•) " •"  " -")
  "Bullet used on the display of lists."
  :type 'string
  :group 'stack-mode)

(defun sx-lto--body (data)
  "Get and cleanup `body_markdown' from DATA."
  (concat
   (replace-regexp-in-string
    "\r\n" "\n" (cdr (assoc 'body_markdown data)))
   "\n"))

;; We need to add padding in case the body contains a * at column 1
;; (which would break org-mode).
(defvar sx-lto--padding
  (propertize "  " 'display "  ")
  "Left-padding added to each line of a body.")

(defvar sx-lto-comment-item
  '(:bullet "- " :checkbox nil :counter nil :hiddenp nil)
  "Properties used on the items which represent comments.")

(defun sx-lto--comment (data)
  ""
  (let* ((owner (cdr (assoc 'owner data)))
         (owner-name (cdr (assoc 'display_name owner))))
    `(item (:tag ,owner-name . ,sx-lto-comment-item)
           (paragraph () ,(cdr (assoc 'body_markdown data))))))

(provide 'sx-lto)
;;; sx.el ends here
