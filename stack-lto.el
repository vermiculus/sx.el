;;; stack-core.el --- lisp-to-org conversion functions for stack-mode  -*- lexical-binding: t; -*-

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
(require 'stack-core)
(require 'json)

(defun stack-lto--question (data)
  "Return question DATA in a format acceptable by `org-element-interpret-data'.
DATA is a list of cons cells representing a question, as received
by the API and read by `json-read'."
  `(headline (:title ,(cdr (assoc 'title data))
                     :level 1
                     :tags ,(mapcar #'identity (cdr (assoc 'tags data))))
             (section ()
                      (headline (:title "Question" :level 2)
                                ,@(stack-lto--question-answer data))
                      ,@(mapcar #'stack-lto--answer (cdr (assoc 'answers data))))))

(defun stack-lto--answer (data)
  "Return answer DATA in a format acceptable by `org-element-interpret-data'.
DATA is a list of cons cells representing a question, as received
by the API and read by `json-read'."
  ;; Right now this doesn't do anything special. But it should check
  ;; whether the answer is accepted. How do we display that?
  `(headline (:title "Answer" :level 2)
             ,@(stack-lto--question-answer data)))

(defvar stack-lto-body-src-block 
  '(:language "markdown" :switches nil :parameters nil :hiddenp nil) 
  "Properties used on the src-block which represents the body.")

(defun stack-lto--question-answer (data)
  "Process and return the elements of DATA which questions and answers have in common."
  `((property-drawer nil ,(stack-lto--properties data))
    (src-block (:value ,(stack-lto--body data) 
                       . ,stack-lto-body-src-block))))

(defun stack-lto--body (data)
  "Get and cleanup `body_markdown' from DATA."
  (concat 
   (replace-regexp-in-string
    "\r\n" "\n" (cdr (assoc 'body_markdown data)))
   "\n"))

(defvar stack-lto-comment-item
  '(:bullet "- " :checkbox nil :counter nil :hiddenp nil) 
  "Properties used on the items which represent comments.")

(defun stack-lto--comment (data)
  ""
  (let* ((owner (cdr (assoc 'owner data)))
         (owner-name (cdr (assoc 'display_name owner))))
    `(item (:tag ,owner-name . ,stack-lto-comment-item)
           (paragraph () ,(cdr (assoc 'body_markdown data))))))

(defun stack-lto--properties (data)
  ""
  nil)

(provide 'stack-lto)
;;; stack-core.el ends here
