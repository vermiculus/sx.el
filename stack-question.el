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

(if (boundp 'production)
    (require 'stack-core)
  (setq load-path (cons "." load-path))
  (load "stack-core.el"))

(defun stack-question-parse (data)
  "Parse and return the questions from DATA as returned by
`stack-core-make-request'"
  (cdr (assoc 'items data)))

(provide 'stack-question)
;;; stack-question.el ends here
