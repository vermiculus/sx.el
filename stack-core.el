;;; stack-core.el --- core functions for stack-mode  -*- lexical-binding: t; -*-

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

;; This file defines basic commands used by all other parts of
;; StackMode.  Currently, there are sections that are pretty wildly
;; different from each other (e.g. `Filters' and `Questions'.  These
;; will eventually be migrated to their own files with related functions
;; of their ilk -- for now, it is more convenient to keep them handy.

;;; Code:


;;; Requirements
(require 'json)
(require 'url)


;;; Package Logging

(defun stack-message (format-string &rest args)
  (message "[stack] %s" (apply #'format format-string args)))


;;; Constants and Customizable Options

(defconst stack-core-api-version
  "2.2"
  "The current version of the API.")

(defconst stack-core-api-root
  (format "http://api.stackexchange.com/%s/" stack-core-api-version)
  "The base URL to make requests from.")

(defcustom stack-core-default-keyword-arguments-alist
  '(("filters/create" . nil)
    (t . ((site . emacs))))
  "An alist of keywords to use as the default for a given method.
This collection is in itself an alist; the key is the method and
the value is an alist of the default arguments for this method.
The value for `t' is the default-default... a super-default, if
you will.

See `stack-core-get-default-keyword-arguments' and
`stack-core-build-keyword-arguments'.
")

(defcustom stack-core-cache-requests
  t
  "Cache requests made to the StackExchange API.")

(defcustom stack-core-unzip-program
  "gunzip"
  "program used to unzip the response")

(defvar stack-core-remaining-api-requests
  nil
  "The number of API requests remaining according to the most
recent call.  Set by `stack-core-make-request'.")

(defcustom stack-core-remaining-api-requests-message-threshold
  50
  "After `stack-core-remaining-api-requests' drops below this
number, `stack-core-make-request' will begin printing out the
number of requests left every time it finishes a call.")


;;; Keyword Arguments

(defun stack-core-thing-as-string (thing)
  "Return a string representation of THING.  If THING is already
a string, just return it."
  (cond
   ((stringp thing) thing)
   ((symbolp thing) (symbol-name thing))
   ((numberp thing) (number-to-string thing))))

(defun stack-core-get-default-keyword-arguments (method)
  "Gets the correct keyword arguments for METHOD."
  (let ((entry (assoc method stack-core-default-keyword-arguments-alist)))
    (cdr (or entry (assoc t stack-core-default-keyword-arguments-alist)))))

;;; @todo stack-core-change-default-keyword-arguments
;;;       (method new-keyword-arguments)
;;; @todo stack-core-change-default-keyword-arguments-for-key
;;;       (method key new-value)

(defun stack-core-build-keyword-arguments (alist)
  "Build a \"key=value&key=value&...\"-style string with the elements
of ALIST.  If any value in the alist is `nil', that pair will not
be included in the return.  If you wish to pass a notion of
false, use the symbol `false'.  Each element is processed with
`stack-core-thing-as-string'."
  (mapconcat
   (lambda (pair)
     (concat
      (stack-core-thing-as-string (car pair))
      "="
      (stack-core-thing-as-string (cdr pair))))
   (delq nil (mapcar
	      (lambda (pair)
		(when (cdr pair) pair))
	      alist))
   "&"))


;;; Making Requests of StackExchange

(defun stack-core-build-request (method keyword-arguments)
  "Build the request string that will be used to process REQUEST
with the given KEYWORD-ARGUMENTS."
  (let ((base (concat stack-core-api-root method))
	(args (stack-core-build-keyword-arguments keyword-arguments)))
    (if (string-equal "" args)
	base
      (concat base "?" args))))

(defun stack-core-make-request (method &optional keyword-arguments filter)
  "Make a request to the StackExchange API using METHOD and
optional KEYWORD-ARGUMENTS.  If no KEYWORD-ARGUMENTS are given,
`stack-core-default-keyword-arguments-alist' is used.  Return the
entire response as a complex alist."
  (let ((response
	 (json-read-from-string
	  (let ((call (stack-core-build-request
		       method
		       (cons `(filter . ,(cond
					  (filter filter)
					  ((boundp 'stack-filter)
					   stack-filter)))
			     (if keyword-arguments keyword-arguments
			       (stack-core-get-default-keyword-arguments
				method)))))
		(url-automatic-caching stack-core-cache-requests))
	    ;; TODO: url-retrieve-synchronously can return nil if the call is
	    ;; unsuccessful should handle this case
	    (stack-message "Request: %s" call)
	    (with-current-buffer (url-retrieve-synchronously call)
	      (goto-char (point-min))
	      (if (not (search-forward "\n\n" nil t))
		  (error "Response corrupted")
		(delete-region (point-min) (point))
		(buffer-string)))))))
    (setq stack-core-remaining-api-requests
	  (cdr (assoc 'quota_remaining response)))
    (when (< stack-core-remaining-api-requests
	     stack-core-remaining-api-requests-message-threshold)
      (stack-message "%d API requests remaining"
		     stack-core-remaining-api-requests))
    response))

(provide 'stack-core)
;;; stack-core.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
