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
  "Display a message"
  (message "[stack] %s" (apply #'format format-string args)))


;;; Constants and Customizable Options

(defconst stack-core-api-version
  "2.2"
  "The current version of the API.")

(defconst stack-core-api-root
  (format "http://api.stackexchange.com/%s/" stack-core-api-version)
  "The base URL to make requests from.")

(defcustom stack-core-default-keyword-arguments-alist
  '(("filters/create")
    ("sites")
    ("questions" (site . emacs))
    (t nil))
  "Keywords to use as the default for a given method.

The first element of each list is the method call the keywords
apply to.  The remaining cons cells (and they must be conses) are
the values for each keyword.

For each list, if no keywords are provided, the method's
arguments are forced to the default as determined by the API.

For each cons cell, if the cdr is `nil', then the keyword will be
forced to the default as determined by the API.

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

(defcustom stack-core-silent-requests
  t
  "When `t', requests default to being silent.")


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

(defun stack-core-make-request
    (method &optional keyword-arguments filter silent)
  "Make a request to the StackExchange API using METHOD and
optional KEYWORD-ARGUMENTS.  If no KEYWORD-ARGUMENTS are given,
`stack-core-default-keyword-arguments-alist' is used.  Return the
entire response as a complex alist."
  (let ((url-automatic-caching stack-core-cache-requests)
	(url-inhibit-uncompression t)
	(silent (or silent stack-core-silent-requests))
	(call
	 (stack-core-build-request
	  method
	  (cons `(filter . ,(cond (filter filter)
				  ((boundp 'stack-filter) stack-filter)))
		(if keyword-arguments keyword-arguments
		  (stack-core-get-default-keyword-arguments method))))))
    ;; TODO: url-retrieve-synchronously can return nil if the call is
    ;; unsuccessful should handle this case
    (unless silent (stack-message "Request: %S" call))
    (let ((response-buffer (cond
			    ((= emacs-minor-version 4)
			     (url-retrieve-synchronously call silent))
			    (t (url-retrieve-synchronously call)))))
      (if (not response-buffer)
	  (error "Something went wrong in `url-retrieve-synchronously'")
	(with-current-buffer response-buffer
	  (let* ((data (progn
			 (goto-char (point-min))
			 (if (not (search-forward "\n\n" nil t))
			     (error "Response headers missing")
			   (delete-region (point-min) (point))
			   (buffer-string))))
		 (response (ignore-errors
			     (json-read-from-string data))))
	    ;; if response isn't nil, the response was in plain text
	    (unless response
	      ;; try to decompress the response
	      (setq response
		    (with-demoted-errors "JSON Error: %s"
		      (shell-command-on-region
		       (point-min) (point-max)
		       stack-core-unzip-program
		       nil t)
		      (json-read-from-string
		       (buffer-substring
			(point-min) (point-max)))))
	      ;; If it still fails, error out
	      (unless response
		(stack-message "Unable to parse response")
		(stack-message "Printing response as message")
		(message "%S" response)
		(error "Response could not be read by json-read-string")))
	    ;; At this point, either response is a valid data structure
	    ;; or we have already thrown an error
	    (when (assoc 'error_id response)
	      (error "Request failed: (%s) [%i %s] %s"
		     method
		     (cdr (assoc 'error_id response))
		     (cdr (assoc 'error_name response))
		     (cdr (assoc 'error_message response))))
	    (when (< (setq stack-core-remaining-api-requests
			   (cdr (assoc 'quota_remaining response)))
		     stack-core-remaining-api-requests-message-threshold)
	      (stack-message "%d API requests remaining"
			     stack-core-remaining-api-requests))
	    (cdr (assoc 'items response))))))))

(defun stack-core-filter-data (data desired-tree)
  "Filters DATA and returns the DESIRED-TREE"
  (if (vectorp data)
      (apply #'vector 
	     (mapcar (lambda (entry)
		       (stack-core-filter-data
			entry desired-tree))
		     data))
    (delq
     nil
     (mapcar (lambda (cons-cell)
	       (let ((f (stack-core-filter-data--item-in-tree
			 (car cons-cell) desired-tree)))
		 (when f
		   (if (and (sequencep (cdr cons-cell))
			    (sequencep (elt (cdr cons-cell) 0)))
		       (cons (car cons-cell)
			     (stack-core-filter-data
		     	      (cdr cons-cell) (cdr f)))
		     cons-cell))))
	     data))))

(defun stack-core-filter-data--item-in-tree (item tree)
  "Check if ITEM is in the direct leaves of TREE

For example, when called with (f 'item '(some item here)), the
return would be `t'.  When called as (f 'item '(some (item here)
in a (deep structure))), `(item here)' would be returned.
"
  (when tree
    (if (equal item (car tree)) tree
      (if (and (listp (car tree))
	       (equal item (caar tree)))
	  (car tree)
	(stack-core-filter-data--item-in-tree item (cdr tree))))))

(provide 'stack-core)
;;; stack-core.el ends here
