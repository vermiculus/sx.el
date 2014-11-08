;;; sx-request.el --- requests for stack-mode

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <sallred@calamity.tcs.com>
;; Keywords:

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
(require 'json)
(require 'url)
(require 'sx)
(require 'sx-filter)

(defcustom sx-request-silent-p
  t
  "When `t', requests default to being silent.")

(defcustom sx-request-cache-p
  t
  "Cache requests made to the StackExchange API.")

(defcustom sx-request-unzip-program
  "gunzip"
  "program used to unzip the response")

(defvar sx-request-remaining-api-requests
  nil
  "The number of API requests remaining according to the most
recent call.  Set by `sx-request-make'.")

(defcustom sx-request-remaining-api-requests-message-threshold
  50
  "After `sx-request-remaining-api-requests' drops below this
number, `sx-request-make' will begin printing out the
number of requests left every time it finishes a call.")

(defcustom sx-request-default-keyword-arguments-alist
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

See `sx-request-get-default-keyword-arguments' and
`sx-request-build-keyword-arguments'.
")

(defconst sx-request-api-version
  "2.2"
  "The current version of the API.")

(defconst sx-request-api-root
  (format "http://api.stackexchange.com/%s/" sx-request-api-version)
  "The base URL to make requests from.")

(defconst sx-request-api-key
  "0TE6s1tveCpP9K5r5JNDNQ(("
  "When passed, this key provides a higher request quota.")

(defun sx-request-make
    (method &optional keyword-arguments filter silent)
  "Make a request to the StackExchange API using METHOD and
optional KEYWORD-ARGUMENTS.  If no KEYWORD-ARGUMENTS are given,
`sx-default-keyword-arguments-alist' is used.  Return the
entire response as a complex alist."
  (let ((url-automatic-caching sx-request-cache-p)
	(url-inhibit-uncompression t)
	(silent (or silent sx-request-silent-p))
	(call
	 (sx-request--build
	  method
	  (append `((filter . ,(unless (string-equal method "filter/create")
                                 (sx-filter-get-var
                                  (cond (filter filter)
                                        ((boundp 'stack-filter)
                                         stack-filter)))))
                    (key . ,sx-request-api-key))
		(if keyword-arguments keyword-arguments
		  (sx-request--get-default-keyword-arguments method))))))
    ;; TODO: url-retrieve-synchronously can return nil if the call is
    ;; unsuccessful should handle this case
    (unless silent (sx-message "Request: %S" call))
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
		       sx-request-unzip-program
		       nil t)
		      (json-read-from-string
		       (buffer-substring
			(point-min) (point-max)))))
	      ;; If it still fails, error out
	      (unless response
		(sx-message "Unable to parse response")
		(sx-message "Printing response as message")
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
	    (when (< (setq sx-request-remaining-api-requests
			   (cdr (assoc 'quota_remaining response)))
		     sx-request-remaining-api-requests-message-threshold)
	      (sx-message "%d API requests remaining"
			     sx-request-remaining-api-requests))
	    (cdr (assoc 'items response))))))))

(defun sx-request--build (method keyword-arguments &optional kv-value-sep)
  "Build the request string that will be used to process REQUEST
with the given KEYWORD-ARGUMENTS."
  (let ((base (concat sx-request-api-root method))
	(args (sx-request--build-keyword-arguments
               keyword-arguments kv-value-sep)))
    (if (string-equal "" args)
	base
      (concat base "?" args))))

(defun sx-request--build-keyword-arguments (alist &optional kv-value-sep)
  "Build a \"key=value&key=value&...\"-style string with the elements
of ALIST.  If any value in the alist is `nil', that pair will not
be included in the return.  If you wish to pass a notion of
false, use the symbol `false'.  Each element is processed with
`sx--thing-as-string'."
  (mapconcat
   (lambda (pair)
     (concat
      (sx--thing-as-string (car pair))
      "="
      (sx--thing-as-string (cdr pair) kv-value-sep)))
   (delq nil (mapcar
	      (lambda (pair)
		(when (cdr pair) pair))
	      alist))
   "&"))

(defun sx-request--get-default-keyword-arguments (method)
  "Gets the correct keyword arguments for METHOD."
  (let ((entry (assoc method sx-request-default-keyword-arguments-alist)))
    (cdr (or entry (assoc t sx-request-default-keyword-arguments-alist)))))

;;; @todo sx-request-change-default-keyword-arguments
;;;       (method new-keyword-arguments)
;;; @todo sx-request-change-default-keyword-arguments-for-key
;;;       (method key new-value)


(provide 'sx-request)
;;; sx-request.el ends here
