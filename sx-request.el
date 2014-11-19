;;; sx-request.el --- requests and url manipulation  -*- lexical-binding: t; -*-

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

;; API requests are handled on three separate tiers:
;; 
;; `sx-method-call':
;;
;;    This is the function that should be used most often, since it
;;    runs necessary checks (authentication) and provides basic
;;    processing of the result for consistency.
;;
;; `sx-request-make':
;;
;;    This is the fundamental function for interacting with the API.
;;    It makes no provisions for 'common' usage, but it does ensure
;;    data is retrieved successfully or an appropriate signal is
;;    thrown.
;;
;; `url.el' and `json.el':
;;
;;    The whole solution is built upon `url-retrieve-synchronously'
;;    for making the request and `json-read-from-string' for parsing
;;    it into a properly symbolic data structure.
;; 
;; When at all possible, use ~sx-method-call~.  There are specialized
;; cases for the use of ~sx-request-make~ outside of =sx-method.el=, but
;; these must be well-documented inline with the code.

;;; Code:

(require 'url)
(require 'json)

(require 'sx)
(require 'sx-encoding)


;;; Variables

(defconst sx-request-api-key
  "0TE6s1tveCpP9K5r5JNDNQ(("
  "When passed, this key provides a higher request quota.")

(defconst sx-request-api-version
  "2.2"
  "The current version of the API.")

(defconst sx-request-api-root
  (format "https://api.stackexchange.com/%s/" sx-request-api-version)
  "The base URL to make requests from.")

;;; @TODO Shouldn't this be made moot by our caching system?
(defcustom sx-request-cache-p
  t
  "Cache requests made to the StackExchange API.")

(defcustom sx-request-unzip-program
  "gunzip"
  "Program used to unzip the response if it is compressed.

This program must accept compressed data on standard input.")

(defvar sx-request-remaining-api-requests
  nil
  "The number of API requests remaining.

Set by `sx-request-make'.")

(defcustom sx-request-remaining-api-requests-message-threshold
  50
  "Lower bound for printed warnings of API usage limits.

After `sx-request-remaining-api-requests' drops below this
number, `sx-request-make' will begin printing out the
number of requests left every time it finishes a call.")


;;; Making Requests

(defun sx-request-make
    (method &optional args need-auth use-post)
  "Make a request to the API, executing METHOD with ARGS.

You should almost certainly be using `sx-method-call' instead of
this function.

Returns cleaned response content.
See (`sx-encoding-clean-content-deep').

The full call is built with `sx-request-build', prepending
`sx-request-api-key' to receive a higher quota.  This call is
then resolved with `url-retrieve-synchronously' to a temporary
buffer that it returns.  The headers are then stripped using a
search a blank line (\"\\n\\n\").  The main body of the response
is then tested with `sx-encoding-gzipped-buffer-p' for
compression.  If it is compressed, `sx-request-unzip-program' is
called to uncompress the response.  The uncompressed respons is
then read with `json-read-from-string'.

`sx-request-remaining-api-requests' is updated appropriately and
the main content of the response is returned."
  (let ((url-automatic-caching sx-request-cache-p)
        (url-inhibit-uncompression t)
        (request-method (if use-post "POST" "GET"))
        (request-args
         (sx-request--build-keyword-arguments args nil need-auth))
        (request-url (concat sx-request-api-root method)))
    (sx-message "Request: %S" request-url)
    (let ((response-buffer (sx-request--request request-url
                                                request-args
                                                request-method)))
      (if (not response-buffer)
          (error "Something went wrong in `url-retrieve-synchronously'")
        (with-current-buffer response-buffer
          (let* ((data (progn
                         (goto-char (point-min))
                         (if (not (search-forward "\n\n" nil t))
                             (error "Headers missing; response corrupt")
                           (delete-region (point-min) (point))
                           (buffer-string))))
                 (response-zipped-p (sx-encoding-gzipped-p data))
                 (data (if (not response-zipped-p) data
                         (shell-command-on-region
                          (point-min) (point-max)
                          sx-request-unzip-program
                          nil t)
                         (buffer-string)))
                 ;; @TODO should use `condition-case' here -- set
                 ;; RESPONSE to 'corrupt or something
                 (response (with-demoted-errors "`json' error: %S"
                             (json-read-from-string data))))
            (when (and (not response) (string-equal data "{}"))
              (sx-message "Unable to parse response: %S" response)
              (error "Response could not be read by `json-read-from-string'"))
            ;; If we get here, the response is a valid data structure
            (sx-assoc-let response
              (when .error_id
                (error "Request failed: (%s) [%i %s] %S"
                       .method .error_id .error_name .error_message))
              (when (< (setq sx-request-remaining-api-requests .quota_remaining)
                       sx-request-remaining-api-requests-message-threshold)
                (sx-message "%d API requests reamining"
                            sx-request-remaining-api-requests))
              (sx-encoding-clean-content-deep .items))))))))


;;; Support Functions

(defun sx-request--request (url args method)
  "Return the response buffer for URL with ARGS using METHOD."
  (let ((url-request-method method)
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data args))
    (url-retrieve-synchronously url)))


(defun sx-request--build-keyword-arguments (alist &optional
						  kv-sep need-auth)
  "Format ALIST as a key-value list joined with KV-SEP.

If authentication is needed, include it also or error if it is
not available.

Build a \"key=value&key=value&...\"-style string with the elements
of ALIST.  If any value in the alist is `nil', that pair will not
be included in the return.  If you wish to pass a notion of
false, use the symbol `false'.  Each element is processed with
`sx--thing-as-string'."
  ;; Add API key to list of arguments, this allows for increased quota
  ;; automatically.
  (let* ((warn (equal need-auth 'warn))
         (api-key (cons "key" sx-request-api-key))
         (auth
          (let ((auth (car (sx-cache-get 'auth))))
            (cond
             (auth)
             ;; Pass user error when asking to warn
             (warn
              (user-error
               "This query requires authentication; run `M-x sx-auth-authenticate' and try again"))
             ((not auth)
              (lwarn "stack-mode" :debug
                     "This query requires authentication")
              nil)))))
    (push api-key alist)
    (if (and need-auth auth)
        (push auth alist))
    (mapconcat
     (lambda (pair)
       (concat
        (sx--thing-as-string (car pair))
        "="
        (sx--thing-as-string (cdr pair) kv-sep)))
     (delq nil (mapcar
                (lambda (pair)
                  (when (cdr pair) pair))
                alist))
     "&")))


(provide 'sx-request)
;;; sx-request.el ends here
