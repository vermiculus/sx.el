;;; sx-request.el --- Requests and url manipulation.  -*- lexical-binding: t; -*-

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
;; When at all possible, use `sx-method-call'.  There are specialized
;; cases for the use of `sx-request-make' outside of sx-method.el, but
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

(defcustom sx-request-unzip-program
  "gunzip"
  "Program used to unzip the response if it is compressed.
This program must accept compressed data on standard input."
  :group 'sx
  :type 'string)

(defvar sx-request-remaining-api-requests
  nil
  "The number of API requests remaining.
Set by `sx-request-make'.")

(defcustom sx-request-remaining-api-requests-message-threshold
  50
  "Lower bound for printed warnings of API usage limits.
After `sx-request-remaining-api-requests' drops below this
number, `sx-request-make' will begin printing out the
number of requests left every time it finishes a call."
  :group 'sx
  :type 'integer)


;;; Making Requests

(defun sx-request-make (method &optional args request-method)
  "Make a request to the API, executing METHOD with ARGS.
You should almost certainly be using `sx-method-call' instead of
this function. REQUEST-METHOD is one of `GET' (default) or `POST'.

Returns cleaned response content.
See (`sx-encoding-clean-content-deep').

The full set of arguments is built with
`sx-request--build-keyword-arguments', prepending
`sx-request-api-key' to receive a higher quota. It will also
include user's `access_token` if it is avaialble. This call is
then resolved with `url-retrieve-synchronously' to a temporary
buffer that it returns.  The headers are then stripped using a
search a blank line (\"\\n\\n\").  The main body of the response
is then tested with `sx-encoding-gzipped-buffer-p' for
compression.  If it is compressed, `sx-request-unzip-program' is
called to uncompress the response.  The uncompressed respons is
then read with `json-read-from-string'.

`sx-request-remaining-api-requests' is updated appropriately and
the main content of the response is returned."
  (let* ((url-automatic-caching t)
         (url-inhibit-uncompression t)
         (url-request-data (sx-request--build-keyword-arguments args nil))
         (request-url (concat sx-request-api-root method))
         (url-request-method request-method)
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (response-buffer (url-retrieve-synchronously request-url)))
      (if (not response-buffer)
          (error "Something went wrong in `url-retrieve-synchronously'")
        (with-current-buffer response-buffer
          (let* ((data (progn
                         ;; @TODO use url-http-end-of-headers
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
              (sx-encoding-clean-content-deep .items)))))))

(defun sx-request-fallback (method &optional args request-method)
  "Fallback method when authentication is not available.
This is for UI generation when the associated API call would
require authentication.

Currently returns nil."
  '(()))


;;; Support Functions
(defun sx-request--build-keyword-arguments (alist &optional kv-sep)
  "Format ALIST as a key-value list joined with KV-SEP.
If authentication is needed, include it also or error if it is
not available.

Build a \"key=value&key=value&...\"-style string with the elements
of ALIST.  If any value in the alist is nil, that pair will not
be included in the return.  If you wish to pass a notion of
false, use the symbol `false'.  Each element is processed with
`sx--thing-as-string'."
  ;; Add API key to list of arguments, this allows for increased quota
  ;; automatically.
  (let ((api-key (cons "key" sx-request-api-key))
        (auth (car (sx-cache-get 'auth))))
    (push api-key alist)
    (when auth
      (push auth alist))
    (mapconcat
     (lambda (pair)
       (concat
        (sx--thing-as-string (car pair))
        "="
        (sx--thing-as-string (cdr pair) kv-sep t)))
     (delq nil (mapcar
                (lambda (pair)
                  (when (cdr pair) pair))
                alist))
     "&")))


(provide 'sx-request)
;;; sx-request.el ends here
