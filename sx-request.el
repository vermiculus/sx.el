;;; sx-request.el --- requests and url manipulation  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Sean Allred

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
This program must accept compressed data on standard input.

This is only used (and necessary) if the function
`zlib-decompress-region' is not defined, which is the case for
Emacs versions < 24.4."
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

(defvar sx-request-all-items-delay 0
  "Delay in seconds with each `sx-request-all-items' iteration.
It is good to use a reasonable delay to avoid rate-limiting.")


;;; Making Requests
(defvar sx--backoff-time nil)

(defun sx-request--wait-while-backoff ()
  (when sx--backoff-time
    (message "Waiting for backoff time: %s" sx--backoff-time)
    (let ((time  (cadr (current-time))))
      (if (> (- sx--backoff-time time) 1000)
          ;; If backoff-time is more than 1000 seconds in the future,
          ;; we've likely just looped around the "least significant"
          ;; bits of `current-time'.
          (setq sx--backoff-time time)
        (when (< time sx--backoff-time)
          (message "Backoff detected, waiting %s seconds" (- sx--backoff-time time))
          (sleep-for (+ 0.3 (- sx--backoff-time time))))))))

(defun sx-request-all-items (method &optional args request-method
                                    stop-when)
  "Call METHOD with ARGS until there are no more items.
STOP-WHEN is a function that takes the entire response and
returns non-nil if the process should stop.

All other arguments are identical to `sx-request-make', but
PROCESS-FUNCTION is given the default value of `identity' (rather
than `sx-request-response-get-items') to allow STOP-WHEN to
access the response wrapper."
  ;; @TODO: Refactor.  This is the product of a late-night jam
  ;; session...  it is not intended to be model code.
  (declare (indent 1))
  (let* ((return-value nil)
         (current-page 1)
         (stop-when (or stop-when #'sx-request-all-stop-when-no-more))
         (process-function #'identity)
         (response
          (sx-request-make method `((page . ,current-page) ,@args)
                           request-method process-function)))
    (while (not (funcall stop-when response))
      (let-alist response
        (setq current-page (1+ current-page)
              return-value
              (nconc return-value .items)))
      (sleep-for sx-request-all-items-delay)
      (setq response
            (sx-request-make method `((page . ,current-page) ,@args)
                             request-method process-function)))
    (nconc return-value
           (cdr (assoc 'items response)))))

;;; NOTE: Whenever this is arglist changes, `sx-request-fallback' must
;;; also change.
(defun sx-request-make (method &optional args request-method process-function)
  "Make a request to the API, executing METHOD with ARGS.
You should almost certainly be using `sx-method-call' instead of
this function. REQUEST-METHOD is one of `get' (default) or `post'.

Returns the entire response as processed by PROCESS-FUNCTION.
This defaults to `sx-request-response-get-items'.

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
  (declare (indent 1))
  (sx-request--wait-while-backoff)
  (let* ((url-automatic-caching t)
         (url-inhibit-uncompression t)
         (url-request-data (sx-request--build-keyword-arguments args nil))
         (request-url (concat sx-request-api-root method))
         (url-request-method (and request-method (upcase (symbol-name request-method))))
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
               (data
                ;; Turn string of bytes into string of characters. See
                ;; http://emacs.stackexchange.com/q/4100/50
                (decode-coding-string
                 (if (not response-zipped-p) data
                   (if (fboundp 'zlib-decompress-region)
                       (zlib-decompress-region (point-min) (point-max))
                     (shell-command-on-region
                      (point-min) (point-max)
                      sx-request-unzip-program nil t))
                   (buffer-string))
                 'utf-8 'nocopy))
               ;; @TODO should use `condition-case' here -- set
               ;; RESPONSE to 'corrupt or something
               (response (with-demoted-errors "`json' error: %S"
                           (let ((json-false nil)
                                 (json-array-type 'list)
                                 (json-null :null))
                             (json-read-from-string data)))))
          (kill-buffer response-buffer)
          (when (not response)
            (error "Invalid response to the url request: %s" data))
          ;; If we get here, the response is a valid data structure
          (sx-assoc-let response
            (when .error_id
              (error "Request failed: (%s) [%i %s] %S"
                .method .error_id .error_name .error_message))
            (when .backoff
              (message "Backoff received %s" .backoff)
              (setq sx--backoff-time (+ (cadr (current-time)) .backoff)))
            (when (< (setq sx-request-remaining-api-requests .quota_remaining)
                     sx-request-remaining-api-requests-message-threshold)
              (sx-message "%d API requests remaining"
                          sx-request-remaining-api-requests))
            (funcall (or process-function #'sx-request-response-get-items)
              response)))))))

(defun sx-request-fallback (_method &optional _args _request-method _process-function)
  "Fallback method when authentication is not available.
This is for UI generation when the associated API call would
require authentication.

Currently returns nil."
  '(()))


;;; Our own generated data
(defconst sx-request--data-url-format
  "https://raw.githubusercontent.com/vermiculus/sx.el/data/data/%s.el"
  "Url of the \"data\" directory inside the SX `data' branch.")

(defun sx-request--read-buffer-data ()
  "Return the buffer contents after any url headers.
Error if url headers are absent or if they indicate something
went wrong."
  (goto-char (point-min))
  (unless (string-match "200" (thing-at-point 'line))
    (error "Page not found."))
  (if (not (search-forward "\n\n" nil t))
      (error "Headers missing; response corrupt")
    (prog1 (buffer-substring (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun sx-request-get-url (url &optional callback)
  "Fetch and return data stored online at URL.
If CALLBACK is nil, fetching is done synchronously and the
data (buffer contents sans headers) is returned as a string.

Otherwise CALLBACK must be a function of a single argument.  Then
`url-retrieve' is called asynchronously and CALLBACK is passed
the retrieved data."
  (let* ((url-automatic-caching t)
         (url-inhibit-uncompression t)
         (url-request-method "GET")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (callback-internal
          (when callback
            ;; @TODO: Error check in STATUS.
            (lambda (_status)
              (funcall callback (sx-request--read-buffer-data)))))
         (response-buffer
          (if callback (url-retrieve url callback-internal nil 'silent)
            (url-retrieve-synchronously url))))
    (unless callback
      (if (not response-buffer)
          (error "Something went wrong in `url-retrieve-synchronously'")
        (with-current-buffer response-buffer
          (sx-request--read-buffer-data))))))

(defun sx-request-get-data (file)
  "Fetch and return data stored online by SX.
FILE is a string or symbol, the name of the file which holds the
desired data, relative to `sx-request--data-url-format'.  For
instance, `tags/emacs' returns the list of tags on Emacs.SE."
  (read (sx-request-get-url
         (format sx-request--data-url-format file))))


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


;;; Response Processors
(defun sx-request-response-get-items (response)
  "Returns the items from RESPONSE."
  (sx-assoc-let response
    (sx-encoding-clean-content-deep .items)))

(defun sx-request-all-stop-when-no-more (response)
  (or (not response)
      (not (cdr (assoc 'has_more response)))))

(provide 'sx-request)
;;; sx-request.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
