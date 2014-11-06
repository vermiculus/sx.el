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
(require 'time-date)


;;; Package Logging

(defun stack-message (format-string &rest args)
  "Display a message"
  (message "[stack] %s" (apply #'format format-string args)))


;;; Constants and Customizable Options

(defcustom stack-cache-directory
  (expand-file-name ".stackmode" user-emacs-directory)
  "Directory containined cached files and precompiled filters.")

(defconst stack-core-api-version
  "2.2"
  "The current version of the API.")

(defconst stack-core-api-root
  (format "http://api.stackexchange.com/%s/" stack-core-api-version)
  "The base URL to make requests from.")

(defvar stack-core-api-batch-request-separator
  ";"
  "The separator character to use when making batch requests.

Do not change this unless you know what you are doing!")

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
   ((numberp thing) (number-to-string thing))
   ((sequencep thing)
    (mapconcat #'stack-core-thing-as-string
               thing stack-core-api-batch-request-separator))))

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
	       ;; TODO the resolution of `f' is O(2n) in the worst
	       ;; case.  It may be faster to implement the same
	       ;; functionality as a `while' loop to stop looking the
	       ;; list once it has found a match.  Do speed tests.
	       ;; See edfab4443ec3d376c31a38bef12d305838d3fa2e.
	       (let ((f (or (memq (car cons-cell) desired-tree)
			    (assoc (car cons-cell) desired-tree))))
		 (when f
		   (if (and (sequencep (cdr cons-cell))
			    (sequencep (elt (cdr cons-cell) 0)))
		       (cons (car cons-cell)
			     (stack-core-filter-data
		     	      (cdr cons-cell) (cdr f)))
		     cons-cell))))
	     data))))

(defun stack-cache-get-file-name (filename)
  "Expands FILENAME in the context of `stack-cache-directory'."
  (expand-file-name filename stack-cache-directory))

(defun stack-cache-get (cache)
  "Return the data within CACHE.

As with `stack-cache-set', CACHE is a file name within the
context of `stack-cache-directory'."
  (unless (file-exists-p stack-cache-directory)
    (mkdir stack-cache-directory))
  (let ((file (stack-cache-get-file-name cache)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents (stack-cache-get-file-name cache))
        (read (buffer-string))))))

(defun stack-cache-set (cache data)
  "Set the content of CACHE to DATA.

As with `stack-cache-get', CACHE is a file name within the
context of `stack-cache-directory'.

DATA will be written as returned by `prin1'."
  (unless (file-exists-p stack-cache-directory)
    (mkdir stack-cache-directory))
  (write-region (prin1-to-string data) nil
                (stack-cache-get-file-name cache))
  data)

(defvar stack-core--seconds-to-string
  ;; (LIMIT NAME VALUE)
  ;; We use an entry if the number of seconds in question is less than
  ;; LIMIT, but more than the previous entry's LIMIT.
  '((100      "s"  1)
    (6000     "m"  60.0)
    (108000   "h"  3600.0)
    (34560000 "d"  86400.0)
    (nil      "y"  31557600.0))
  "Auxiliary variable used by `stack--time-since'.")

(defun stack--time-since (time)
  "Convert the time interval since TIME (in seconds) to a short string."
  (let ((delay (- (time-to-seconds) time)))
    (concat
     (if (> 0 delay) "-" "")
     (if (= 0 delay) "0s"
       (setq delay (abs delay))
       (let ((sts stack-core--seconds-to-string) here)
         (while (and (car (setq here (pop sts)))
                     (<= (car here) delay)))
         (concat (format "%.0f" (/ delay (car (cddr here))))
                 (cadr here)))))))

(defcustom stack-core-html-entities-plist
  '(Aacute "Á" aacute "á" Acirc "Â" acirc "â" acute "´" AElig "Æ" aelig "æ"
           Agrave "À" agrave "à" alefsym "ℵ" Alpha "Α" alpha "α" amp "&" and "∧"
           ang "∠" apos "'" aring "å" Aring "Å" asymp "≈" atilde "ã" Atilde "Ã"
           auml "ä" Auml "Ä" bdquo "„" Beta "Β" beta "β" brvbar "¦" bull "•"
           cap "∩" ccedil "ç" Ccedil "Ç" cedil "¸" cent "¢" Chi "Χ" chi "χ"
           circ "ˆ" clubs "♣" cong "≅" copy "©" crarr "↵" cup "∪" curren "¤"
           Dagger "‡" dagger "†" darr "↓" dArr "⇓" deg "°" Delta "Δ" delta "δ"
           diams "♦" divide "÷" eacute "é" Eacute "É" ecirc "ê" Ecirc "Ê" egrave "è"
           Egrave "È" empty "∅" emsp " " ensp " " Epsilon "Ε" epsilon "ε" equiv "≡"
           Eta "Η" eta "η" eth "ð" ETH "Ð" euml "ë" Euml "Ë" euro "€"
           exist "∃" fnof "ƒ" forall "∀" frac12 "½" frac14 "¼" frac34 "¾" frasl "⁄"
           Gamma "Γ" gamma "γ" ge "≥" gt ">" harr "↔" hArr "⇔" hearts "♥"
           hellip "…" iacute "í" Iacute "Í" icirc "î" Icirc "Î" iexcl "¡" igrave "ì"
           Igrave "Ì" image "ℑ" infin "∞" int "∫" Iota "Ι" iota "ι" iquest "¿"
           isin "∈" iuml "ï" Iuml "Ï" Kappa "Κ" kappa "κ" Lambda "Λ" lambda "λ"
           lang "〈" laquo "«" larr "←" lArr "⇐" lceil "⌈" ldquo "“" le "≤"
           lfloor "⌊" lowast "∗" loz "◊" lrm "" lsaquo "‹" lsquo "‘" lt "<"
           macr "¯" mdash "—" micro "µ" middot "·" minus "−" Mu "Μ" mu "μ"
           nabla "∇" nbsp "" ndash "–" ne "≠" ni "∋" not "¬" notin "∉"
           nsub "⊄" ntilde "ñ" Ntilde "Ñ" Nu "Ν" nu "ν" oacute "ó" Oacute "Ó"
           ocirc "ô" Ocirc "Ô" OElig "Œ" oelig "œ" ograve "ò" Ograve "Ò" oline "‾"
           omega "ω" Omega "Ω" Omicron "Ο" omicron "ο" oplus "⊕" or "∨" ordf "ª"
           ordm "º" oslash "ø" Oslash "Ø" otilde "õ" Otilde "Õ" otimes "⊗" ouml "ö"
           Ouml "Ö" para "¶" part "∂" permil "‰" perp "⊥" Phi "Φ" phi "φ"
           Pi "Π" pi "π" piv "ϖ" plusmn "±" pound "£" Prime "″" prime "′"
           prod "∏" prop "∝" Psi "Ψ" psi "ψ" quot "\"" radic "√" rang "〉"
           raquo "»" rarr "→" rArr "⇒" rceil "⌉" rdquo "”" real "ℜ" reg "®"
           rfloor "⌋" Rho "Ρ" rho "ρ" rlm "" rsaquo "›" rsquo "’" sbquo "‚"
           scaron "š" Scaron "Š" sdot "⋅" sect "§" shy "" Sigma "Σ" sigma "σ"
           sigmaf "ς" sim "∼" spades "♠" sub "⊂" sube "⊆" sum "∑" sup "⊃"
           sup1 "¹" sup2 "²" sup3 "³" supe "⊇" szlig "ß" Tau "Τ" tau "τ"
           there4 "∴" Theta "Θ" theta "θ" thetasym "ϑ" thinsp " " thorn "þ" THORN "Þ"
           tilde "˜" times "×" trade "™" uacute "ú" Uacute "Ú" uarr "↑" uArr "⇑"
           ucirc "û" Ucirc "Û" ugrave "ù" Ugrave "Ù" uml "¨" upsih "ϒ" Upsilon "Υ"
           upsilon "υ" uuml "ü" Uuml "Ü" weierp "℘" Xi "Ξ" xi "ξ" yacute "ý"
           Yacute "Ý" yen "¥" yuml "ÿ" Yuml "Ÿ" Zeta "Ζ" zeta "ζ" zwj "" zwnj "")
  "Plist of html entities to replace when displaying question titles and other text."
  :type '(repeat (choice symbol string))
  :group 'stack-core)

(defun stack-core--decode-entities (string)
  (let* ((plist stack-core-html-entities-plist)
         (get-function (lambda (s) (let ((ss (substring s 1 -1)))
                                ;; Handle things like &quot;
                                (or (plist-get plist (intern ss))
                                    ;; Handle things like &#39;
                                    (format "%c" (string-to-int
                                                  (substring ss 1))))))))
    (replace-regexp-in-string "&[^; ]*;" get-function string)))

(provide 'stack-core)
;;; stack-core.el ends here
