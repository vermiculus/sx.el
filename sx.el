;;; sx.el --- StackExchange client. Ask and answer questions on Stack Overflow, Super User, and the likes  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; URL: https://github.com/vermiculus/sx.el/
;; Version: 0.3
;; Keywords: help, hypermedia, tools
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (json "1.3") (markdown-mode "2.0") (let-alist "1.0.3"))

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

;; This file defines basic commands used by all other parts of SX.

;;; Code:
(require 'tabulated-list)

(defconst sx-version "0.3" "Version of the `sx' package.")

(defgroup sx nil
  "Customization group for the `sx' package."
  :prefix "sx-"
  :tag "SX"
  :group 'applications)


;;; User commands
(defun sx-version ()
  "Print and return the version of the `sx' package."
  (interactive)
  (message "%s: %s" 'sx-version sx-version)
  sx-version)

;;;###autoload
(defun sx-bug-report ()
  "File a bug report about the `sx' package."
  (interactive)
  (browse-url "https://github.com/vermiculus/sx.el/issues/new"))


;;; Site
(defun sx--site (data)
  "Get the site in which DATA belongs.
DATA can be a question, answer, comment, or user (or any object
with a `link' property).
DATA can also be the link itself."
  (let ((link (if (stringp data) data
                (cdr (assoc 'link data)))))
    (when (stringp link)
      (replace-regexp-in-string
       (rx string-start
           "http" (optional "s") "://"
           (or
            (sequence
             (group-n 1 (+ (not (any "/"))))
             ".stackexchange")
            (group-n 2 (+ (not (any "/")))))
           "." (+ (not (any ".")))
           "/" (* any)
           string-end)
       "\\1\\2" link))))

(defun sx--ensure-site (data)
  "Add a `site' property to DATA if it doesn't have one.  Return DATA.
DATA can be a question, answer, comment, or user (or any object
with a `link' property)."
  (when data
    (let-alist data
      (unless .site_par
        ;; @TODO: Change this to .site.api_site_parameter sometime
        ;; after February.
        (setcdr data (cons (cons 'site_par
                                 (or (cdr (assq 'api_site_parameter .site))
                                     (sx--site data)))
                           (cdr data)))))
    data))

(defun sx--link-to-data (link)
  "Convert string LINK into data that can be displayed."
  (let ((result (list (cons 'site_par (sx--site link)))))
    ;; Try to strip a question or answer ID
    (when (cond ;; Comment
           ((or ;; If there's a #commentNUMBER_NUMBER at the end, we
             ;; know it's a comment with that ID.
             (string-match (rx "#comment" (group-n 1 (+ digit))
                               "_" (+ digit) string-end)
                           link)
             ;; From inbox items
             (string-match (rx "/posts/comments/"
                               ;; Comment ID
                               (group-n 1 (+ digit))
                               ;; Optional stuff at the end
                               (or (and (any "?#") (* any)) "")
                               string-end)
                           link))
            (push '(type . comment) result))
           ;; Answer
           ((or ;; If there's a #NUMBER at the end, we know it's an
             ;; answer with that ID.
             (string-match (rx "#" (group-n 1 (+ digit)) string-end) link)
             ;; From 'Share' button
             (string-match (rx "/a/"
                               ;; Answer ID
                               (group-n 1 (+ digit)) "/"
                               ;; User ID
                               (+ digit)
                               ;; Garbage at the end
                               (optional (and (any "?#") (* any)))
                               string-end)
                           link)
             ;; From URL
             (string-match (rx "/questions/" (+ digit) "/"
                               ;; Question title
                               (+ (not (any "/"))) "/"
                               ;; Answer ID. If this is absent, we match on
                               ;; Question clause below.
                               (group-n 1 (+ digit))
                               (opt "/")
                               ;; Garbage at the end
                               (optional (and (any "?#") (* any)))
                               string-end)
                           link))
            (push '(type . answer) result))
           ;; Question
           ((or ;; From 'Share' button
             (string-match (rx "/q/"
                               ;; Question ID
                               (group-n 1 (+ digit))
                               ;; User ID
                               (optional "/" (+ digit))
                               ;; Garbage at the end
                               (optional (and (any "?#") (* any)))
                               string-end)
                           link)
             ;; From URL
             (string-match (rx "/questions/"
                               ;; Question ID
                               (group-n 1 (+ digit)) "/")
                           link))
            (push '(type . question) result)))
      (push (cons 'id (string-to-number (match-string-no-properties 1 link)))
            result))
    result))

(defun sx--tree-paths (tree)
  "Return a list of all paths in TREE.
Adapted from http://stackoverflow.com/q/3019250."
  (if (atom tree)
      (list (list tree))
    (apply #'append
           (mapcar (lambda (node)
                     (mapcar (lambda (path)
                               (cons (car tree) path))
                             (sx--tree-paths node)))
                   (cdr tree)))))

(defun sx--tree-expand (path-func tree)
  "Apply PATH-FUNC to every path in TREE.
Return the result.  See `sx--tree-paths'."
  (mapcar path-func
          (apply #'append
                 (mapcar #'sx--tree-paths
                         tree))))

(defmacro sx-assoc-let (alist &rest body)
  "Use ALIST with `let-alist' to execute BODY.
`.site_par' has a special meaning, thanks to `sx--ensure-site'.
If ALIST doesn't have a `site' property, one is created using the
`link' property."
  (declare (indent 1) (debug t))
  (require 'let-alist)
  `(progn
     (sx--ensure-site ,alist)
     ,(macroexpand
       `(let-alist ,alist ,@body))))

(defun sx--pretty-site-parameter (site)
  "Returned a pretty and capitalized version of string SITE."
  (mapconcat #'capitalize
             (split-string site "\\.")
             " "))


;;; Utility Functions
(defun sx--split-string (string &optional separators)
  "Split STRING into substrings bounded by matches for SEPARATORS."
  (mapcar (lambda (s) (replace-regexp-in-string "\\` +\\| +\\'" "" s))
    (split-string string separators 'omit-nulls)))

(defun sx-completing-read (&rest args)
  "Like `completing-read', but possibly use ido.
All ARGS are passed to `completing-read' or `ido-completing-read'."
  (apply (if ido-mode #'ido-completing-read #'completing-read)
    args))

(defun sx-user-error (format-string &rest args)
  "Like `user-error', but prepend FORMAT-STRING with \"[sx]\".
See `format'."
  (signal 'user-error
          (list (apply #'format (concat "[sx] " format-string) args))))

(defun sx-message (format-string &rest args)
  "Display FORMAT-STRING as a message with ARGS.
See `format'."
  (message "[sx] %s" (apply #'format format-string args)))

(defun sx-message-help-echo ()
  "If there's a 'help-echo property under point, message it."
  (let ((echo (get-text-property (point) 'help-echo)))
    (when echo (message "%s" echo))))

(defun sx--thing-as-string (thing &optional sequence-sep url-hexify)
  "Return a string representation of THING.
If THING is already a string, just return it.

Optional argument SEQUENCE-SEP is the separator applied between
elements of a sequence.  If SEQUENCE-SEP is a list, use the first
element for the top level joining, the second for the next level,
etc.  \";\" is used as a default.

If optional argument URL-HEXIFY is non-nil, this function behaves
as `url-hexify-string'; this option is only effective on strings
and sequences of strings."
  (let ((process (if url-hexify #'url-hexify-string #'identity))
        (first-f (if (listp sequence-sep) #'car #'identity))
        (rest-f (if (listp sequence-sep) #'cdr #'identity)))
    (cond
     ((stringp thing) (funcall process thing))
     ((symbolp thing) (funcall process (symbol-name thing)))
     ((numberp thing) (number-to-string thing))
     ((sequencep thing)
      (mapconcat (lambda (thing)
                   (sx--thing-as-string
                    thing (funcall rest-f sequence-sep) url-hexify))
                 thing (if sequence-sep
                           (funcall first-f sequence-sep)
                         ";"))))))

(defun sx--shorten-url (url)
  "Shorten URL hiding anything other than the domain.
Paths after the domain are replaced with \"...\".
Anything before the (sub)domain is removed."
  (replace-regexp-in-string
   ;; Remove anything after domain.
   (rx (group-n 1 (and (1+ (any word ".")) "/"))
       (1+ anything) string-end)
   (eval-when-compile
     (concat "\\1" (if (char-displayable-p ?…) "…" "...")))
   ;; Remove anything before subdomain.
   (replace-regexp-in-string
    (rx string-start (or (and (0+ word) (optional ":") "//")))
    "" url)))

(defmacro sx--define-conditional-key (keymap key def &rest body)
  "In KEYMAP, define key sequence KEY as DEF conditionally.
This is like `define-key', except the definition \"disappears\"
whenever BODY evaluates to nil."
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  `(define-key ,keymap ,key
     '(menu-item
       ,(format "maybe-%s" (or (car (cdr-safe def)) def)) ignore
       :filter (lambda (&optional _)
                 (when (progn ,@body) ,def)))))

(defun sx--goto-property-change (prop &optional direction)
  "Move forward to the next change of text-property PROP.
Return the new value of PROP at point.

If DIRECTION is negative, move backwards instead."
  (let ((func (if (and (numberp direction)
                       (< direction 0))
                  #'previous-single-property-change
                #'next-single-property-change))
        (limit (if (and (numberp direction)
                        (< direction 0))
                   (point-min) (point-max))))
    (goto-char (funcall func (point) prop nil limit))
    (get-text-property (point) prop)))

(defun sx--find-in-buffer (type id)
  "Move point to an object of TYPE and ID.
That is, move forward from beginning of buffer until
`sx--data-here' is an object of type TYPE with the respective id
ID.  If point is left at the of a line, move over the line break.

TYPE is either question, answer, or comment.
ID is an integer."
  (let* ((id-symbol (cl-case type
                      (answer 'answer_id)
                      (comment 'comment_id)
                      (question 'question_id)))
         (pos
          (save-excursion
            (goto-char (point-min))
            (while (not (or (eobp)
                            (let ((data (sx--data-here type t)))
                              (and data
                                   (= id (or (cdr (assq id-symbol data))))))))
              (forward-char 1))
            (point))))
    (if (equal pos (point-max))
        (sx-message "Can't find the specified %s" type)
      (goto-char pos)
      (when (looking-at-p "$")
        (forward-char 1)))))

(defmacro sx--create-comparator (name doc compare-func get-func)
  "Define a new comparator called NAME with documentation DOC.
COMPARE-FUNC is a function that takes the return value of
GET-FUNC and performs the actual comparison."
  (declare (indent 1) (doc-string 2))
  `(defun ,name (a b)
     ,doc
     (funcall ,compare-func
              (funcall ,get-func a)
              (funcall ,get-func b))))

(defun sx--squash-whitespace (string)
  "Return STRING with consecutive whitespace squashed together."
  (replace-regexp-in-string "[ 	\r\n]+" " " string))

(defun sx--deleted-p (data)
  "Return non-nil if DATA represents a deleted object."
  (eq (car data) 'deleted))

(defun sx--invert-predicate (predicate)
  "Return PREDICATE function with arguments inverted.
For instance (sx--invert-predicate #'<) is the same as #'>.
Note this is not the same as negating PREDICATE."
  (lambda (&rest args) (apply predicate (reverse args))))


;;; Printing request data
(defvar sx--overlays nil
  "Overlays created by sx on this buffer.")
(make-variable-buffer-local 'sx--overlays)

(defvar sx--overlay-printing-depth 0
  "Track how many overlays we're printing on top of each other.
Used for assigning higher priority to inner overlays.")
(make-variable-buffer-local 'sx--overlay-printing-depth)

(defmacro sx--wrap-in-overlay (properties &rest body)
  "Start a scope with overlay PROPERTIES and execute BODY.
Overlay is pushed on the buffer-local variable `sx--overlays' and
given PROPERTIES.

Return the result of BODY."
  (declare (indent 1)
           (debug t))
  `(let ((p (point-marker))
         (result (progn ,@body))
         ;; The first overlay is the shallowest. Any overlays created
         ;; while the first one is still being created go deeper and
         ;; deeper.
         (sx--overlay-printing-depth (1+ sx--overlay-printing-depth)))
     (let ((ov (make-overlay p (point)))
           (props ,properties))
       (while props
         (overlay-put ov (pop props) (pop props)))
       ;; Let's multiply by 10 just in case we ever want to put
       ;; something in the middle.
       (overlay-put ov 'priority (* 10 sx--overlay-printing-depth))
       (push ov sx--overlays))
     result))

(defun sx--recursive-replace (alist string)
  "Replace each car of ALIST with its cdr in STRING."
  (if alist
      (sx--recursive-replace
       (cdr alist)
       (let ((kar (car alist)))
         (replace-regexp-in-string
          (format "[%s]" (car kar)) (cdr kar) string)))
    string))

(defun sx-format-replacements (format alist &optional property-alist)
  "Use FORMAT-STRING to format the values in ALIST.
ALIST is a list with elements of the form (CHAR . STRING).
The value is a copy of FORMAT-STRING, but with certain constructs
replaced by text as given by ALIST.  

The construct is a `%' character followed by any other character.
The replacement is the STRING corresponding to CHAR in ALIST.  In
addition, if CHAR is also the car of an element in
PROPERTY-ALIST, the cdr of that element should be a list of text
properties which will be applied on the replacement.

The %% construct is special, it is replaced with a single %, even
if ALIST contains a different string at the ?% entry."
  (let ((alist (cons '(?% . "%") alist)))
    (with-temp-buffer
      (insert format)
      (goto-char (point-min))
      (while (search-forward-regexp
              (rx "%" (group-n 1 (* (any "-+ #0-9.")))) nil 'noerror)
        (let* ((char (char-after))
               ;; Understand flags
               (flag (match-string 1))
               (val (cdr-safe (assq char alist))))
          (unless val
            (error "Invalid format character: `%%%c'" char))
          ;; Insert first, to preserve text properties.
          (insert-and-inherit (format (concat "%" flag "s") val))
          (when property-alist
            (add-text-properties (match-end 0) (point)
                                 (cdr-safe (assq char property-alist))))
          ;; Delete the specifier body.
          (delete-region (match-beginning 0)
                         (match-end 0))
          ;; Delete `char-after'.
          (delete-char 1)))
      (buffer-string))))


;;; Key definitions
(defun sx--key-definitions-to-header-line (definitions)
  "Return a `header-line-format' from DEFINITIONS.
DEFINITIONS is a list where each element has one of the following two forms
    (KEY COMMAND)
    (KEY COMMAND DESCRIPTION)

The latter are used to build the return value, the former are
ignored."
  (let ((ptize (lambda (x) `(:propertize ,x face mode-line-buffer-id)))
        alist out)
    (dolist (it definitions)
      (when (> (length it) 2)
        (let* ((key (car it))
               (desc (elt it 2))
               (cell (assoc desc alist)))
          (if cell (push key (cdr cell))
            (push (cons desc (list key)) alist)))))
    (dolist (it alist out)
      (let ((desc (car it))
            (keys (cdr it)))
        (push (list "   "
                    (cons (funcall ptize (car keys))
                          (mapcar (lambda (k) `("," ,(funcall ptize k))) (cdr keys)))
                    (let ((match
                           (and (= 1 (length keys))
                                (string-match (regexp-quote (car keys)) desc))))
                      (if (and (numberp match) (= 0 match))
                          (substring desc (length (car keys)))
                        (concat ":" desc))))
              out)))))


(defcustom sx-init-hook nil
  "Hook run when SX initializes.
Run after `sx-init--internal-hook'."
  :group 'sx
  :type 'hook)

(defvar sx-init--internal-hook nil
  "Hook run when SX initializes.
This is used internally to set initial values for variables such
as filters.")

(defmacro sx-init-variable (variable value &optional setter)
  "Set VARIABLE to VALUE using SETTER.
SETTER should be a function of two arguments.  If SETTER is nil,
`set' is used."
  (eval
   `(add-hook
     'sx-init--internal-hook
     (lambda ()
       (,(or setter #'setq) ,variable ,value))))
  nil)

(defvar sx-initialized nil
  "Nil if sx hasn't been initialized yet.
If it has, holds the time at which initialization happened.")

(defun sx-initialize (&optional force)
  "Run initialization hooks if they haven't been run yet.
These are `sx-init--internal-hook' and `sx-init-hook'.

If FORCE is non-nil, run them even if they've already been run."
  (when (or force (not sx-initialized))
    (prog1
        (run-hooks 'sx-init--internal-hook
                   'sx-init-hook)
      (setq sx-initialized (current-time)))))

(provide 'sx)
;;; sx.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
