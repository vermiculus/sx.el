;;; sx.el --- StackExchange client. Ask and answer questions on Stack Overflow, Super User, and the likes. -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; URL: https://github.com/vermiculus/sx.el/
;; Version: 0.1
;; Keywords: help, hypermedia, tools
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (json "1.3") (markdown-mode "2.0") (let-alist "1.0.1"))

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

(defconst sx-version "0.1" "Version of the `sx' package.")

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
  "Add a `site' property to DATA if it doesn't have one. Return DATA.
DATA can be a question, answer, comment, or user (or any object
with a `link' property)."
  (when data
    (unless (assq 'site data)
      (setcdr data (cons (cons 'site (sx--site data))
                         (cdr data))))
    data))

(defun sx--link-to-data (link)
  "Convert string LINK into data that can be displayed."
  (let ((result (list (cons 'site (sx--site link)))))
    ;; Try to strip a question or answer ID
    (when (or
           ;; Answer
           (and (or (string-match
                     ;; From 'Share' button
                     (rx "/a/"
                         ;; Question ID
                         (group (+ digit))
                         ;; User ID
                         "/" (+ digit)
                         ;; Answer ID
                         (group (or (sequence "#" (* any)) ""))
                         string-end) link)
                    (string-match
                     ;; From URL
                     (rx "/questions/" (+ digit) "/"
                         (+ (not (any "/"))) "/"
                         ;; User ID
                         (optional (group (+ digit)))
                         (optional "/")
                         (group (or (sequence "#" (* any)) ""))
                         string-end) link))
                (push '(type . answer) result))
           ;; Question
           (and (or (string-match
                     ;; From 'Share' button
                     (rx "/q/"
                         ;; Question ID
                         (group (+ digit))
                         ;; User ID
                         (optional "/" (+ digit))
                         ;; Answer or Comment ID
                         (group (or (sequence "#" (* any)) ""))
                         string-end) link)
                    (string-match
                     ;; From URL
                     (rx "/questions/"
                         ;; Question ID
                         (group (+ digit))
                         "/") link))
                (push '(type . question) result)))
      (push (cons 'id (string-to-number (match-string-no-properties 1 link)))
            result))
    result))

(defmacro sx-assoc-let (alist &rest body)
  "Identical to `let-alist', except `.site' has a special meaning.
If ALIST doesn't have a `site' property, one is created using the
`link' property."
  (declare (indent 1) (debug t))
  `(progn
     (require 'let-alist)
     (sx--ensure-site ,alist)
     (let-alist ,alist ,@body)))


;;; Browsing filter
(defvar sx-browse-filter
  '((question.body_markdown
     question.comments
     question.answers
     question.last_editor
     question.accepted_answer_id
     question.link
     question.upvoted
     question.downvoted
     question.question_id
     question.share_link
     user.display_name
     comment.owner
     comment.body_markdown
     comment.body
     comment.link
     comment.edited
     comment.creation_date
     comment.upvoted
     comment.score
     comment.post_type
     comment.post_id
     comment.comment_id
     answer.answer_id
     answer.last_editor
     answer.link
     answer.share_link
     answer.owner
     answer.body_markdown
     answer.upvoted
     answer.downvoted
     answer.comments)
    (user.profile_image shallow_user.profile_image))
  "The filter applied when retrieving question data.
See `sx-question-get-questions' and `sx-question-get-question'.")


;;; Utility Functions

(defmacro sx-sorted-insert-skip-first (newelt list &optional predicate)
  "Inserted NEWELT into LIST sorted by PREDICATE.
This is designed for the (site id id ...) lists.  So the first car
is intentionally skipped."
  `(let ((tail ,list)
         (x ,newelt))
     (while (and ;; We're not at the end.
             (cdr-safe tail)
             ;; We're not at the right place.
             (,(or predicate #'<) x (cadr tail)))
       (setq tail (cdr tail)))
     (setcdr tail (cons x (cdr tail)))))

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

(defun sx--filter-data (data desired-tree)
  "Filter DATA and return the DESIRED-TREE.

For example:

  (sx--filter-data
    '((prop1 . value1)
      (prop2 . value2)
      (prop3
       (test1 . 1)
       (test2 . 2))
      (prop4 . t))
    '(prop1 (prop3 test2)))

would yield

  ((prop1 . value1)
   (prop3
    (test2 . 2)))"
  (if (vectorp data)
      (apply #'vector
             (mapcar (lambda (entry)
                       (sx--filter-data
                        entry desired-tree))
                     data))
    (delq
     nil
     (mapcar (lambda (cons-cell)
               ;; @TODO the resolution of `f' is O(2n) in the worst
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
                             (sx--filter-data
                              (cdr cons-cell) (cdr f)))
                     cons-cell))))
             data))))

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

(defvar sx--ascii-replacement-list
  '(("[:space:]" . "")
    ("àåáâäãåą" .  "a")
    ("èéêëę" .  "e")
    ("ìíîïı" .  "i")
    ("òóôõöøőð" .  "o")
    ("ùúûüŭů" .  "u")
    ("çćčĉ" .  "c")
    ("żźž" .  "z")
    ("śşšŝ" .  "s")
    ("ñń" .  "n")
    ("ýÿ" .  "y")
    ("ğĝ" .  "g")
    ("ř" . "r")
    ("ł" . "l")
    ("đ" . "d")
    ("ß" . "ss")
    ("Þ" . "th")
    ("ĥ" . "h")
    ("ĵ" . "j")
    ("^[:ascii:]" . ""))
  "List of replacements to use for non-ascii characters
Used to convert user names into @mentions.")

(defun sx--user-@name (user)
  "Get the `display_name' of USER prepended with @.
In order to correctly @mention the user, all whitespace is
removed from the display name before it is returned."
  (sx-assoc-let user
    (when (stringp .display_name)
      (concat "@" (sx--recursive-replace
                   sx--ascii-replacement-list .display_name)))))

(defun sx--recursive-replace (alist string)
  "Replace each car of ALIST with its cdr in STRING."
  (if alist
      (sx--recursive-replace
       (cdr alist)
       (let ((kar (car alist)))
         (replace-regexp-in-string
          (format "[%s]" (car kar)) (cdr kar) string)))
    string))


(defcustom sx-init-hook nil
  "Hook run when SX initializes.
Run after `sx-init--internal-hook'."
  :group 'sx
  :type 'hook)

(defvar sx-init--internal-hook nil
  "Hook run when SX initializes.
This is used internally to set initial values for variables such
as filters.")

(defun sx--< (property x y &optional predicate)
  "Non-nil if PROPERTY attribute of alist X is less than that of Y.
With optional argument PREDICATE, use it instead of `<'."
  (funcall (or predicate #'<)
           (cdr (assoc property x))
           (cdr (assoc property y))))

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
