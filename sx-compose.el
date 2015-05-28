;;; sx-compose.el --- major-mode for composing questions and answers  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

;; This file defines `sx-compose-mode' and its auxiliary functions and
;; variables. In order to use `sx-compose-mode', it is vital that the
;; variable `sx-compose--send-function' be set. Otherwise it's just a
;; regular markdown buffer.
;;
;; In order to help avoid mistakes, there is the function
;; `sx-compose-create'. This is the preferred way of activating the
;; mode. It creates a buffer, activates the major mode, and sets the
;; `send-function' variable according to the arguments it is given.


;;; Code:
(require 'markdown-mode)

(require 'sx)
(require 'sx-tag)

(defgroup sx-compose-mode nil
  "Customization group for sx-compose-mode."
  :prefix "sx-compose-mode-"
  :tag "SX compose Mode"
  :group 'sx)


;;; Faces and Variables
(defvar sx-compose-before-send-hook nil
  "Hook run before POSTing to the API.
Functions are called without arguments and should return non-nil.

Returning nil indicates something went wrong and the sending will
be aborted. In this case, the function is responsible for
notifying the user.

Current buffer is the compose-mode buffer whose content is about
to be POSTed.")

(defvar sx-compose-after-send-functions nil
  "Hook run after POSTing to the API.
Functions on this hook should take two arguments, the
`sx-compose-mode' buffer (which not be live) and the data
returned by `sx-compose--send-function' (usually the object
created by the API). They are only called if the transaction
succeeds.")

(defvar sx-compose--send-function nil
  "Function used by `sx-compose-send' to send the data.
Is invoked between `sx-compose-before-send-hook' and
`sx-compose-after-send-functions'.")

(defconst sx-compose--question-headers
  (concat
   #("Title: " 0 7 (intangible t read-only t rear-nonsticky t))
   "%s"
   #("\n" 0 1 (read-only t))
   #("Tags : " 0 7 (read-only t intangible t rear-nonsticky t))
   "%s"
   #("\n" 0 1 (read-only t rear-nonsticky t))
   #("________________________________________\n"
     0 41 (read-only t rear-nonsticky t intangible t
                     sx-compose-separator t))
   "\n")
  "Headers inserted when composing a new question.
Used by `sx-compose-create'.")

(defconst sx-compose--header-line
  '("    "
    (:propertize "C-c C-c" face mode-line-buffer-id)
    ": Finish and Send"
    (sx-compose--is-question-p
     ("    "
      (:propertize "C-c C-q" face mode-line-buffer-id)
      ": Insert tags"))
    "    "
    (:propertize "C-c C-k" face mode-line-buffer-id)
    ": Discard Draft")
  "Header-line used on `sx-compose-mode' drafts.")

(defvar sx-compose--is-question-p nil
  "Non-nil if this `sx-compose-mode' buffer is a question.")
(make-variable-buffer-local 'sx-compose--is-question-p)

(defvar sx-compose--site nil
  "Site which the curent compose buffer belongs to.")
(make-variable-buffer-local 'sx-compose--site)


;;; Major-mode
(define-derived-mode sx-compose-mode markdown-mode "Compose"
  "Major mode for coposing questions and answers.
Most of the functionality comes from `markdown-mode'. This mode
just implements some extra features related to posting to the
API.

This mode won't function if `sx-compose--send-function' isn't
set.  To make sure you set it correctly, you can create the
buffer with the `sx-compose-create' function.

If creating a question draft, the `sx-compose--is-question-p'
variable should also be set to enable more functionality.

\\<sx-compose-mode>
\\{sx-compose-mode}"
  (setq header-line-format sx-compose--header-line)
  (add-hook 'sx-compose-after-send-functions
    #'sx-compose-quit nil t)
  (add-hook 'sx-compose-after-send-functions
    #'sx-compose--copy-as-kill nil t))

(define-key sx-compose-mode-map "\C-c\C-c" #'sx-compose-send)
(define-key sx-compose-mode-map "\C-c\C-k" #'sx-compose-quit)
(sx--define-conditional-key
    sx-compose-mode-map "\C-c\C-q" #'sx-compose-insert-tags
  sx-compose--is-question-p)

(defun sx-compose-send ()
  "Finish composing current buffer and send it.
Calls `sx-compose-before-send-hook', POSTs the the current buffer
contents to the API, then calls `sx-compose-after-send-functions'."
  (interactive)
  (when (run-hook-with-args-until-failure
         'sx-compose-before-send-hook)
    (let ((result (funcall sx-compose--send-function))
          (buf (current-buffer)))
      (run-hook-wrapped
       'sx-compose-after-send-functions
       (lambda (func)
         (with-demoted-errors
             "[sx] Error encountered AFTER sending post, but the post was sent successfully: %s"
           (funcall func buf result))
         nil)))))

(defun sx-compose-insert-tags ()
  "Prompt for a tag list for this draft and insert them."
  (interactive)
  (save-excursion
    (let* ((old (sx-compose--goto-tag-header))
           (new
            (save-match-data
              (mapconcat
               #'identity
               (sx-tag-multiple-read sx-compose--site "Tags" old)
               " "))))
      (if (match-string 1)
          (replace-match new :fixedcase nil nil 1)
        (insert new)))))


;;; Functions for use in hooks
(defun sx-compose-quit (buffer _)
  "Close BUFFER's window and kill it."
  (interactive (list (current-buffer) nil))
  (when (buffer-live-p buffer)
    (let ((w (get-buffer-window buffer)))
      (when (window-live-p w)
        (ignore-errors (delete-window w))))
    (kill-buffer buffer)))

(defun sx-compose--copy-as-kill (buffer _)
  "Copy BUFFER contents to the kill-ring."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (kill-new (buffer-string)))))

(defun sx-compose--goto-tag-header ()
  "Move to the \"Tags:\" header.
Match data is set so group 1 encompasses any already inserted
tags.  Return a list of already inserted tags."
  (goto-char (point-min))
  (unless (search-forward-regexp
           (rx bol "Tags : " (group-n 1 (* not-newline)) eol)
           (next-single-property-change (point-min) 'sx-compose-separator)
           'noerror)
    (error "No Tags header found"))
  (save-match-data
    (sx--split-string (match-string 1) (rx (any space ",;")))))

(defun sx-compose--check-tags ()
  "Check if tags in current compose buffer are valid."
  (save-excursion
    (let ((invalid-tags
           (sx-tag--invalid-name-p
            sx-compose--site (sx-compose--goto-tag-header))))
      (if invalid-tags
          ;; If the user doesn't want to create the tags, we return
          ;; nil and sending is aborted.
          (y-or-n-p (format "Following tags don't exist. Create them? %s " invalid-tags))
        t))))


;;; Functions to help preparing buffers
(defun sx-compose-create (site parent &optional before-functions after-functions)
  "Create an `sx-compose-mode' buffer.
SITE is the site where it will be posted.

If composing questions, PARENT is nil.
If composing answers, it is the `question_id'.
If editing answers or questions, it should be the alist data
related to that object.

Each element of BEFORE-FUNCTIONS and AFTER-FUNCTIONS are
respectively added locally to `sx-compose-before-send-hook' and
`sx-compose-after-send-functions'."
  (or (integerp parent) (listp parent)
      (error "Invalid PARENT"))
  (let ((is-question
         (and (listp parent)
              (or (null parent)
                  (cdr (assoc 'title parent))))))
    (with-current-buffer (sx-compose--get-buffer-create site parent)
      (sx-compose-mode)
      (setq sx-compose--site site)
      (setq sx-compose--is-question-p is-question)
      (setq sx-compose--send-function
            (if (consp parent)
                (sx-assoc-let parent
                  (lambda () (sx-method-call (cond
                                         (.title 'questions)
                                         (.comment_id 'comments)
                                         (t 'answers))
                          :auth 'warn
                          :url-method 'post
                          :filter sx-browse-filter
                          :site site
                          :keywords (sx-compose--generate-keywords is-question)
                          :id (or .comment_id .answer_id .question_id)
                          :submethod 'edit)))
              (lambda () (sx-method-call 'questions
                      :auth 'warn
                      :url-method 'post
                      :filter sx-browse-filter
                      :site site
                      :keywords (sx-compose--generate-keywords is-question)
                      :id parent
                      :submethod (if parent 'answers/add 'add)))))
      ;; Reverse so they're left in the same order.
      (dolist (it (reverse before-functions))
        (add-hook 'sx-compose-before-send-hook it nil t))
      (dolist (it (reverse after-functions))
        (add-hook 'sx-compose-after-send-functions it nil t))
      (when is-question
        (add-hook 'sx-compose-before-send-hook #'sx-compose--check-tags nil t))
      ;; If the buffer is empty, the draft didn't exist. So prepare the
      ;; question.
      (when (or (string= (buffer-string) "")
                (y-or-n-p "Draft buffer exists. Reset it? "))
        (let ((inhibit-point-motion-hooks t)
              (inhibit-read-only t))
          (erase-buffer)
          (when (consp parent)
            (insert (cdr (assoc 'body_markdown parent))))
          (when is-question
            (sx-compose--print-question-headers
             (when (consp parent) parent))
            (unless (consp parent)
              (goto-char (point-min))
              (goto-char (line-end-position))))))
      ;; Return the buffer
      (current-buffer))))

(defun sx-compose--print-question-headers (question)
  "Print question headers for the compose buffer.
If QUESTION is non-nil, fill the headers with the data from
QUESTION."
  (sx-assoc-let question
    (goto-char (point-min))
    (insert
     (format sx-compose--question-headers
       (or .title "") (mapconcat #'identity .tags " ")))))

(defun sx-compose--generate-keywords (is-question)
  "Reading current buffer, generate a keywords alist.
Keywords meant to be used in `sx-method-call'.

`body' is read as the `buffer-string'. If IS-QUESTION is non-nil,
other keywords are read from the header "
  (goto-char (point-min))
  `(,@(when is-question
        (let ((inhibit-point-motion-hooks t)
              (header-end
               (next-single-property-change
                (point-min) 'sx-compose-separator))
              keywords)
          ;; Read the Title.
          (unless (search-forward-regexp
                   "^Title: *\\(.*\\) *$" header-end 'noerror)
            (error "No Title header found"))
          (push (cons 'title (match-string 1)) keywords)
          ;; And the tags
          (goto-char (point-min))
          (unless (search-forward-regexp "^Tags : *\\([^[:space:]].*\\) *$"
                                         header-end 'noerror)
            (error "No Tags header found"))
          (push (cons 'tags (sx--split-string (match-string 1) "[[:space:],;]"))
                keywords)
          ;; And move past the header so it doesn't get sent.
          (goto-char (next-single-property-change
                      header-end 'sx-compose-separator))
          keywords))
    (body . ,(buffer-substring-no-properties (point) (point-max)))))

(defun sx-compose--get-buffer-create (site data)
  "Get or create a buffer for use with `sx-compose-mode'.
SITE is the site for which composing is aimed (just used to
uniquely identify the buffers).

If DATA is nil, get a fresh compose buffer.
If DATA is an integer, try to find an existing buffer
corresponding to that integer, otherwise create one.
If DATA is an alist (question or answer data), like above but use
the id property."
  (cond
   ((null data)
    (generate-new-buffer
     (format "*sx draft question %s*" site)))
   ((integerp data)
    (get-buffer-create
     (format "*sx draft answer %s %s*"
       site data)))
   (t
    (get-buffer-create
     (sx-assoc-let data
       (format "*sx draft edit %s %s %s*"
         site
         (cond (.title "question")
               (.comment_id "comment")
               (t "answer"))
         (or .comment_id .answer_id .question_id)))))))

(provide 'sx-compose)
;;; sx-compose.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
