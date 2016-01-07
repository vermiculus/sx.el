;;; sx-interaction.el --- voting, commenting, and other interaction  -*- lexical-binding: t; -*-

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

;; This file holds a series of functions for performing arbitrary
;; interactions with arbitrary objects (objects here always mean the
;; alist of a question, answer, or comment). All commands take at
;; least a DATA argument corresponding to the object which, when
;; called interactively, is always derived from the context at point
;; (usually using the `sx--data-here' function).
;;
;; Interactions represented here involve voting, commenting, asking,
;; answering, editing.
;;
;; These are commands are meant to be available throughout the
;; interface. So it didn't make sense to put them in a specific
;; module. They also rely on a lot of dependencies, so they couldn't
;; be put in sx.el.


;;; Code:
(eval-when-compile
  '(require 'cl-lib))

(require 'sx)
(require 'sx-question)
(require 'sx-question-mode)
(require 'sx-question-list)
(require 'sx-compose)
(require 'sx-cache)


;;; Using data in buffer
(defun sx--data-here (&optional type noerror)
  "Get the alist regarding object under point of type TYPE.
Looks at the text property `sx--data-here'. If it's not set, it
looks at a few other reasonable variables. If those fail too, it
throws an error.

TYPE is a symbol restricting the type of object desired. Possible
values are 'question, 'answer, 'comment, or nil (for any type).

If no object of the requested type could be returned, an error is
thrown unless NOERROR is non-nil."
  (or (let ((data (get-char-property (point) 'sx--data-here)))
        (if (null type) data
          (sx-assoc-let data
            ;; Is data of the right type?
            (cl-case type
              (question (when .title data))
              (answer (when .answer_id data))
              (comment (when .comment_id data))))))
      ;; The following two only ever return questions.
      (when (or (null type) (eq type 'question))
        ;; @TODO: `sx-question-list-mode' may one day display answers.
        ;; Ideally, it would use the `sx--data-here' (so no special
        ;; handling would be necessary.
        (or (and (derived-mode-p 'sx-question-list-mode)
                 (tabulated-list-get-id))
            (and (derived-mode-p 'sx-question-mode)
                 sx-question-mode--data)))
      ;; Nothing was found
      (and (null noerror)
           (error "No %s found here" (or type "data")))))

(defun sx--marker-to-data (marker &rest rest)
  "Get the data at MARKER.
REST is passed to `sx--data-here'."
  (save-excursion
    (goto-char marker)
    (apply #'sx--data-here rest)))

(defun sx--error-if-unread (data)
  "Throw a user-error if DATA is an unread question.
If it's not a question, or if it is read, return DATA."
  ;; If we found a question, we may need to check if it's read.
  (if (and (assoc 'title data)
           (null (sx-question--read-p data)))
      (sx-user-error "Question not yet read. View it before acting on it")
    data))

(defun sx--maybe-update-display (&optional buffer site id)
  "Refresh whatever is displayed in BUFFER or the current buffer.
If BUFFER is not live, nothing is done.

If SITE is given but ID isn't, only update if BUFFER appears to
be a question-list displaying SITE.
If both SITE and ID are given, only update if BUFFER appears to
be a question matching SITE and ID."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond ((derived-mode-p 'sx-question-list-mode)
             (when (or (not site)
                       (and (not id)
                            (string= site sx-question-list--site)))
               (sx-question-list-refresh 'redisplay 'no-update)))
            ((derived-mode-p 'sx-question-mode)
             (when (or (not site)
                       (and id
                            (equal
                             (let-alist (sx--data-here 'question)
                               (cons .site_par .question_id))
                             (cons site id))))
               (sx-question-mode-refresh 'no-update)))))))

(defun sx--copy-data (from to)
  "Copy all fields of alist FORM onto TO.
Only fields contained in TO are copied."
  (setcar to (car from))
  (setcdr to (cdr from)))

(defun sx-ensure-authentication ()
  "Signal user-error if the user refuses to authenticate.
Note that `sx-method-call' already does authentication checking.
This function is meant to be used by commands that don't
immediately perform method calls, such as `sx-ask'.  This way,
the unauthenticated user will be prompted before going through
the trouble of composing an entire question."
  (unless (sx-cache-get 'auth)
    (if (y-or-n-p "This command requires authentication, would you like to authenticate? ")
        (sx-authenticate)
      (sx-user-error "This command requires authentication, please run `M-x sx-authenticate' and try again."))))

(defmacro sx--make-update-callback (&rest body)
  "Return a function that runs BODY and updates display.
`sx--maybe-update-display' is only called if the buffer where the
function was created still exists.  In that case, BODY is also
run in this buffer."
  (declare (debug t))
  `(let ((buffer (current-buffer)))
     (lambda (result)
       ;; See http://emacs.stackexchange.com/a/10725/50
       (ignore result)
       (if (buffer-live-p buffer)
           (with-current-buffer buffer
             ,@body
             (sx--maybe-update-display))
         ,@body))))

(defun sx--copy-update-callback (data)
  "Return a function that overwrites DATA and updates display.
First, DATA is destructively overwritten with the car of the
argument passed to the function.  Then,
`sx--maybe-update-display' is called in the original buffer."
  (sx--make-update-callback
   ;; The api returns the new DATA.
   (when result
     (sx--copy-data (elt result 0) data))))


;;; Visiting
(defun sx-visit-externally (data &optional copy-as-kill)
  "Visit DATA in a web browser.
DATA can be a question, answer, or comment. Interactively, it is
derived from point position.

If copy-as-kill is non-nil, do not call `browse-url'.
Instead, copy the link as a new kill with `kill-new'.
Interactively, this is specified with a prefix argument.

If DATA is a question, also mark it as read."
  (interactive (list (sx--data-here) current-prefix-arg))
  (sx-assoc-let data
    (if (not (stringp .link))
        (sx-message "Nothing to visit here.")
      (funcall (if copy-as-kill #'kill-new #'browse-url) .link)
      (when (and (called-interactively-p 'any) copy-as-kill)
        (message "Copied: %S" .link))
      (when (and .title (not copy-as-kill))
        (sx-question--mark-read data)
        (sx--maybe-update-display)))))

(defun sx-open-link (link)
  "Visit element given by LINK inside Emacs.
Element can be a question, answer, or comment."
  (interactive
   (let ((def (with-temp-buffer
                (save-excursion (yank))
                (thing-at-point 'url))))
     (list (read-string (concat "Link (" def "): ") nil nil def))))
  ;; For now, we have no chance of handling chat links, let's just
  ;; send them to the browser.
  (if (string-match (rx string-start "http" (opt "s") "://chat.") link)
      (sx-visit-externally link)
    (let ((data (sx--link-to-data link)))
      (sx-assoc-let data
        (cl-case .type
          (comment
           (sx-display-question
            (sx-question-get-from-comment .site_par .id) 'focus)
           (sx--find-in-buffer 'comment .id))
          (answer
           (sx-display-question
            (sx-question-get-from-answer .site_par .id) 'focus)
           (sx--find-in-buffer 'answer .id))
          (question
           (sx-display-question
            (sx-question-get-question .site_par .id) 'focus))
          (t (error "Don't know how to open this link, please file a bug report: %s"
                    link)
             nil))))))

;;;###autoload
(defun sx-org-get-link ()
  "Add a link to this post to Org's memory."
  (when (memq major-mode '(sx-question-mode sx-question-list-mode))
    (sx-assoc-let (sx--data-here)
      (when .link
        (org-store-link-props :type 'http
                              :link .link
                              :description .title)))))
(eval-after-load "org"
  '(add-to-list 'org-store-link-functions #'sx-org-get-link))


;;; Displaying
(defun sx-display (&optional data)
  "Display object given by DATA.
Interactively, display object under point. Object can be a
question, an answer, or an inbox_item.

This is meant for interactive use. In lisp code, use
object-specific functions such as `sx-display-question' and the
likes."
  (interactive (list (sx--data-here)))
  (sx-assoc-let data
    (cond
     ;; This is an attempt to identify when we have the question
     ;; object itself, so there's no need to fetch anything.  This
     ;; happens inside the question-list, but it can be easily
     ;; confused with the inbox (whose items have a title, a body, and
     ;; a question_id).
     ((and .title .question_id .score
           (not .item_type) (not .notification_type))
      (sx-display-question data 'focus))
     (.answer_id
      (sx-display-question
       (sx-question-get-from-answer .site_par .answer_id)
       'focus)
      (if .comment_id
          (sx--find-in-buffer 'comment .comment_id)
        (sx--find-in-buffer 'answer .answer_id)))
     (.question_id
      (sx-display-question
       (sx-question-get-question .site_par .question_id) 'focus)
      (when .comment_id
        (sx--find-in-buffer 'comment .comment_id)))
     ;; `sx-question-get-from-comment' takes 2 api requests, so we
     ;; test it last.
     (.comment_id
      (sx-display-question
       (sx-question-get-from-comment .site_par .comment_id) 'focus)
      (sx--find-in-buffer 'comment .comment_id))
     (.notification_type
      (sx-message "Viewing notifications is not yet implemented"))
     (.item_type (sx-open-link .link)))))

(defun sx-display-question (&optional data focus window)
  "Display question given by DATA, on WINDOW.
Interactively, display question under point. When FOCUS is
non-nil (the default when called interactively), also focus the
relevant window.

If WINDOW nil, the window is decided by
`sx-question-mode-display-buffer-function'."
  (interactive (list (sx--data-here 'question) t))
  (when (sx-question--mark-read data)
    (sx--maybe-update-display))
  ;; Display the question.
  (setq window
        (get-buffer-window
         (sx-question-mode--display data window)))
  (when focus
    (if (window-live-p window)
        (select-window window)
      (switch-to-buffer sx-question-mode--buffer))))


;;; Simple interactions
(defun sx-favorite (data &optional undo)
  "Favorite question given by DATA.
Interactively, it is guessed from context at point.
With the UNDO prefix argument, unfavorite the question instead."
  (interactive (list (sx--error-if-unread (sx--data-here 'question))
                     current-prefix-arg))
  (sx-method-post-from-data data
    (if undo 'favorite/undo 'favorite)
    :callback (sx--copy-update-callback data)))
(defalias 'sx-star #'sx-favorite)

(defun sx-accept (data &optional undo)
  "Accept answer given by DATA.
Interactively, it is guessed from context at point.
With the UNDO prefix argument, unaccept the question instead."
  (interactive (list (sx--data-here 'answer)
                     current-prefix-arg))
  (sx-ensure-authentication)
  ;; When clicking the "Accept" button, first arg is a marker.
  (when (markerp data)
    (setq data (sx--marker-to-data data 'answer)))
  (sx-method-post-from-data data
    (if undo 'accept/undo 'accept)
    :callback (sx--copy-update-callback data)))


;;; Voting
(defun sx-upvote (data &optional undo)
  "Upvote an object given by DATA.
DATA can be a question, answer, or comment. Interactively, it is
guessed from context at point.
With UNDO prefix argument, remove upvote instead of applying it."
  (interactive (list (sx--error-if-unread (sx--data-here))
                     current-prefix-arg))
  (sx-set-vote data "upvote" (not undo)))

(defun sx-downvote (data &optional undo)
  "Downvote an object given by DATA.
DATA can be a question or an answer. Interactively, it is guessed
from context at point.
With UNDO prefix argument, remove downvote instead of applying it."
  (interactive (list (sx--error-if-unread (sx--data-here))
                     current-prefix-arg))
  (sx-set-vote data "downvote" (not undo)))

(defun sx-set-vote (data type status)
  "Set the DATA's vote TYPE to STATUS.
DATA can be a question, answer, or comment. TYPE can be
\"upvote\" or \"downvote\". STATUS is a boolean.

Besides posting to the api, DATA is also altered to reflect the
changes."
  (sx-ensure-authentication)
  (sx-method-post-from-data data
    (concat type (unless status "/undo"))
    :callback (sx--copy-update-callback data)))


;;; Delete
(defun sx-delete (data &optional undo)
  "Delete an object given by DATA.
DATA can be a question, answer, or comment. Interactively, it is
guessed from context at point.
With UNDO prefix argument, undelete instead."
  (interactive (list (sx--error-if-unread (sx--data-here))
                     current-prefix-arg))
  (sx-ensure-authentication)
  (when (y-or-n-p (format "DELETE this %s? "
                    (let-alist data
                      (cond (.comment_id "comment")
                            (.answer_id "answer")
                            (.question_id "question")))))
    (sx-method-post-from-data data
      (if undo 'delete/undo 'delete)
      :callback (sx--make-update-callback
                 ;; Indicate to ourselves this has been deleted.
                 (setcdr data (cons (car data) (cdr data)))
                 (setcar data 'deleted)))))


;;; Commenting
(defun sx-comment (data &optional text)
  "Post a comment on DATA given by TEXT.
DATA can be a question, an answer, or a comment. Interactively,
it is guessed from context at point.
If DATA is a comment, the comment is posted as a reply to it.

TEXT is a string. Interactively, it is read from the minibufer."
  (interactive (list (sx--error-if-unread (sx--data-here)) 'query))
  (sx-ensure-authentication)
  ;; When clicking the "Add a Comment" button, first arg is a marker.
  (when (markerp data)
    (setq data (sx--marker-to-data data))
    (setq text 'query))
  (sx-assoc-let data
    ;; Get the comment text
    (when (eq text 'query)
      (setq text (read-string
                  "Comment text: "
                  (when .comment_id
                    (substring-no-properties (sx-user--format "%@ " .owner)))))
      (while (not (sx--comment-valid-p text 'silent))
        (setq text (read-string "Comment text (between 16 and 600 characters): " text))))
    ;; If non-interactive, `text' could be anything.
    (unless (stringp text)
      (error "Comment body must be a string"))
    ;; And post
    (let ((result
           (sx-method-call 'posts
             :id (or .post_id .answer_id .question_id)
             :submethod "comments/add"
             :auth 'warn
             :url-method 'post
             :filter sx-browse-filter
             :site .site_par
             :keywords `((body . ,text)))))
      ;; The api returns the new DATA.
      (when result
        (sx--add-comment-to-object
         (sx--ensure-owner-in-object (list (cons 'display_name "(You)")) (elt result 0))
         (if .post_id (sx--get-post .post_type .site_par .post_id) data))
        ;; Display the changes in `data'.
        (sx--maybe-update-display)))))

(defun sx--comment-valid-p (&optional text silent)
  "Non-nil if TEXT fits stack exchange comment length limits.
If TEXT is nil, use `buffer-string'. Must have more than 15 and
less than 601 characters.
If SILENT is nil, message the user about this limit."
  (let ((w (string-width (or text (buffer-string)))))
    (if (and (< 15 w) (< w 601))
        t
      (unless silent
        (message "Comments must be within 16 and 600 characters."))
      nil)))

(defun sx--get-post (type site id)
  "Find in the database a post identified by TYPE, SITE and ID.
TYPE is `question' or `answer'.
SITE is a string.
ID is an integer."
  (let ((db (cons sx-question-mode--data
                  sx-question-list--dataset)))
    (setq db
          (cond
           ((string= type "question") db)
           ((string= type "answer")
            (apply #'cl-map 'list #'identity
                   (mapcar (lambda (x) (cdr (assoc 'answers x))) db)))))
    (car (cl-member-if
          (lambda (x) (sx-assoc-let x
                   (and (equal (or .answer_id .question_id) id)
                        (equal .site_par site))))
          db))))

(defun sx--add-comment-to-object (comment object)
  "Add COMMENT to OBJECT's `comments' property.
OBJECT can be a question or an answer."
  (let ((com-cell (assoc 'comments object)))
    (if com-cell
        (progn
          (setcdr
           com-cell
           (append (cdr com-cell)
                   (list comment))))
      ;; No previous comments, add it manually.
      (setcdr object (cons (car object) (cdr object)))
      (setcar object `(comments . (,comment)))))
  object)

(defun sx--ensure-owner-in-object (owner object)
  "Add `owner' property with value OWNER to OBJECT."
  (unless (cdr-safe (assq 'owner object))
    (setcdr object (cons (car object) (cdr object)))
    (setcar object `(owner . ,owner)))
  object)


;;; Editing
(defun sx-edit (data)
  "Start editing an answer or question given by DATA.
DATA is an answer or question alist. Interactively, it is guessed
from context at point."
  (interactive (list (sx--data-here)))
  (sx-ensure-authentication)
  ;; If we ever make an "Edit" button, first arg is a marker.
  (when (markerp data) (setq data (sx--data-here)))
  (sx-assoc-let data
    (let ((buffer (current-buffer)))
      (pop-to-buffer
       (sx-compose-create
        .site_par data
        ;; Before send hook
        (when .comment_id (list #'sx--comment-valid-p))
        ;; After send functions
        (list (lambda (_ res)
                (sx--copy-data (elt res 0) data)
                (sx--maybe-update-display buffer))))))))


;;; Asking
(defcustom sx-default-site "emacs"
  "Name of the site to use by default when listing questions."
  :type 'string
  :group 'sx)

(defun sx--interactive-site-prompt ()
  "Query the user for a site."
  (let ((default (or sx-question-list--site
                     (sx-assoc-let sx-question-mode--data .site_par)
                     sx-default-site)))
    (sx-completing-read
     (format "Site (%s): " default)
     (sx-site-get-api-tokens) nil t nil nil
     default)))

(defun sx--maybe-site-prompt (arg)
  "Get a site token conditionally in an interactive context.
If ARG is non-nil, use `sx--interactive-site-prompt'.
Otherwise, use `sx-question-list--site' if non-nil.
If nil, use `sx--interactive-site-prompt' anyway."
  ;; This could eventually be generalized into (sx--maybe-prompt
  ;; prefix-arg value-if-non-nil #'prompt-function).
  (if arg
      (sx--interactive-site-prompt)
    (or sx-question-list--site
        (sx--interactive-site-prompt))))

;;;###autoload
(defun sx-ask (site)
  "Start composing a question for SITE.
SITE is a string, indicating where the question will be posted."
  (interactive (list (sx--interactive-site-prompt)))
  (sx-ensure-authentication)
  (let ((buffer (current-buffer)))
    (pop-to-buffer
     (sx-compose-create
      site nil nil
      ;; After send functions
      (list (lambda (_b _res) (sx--maybe-update-display buffer)))))))


;;; Answering
(defun sx-answer (data)
  "Start composing an answer for question given by DATA.
DATA is a question alist. Interactively, it is guessed from
context at point. "
  ;; If the user tries to answer a question that's not viewed, he
  ;; probaby hit the button by accident.
  (interactive
   (list (sx--error-if-unread (sx--data-here 'question))))
  (sx-ensure-authentication)
  ;; When clicking the "Write an Answer" button, first arg is a marker.
  (when (markerp data) (setq data (sx--data-here)))
  (let ((buffer (current-buffer)))
    (sx-assoc-let data
      (pop-to-buffer
       (sx-compose-create
        .site_par .question_id nil
        ;; After send functions
        (list (lambda (_ res)
                (sx--add-answer-to-question-object (elt res 0) data)
                (sx--maybe-update-display buffer .site_par .question_id))))))))

(defun sx--add-answer-to-question-object (answer question)
  "Add alist ANSWER to alist QUESTION in the right place."
  (let ((cell (assoc 'answers question)))
    (if cell
        (setcdr cell (append (cdr cell) (list answer)))
      ;; No previous comments, add it manually.
      (setcdr question (cons (car question) (cdr question)))
      (setcar question `(answers . (,answer))))
    question))

(provide 'sx-interaction)
;;; sx-interaction.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
