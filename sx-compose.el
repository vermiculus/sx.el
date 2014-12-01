;;; sx-compose.el --- Major-mode for coposing questions and answers. -*- lexical-binding: t; -*-

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


;;; Code:
(require 'markdown-mode)

(require 'sx)

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


;;; Major-mode
(define-derived-mode sx-compose-mode markdown-mode "Compose"
  "Major mode for coposing questions and answers.
Most of the functionality comes from `markdown-mode'. This mode
just implements some extra features related to posting to the
API.

This mode won't function if `sx-compose--send-function' isn't
set. To make sure you set it correctly, you can create the buffer
with the `sx-compose--create' function.

\\<sx-compose-mode>
\\{sx-compose-mode}"
  (add-hook 'sx-compose-after-send-functions
    #'sx-compose-quit nil t)
  (add-hook 'sx-compose-after-send-functions
    #'sx-compose--copy-as-kill nil t))

(define-key sx-compose-mode-map "\C-c\C-c" #'sx-compose-send)
(define-key sx-compose-mode-map "\C-c\C-k" #'sx-compose-quit)

(defun sx-compose-send ()
  "Finish composing current buffer and send it.
Calls `sx-compose-before-send-hook', POSTs the the current buffer
contents to the API, then calls `sx-compose-after-send-functions'."
  (interactive)
  (when (run-hook-with-args-until-failure
         sx-compose-before-send-hook)
    (let ((result (funcall sx-compose--send-function)))
      (with-demoted-errors
          (run-hook-with-args sx-compose-after-send-functions
                              (current-buffer) result)))))

(defun sx-compose-quit (buffer _)
  "Kill BUFFER."
  (interactive (list (current-buffer) nil))
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun sx-compose--copy-as-kill (buffer _)
  "Copy BUFFER contents to the kill-ring."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (kill-new (buffer-string)))))


;;; Functions to help preparing buffers
(defvar sx-compose--question-headers
  (insert (concat
           (propertize "Title: " 'rear-nonsticky t
                       'read-only t 
                       'field 'sx-compose-header-title)
           (propertize "\nTags:  " 'rear-nonsticky t
                       'field 'sx-compose-header-tags
                       'read-only t )))
  "")

(defun sx-compose--create (site parent &optional before-functions after-functions)
  "Create a `sx-compose-mode' buffer.
SITE is the site where it will be posted. 

If composing questions (not yet supported), PARENT is nil. 
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
              (null (cdr (assoc 'answer_id parent))))))
    (with-current-buffer (sx-compose--get-buffer-create site parent)
      (sx-compose-mode)
      (setq sx-compose--send-function
            (if (consp parent)
                (sx-assoc-let parent
                  (lambda () (sx-method-call (if .title 'questions 'answers)
                          :auth 'warn
                          :url-method "POST"
                          :filter sx-browse-filter
                          :site site
                          :keywords (sx-compose--generate-keywords is-question)
                          :id (or .answer_id .question_id)
                          :submethod 'edit)))
              (lambda () (sx-method-call 'questions
                      :auth 'warn
                      :url-method "POST"
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
      ;; If the buffer is empty, the draft didn't exist. So prepare the
      ;; question.
      (when (and is-question
                 (string= (buffer-string) ""))
        (insert sx-compose--question-headers))
      ;; Return the buffer
      (current-buffer))))

(defun sx-compose--generate-keywords (is-question)
  "Reading current buffer, generate a keywords alist.
Keywords meant to be used in `sx-method-call'.

`body' is read as the `buffer-string'. If IS-QUESTION is non-nil,
other keywords are read from the header "
  (if (null is-question)
      `((body . ,(buffer-string)))
    ;; Question code will go here.
    ))

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
     (format "*sx draft edit %s %s*"
       site (sx-assoc-let data (or .answer_id .question_id)))))))

(provide 'sx-compose)
;;; sx-compose.el ends here
