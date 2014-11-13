;;; sx-question-mode.el --- Creating the buffer that displays questions  -*- lexical-binding: t; -*-

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

;;


;;; Code:

(require 'sx)
(require 'sx-filter)
(require 'sx-lto)
(require 'markdown-mode)


;;; Displaying a question
(defvar sx-question-mode--window nil
  "Window where the content of questions is displayed.")

(defvar sx-question-mode--buffer nil
  "Buffer being used to display questions.")

(defvar sx-question-mode--data nil
  "The data of the question being displayed.")

(defun sx-question-mode--display (data &optional window)
  "Display question given by DATA on WINDOW.
If WINDOW is nil, use selected one.
Returns the question buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer
        (sx-question-mode--display-buffer window)
      (erase-buffer)
      (sx-question-mode)
      (sx-question-mode--print-question data)
      (current-buffer))))

(defun sx-question-mode--display-buffer (window)
  "Display and return the buffer used for displaying a question.
Create the buffer if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p sx-question-mode--buffer)
    (setq sx-question-mode--buffer
          (generate-new-buffer "*stack-question*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window sx-question-mode--buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window sx-question-mode--buffer 'visible))
   ;; Neither, so we create the window.
   (t (switch-to-buffer sx-question-mode--buffer)))
  sx-question-mode--buffer)


;;; Printing a question's content
(defvar sx-question-mode--overlays nil 
  "")
(make-variable-buffer-local 'sx-question-mode--overlays)

(defface sx-question-mode-header
  '((t :inherit font-lock-variable-name-face))
  "Face used on the question headers in the question buffer."
  :group 'sx-question-mode-faces)

(defface sx-question-mode-title
  '((t :height 1.3 :weight bold :inherit default))
  "Face used on the question title in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-title "\n"
  "String used before the question title at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-author
  '((t :inherit font-lock-string-face))
  "Face used on the question author in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-author "\nAuthor:   "
  "String used before the question author at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-date
  '((t :inherit font-lock-string-face))
  "Face used on the question date in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-date "\nAsked on: "
  "String used before the question date at the header."
  :type 'string
  :group 'sx-question-mode)

(defface sx-question-mode-tags
  '((t :inherit font-lock-function-name-face))
  "Face used on the question tags in the question buffer."
  :group 'sx-question-mode-faces)

(defcustom sx-question-mode-header-tags "\nTags:     "
  "String used before the question tags at the header."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-last-edit-format " (edited %s ago by %s)"
  "Format used to describe last edit date in the header.
First %s is replaced with the date, and the second %s with the
editor's name."
  :type 'string
  :group 'sx-question-mode)

(defcustom sx-question-mode-separator
  (concat "\n" (make-string 80 ?_) "\n")
  "Separator used between header and body."
  :type 'string
  :group 'sx-question-mode)

;;; This is where most of the work is still left to be done! Need to
;;; insert more data from QUESTION.
(defun sx-question-mode--print-question (question)
  "Print a buffer describing QUESTION.
QUESTION must be a data structure returned by `json-read'."
  (sx-assoc-let question
    (insert sx-question-mode-header-title
            (propertize .title
                        'font-lock-face 'sx-question-mode-title
                        'sx-question-mode--section 1))
    (sx-question-mode--wrap-in-overlay
     (sx-question-mode--insert-header
      sx-question-mode-header-author
      (cdr (assoc 'display_name .owner))
      'sx-question-mode-author
      ;; Date
      sx-question-mode-header-date
      (concat
       (sx-time-seconds-to-date creation_date)
       (when .last_edit_date
         (format sx-question-mode-last-edit-format
                 (sx-time-since .last_edit_date)
                 (cdr (assoc 'display_name .last_editor)))))
      'sx-question-mode-date
      ;; Tags
      sx-question-mode-header-tags
      (concat "(" (mapconcat 'identity .tags ") (") ")")
      'sx-question-mode-tags)
     (insert sx-question-mode-separator
             ;; @TODO: This is temporary, of course. It prevents
             ;; errors while the filters aren't setup yet.
             (or .body "BODY")))))

(defmacro sx-question-mode--wrap-in-overlay (&rest body)
  "Execute BODY and wrap any inserted text in an overlay.
Overlay is stored in `sx-question-mode--overlays' and given the
property `sx-question-mode--section-content'."
  `(let ((p (point-marker)))
     ,@body
     (let ((ov (make-overlay p (point))))
       (overlay-put ov 'sx-question-mode--section-content t)
       (overlay-put ov 'face 'sx-question-mode-content-face)
       (push ov sx-question-mode--overlays))))

(defun sx-question-mode--insert-header (&rest args)
  "Insert HEADER and VALUE.
HEADER is given `sx-question-mode-header' face, and value is given FACE.
\(fn header value face [header value face] [header value face] ...)"
  (while args
    (insert 
     (propertize (pop args) 'font-lock-face 'sx-question-mode-header)
     (propertize (pop args) 'font-lock-face (pop args)))))


;;; Movement commands
;; Sections are headers placed above a question's content or an
;; answer's content, or above the list of comments. They are
;; identified with the `sx-question-mode--section' text property.
;; To move between sections, just search for the property. The value
;; of the text-property is the depth of the section (1 for contents, 2
;; for comments).
(defcustom sx-question-mode-recenter-line 1
  "Screen line to which we recenter after moving between sections.
This is used as an argument to `recenter'. 
If nil, no recentering is performed."
  :type '(choice (const :tag "Don't recenter" nil)
                 integer)
  :group 'sx-question-mode)

(defun sx-question-mode-next-section (n)
  "Move down to next section (question or answer) of this buffer.
Prefix argument N moves N sections down or up."
  (interactive "p")
  (dotimes (_ (abs n))
    ;; This will either move us to the next section, or move out of
    ;; the current one.
    (unless (sx-question-mode--goto-propety-change 'section n)
      ;; If all we did was move out the current one, then move again
      ;; and we're guaranteed to reach the next section.
      (sx-question-mode--goto-propety-change 'section n)))
  (when sx-question-mode-recenter-line
    (recenter sx-question-mode-recenter-line)))

(defun sx-question-mode-previous-section (n)
  "Move down to previous section (question or answer) of this buffer.
Prefix argument N moves N sections up or down."
  (interactive "p")
  (sx-question-mode-next-section (- n)))

(defun sx-question-mode--goto-propety-change (prop &optional direction)
  "Move forward until the value of text-property `sx-question-mode--PROP' changes.
Return the new value of PROP at point.
If DIRECTION is negative, move backwards instead."
  (let ((prop (intern (format "sx-question-mode--%s" prop)))
        (func (if (and (numberp direction)
                       (< direction 0))
                  #'previous-single-property-change
                #'next-single-property-change))
        (limit (if (and (numberp direction)
                        (< direction 0))
                   (point-min) (point-max))))
    (goto-char (funcall func (point) prop nil limit))
    (get-text-property (point) prop)))


;;; Major-mode
(define-derived-mode sx-question-mode markdown-mode "Question"
  "Major mode for a question and its answers.
Letters do not insert themselves; instead, they are commands.
\\<sx-question-mode>
\\{sx-question-mode}"
  (sx-question-mode--update-mode-line)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t)
  (remove-hook 'window-configuration-change-hook
               'markdown-fontify-buffer-wiki-links t)
  (read-only-mode))

(defun sx-question-mode--update-mode-line ()
  ""
  )

(mapc
 (lambda (x) (define-key sx-question-mode-map
               (car x) (cadr x)))
 '(("n" sx-question-mode-next-section)
   ("p" sx-question-mode-previous-section)
   ("j" sx-question-mode-next-section)
   ("k" sx-question-mode-previous-section)
   ("g" sx-question-mode-refresh)))

(defun sx-question-mode-refresh ()
  "Refresh currently displayed question.
Queries the API for any changes to the question or its answers or
comments, and redisplays it."
  (interactive)
  (unless (derived-mode-p 'sx-question-mode)
    (error "Not in `sx-question-mode'"))
  (sx-assoc-let sx-question-mode--data
    (sx-question-mode--display
     (sx-question-get-question
      sx-question-list--current-site .question_id)
     (selected-window))))

(defconst stack-question-list--mode-line-format
  '("  "
    mode-name
    " "
    (:propertize stack-question-list--current-page
                 face mode-line-buffer-id)
    " ["
    "Unread: "
    (:propertize
     (:eval (int-to-string stack-question-list--unread-count))
     face mode-line-buffer-id)
    ", "
    "Total: "
    (:propertize
     (:eval (int-to-string stack-question-list--total-count))
     face mode-line-buffer-id)
    "] ")
  "Mode-line construct to use in question-list buffers.")

(provide 'sx-question-mode)
;;; sx-question-mode.el ends here
