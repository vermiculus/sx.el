;;; sx-question-mode.el --- major-mode for displaying questions  -*- lexical-binding: t; -*-

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

;; This file provides a means to print questions with their answers
;; and all comments.  See the customizable group `sx-question-mode'.


;;; Code:
(eval-when-compile
  (require 'rx))

(require 'sx)
(require 'sx-question)
(require 'sx-question-print)


;;; Displaying a question
(defcustom sx-question-mode-display-buffer-function #'pop-to-buffer
  "Function used to display the question buffer.
Called, for instance, when hitting \\<sx-question-list-mode-map>`\\[sx-question-list-display-question]' on an entry in the
question list.
This is not used when navigating the question list with `\\[sx-question-list-view-next].

Common values for this variable are `pop-to-buffer' and `switch-to-buffer'."
  :type 'function
  :group 'sx-question-mode)

(defvar sx-question-mode--buffer nil
  "Buffer being used to display questions.")

(defvar sx-question-mode--data nil
  "The data of the question being displayed.")

(defun sx-question-mode--get-window ()
  "Return a window displaying a question, or nil."
  (car-safe
   (cl-member-if
    (lambda (x) (with-selected-window x
             (derived-mode-p 'sx-question-mode)))
    (window-list nil 'never nil))))

(defun sx-question-mode--display (data &optional window)
  "Display question given by DATA on WINDOW.
If WINDOW is nil, use selected one.

Returns the question buffer."
  (with-current-buffer
      (sx-question-mode--display-buffer window)
    (sx-question-mode--erase-and-print-question data)))

(defun sx-question-mode--erase-and-print-question (data)
  "Erase contents of buffer and print question given by DATA.
Also marks the question as read with `sx-question--mark-read'."
  (sx-question--mark-read data)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sx-question-mode)
    (sx-question-mode--print-question data)
    (current-buffer)))

(defun sx-question-mode--display-buffer (window)
  "Display and return the buffer used for displaying a question.
Create `sx-question-mode--buffer' if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p sx-question-mode--buffer)
    (setq sx-question-mode--buffer
          (generate-new-buffer "*sx-question*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window sx-question-mode--buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window sx-question-mode--buffer 'visible))
   ;; Neither, so we create the window.
   (t (funcall sx-question-mode-display-buffer-function
        sx-question-mode--buffer)))
  sx-question-mode--buffer)


;;; Movement commands
;; Sections are headers placed above a question's content or an
;; answer's content, or above the list of comments. They are
;; identified with the `sx-question-mode--section' text property.
;; To move between sections, just search for the property. The value
;; of the text-property is the depth of the section (1 for contents, 2
;; for comments).
(defcustom sx-question-mode-recenter-line 2
  "Screen line to which we recenter after moving between sections.
This is used as an argument to `recenter', only used if the end
of section is outside the window.

If nil, no recentering is performed."
  :type '(choice (const :tag "Don't recenter" nil)
                 integer)
  :group 'sx-question-mode)

(defun sx-question-mode-next-section (&optional n)
  "Move down to next section (question or answer) of this buffer.
Prefix argument N moves N sections down or up."
  (interactive "p")
  (let ((count (if n (abs n) 1)))
    (while (> count 0)
      ;; This will either move us to the next section, or move out of
      ;; the current one.
      (unless (sx-question-mode--goto-property-change 'section n)
        ;; If all we did was move out the current one, then move again
        ;; and we're guaranteed to reach the next section.
        (sx-question-mode--goto-property-change 'section n))
      (unless (get-char-property (point) 'invisible)
        (cl-decf count))))
  (when (equal (selected-window) (get-buffer-window))
    (when sx-question-mode-recenter-line
      (let ((ov (sx-question-mode--section-overlays-at (line-end-position))))
        (when (and (overlayp ov) (> (overlay-end ov) (window-end)))
          (recenter sx-question-mode-recenter-line))))
    (sx-message-help-echo)))

(defun sx-question-mode-previous-section (&optional n)
  "Move down to previous section (question or answer) of this buffer.
Prefix argument moves N sections up or down."
  (interactive "p")
  (sx-question-mode-next-section (- (or n 1))))

(defun sx-question-mode--goto-property-change (prop &optional direction)
  "Move forward to the next change of text-property sx-question-mode--PROP.
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

(defun sx-question-mode-hide-show-section (&optional _)
  "Hide or show section under point.
Optional argument _ is for `push-button'."
  (interactive)
  (let ((ov (or (sx-question-mode--section-overlays-at
                 (line-end-position))
                (sx-question-mode--section-overlays-at (point)))))
    (goto-char (overlay-start ov))
    (forward-line 0)
    (overlay-put
     ov 'invisible
     (null (overlay-get ov 'invisible)))))

(defun sx-question-mode--section-overlays-at (pos)
  "Return the highest priority section overlay at POS.
A section overlay has a `sx-question-mode--section-content'
property."
  (cdr-safe (get-char-property-and-overlay
             pos 'sx-question-mode--section-content nil)))


;;; Major-mode
(defconst sx-question-mode--header-line
  '("    "
    (:propertize "n p TAB" face mode-line-buffer-id)
    ": Navigate"
    "    "
    (:propertize "u d" face mode-line-buffer-id)
    ": Up/Down Vote"
    "    "
    (:propertize "c" face mode-line-buffer-id)
    ": Comment"
    "    "
    (:propertize "a" face mode-line-buffer-id)
    ": Answer"
    "    "
    (:propertize "e" face mode-line-buffer-id)
    ": Edit"
    "    "
    (:propertize "q" face mode-line-buffer-id)
    ": Quit")
  "Header-line used on the question list.")

(define-derived-mode sx-question-mode special-mode "Question"
  "Major mode to display and navigate a question and its answers.
Letters do not insert themselves; instead, they are commands.

\\<sx-question-mode>
\\{sx-question-mode}"
  (setq header-line-format sx-question-mode--header-line)
  ;; Determine how to close this window.
  (unless (window-parameter nil 'quit-restore)
    (set-window-parameter
     nil 'quit-restore
     `(other window nil ,(current-buffer))))
  ;; We call font-lock-region manually. See `sx-question-mode--fill-and-fontify'
  (font-lock-mode -1)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t)
  (remove-hook 'window-configuration-change-hook
    'markdown-fontify-buffer-wiki-links t))

(mapc
 (lambda (x) (define-key sx-question-mode-map
          (car x) (cadr x)))
 `(
   ([down] sx-question-mode-next-section)
   ([up] sx-question-mode-previous-section)
   ("n" sx-question-mode-next-section)
   ("p" sx-question-mode-previous-section)
   ("g" sx-question-mode-refresh)
   ("c" sx-comment)
   ("v" sx-visit-externally)
   ("u" sx-upvote)
   ("d" sx-downvote)
   ("q" quit-window)
   (" " scroll-up-command)
   ("a" sx-answer)
   ("e" sx-edit)
   ("S" sx-search)
   ("s" sx-switchto-map)
   ("*" sx-favorite)
   (,(kbd "S-SPC") scroll-down-command)
   ([backspace] scroll-down-command)
   ([tab] forward-button)
   (,(kbd "<S-iso-lefttab>") backward-button)
   (,(kbd "<S-tab>") backward-button)
   (,(kbd "<backtab>") backward-button)))

(defun sx-question-mode-refresh (&optional no-update)
  "Refresh currently displayed question.
Queries the API for any changes to the question or its answers or
comments, and redisplays it.

With non-nil prefix argument NO-UPDATE, just redisplay, don't
query the api."
  (interactive "P")
  (sx-question-mode--ensure-mode)
  (let ((point (point))
        (line (count-screen-lines
               (window-start) (point))))
    (sx-question-mode--erase-and-print-question
     (if no-update
         sx-question-mode--data
       (sx-assoc-let sx-question-mode--data
         (sx-question-get-question .site_par .question_id))))
    (goto-char point)
    (when (equal (selected-window)
                 (get-buffer-window (current-buffer)))
      (recenter line)))
  (sx-message "Done."))

(defun sx-question-mode--ensure-mode ()
  "Ensures we are in question mode, erroring otherwise."
  (unless (derived-mode-p 'sx-question-mode)
    (error "Not in `sx-question-mode'")))

(provide 'sx-question-mode)
;;; sx-question-mode.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
