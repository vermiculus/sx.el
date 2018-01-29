;;; sx-button.el --- defining buttons                -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Artur Malabarba

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
;; This file defines all buttons used by SX. For information on
;; buttons, see:
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Buttons.html
;;
;; Most interactive parts of the SX buffers are buttons. Wherever you
;; are, you can always cycle through all buttons by hitting `TAB',
;; that should help identify what's a button in each buffer.
;;
;; To define a new type of button follow the examples below using
;; `define-button-type' with :supertype `sx-button'. Required
;; properties are `action' and `help-echo'. You'll probably want to
;; give it a `face' as well, unless you want it to look like a link.
;;
;; Buttons can then be inserted in their respective files using
;; `insert-text-button'. Give it the string, the `:type' you defined,
;; and any additional properties that can only be determined at
;; creation. Existing text can be transformed into a button with
;; `make-text-button' instead.


;;; Code:
(require 'button)

(require 'sx)
(require 'sx-question)

(declare-function sx-accept "sx-interaction")
(declare-function sx-answer "sx-interaction")
(declare-function sx-comment "sx-interaction")
(declare-function sx-open-link "sx-interaction")
(declare-function sx-question-mode-hide-show-section "sx-question-mode")


;;; Face
(defface sx-custom-button
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 3 :style released-button)
     :height 0.9
     :background "lightgrey" :foreground "black"))
  "Face used on buttons such as \"Write an Answer\"."
  :group 'sx)


;;; Command definitions
;; This extends `button-map', which already defines RET and mouse-1.
(defvar sx-button-map
  (let ((map (copy-keymap button-map)))
    (define-key map "w" #'sx-button-copy)
    map)
  "Keymap used on buttons.")

(defun sx-button-copy ()
  "Copy the content of thing at point.
This is usually a link's URL, or the content of a code block."
  (interactive)
  (let ((content
         (get-text-property (point) 'sx-button-copy)))
    (if (null content)
        (sx-message "Nothing to copy here.")
      (kill-new content)
      (sx-message "Copied %s to kill ring."
                  (or (get-text-property
                       (point) 'sx-button-copy-type)
                      content)))))

(defun sx-button-edit-this (text-or-marker &optional majormode)
  "Open a temp buffer populated with the string TEXT-OR-MARKER using MAJORMODE.
When given a marker (or interactively), use the 'sx-button-copy
and the 'sx-mode text-properties under the marker. These are
usually part of a code-block."
  (interactive (list (point-marker)))
  ;; Buttons receive markers.
  (when (markerp text-or-marker)
    (setq majormode (get-text-property text-or-marker 'sx-mode))
    (unless (setq text-or-marker
                  (get-text-property text-or-marker 'sx-button-copy))
      (sx-message "Nothing of interest here.")))
  (with-current-buffer (pop-to-buffer (generate-new-buffer
                                       "*sx temp buffer*"))
    (insert text-or-marker)
    (when majormode
      (funcall majormode))))

(defun sx-button-follow-link (&optional pos)
  "Follow link at POS.  If POS is nil, use `point'."
  (interactive)
  (let ((url (or (get-text-property (or pos (point)) 'sx-button-url)
                 (sx-user-error "No url under point: %s" (or pos (point))))))
    ;; If we didn't recognize the link, this errors immediately.  If
    ;; we mistakenly recognize it, it will error when we try to fetch
    ;; whatever we thought it was.
    (condition-case nil (sx-open-link url)
      ;; When it errors, don't blame the user, just visit externally.
      (error (browse-url url)))))


;;; Help-echo definitions
(defconst sx-button--help-echo
  (concat "mouse-1, RET"
          (propertize ": %s -- " 'face 'minibuffer-prompt)
          "w"
          (propertize ": copy %s" 'face 'minibuffer-prompt))
  "Base help-echo on which others can be written.")

(defconst sx-button--user-help-echo
  (format sx-button--help-echo
    "visit user page"
    "link")
  "Help echoed in the minibuffer when point is on a user.")

(defconst sx-button--tag-help-echo
  (format sx-button--help-echo
    "Tag search"
    "tag")
  "Help echoed in the minibuffer when point is on a tag.")

(defconst sx-button--question-title-help-echo
  (format sx-button--help-echo
    "hide content"
    "link")
  "Help echoed in the minibuffer when point is on a section.")

(defconst sx-button--link-help-echo
  (format sx-button--help-echo
    "visit %s"
    "URL")
  "Help echoed in the minibuffer when point is on a section.")


;;; Type definitions
(define-button-type 'sx-button
  'follow-link t
  'keymap sx-button-map)

(define-button-type 'sx-question-mode-title
  'face      'sx-question-mode-title
  'action    #'sx-question-mode-hide-show-section
  'help-echo sx-button--question-title-help-echo
  'sx-button-copy-type "Share Link"
  :supertype 'sx-button)

(define-button-type 'sx-question-mode-code-block
  'action    #'sx-button-edit-this
  'face      nil
  :supertype 'sx-button)

(define-button-type 'sx-button-link
  'action    #'sx-button-follow-link
  :supertype 'sx-button)

(define-button-type 'sx-button-user
  'action    #'sx-button-follow-link
  'help-echo sx-button--user-help-echo
  ;; We use different faces on different parts of the user button.
  'face      'sx-user-name
  :supertype 'sx-button)

(declare-function sx-search-tag-at-point "sx-search")
(define-button-type 'sx-button-tag
  'action    #'sx-search-tag-at-point
  'help-echo sx-button--tag-help-echo
  'face      'sx-tag
  :supertype 'sx-button)

(define-button-type 'sx-button-comment
  'help-echo (concat "mouse-1, RET"
                     (propertize ": write a comment"
                                 'face 'minibuffer-prompt))
  'face 'sx-custom-button
  'action    #'sx-comment
  :supertype 'sx-button)

(define-button-type 'sx-button-accept
  'help-echo (concat "mouse-1, RET"
                     (propertize ": accept answer"
                                 'face 'minibuffer-prompt))
  'face 'sx-custom-button
  'action    #'sx-accept
  :supertype 'sx-button)

(define-button-type 'sx-button-answer
  'help-echo (concat "mouse-1, RET"
                     (propertize ": write an answer"
                                 'face 'minibuffer-prompt))
  'face 'sx-custom-button
  'action    #'sx-answer
  :supertype 'sx-button)

(provide 'sx-button)
;;; sx-button.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
