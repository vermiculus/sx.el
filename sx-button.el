;;; sx-button.el --- Defining buttons used throughout SX.

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
(require 'button)

(require 'sx)
(require 'sx-question)


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
         (get-text-property
          (point) 'sx-button-copy-content)))
    (if content
        (kill-new content)
      (sx-message "Nothing to copy here."))))

(defun sx-button-follow-link (&optional pos)
  "Follow link at POS.  If POS is nil, use `point'."
  (interactive)
  (browse-url
   (or (get-text-property (or pos (point)) 'sx-button-url)
       (user-error "No url under point: %s" (or pos (point))))))


;;; Type definitions
(define-button-type 'sx-button
  'follow-link t
  'keymap sx-button-map)

(define-button-type 'sx-question-mode-title
  'face      'sx-question-mode-title
  'action    #'sx-question-mode-hide-show-section
  'help-echo 'sx-question-mode--section-help-echo
  :supertype 'sx-button)

(define-button-type 'sx-button-link
  'action    #'sx-button-follow-link
  :supertype 'sx-button)

(provide 'sx-button)
;;; sx-button.el ends here

;; Local Variables:
;; lexical-binding: t
;; End:
