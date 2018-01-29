;;; sx-notify.el --- mode-line notifications         -*- lexical-binding: t; -*-

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


;;; Code:

(require 'sx)
(require 'sx-inbox)


;;; mode-line notification
(defconst sx-notify--mode-line
  '((sx-inbox--unread-inbox (sx-inbox--unread-notifications " ["))
    (sx-inbox--unread-inbox
     (:propertize
      (:eval (format "i:%s" (length sx-inbox--unread-inbox)))
      face mode-line-buffer-id
      mouse-face mode-line-highlight))
    (sx-inbox--unread-inbox (sx-inbox--unread-notifications " "))
    (sx-inbox--unread-notifications
     (:propertize
      (:eval (format "n:%s" (length sx-inbox--unread-notifications)))
      mouse-face mode-line-highlight))
    (sx-inbox--unread-inbox (sx-notify--unread-notifications "]")))
  "")
(put 'sx-notify--mode-line 'risky-local-variable t)


;;; minor-mode definition
(defcustom sx-notify-timer-delay (* 60 5)
  "Idle time, in seconds, before querying for inbox items."
  :type 'integer
  :group 'sx-notify)

(defvar sx-notify--timer nil
  "Timer used for fetching notifications.")

(define-minor-mode sx-notify-mode nil nil nil nil
  :global t
  (if sx-notify-mode
      (progn
        (add-to-list 'global-mode-string '(t sx-notify--mode-line) 'append)
        (setq sx-notify--timer
              (run-with-idle-timer sx-notify-timer-delay 'repeat
                                   #'sx-notify--update-unread)))
    (when (timerp sx-notify--timer)
      (cancel-timer sx-notify--timer)
      (setq sx-notify--timer nil))
    (setq global-mode-string
          (delete '(t sx-notify--mode-line) global-mode-string))))

(defun sx-notify--update-unread ()
  "Update the lists of unread notifications."
  (setq sx-inbox--unread-inbox
        (cl-remove-if
         (lambda (x) (member (cdr (assq 'link x)) sx-inbox--read-inbox))
         (sx-inbox-get)))
  (setq sx-inbox--unread-notifications
        (cl-remove-if
         (lambda (x) (member (cdr (assq 'link x)) sx-inbox--read-notifications))
         (sx-inbox-get t))))

(provide 'sx-notify)
;;; sx-notify.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
