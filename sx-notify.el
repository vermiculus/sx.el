;;; sx-notify.el --- Mode-line notifications. -*- lexical-binding: t; -*-

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

(require 'sx)
(require 'sx-inbox)


;;; mode-line notification
(defvar sx-notify--unread-inbox nil 
  "List of inbox items still unread.")

(defvar sx-notify--unread-notifications nil 
  "List of notifications items still unread.")

(defvar sx-notify--mode-line
  '((sx-notify--unread-inbox (sx-notify--unread-notifications "["))
    (sx-notify--unread-inbox
     (:propertize
      (:eval (format "i:%s" (length sx-notify--unread-inbox)))
      face mode-line-buffer-id
      mouse-face mode-line-highlight))
    (sx-notify--unread-inbox (sx-notify--unread-notifications ","))
    (sx-notify--unread-notifications
     (:propertize
      (:eval (format "n:%s" (length sx-notify--unread-notifications)))
      mouse-face mode-line-highlight))
    (sx-notify--unread-inbox (sx-notify--unread-notifications "]")))
  "")
(put 'sx-notify--mode-line 'risky-local-variable t)


;;; minor-mode definition
(define-minor-mode sx-notify-mode nil nil "sx" nil
  (if sx-notify-mode
      (add-to-list 'global-mode-string '(t sx-notify--mode-line) 'append)
    (setq global-mode-string
          (delete '(t sx-notify--mode-line) global-mode-string))))


(provide 'sx-notify)
;;; sx-notify.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
