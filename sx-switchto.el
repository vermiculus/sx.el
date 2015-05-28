;;; sx-switchto.el --- keymap for navigating between pages  -*- lexical-binding: t; -*-

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


;;; Keybinds
;;;###autoload
(define-prefix-command 'sx-switchto-map)

(mapc (lambda (x) (define-key sx-switchto-map (car x) (cadr x)))
  '(
    ;; These imitate the site's G hotkey.
    ("a" sx-ask)
    ("h" sx-tab-frontpage)
    ("m" sx-tab-meta-or-main)
    ;; This is `n' on the site.
    ("u" sx-tab-unanswered)
    ;; These are extra things we can do, because we're awesome.
    ("f" sx-tab-featured)
    ("i" sx-inbox)
    ("n" sx-tab-newest)
    ("t" sx-tab-switch)
    ("U" sx-tab-unanswered-my-tags)
    ("v" sx-tab-topvoted)
    ("w" sx-tab-week)
    ("*" sx-tab-starred)
    ))


;;; These are keys which depend on context.
;;;; For instance, it makes no sense to have `switch-site' bound to a
;;;; key on a buffer with no `sx-question-list--site' variable.
(defvar sx-question-list--site)
(sx--define-conditional-key sx-switchto-map "s" #'sx-question-list-switch-site
  (and (boundp 'sx-question-list--site) sx-question-list--site))

(provide 'sx-switchto)
;;; sx-switchto.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
