;;; sx-switchto.el --- Keymap for navigating between pages. -*- lexical-binding: t; -*-

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
(require 'sx-filter)
(require 'sx-method)
(require 'sx-question-list)


;;; Keybinds
;;;###autoload
(define-prefix-command 'sx-switchto-map)

(mapc (lambda (x) (define-key sx-switchto-map (car x) (cadr x)))
  '(
    ;; These immitate the site's G hotkey.
    ("m" sx-tab-meta-or-main)
    ("a" sx-ask)
    ("h" sx-tab-frontpage)
    ;; This is `n' on the site.
    ("u" sx-tab-unanswered)
    ;; These are extra things we can do, because we're awesome.
    ("i" sx-inbox)
    ("f" sx-tab-featured)
    ("U" sx-tab-unanswered-my-tags)
    ("n" sx-tab-newest)
    ("w" sx-tab-week)
    ("v" sx-tab-topvoted)
    ))

(provide 'sx-switchto)
;;; sx-switchto.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
