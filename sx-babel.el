;;; sx-babel.el --- font-locking pre blocks according to language  -*- lexical-binding: t; -*-

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

;; This file contains functions and a variable for font-locking the
;; content of markdown pre blocks according to their language. The
;; main configuration point, for both the user and the developer is
;; the variable `sx-babel-major-mode-alist', which see.


;;; Code:
(require 'sx-button)

(defvar sx-babel-major-mode-alist
  `((,(rx (or "*" "#+")) org-mode)
    (,(rx (or "[" "(" ";" "#(")) emacs-lisp-mode)
    ;; @TODO: Make shell-mode work here. Currently errors because it
    ;; needs a process. `sh-mode' isn't as nice.
    (,(rx (or "$ " "# ")) sh-mode)
    ;; Not sure if leaving out "[{" might lead to false positives.
    (,(rx "\\" (+ alnum) (any "[{")) latex-mode)
    ;; Right now, this will match a lot of stuff. Once we are capable
    ;; of determining major-mode from tags, site, and comments, this
    ;; will work as a last case fallback.
    (,(rx (or (and "int" (+ space) "main" (* space) "("))) c-mode)
    )
  "List of cons cells determining which major-mode to use when.
Each car is a rule and each cdr is a major-mode.  The first rule
which is satisfied activates the major-mode.

Point is moved to the first non-blank character before testing
the rule, which can either be a string or a function.  If it is a
string, is tested as a regexp starting from point.  If it is a
function, is called with no arguments and should return non-nil
on a match.")
(put 'sx-babel-major-mode-alist 'risky-local-variable-p t)


;;; Font-locking the text
(defun sx-babel--make-pre-button (beg end)
  "Turn the region between BEG and END into a button."
  (let ((text (buffer-substring-no-properties beg end))
        indent mode copy)
    (with-temp-buffer
      (insert text)
      (setq indent (sx-babel--unindent-buffer))
      (goto-char (point-min))
      (setq mode (sx-babel--determine-major-mode))
      (setq copy (replace-regexp-in-string "[[:space:]]+\\'" "" (buffer-string)))
      (when mode
        (delay-mode-hooks (funcall mode)))
      (font-lock-fontify-region (point-min) (point-max))
      (goto-char (point-min))
      (let ((space (make-string indent ?\s)))
        (while (not (eobp))
          (insert-and-inherit space)
          (forward-line 1)))
      (setq text (buffer-string)))
    (goto-char beg)
    (delete-region beg end)
    (insert-text-button
     text
     'sx-button-copy copy
     ;; We store the mode here so it can be used if the user wants
     ;; to edit the code block.
     'sx-mode mode
     :type 'sx-question-mode-code-block)))

(defun sx-babel--determine-major-mode ()
  "Return the major-mode most suitable for the current buffer."
  (let ((alist sx-babel-major-mode-alist)
        cell out)
    (while (setq cell (pop alist))
      (goto-char (point-min))
      (skip-chars-forward "\r\n[:blank:]")
      (let ((kar (car cell)))
        (when (if (stringp kar) (looking-at kar) (funcall kar))
          (setq alist nil)
          (setq out (cadr cell)))))
    out))

(defun sx-babel--unindent-buffer ()
  "Remove absolute indentation in current buffer.
Finds the least indented line, and removes that amount of
indentation from all lines.  Primarily designed to extract the
content of markdown code blocks.

Returns the amount of indentation removed."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      ;; Get indentation of each non-blank line
      (while (null (eobp))
        (skip-chars-forward "[:blank:]")
        (unless (looking-at "$")
          (push (current-column) result))
        (forward-line 1))
      (when result
        (setq result (apply #'min result))
        ;; Build a regexp with the smallest indentation
        (let ((rx (format "^ \\{0,%s\\}" result)))
          (goto-char (point-min))
          ;; Use this regexp to remove that much indentation
          ;; throughout the buffer.
          (while (and (null (eobp))
                      (search-forward-regexp rx nil 'noerror))
            (replace-match "")
            (forward-line 1))))
      (or result 0))))

(provide 'sx-babel)
;;; sx-babel.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
