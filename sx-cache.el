;;; sx-cache.el --- caching                          -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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

;; This file handles the cache system.  All caches are retrieved and
;; set using symbols.  The symbol should be the sub-package that is
;; using the cache.  For example, `sx-pkg' would use
;;
;;   `(sx-cache-get 'pkg)'
;;
;; This symbol is then converted into a filename within
;; `sx-cache-directory' using `sx-cache-get-file-name'.
;;
;; Currently, the cache is written at every `sx-cache-set', but this
;; write will eventually be done by some write-all function which will
;; be set on an idle timer.

;;; Code:

(defcustom sx-cache-directory (locate-user-emacs-file ".sx")
  "Directory containing cached data."
  :type 'directory
  :group 'sx)

(defun sx-cache--ensure-sx-cache-directory-exists ()
  "Ensure `sx-cache-directory' exists."
  (unless (file-exists-p sx-cache-directory)
    (mkdir sx-cache-directory)))

(defun sx-cache-get-file-name (filename)
  "Expand FILENAME in the context of `sx-cache-directory'."
  (expand-file-name
   (concat (symbol-name filename) ".el")
   sx-cache-directory))

(defun sx-cache-get (cache &optional form)
  "Return the data within CACHE.
If CACHE does not exist, use `sx-cache-set' to set CACHE to the
result of evaluating FORM.

CACHE is resolved to a file name by `sx-cache-get-file-name'."
  (sx-cache--ensure-sx-cache-directory-exists)
  (let ((file (sx-cache-get-file-name cache)))
    ;; If the file exists, return the data it contains
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents (sx-cache-get-file-name cache))
          (read (buffer-string)))
      ;; Otherwise, set CACHE to the evaluation of FORM.
      ;; `sx-cache-set' returns the data that CACHE was set to.
      (sx-cache-set cache (eval form)))))

(defun sx-cache-set (cache data)
  "Set the content of CACHE to DATA and save.
DATA will be written as returned by `prin1'.

CACHE is resolved to a file name by `sx-cache-get-file-name'."
  (sx-cache--ensure-sx-cache-directory-exists)
  (let (print-length print-level)
    (write-region (prin1-to-string data) nil
                  (sx-cache-get-file-name cache)))
  data)

(defun sx-cache--invalidate (cache &optional vars init-method)
  "Set cache CACHE to nil.

VARS is a list of variables to unbind to ensure cache is cleared.
If INIT-METHOD is defined, call it after all invalidation to
re-initialize the cache."
  (let ((file (sx-cache-get-file-name cache)))
    (delete-file file))
  (mapc #'makunbound vars)
  (when init-method
    (funcall init-method)))

(defun sx-cache-invalidate-all (&optional save-auth)
  "Invalidate all caches using `sx-cache--invalidate'.
Afterwards reinitialize caches using `sx-initialize'. If
SAVE-AUTH is non-nil, do not clear AUTH cache.

Interactively, SAVE-AUTH is the negation of the prefix argument.
That is, by default the auth cache is PRESERVED interactively.
If you provide a prefix argument, the auth cache is INVALIDATED.

Note:  This will also remove read/unread status of questions as well
as delete the list of hidden questions."
  (interactive (list (not current-prefix-arg)))
  (let* ((default-directory sx-cache-directory)
         (caches (file-expand-wildcards "*.el")))
    (when save-auth
      (setq caches (cl-remove-if (lambda (x)
                                   (string= x "auth.el")) caches)))
    (lwarn 'sx :debug "Invalidating: %S" caches)
    (mapc #'delete-file caches)
    (sx-initialize 'force)))

(provide 'sx-cache)
;;; sx-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
