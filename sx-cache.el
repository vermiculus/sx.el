;;; sx-cache.el --- caching for stack-mode

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

;; All caches are retrieved and set using symbols.  The symbol should
;; be the sub-subpackage that is using the cache.  For example,
;; `sx-pkg' would use `(sx-cache-get 'pkg)'.
;;
;; This symbol is then converted into a filename within
;; `sx-cache-directory'.

;;; Code:

(defcustom sx-cache-directory
  (expand-file-name ".stackmode" user-emacs-directory)
  "Directory containined cached files and precompiled filters.")

(defun sx-cache-get-file-name (filename)
  "Expands FILENAME in the context of `sx-cache-directory'."
  (expand-file-name
   (concat (symbol-name filename) ".el")
   sx-cache-directory))

(defun sx-cache-get (cache &optional form)
  "Return the data within CACHE.

If CACHE does not exist, evaluate FORM and set it to its return.

As with `sx-cache-set', CACHE is a file name within the
context of `sx-cache-directory'."
  (unless (file-exists-p sx-cache-directory)
    (mkdir sx-cache-directory))
  (let ((file (sx-cache-get-file-name cache)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents (sx-cache-get-file-name cache))
          (read (buffer-string)))
      (sx-cache-set cache (eval form)))))

(defun sx-cache-set (cache data)
  "Set the content of CACHE to DATA.

As with `sx-cache-get', CACHE is a file name within the
context of `sx-cache-directory'.

DATA will be written as returned by `prin1'."
  (unless (file-exists-p sx-cache-directory)
    (mkdir sx-cache-directory))
  (write-region (prin1-to-string data) nil
                (sx-cache-get-file-name cache))
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

Afterwards reinitialize caches using `sx-initialize'.

Note:  This will also remove read/unread status of questions as well
as delete the list of hidden questions.

If SAVE-AUTH is non-nil, do not clear AUTH cache."
  (let* ((default-directory sx-cache-directory)
         (caches (file-expand-wildcards "*.el")))
    (when save-auth
      (setq caches (cl-remove-if (lambda (x)
                                   (string= x "auth.el")) caches)))
    (lwarn 'stack-mode :debug "Invalidating: %S" caches)
    (mapc #'delete-file caches)
    (sx-initialize 'force)))

(provide 'sx-cache)
;;; sx-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
