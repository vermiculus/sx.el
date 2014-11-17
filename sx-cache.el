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
  "Directory containining cached data.")

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
  "Set the content of CACHE to DATA and save changes permanently.

DATA will be written as returned by `prin1'.

CACHE is resolved to a file name by `sx-cache-get-file-name'."
  (sx-cache--ensure-sx-cache-directory-exists)
  (write-region (prin1-to-string data) nil
                (sx-cache-get-file-name cache))
  data)

(provide 'sx-cache)
;;; sx-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
