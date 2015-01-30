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
;; `sx-cache-directory'.

;;; Code:

(require 'stash)

(defcustom sx-cache-directory (locate-user-emacs-file ".sx")
  "Directory containing cached data."
  :type 'directory
  :group 'sx)

(defcustom sx-cache-write-delay 5
  "Idle delay in seconds to write cache data."
  :type 'integer
  :group 'sx)

(defvar sx-cache--list nil
  "List of cache variables.")

(defmacro sx-cache-new (variable &optional default)
  `(progn
     (stash-new ',variable
                ,(expand-file-name
                  (concat (symbol-name variable) ".el")
                  sx-cache-directory)
                ,default
                ,sx-cache-write-delay)
     (add-to-list 'sx-cache--list ,variable)))

(defun sx-cache-read (cache)
  "Return the data within CACHE."
  (stash-load cache))

(defun sx-cache-set (cache data &optional immediate)
  "Set the content of CACHE to DATA.
If IMMEDIATE is non-nil, immediately write to disk."
  (stash-set cache data immediate))

(defun sx-cache--invalidate (cache)
  "Invalidate CACHE."
  (stash-reset cache))

(defun sx-cache-invalidate-all (&rest except)
  "Invalidate all caches using `sx-cache--invalidate'.
Afterwards reinitialize caches using `sx-initialize'.  All those
cache symbols in EXCEPT are spared.  Interactively, this includes
only `auth'.

Note:  This will also remove read/unread status of questions as well
as delete the list of hidden questions."
  (interactive (list (unless current-prefix-arg '(auth))))
  (mapc #'sx-cache--invalidate
        (cl-set-difference sx-cache--list except))
  (sx-initialize 'force))

(provide 'sx-cache)
;;; sx-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
