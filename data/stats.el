;;; stats.el --- data statistics                     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: data

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

;; Provides statistical analysis functions for cached data.

;;; Code:

(defvar sx--stat-tags-directory "tags")

(defun sx--stat-avg (numbers)
  (/ (float (apply #'+ numbers)) (length numbers)))

(defun sx--stat-avg-tag-length ()
  "The average length of a tag across all sites.
Makes no account for tag popularity."
  (sx--stat-avg
   (mapcar (lambda (file)
             (sx--stat-avg
              (mapcar #'length
                      (read
                       (with-temp-buffer
                         (insert-file-contents
                          (format "%s/%s" sx--stat-tags-directory file))
                         (buffer-string))))))
           (cddr (sort (directory-files sx--stat-tags-directory)
                       #'string-lessp)))))

(provide 'sx-stats)
;;; stats.el ends here
