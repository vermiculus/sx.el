;;; sx-bot.el --- Functions for viewing different tabs.       -*- lexical-binding: t; -*-

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

;;


;;; Code:

(require 'sx-site)
(require 'sx-tag)

(defcustom sx-bot-out-dir "./data/tags/"
  "Directory where output tag files are saved."
  :type 'directory
  :group 'sx)


;;; Printing
(defun sx-bot-write-to-file (data)
  "Write (cdr DATA) to file named (car DATA).
File is savedd in `sx-bot-out-dir'."
  (with-temp-file (expand-file-name (car data) sx-bot-out-dir)
    (let (print-length)
      (prin1 (cdr data) (current-buffer)))))


(defun sx-bot-fetch-and-write-tags ()
  "Get a list of all tags of all sites and save to disk."
  (make-directory sx-bot-out-dir t)
  (mapc #'sx-bot-write-to-file
        ;; @TODO: Not yet implemented!
        (mapcar #'sx-tag--get-all (sx-site-get-api-tokens))))

;;; Newest
(provide 'sx-bot)
;;; sx-bot.el ends here
