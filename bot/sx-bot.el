;;; sx-bot.el --- Functions for automated maintanence  -*- lexical-binding: t; -*-

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

;; This file defines the behavior of a bot.  To allow completion for
;; tags, this bot runs through all sites in the network and retrieves
;; all of their tags.  This data is then written to a directory which
;; is tracked by the git repository.


;;; Code:

(require 'package)
(package-initialize)

(require 'sx-load)

(setq sx-request-remaining-api-requests-message-threshold 50000)

(defcustom sx-bot-out-dir "./data/tags/"
  "Directory where output tag files are saved."
  :type 'directory
  :group 'sx)


;;; Printing
(defun sx-bot-write-to-file (data)
  "Write (cdr DATA) to file named (car DATA).
File is savedd in `sx-bot-out-dir'."
  (let ((file-name (expand-file-name (car data) sx-bot-out-dir)))
    (with-temp-file file-name
      (let* (print-length
             (repr (prin1-to-string
                    (sort (cdr data) #'string-lessp))))
        (insert repr "\n")
        (goto-char (point-min))
        (while (search-forward "\" \"" nil t)
          (replace-match "\"\n\"" nil t))))
    (message "Wrote %S" file-name)
    file-name))

(defun sx-bot-fetch-and-write-tags ()
  "Get a list of all tags of all sites and save to disk."
  (make-directory sx-bot-out-dir t)
  (let* ((url-show-status nil)
         (site-tokens (mapcar (lambda (site)  ;Fix bad sites
                                (pcase site
                                  ("metamunitybuilding" "meta.communitybuilding")
                                  ((or "gamification" "meta.gamification") nil)
                                  (_ site)))
                              (sx-site-get-api-tokens)))
         (number-of-sites (length site-tokens))
         (current-site-number 0)
         (sx-request-remaining-api-requests-message-threshold 4000)
         (sx-request-all-items-delay 0))
    (mapcar
     (lambda (site)
       (when site
         (message "[%d/%d] Working on %S"
                  (cl-incf current-site-number)
                  number-of-sites
                  site)
         (with-demoted-errors (concat "Failed to get " site ": %S")
           (sx-bot-write-to-file
            (cons (concat site ".el")
                  (sx-tag--get-all site))))))
     site-tokens)))


;;; Newest
(provide 'sx-bot)
;;; sx-bot.el ends here
