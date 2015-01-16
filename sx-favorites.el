;;; sx-favorites.el --- starred questions            -*- lexical-binding: t; -*-

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

;; This file provides logic for retrieving and managing a user's
;; starred questions.

;;; Code:

(require 'sx-method)
(require 'sx-cache)
(require 'sx-site)
(require 'sx-networks)
(require 'sx-filter)

(defconst sx-favorite-list-filter
  (sx-filter-from-nil
   (question.question_id)))

(defvar sx-favorites--user-favorite-list nil
  "Alist of questions favorited by the user.
Each element has the form (SITE FAVORITE-LIST).  And each element
in FAVORITE-LIST is the numerical QUESTION_ID.")

(defun sx-favorites--initialize ()
  "Ensure question-favorites cache is available.
Added as hook to initialization."
  (or   (setq sx-favorites--user-favorite-list
              (sx-cache-get 'question-favorites))
        (sx-favorites-update)))
;; ;; Append to ensure `sx-network--initialize' is run before it.
;; This is removed for now because it performs a lot of API calls and
;; was never used.
;; (add-hook 'sx-init--internal-hook #'sx-favorites--initialize 'append)

(defun sx-favorites--retrieve-favorites (site)
  "Obtain list of starred QUESTION_IDs for SITE."
  (sx-method-call 'me
    :submethod 'favorites
    :site site
    :filter sx-favorite-list-filter
    :auth t))

(defun sx-favorites--update-site-favorites (site)
  "Update list of starred QUESTION_IDs for SITE.
Writes list to cache QUESTION-FAVORITES."
  (let* ((favs (sx-favorites--retrieve-favorites site))
         (site-cell (assoc site
                           sx-favorites--user-favorite-list))
         (fav-cell (mapcar #'cdar favs)))
    (if site-cell
        (setcdr site-cell fav-cell)
      (push (cons site fav-cell) sx-favorites--user-favorite-list))
  (sx-cache-set 'question-favorites sx-favorites--user-favorite-list)))

(defun sx-favorites-update ()
  "Update all sites retrieved from `sx-network--user-sites'."
  (mapc #'sx-favorites--update-site-favorites
        sx-network--user-sites))

(provide 'sx-favorites)
;;; sx-favorites.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
