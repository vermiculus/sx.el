;;; sx-site.el --- browsing sites                    -*- lexical-binding: t; -*-

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

;;; Code:

(require 'sx-method)
(require 'sx-cache)

(defvar sx-site-browse-filter
  '((.backoff
     .error_id
     .error_message
     .error_name
     .has_more
     .items
     .quota_max
     .quota_remaining
     site.site_type
     site.name
     site.site_url
     site.api_site_parameter
     site.related_sites
     related_site.api_site_parameter
     related_site.relation)
    nil
    none)
  "Filter for browsing sites.")

(defun sx-site--get-site-list ()
  "Return all sites with `sx-site-browse-filter'."
  (sx-cache-get
   'site-list
   '(sx-method-call 'sites
      :keywords '((pagesize . 999))
      :filter sx-site-browse-filter)))

(defcustom sx-site-favorites
  nil
  "List of favorite sites.
Each entry is a string corresponding to a single site's
api_site_parameter."
  :group 'sx)

(defun sx-site-get-api-tokens ()
  "Return a list of all known site tokens."
  (mapcar
   (lambda (site) (cdr (assoc 'api_site_parameter site)))
   (sx-site--get-site-list)))

(provide 'sx-site)
;;; sx-site.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
