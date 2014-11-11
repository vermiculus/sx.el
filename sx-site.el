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

;;

;;; Code:

(require 'sx-method)

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
    none))

(defun sx-site-get-sites ()
  (sx-method-call "sites" nil sx-site-browse-filter))

(defcustom sx-site-favorites
  nil
  "Favorite sites."
  :group 'sx-site)

(provide 'sx-site)
;;; stack-site.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
