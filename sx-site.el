;;; sx-site.el --- site functions

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: 

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

;;; @TODO use new caching system implemented in branch `network-list'
(defun sx-site-get-api-tokens ()
  "Return a list of all known "
  (mapcar
   (lambda (site) (cdr (assoc 'api_site_parameter site)))
   (sx-method-call "sites" '((pagesize . 9999)))))

(provide 'sx-site)
;;; sx-site.el ends here
