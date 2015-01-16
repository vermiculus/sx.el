;;; sx-networks.el --- user network information      -*- lexical-binding: t; -*-

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

;; This file provides logic for retrieving information about the user
;; across the entire network, e.g. their registered sites.

;;; Code:

(require 'sx-method)
(require 'sx-cache)
(require 'sx-site)
(require 'sx-filter)

(defconst sx-network--user-filter
  (sx-filter-from-nil
   ((badge_count bronze
                 silver
                 gold)
    (network_user account_id
                  answer_count
                  badge_counts
                  creation_date
                  last_access_date
                  reputation
                  site_name
                  site_url
                  user_id
                  user_type))))

(defun sx-network--get-associated ()
  "Retrieve cached information for network user.
If cache is not available, retrieve current data."
  (or (and (setq sx-network--user-information (sx-cache-get 'network-user)
                 sx-network--user-sites
                 (sx-network--map-site-url-to-site-api)))
      (sx-network--update)))

(defun sx-network--update ()
  "Update user information.
Sets cache and then uses `sx-network--get-associated' to update
the variables."
  (sx-cache-set 'network-user
                (sx-method-call 'me
                  :submethod 'associated
                  :keywords '((types . (main_site meta_site)))
                  :filter sx-network--user-filter
                  :auth t))
  (sx-network--get-associated))

(defun sx-network--initialize ()
  "Ensure network-user cache is available.
Added as hook to initialization."
  ;; Cache was not retrieved, retrieve it.
  (sx-network--get-associated))
(add-hook 'sx-init--internal-hook #'sx-network--initialize)

(defun sx-network--map-site-url-to-site-api ()
  "Convert `me/associations' to a set of `api_site_parameter's.
`me/associations' does not return `api_site_parameter' so cannot
be directly used to retrieve content per site.  This creates a
list of sites the user is active on."
  (let ((sites-info (mapcar (lambda (x)
                              (cons (cdr (assoc 'site_url x))
                                    (cdr (assoc 'api_site_parameter
                                                x))))
                            (sx-site--get-site-list))))
    (mapcar (lambda (loc)
              (let ((u-site (cdr (assoc 'site_url loc))))
                (when (member u-site (mapcar #'car sites-info))
                  (cdr (assoc u-site sites-info)))))
            sx-network--user-information)))

(defvar sx-network--user-information nil
  "User information for the various sites.")

(defvar sx-network--user-sites nil
  "List of sites where user already has an account.")

(provide 'sx-networks)
;;; sx-networks.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
