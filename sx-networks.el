;;; sx-networks.el --- user network information  -*- lexical-binding: t; -*-

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
(require 'sx-cache)
(require 'sx-site)

(defvar sx-network--user-filter
  '((.backoff
     .error_id
     .error_message
     .error_name
     .has_more
     .items
     .quota_max
     .quota_remaining
     badge_count.bronze
     badge_count.silver
     badge_count.gold
     network_user.account_id
     network_user.answer_count
     network_user.badge_counts
     network_user.creation_date
     network_user.last_access_date
     network_user.reputation
     network_user.site_name
     network_user.site_url
     network_user.user_id
     network_user.user_type)
    nil
    none))

(defun sx-network--get-associated ()
  (or (sx-cache-get
       'network-user)
      (sx-network--update))

(defun sx-network--update ()
  "Update user information."
  (setq sx-network--user-information
        (sx-method-call "me/associated"
                        '((types . "main_site;meta_site"))
                        sx-network--user-filter
                        'warn))
  (setq sx-network--user-sites (sx-network--map-site-url-to-site-api))
  (sx-cache-set 'network-user sx-network--user-information))

(defun sx-network--ensure-user ()
  "Ensure user-cache is available.

This should be called during initialization."
  (cond
   ((not sx-network--user-information)
    (or (sx-network--get-associated)
        (sx-network--update)))
   ((not sx-network--user-sites)
    (setq sx-network--user-sites
          (sx-network--map-site-url-to-site-api)))))

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
    (mapcar '(lambda (loc)
               (let ((u-site (cdr (assoc 'site_url loc))))
                 (when (member u-site (mapcar 'car sites-info))
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
