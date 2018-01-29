;;; sx-networks.el --- user network information      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Sean Allred

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

(defvar sx-network--user-information nil
  "User information for the various sites.")

(defvar sx-network--user-information-alist nil
  "User information for the various site parameters.")

(defvar sx-network--user-sites nil
  "List of sites where user already has an account.")

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
  (unless (setq sx-network--user-information (sx-cache-get 'network-user))
    (sx-network--update))
  (let ((url-par-alist (mapcar (lambda (x)
                                 (cons (cdr (assoc 'site_url x))
                                       (cdr (assoc 'api_site_parameter
                                                   x))))
                               (sx-site--get-site-list))))
    (setq sx-network--user-sites nil)
    (setq sx-network--user-information-alist nil)
    (mapc (lambda (nu)
            (let ((parameter (cdr (assoc (cdr (assq 'site_url nu))
                                         url-par-alist))))
              (push parameter sx-network--user-sites)
              (push (cons parameter nu)
                    sx-network--user-information-alist)))
          sx-network--user-information)))

(defun sx-network--update ()
  "Update user information.
Sets cache and then uses `sx-network--get-associated' to update
the variables."
  (setq sx-network--user-information
        (sx-method-call 'me
          :submethod 'associated
          :keywords '((types . (main_site meta_site)))
          :filter sx-network--user-filter
          :auth t))
  (sx-cache-set 'network-user sx-network--user-information))

(defun sx-network--initialize ()
  "Ensure network-user cache is available.
Added as hook to initialization."
  ;; Cache was not retrieved, retrieve it.
  (sx-network--get-associated))
(add-hook 'sx-init--internal-hook #'sx-network--initialize)

(defun sx-network-user (site)
  "Return an alist containing user information for SITE."
  (cdr (assoc site sx-network--user-information-alist)))

(provide 'sx-networks)
;;; sx-networks.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
