;;; sx-search.el --- searching for questions         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Artur Malabarba

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

;; Implements search functionality.  The basic function is
;; `sx-search-get-questions', which returns an array of questions
;; according to a search term.
;;
;; This also defines a user-level command, `sx-search', which is an
;; interactive wrapper around `sx-search-get-questions' and
;; `sx-question-list-mode'.


;;; Code:

(require 'sx)
(require 'sx-question-list)
(require 'sx-question-mode)
(require 'sx-tag)
(require 'sx-interaction)

(defvar sx-search--query-history nil
  "Query history for interactive prompts.")


;;; Basic function
(defun sx-search-get-questions (site page query
                                     &optional tags excluded-tags
                                     &rest keywords)
  "Like `sx-question-get-questions', but restrict results by a search.

Perform search on SITE.  PAGE is an integer indicating which page
of results to return.  QUERY, TAGS, and EXCLUDED-TAGS restrict the
possible returned questions as per `sx-search'.

Either QUERY or TAGS must be non-nil, or the search will
fail.  EXCLUDED-TAGS is only is used if TAGS is also provided.

KEYWORDS is passed to `sx-method-call'."
  (sx-method-call 'search/advanced
    :keywords `((page . ,page)
                (q . ,query)
                (tagged . ,tags)
                (nottagged . ,excluded-tags)
                ,@keywords)
    :site site
    :auth t
    :filter sx-browse-filter))

(defconst sx-search--order-methods
  (cons '("Relevance" . relevance)
        (default-value 'sx-question-list--order-methods))
  "Alist of possible values to be passed to the `sort' keyword.")

(defcustom sx-search-default-order 'activity
  "Default ordering method used on new searches.
Possible values are the cdrs of `sx-search--order-methods'."
  :type (cons 'choice
              (mapcar (lambda (c) `(const :tag ,(car c) ,(cdr c)))
                (cl-remove-duplicates
                 sx-search--order-methods
                 :key #'cdr)))
  :group 'sx-question-list)


;;;###autoload
(defun sx-search (site query &optional tags excluded-tags)
  "Display search on SITE for question titles containing QUERY.
When TAGS is given, it is a lists of tags, one of which must
match.  When EXCLUDED-TAGS is given, it is a list of tags, none
of which is allowed to match.

Interactively, the user is asked for SITE and QUERY.  With a
prefix argument, the user is asked for everything."
  (interactive
   (let ((site (sx--maybe-site-prompt current-prefix-arg))
         (query (read-string
                 (format "Query (%s): "
                   (if current-prefix-arg "optional" "mandatory"))
                 ""
                 'sx-search--query-history))
         tags excluded-tags)
     (when (string= query "")
       (setq query nil))
     (when current-prefix-arg
       (setq tags (sx-tag-multiple-read
                   site (concat "Tags" (when query " (optional)"))))
       (unless (or query tags)
         (sx-user-error "Must supply either QUERY or TAGS"))
       (setq excluded-tags
             (sx-tag-multiple-read site "Excluded tags (optional)")))
     (list site query tags excluded-tags)))

  ;; Here starts the actual function
  (sx-initialize)
  (with-current-buffer (get-buffer-create "*sx-search-result*")
    (sx-question-list-mode)
    (setq sx-question-list--next-page-function
          (lambda (page)
            (sx-search-get-questions
             sx-question-list--site page
             query tags excluded-tags
             (cons 'order (if sx-question-list--descending 'desc 'asc))
             (cons 'sort sx-question-list--order))))
    (setq sx-question-list--site site)
    (setq sx-question-list--order sx-search-default-order)
    (setq sx-question-list--order-methods sx-search--order-methods)
    (sx-question-list-refresh 'redisplay)
    (switch-to-buffer (current-buffer))))


;;; Tag
;;;###autoload
(defun sx-search-tag-at-point (&optional pos)
  "Follow tag under position POS or point."
  (interactive)
  (let ((tag (save-excursion
               (when pos (goto-char pos))
               (or (get-text-property (point) 'sx-tag)
                   (thing-at-point 'symbol))))
        (meta (save-excursion
                (when pos (goto-char pos))
                (get-text-property (point) 'sx-tag-meta)))
        (site (replace-regexp-in-string
               (rx string-start "meta.") ""
               (or sx-question-list--site
                   (sx-assoc-let sx-question-mode--data .site_par)))))
    (sx-search (concat (when meta "meta.") site)
               nil tag)))

(provide 'sx-search)
;;; sx-search.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
