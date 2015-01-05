;;; sx-search.el --- searching for questions         -*- lexical-binding: t; -*-

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

;; Implements sarch functionality.  The basic function is
;; `sx-search-get-questions', which returns an array of questions
;; according to a search term.
;;
;; This also defines a user-level command, `sx-search', which is an
;; interactive wrapper around `sx-search-get-questions' and
;; `sx-question-list-mode'.


;;; Code:

(require 'sx)
(require 'sx-question-list)

(defvar sx-search--query-history nil
  "Query history for interactive prompts.")

(defvar sx-search--tag-history nil
  "Tags history for interactive prompts.")


;;; Basic function
(defun sx-search-get-questions (site page query &optional tags excluded-tags keywords)
  "Like `sx-question-get-questions', but restrict results by a search.

Perform search on SITE.  PAGE is an integer indicating which page
of results to return.  QUERY, TAGS, and EXCLUDED-TAGS restrict the
possible returned questions as per `sx-search'.

Either QUERY or TAGS must be non-nil, or the search will
fail.  EXCLUDED-TAGS is only is used if TAGS is also provided.

KEYWORDS is passed to `sx-method-call'."
  (sx-method-call 'search
    :keywords `((page . ,page)
                (sort . activity)
                (intitle . ,query)
                (tagged . ,tags)
                (nottagged . ,excluded-tags)
                ,@keywords)
    :site site
    :auth t
    :filter sx-browse-filter))


;;; User command
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
       (setq tags (sx--multiple-read
                   (format "Tags (%s)"
                     (if query "optional" "mandatory"))
                   'sx-search--tag-history))
       (when (and (not query) (string= "" tags))
         (sx-user-error "Must supply either QUERY or TAGS"))
       (setq excluded-tags
             (sx--multiple-read
              "Excluded tags (optional)" 'sx-search--tag-history)))
     (list site query tags excluded-tags)))
  
  ;; Here starts the actual function
  (sx-initialize)
  (with-current-buffer (get-buffer-create "*sx-search-result*")
    (sx-question-list-mode)
    (setq sx-question-list--next-page-function
          (lambda (page)
            (sx-search-get-questions
             sx-question-list--site page
             query tags excluded-tags)))
    (setq sx-question-list--site site)
    (sx-question-list-refresh 'redisplay)
    (switch-to-buffer (current-buffer))))

(provide 'sx-search)
;;; sx-search.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
