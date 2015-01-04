;;; sx-tag.el --- retrieving list of tags and handling tags  -*- lexical-binding: t; -*-

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
(eval-when-compile
  '(require 'cl-lib))

(require 'sx)
(require 'sx-method)


;;; Getting the list from a site
(defconst sx-tag-filter
  (sx-filter-from-nil
   (tag.name
    tag.synonyms))
  "Filter used when querying tags.")

(defun sx-tag--get-all (site &optional no-synonyms)
  "Retrieve all tags for SITE.
If NO-SYNONYMS is non-nil, don't return synonyms."
  (cl-reduce
   (lambda (so-far tag)
     (let-alist tag
       (cons .name
             (if no-synonyms so-far
               (append .synonyms so-far)))))
   (sx-method-call 'tags
     :get-all t
     :filter sx-tag-filter
     :site site)
   :initial-value nil))

(defun sx-tag--get-some-tags-containing (site string)
  "Return at most 100 tags for SITE containing STRING.
Returns an array."
  (sx-method-call 'tags
    :auth nil
    :filter sx-tag-filter
    :site site
    :keywords `((inname . ,string))))

(defun sx-tag--get-some-tag-names-containing (site string)
  "Return at most 100 tag names for SITE containing STRING.
Returns a list."
  (mapcar (lambda (x) (cdr (assoc 'name x)))
          (sx-tag--get-some-tags-containing site string)))


;;; Check tag validity
(defun sx-tag--invalid-name-p (site tags)
  "Nil if TAGS exist in SITE.
TAGS can be a string (the tag name) or a list of strings.
Fails if TAGS is a list with more than 100 items.
Return the list of invalid tags in TAGS."
  (and (listp tags) (> (length tags) 100)
       (error "Invalid argument. TAG has more than 100 items"))
  (let ((result
         (mapcar
          (lambda (x) (cdr (assoc 'name x)))
          (sx-method-call 'tags
            :id (sx--thing-as-string tags)
            :submethod 'info
            :auth nil
            :filter sx-tag-filter
            :site site))))
    (cl-remove-if (lambda (x) (member x result)) tags)))

(provide 'sx-tag)
;;; sx-tag.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
