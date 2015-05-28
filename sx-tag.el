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
(require 'sx-button)

(defface sx-tag
  '((t :underline nil :inherit font-lock-function-name-face))
  "Face used on the question tags in the question buffer."
  :group 'sx-question-mode-faces
  :group 'sx-question-list-faces)


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


;;; Getting tags from our data branch. Without the API.
;;;; @TODO: Once the cache is finished, this can probably be made into
;;;; a cache variasble with 1 day expiration time.
(defvar sx-tag-list-alist nil
  "Alist where the tag list for each site is stored.
Elements are of the type (SITE . TAG-LIST).")

(defun sx-tag-list--get (site)
  "Retrieve all tags from SITE in a single request.
This does not access the API.  Instead, it uses
`sx-request-get-data', which accesses SX's tag cache."
  (or (cdr (assoc site sx-tag-list-alist))
      (let ((list (sx-request-get-data (concat "tags/" site))))
        (push (cons site list) sx-tag-list-alist)
        list)))


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


;;; Prompt the user for tags.
(defvar sx-tag-history nil
  "Tags history for interactive prompts.")

;;; @TODO: Make it so that hitting BACKSPACE with an empty input
;;; deletes a previously submitted tag.
(defun sx-tag-multiple-read (site prompt &optional initial-value)
  "Interactively read a list of tags for SITE.
Call `sx-completing-read' multiple times, until input is empty,
with completion options given by the tag list of SITE.
Return a list of tags given by the user.

PROMPT is a string displayed to the user and should not end with
a space nor a colon.  INITIAL-VALUE is a list of already-selected
tags."
  (let ((completion-list (sx-tag-list--get site))
        (list (reverse initial-value))
        (empty-string 
         (propertize "--\x000-some-string-representing-empty-\x000--"
                     'display "DONE"))
        input)
    (while (not (string=
                 empty-string
                 (setq input (sx-completing-read
                              (concat prompt " ["
                                      (mapconcat #'identity (reverse list) ",")
                                      "]: ")
                              completion-list
                              nil 'require-match nil 'sx-tag-history
                              empty-string))))
      (push input list))
    (reverse list)))


;;; Printing
(defun sx-tag--format (tag &optional meta)
  "Format and return TAG for display.
If META is non-nil, the tag is for the meta site."
  (with-temp-buffer
    (sx-tag--insert tag meta)
    (buffer-string)))

(defun sx-tag--insert (tag &optional meta)
  "Insert TAG button.
If META is non-nil, the tag is for the meta site."
  (insert-text-button (concat "[" tag "]")
                      'sx-button-copy tag
                      'sx-tag tag
                      'sx-tag-meta meta
                      :type 'sx-button-tag))

(defun sx-tag--format-tags (tags &optional site)
  "Format and concatenate a sequence of TAGS.
Returns a string of all tags in TAGS, separated by a space.

SITE is the site to which the tags refer, it is only used to
decide whether they are main or meta tags.  SITE can also be t or
nil, which respectively indicate meta and main."
  (let ((is-meta
         (if (stringp site) (string-match (rx string-start "meta.") site)
           site)))
    (mapconcat (lambda (tag) (sx-tag--format tag is-meta))
               tags " ")))

(provide 'sx-tag)
;;; sx-tag.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
