(defvar sample-question-unauthenticated
  (json-read-file "sample-question-unauthenticated.json"))
(defvar sample-frontpage
  (json-read-file "sample-frontpage.json"))

;;; A sample of data offered by the question object, without authentication.
;; (pp sample-question-unauthenticated (current-buffer))

(defun sample-question-markdown ()
  "Renders the question list usiong markdown for the content.
Doesn't seem like markdown gets fontified. Which is disapointing. :-("
  (interactive)
  (find-file "sample.org")
  (setf (buffer-string)
        (org-element-interpret-data
         (stack-lto--question sample-question-unauthenticated))))

(defun sample-frontpage-markdown ()
  "Renders the question list usiong markdown for the content.
Doesn't seem like markdown gets fontified. Which is disapointing. :-("
  (interactive)
  (find-file "sample.org")
  (erase-buffer)
  (mapcar
   (lambda (x) 
     (insert 
      (org-element-interpret-data
       (stack-lto--question x))))
   (cdr (assoc 'items sample-frontpage)))
  (org-global-cycle 1))

(provide 'sample-question-unauthenticated)


