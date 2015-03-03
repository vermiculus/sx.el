(defmacro test-with-bogus-string (cell &rest body)
  "Let-bind a bogus string to CELL and execute BODY."
  (declare (indent 1))
  `(let ((,cell "E7631BCF-A94B-4507-8F0C-02CFB3207F55"))
     ,@body))


(ert-deftest test-search-basic ()
  "Test basic search functionality"
  (should
   (sx-search-get-questions
    "emacs" 1 "emacs")))

(ert-deftest test-search-empty ()
  "Test bogus search returns empty vector"
  (test-with-bogus-string query
    (should
     (not (sx-search-get-questions "emacs" 1 query)))))

(ert-deftest test-search-invalid ()
  "Test invalid search"
  (should-error
   ;; @todo: test the interactive call
   (sx-search
    "emacs" nil nil '("emacs"))))

(ert-deftest test-search-full-page ()
  "Test retrieval of the full search page"
  (should
   (= 100 (length (sx-search-get-questions
                   "stackoverflow" 1 "jquery")))))

(ert-deftest test-search-exclude-tags ()
  "Test excluding tags from a search"
  (should
   (cl-every
    (lambda (p)
      (sx-assoc-let p
        (not (member "org-export" .tags))))
    (sx-search-get-questions
     "emacs" 1 nil "org-mode" "org-export")))
  (should
   (cl-every
    (lambda (p)
      (sx-assoc-let p
        (not (or (member "org-export" .tags)
                 (member "org-agenda" .tags)))))
    (sx-search-get-questions
     "emacs" 1 nil "org-mode"
     '("org-export" "org-agenda")))))

