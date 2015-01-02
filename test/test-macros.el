(defmacro sx-test-with-json-data (cell &rest body)
  "Run BODY with sample data let-bound to CELL"
  (declare (indent 1))
  `(let ((,cell '((test . nil) (test-one . 1) (test-two . 2)
                  (link . "http://meta.emacs.stackexchange.com/"))))
     ,@body))

(ert-deftest macro-test--sx-assoc-let ()
  "Test `sx-assoc-let'"
  (sx-test-with-json-data data
    (should
     (null (let-alist data .site_par))))

  (sx-test-with-json-data data
    (should
     (equal (sx-assoc-let data .site_par)
            "meta.emacs")))

  (sx-test-with-json-data data
    (should
     (equal (sx-assoc-let data (cons .test-one .test-two))
            '(1 . 2)))))
