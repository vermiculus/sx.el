(defmacro sx-test-with-json-data (cell &rest body)
    (declare (indent 1))
    `(let ((,cell '((test . nil) (test-one . 1) (test-two . 2)
                   (link . "http://meta.emacs.stackexchange.com/"))))
       ,@body))

(ert-deftest macro-test--sx-assoc-let ()
  "Tests macro expansion for `sx-assoc-let'"
  (sx-test-with-json-data data
    (should
     (null (let-alist data .site))))

  (sx-test-with-json-data data
    (should
     (equal (sx-assoc-let data .site)
            "meta.emacs")))

  (sx-test-with-json-data data
    (should
     (equal (sx-assoc-let data (cons .test-one .test-two))
            '(1 . 2)))))
