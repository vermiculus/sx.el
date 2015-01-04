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

(ert-deftest macro-test--sx-filter-from-nil ()
  "Test `sx-filter-from-nil'"
  (should
   (equal
    (sx-filter-from-nil
     (one two (three four five) (six seven)
          (a b c d e)))
    '((one two three.four three.five six.seven
           a.b a.c a.d a.e
           .backoff
           .error_id
           .error_message
           .error_name
           .has_more
           .items
           .page
           .page_size
           .quota_max
           .quota_remaining
           .total)
      nil none))))
