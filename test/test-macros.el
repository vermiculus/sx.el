(ert-deftest macro-test--sx-assoc-let ()
  "Tests macro expansion for `sx-assoc-let'"
  (let ((prototype '((test . nil) (test-one . 1) (test-two . 2)
                     (link . "http://meta.emacs.stackexchange.com/"))))
    (let ((data (copy-tree prototype)))
      (should
       (null (let-alist data .site))))

    (let ((data (copy-tree prototype)))
      (should
       (equal (sx-assoc-let data .site)
              "meta.emacs")))

    (let ((data (copy-tree prototype)))
      (should
       (equal (sx-assoc-let data (cons .test-one .test-two))
              '(1 . 2))))))

