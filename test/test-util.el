(ert-deftest test-tree-filter ()
  "`sx-core-filter-data'"
  ;; flat
  (should
   (equal
    '((1 . t) (2 . [1 2]) (3))
    (sx--filter-data '((0 . 3) (1 . t) (a . five) (2 . [1 2])
                       ("5" . bop) (3) (p . 4))
                     '(1 2 3))))
  ;; complex
  (should
   (equal
    '((1 . [a b c])
      (2 . [((a . 1) (c . 3))
            ((a . 4) (c . 6))])
      (3 . peach))
    (sx--filter-data '((1 . [a b c])
                       (2 . [((a . 1) (b . 2) (c . 3))
                             ((a . 4) (b . 5) (c . 6))])
                       (3 . peach)
                       (4 . banana))
                     '(1 (2 a c) 3))))

  ;; vector
  (should
   (equal
    [((1 . 2) (2 . 3) (3 . 4))
     ((1 . a) (2 . b) (3 . c))
     nil ((1 . alpha) (2 . beta))]
    (sx--filter-data [((1 . 2) (2 . 3) (3 . 4))
                      ((1 . a) (2 . b) (3 . c) (5 . seven))
                      ((should-not-go))
                      ((1 . alpha) (2 . beta))]
                     '(1 2 3)))))

(ert-deftest thing-as-string ()
  "Tests `sx--thing-as-string'"
  (should
   (string= (sx--thing-as-string
             '(hello world (this is a test))
             '(";" "+"))
            "hello;world;this+is+a+test"))
  (should
   (string= (sx--thing-as-string
             '(this is a test) '(";" "+"))
            "this;is;a;test"))
  (should
   (string= (sx--thing-as-string
             '(this is a test) "+")
            "this+is+a+test"))
  (should
   (string= (sx--thing-as-string
             '(this is a test))
            "this;is;a;test"))
  (should
   (string= (sx--thing-as-string
             'test)
            "test"))
  (should
   (string= (sx--thing-as-string
             'test&)
            "test&"))
  (should
   (string= (sx--thing-as-string
             'test& nil t)
            "test%26")))
