(ert-deftest thing-as-string ()
  "Test `sx--thing-as-string'"
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

(ert-deftest tree ()
  (should
   (equal
    (sx--tree-expand
     (lambda (path) (mapconcat #'symbol-name path "."))
     '(a b (c d (e f g) h i (j k) l) m (n o) p))
    '("a" "b" "c.d" "c.e.f" "c.e.g" "c.h" "c.i" "c.j.k" "c.l" "m" "n.o" "p")))
  (should
   (equal
    (sx--tree-expand
     (lambda (path) (intern (mapconcat #'symbol-name path "/")))
     '(a b (c d (e f g) h i (j k) l) m (n o) p))
    '(a b c/d c/e/f c/e/g c/h c/i c/j/k c/l m n/o p))))
