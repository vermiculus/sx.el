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
