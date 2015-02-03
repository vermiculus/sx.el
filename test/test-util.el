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

(ert-deftest link-to-data ()
  (should
   (equal
    (sx--link-to-data "http://meta.emacs.stackexchange.com/posts/comments/510?noredirect=1")
    '((id . 510) (type . comment) (site_par . "meta.emacs"))))
  (should
   (equal
    (sx--link-to-data "http://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function#comment10965_7409")
    '((id . 10965) (type . comment) (site_par . "emacs"))))
  (should
   (equal
    (sx--link-to-data "http://emacs.stackexchange.com/q/7409/50")
    '((id . 7409) (type . question) (site_par . "emacs"))))
  (should
   (equal
    (sx--link-to-data "http://emacs.stackexchange.com/a/7410/50")
    '((id . 7410) (type . answer) (site_par . "emacs"))))
  (should
   (equal
    (sx--link-to-data "http://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function/9999#7410")
    '((id . 7410) (type . answer) (site_par . "emacs"))))
  (should
   (equal
    (sx--link-to-data "http://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function/7410")
    '((id . 7410) (type . answer) (site_par . "emacs")))))
