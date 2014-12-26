(defun -sx--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "sx-" (symbol-name symbol))
         (unintern symbol)))))

;;; Tests
(defvar sx-test-data-dir
  (expand-file-name
   "data-samples/"
   (file-name-directory (or load-file-name "./"))))

(defun sx-test-sample-data (method &optional directory)
  (let ((file (concat (when directory (concat directory "/"))
                      sx-test-data-dir
                      method ".el")))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (buffer-string))))))

(defmacro line-should-match (regexp)
  ""
  `(let ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
     (message "Line here is: %S" line)
     (should (string-match ,regexp line))))

(setq
 sx-initialized t
 sx-request-remaining-api-requests-message-threshold 50000
 debug-on-error t
 user-emacs-directory "."

 sx-test-data-questions
 (sx-test-sample-data "questions")
 sx-test-data-sites
 (sx-test-sample-data "sites"))

(setq package-user-dir
      (expand-file-name (format "../../.cask/%s/elpa" emacs-version)
                        sx-test-data-dir))
(package-initialize)

(require 'cl-lib)
(require 'sx)
(require 'sx-question)
(require 'sx-question-list)
(require 'sx-tab)

(ert-deftest test-basic-request ()
  "Test basic request functionality"
  (should (sx-request-make "sites")))

(ert-deftest test-question-retrieve ()
  "Test the ability to receive a list of questions."
  (should (sx-question-get-questions 'emacs)))

(ert-deftest test-bad-request ()
  "Test a method given a bad set of keywords"
  (should-error
   (sx-request-make "questions" '(()))))

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

(ert-deftest question-list-display ()
  (cl-letf (((symbol-function #'sx-request-make)
             (lambda (&rest _) sx-test-data-questions)))
    (sx-tab-frontpage nil "emacs")
    (switch-to-buffer "*question-list*")
    (goto-char (point-min))
    (should (equal (buffer-name) "*question-list*"))
    (line-should-match
     "^\\s-+1\\s-+0\\s-+Focus-hook: attenuate colours when losing focus [ 0-9]+\\(y\\|d\\|h\\|mo?\\|s\\) ago\\s-+\\[frames\\] \\[hooks\\] \\[focus\\]")
    (sx-question-list-next 5)
    (line-should-match
     "^\\s-+0\\s-+1\\s-+Babel doesn&#39;t wrap results in verbatim [ 0-9]+\\(y\\|d\\|h\\|mo?\\|s\\) ago\\s-+\\[org-mode\\]")
    ;; ;; Use this when we have a real sx-question buffer.
    ;; (call-interactively 'sx-question-list-display-question)
    ;; (should (equal (buffer-name) "*sx-question*"))
    (switch-to-buffer "*question-list*")
    (sx-question-list-previous 4)
    (line-should-match
     "^\\s-+2\\s-+1\\s-+&quot;Making tag completion table&quot; Freezes/Blocks -- how to disable [ 0-9]+\\(y\\|d\\|h\\|mo?\\|s\\) ago\\s-+\\[autocomplete\\]")))

(ert-deftest macro-test--sx-assoc-let ()
  "Tests macro expansion for `sx-assoc-let'"
  (should
   (equal '(progn (require 'let-alist)
                  (sx--ensure-site data)
                  (let-alist data .test))
          (macroexpand '(sx-assoc-let data .test))))
  (should
   (equal '(progn (require 'let-alist)
                  (sx--ensure-site data)
                  (let-alist data (cons .test-one .test-two)))
          (macroexpand
           '(sx-assoc-let data (cons .test-one .test-two))))))

(ert-deftest sx--user-@name ()
  "Tests macro expansion for `sx-assoc-let'"
  (should
   (string=
    (sx--user-@name '((display_name . "ĥÞßđłřğĝýÿñńśşšŝżźžçćčĉùúûüŭůòóôõöøőðìíîïıèéêëęàåáâäãåąĵ★")))
    "@hTHssdlrggyynnsssszzzccccuuuuuuooooooooiiiiieeeeeaaaaaaaaj"))
  (should
   (string=
    (sx--user-@name '((display_name . "ĤÞßĐŁŘĞĜÝŸÑŃŚŞŠŜŻŹŽÇĆČĈÙÚÛÜŬŮÒÓÔÕÖØŐÐÌÍÎÏıÈÉÊËĘÀÅÁÂÄÃÅĄĴ")))
    "@HTHssDLRGGYYNNSSSSZZZCCCCUUUUUUOOOOOOOOIIIIiEEEEEAAAAAAAAJ")))

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
