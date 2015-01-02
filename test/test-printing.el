
;;; Setup
(require 'cl-lib)

(defmacro line-should-match (regexp)
  "Test if the line at point matches REGEXP"
  `(let ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
     (sx-test-message "Line here is: %S" line)
     (should (string-match ,regexp line))))

(defmacro question-list-regex (title votes answers &rest tags)
  "Construct a matching regexp for TITLE, VOTES, and ANSWERS.
Each element of TAGS is appended at the end of the expression
after being run through `sx-question--tag-format'."
  `(rx line-start
       (+ whitespace) ,(number-to-string votes)
       (+ whitespace) ,(number-to-string answers)
       (+ whitespace)
       ,title
       (+ (any whitespace digit))
       (or "y" "d" "h" "m" "mo" "s") " ago"
       (+ whitespace)
       (eval (mapconcat #'sx-question--tag-format
                        (list ,@tags) " "))))


;;; Tests
(ert-deftest question-list-tag ()
  "Test `sx-question--tag-format'."
  (should
   (string=
    (sx-question--tag-format "tag")
    "[tag]")))

(ert-deftest question-list-display ()
  (cl-letf (((symbol-function #'sx-request-make)
             (lambda (&rest _) sx-test-data-questions)))
    (sx-tab-frontpage nil "emacs")
    (switch-to-buffer "*question-list*")
    (goto-char (point-min))
    (should (equal (buffer-name) "*question-list*"))
    (line-should-match
     (question-list-regex
      "Focus-hook: attenuate colours when losing focus"
      1 0 "frames" "hooks" "focus"))
    (sx-question-list-next 5)
    (line-should-match
     (question-list-regex
      "Babel doesn&#39;t wrap results in verbatim"
      0 1 "org-mode" "org-export" "org-babel"))
    ;; ;; Use this when we have a real sx-question buffer.
    ;; (call-interactively 'sx-question-list-display-question)
    ;; (should (equal (buffer-name) "*sx-question*"))
    (switch-to-buffer "*question-list*")
    (sx-question-list-previous 4)
    (line-should-match
     (question-list-regex
      "&quot;Making tag completion table&quot; Freezes/Blocks -- how to disable"
      2 1 "autocomplete" "performance" "ctags"))))

(ert-deftest sx--user-@name ()
  "Test `sx--user-@name' character substitution"
  (should
   (string=
    (sx--user-@name '((display_name . "ĥÞßđłřğĝýÿñńśşšŝżźžçćčĉùúûüŭůòóôõöøőðìíîïıèéêëęàåáâäãåąĵ★")))
    "@hTHssdlrggyynnsssszzzccccuuuuuuooooooooiiiiieeeeeaaaaaaaaj"))
  (should
   (string=
    (sx--user-@name '((display_name . "ĤÞßĐŁŘĞĜÝŸÑŃŚŞŠŜŻŹŽÇĆČĈÙÚÛÜŬŮÒÓÔÕÖØŐÐÌÍÎÏıÈÉÊËĘÀÅÁÂÄÃÅĄĴ")))
    "@HTHssDLRGGYYNNSSSSZZZCCCCUUUUUUOOOOOOOOIIIIiEEEEEAAAAAAAAJ")))

