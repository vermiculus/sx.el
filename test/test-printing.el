
;;; Setup
(require 'cl-lib)

(defmacro line-should-match (regexp)
  ""
  `(let ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
     (message "Line here is: %S" line)
     (should (string-match ,regexp line))))


;;; Tests
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

