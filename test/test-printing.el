
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
after being run through `sx-tag--format'."
  `(rx line-start
       (+ whitespace) ,(number-to-string votes)
       (+ whitespace) ,(number-to-string answers)
       (+ whitespace)
       ,title
       (+ (any whitespace digit))
       (or "y" "d" "h" "m" "mo" "s") " ago"
       (+ whitespace)
       (eval (mapconcat #'sx-tag--format (list ,@tags) " "))))


;;; Tests
(ert-deftest time-since ()
  (cl-letf (((symbol-function #'float-time)
           (lambda () 1420148997.)))
    (should
     (string=
      "67m"
      (sx-time-since 1420145000.)))
    (should
     (string=
      "12h"
      (sx-time-since 1420105000.)))))

(ert-deftest question-list-tag ()
  "Test `sx-tag--format'."
  (should
   (string= (sx-tag--format "tag") "[tag]"))
  (with-temp-buffer
    (insert (sx-tag--format "tag"))
    (should (get-char-property (point-min) 'button))
    (should
     (eq (get-char-property (point-min) 'face) 'sx-tag))
    (should
     (string= (get-char-property (point-min) 'sx-tag) "tag"))
    (should
     (string= (get-char-property (point-min) 'sx-button-copy) "tag"))))

(ert-deftest question-list-display ()
  (cl-letf (((symbol-function #'sx-request-make)
             (lambda (&rest _) sx-test-data-questions)))
    (sx-tab-all-questions nil "emacs")
    (switch-to-buffer sx-tab--all-questions-buffer)
    (goto-char (point-min))
    (should (equal (buffer-name)
                   (format "*question-list: %s *" sx-question-list--current-tab)))
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
    ;; (save-excursion
    ;;   (call-interactively 'sx-question-list-display-question)
    ;;   (should (equal (buffer-name) "*sx-question*")))
    (sx-question-list-previous 4)
    (line-should-match
     (question-list-regex
      "&quot;Making tag completion table&quot; Freezes/Blocks -- how to disable"
      2 1 "autocomplete" "performance" "ctags"))))

(ert-deftest sx--user-@name ()
  "Test `sx--user-@name' character substitution"
  (should
   (string=
    (sx-user--@name "ĥÞßđłřğĝýÿñńśşšŝżźžçćčĉùúûüŭůòóôõöøőðìíîïıèéêëęàåáâäãåąĵ★")
    "@hTHssdlrggyynnsssszzzccccuuuuuuooooooooiiiiieeeeeaaaaaaaaj"))
  (should
   (string=
    (sx-user--@name "ĤÞßĐŁŘĞĜÝŸÑŃŚŞŠŜŻŹŽÇĆČĈÙÚÛÜŬŮÒÓÔÕÖØŐÐÌÍÎÏıÈÉÊËĘÀÅÁÂÄÃÅĄĴ")
    "@HTHssDLRGGYYNNSSSSZZZCCCCUUUUUUOOOOOOOOIIIIiEEEEEAAAAAAAAJ"))
  (should-error
   (sx-user--@name 2)))

(ert-deftest sx-user--format ()
  "Test various `sx-user--format' features."
  (let ((user
         '((display_name . "ĥÞßđłřğĝýÿñńśşšŝżźžçćčĉùúûüŭůòóôõöøőðìíîïıèéêëęàåáâäãåąĵ★")
           (accept_rate . 90)
           (reputation . 10)
           (link . "link"))))
    (should
     (equal (sx-user--format "%l" user) "link"))
    (should
     (equal
      (sx-user--format "%@" user)
      "@hTHssdlrggyynnsssszzzccccuuuuuuooooooooiiiiieeeeeaaaaaaaaj"))
    (should
     (equal
      (sx-user--format "%@%%d%%%-30d  %9r%l" user)
      "@hTHssdlrggyynnsssszzzccccuuuuuuooooooooiiiiieeeeeaaaaaaaaj%d%ĥÞßđłřğĝýÿñńśşšŝżźžçćčĉùúûüŭůòóôõöøőðìíîïıèéêëęàåáâäãåąĵ★         10link")))
  (should
   (string=
    (sx-user--format "%@" '((display_name . "ĤÞßĐŁŘĞĜÝŸÑŃŚŞŠŜŻŹŽÇĆČĈÙÚÛÜŬŮÒÓÔÕÖØŐÐÌÍÎÏıÈÉÊËĘÀÅÁÂÄÃÅĄĴ")))
    "@HTHssDLRGGYYNNSSSSZZZCCCCUUUUUUOOOOOOOOIIIIiEEEEEAAAAAAAAJ")))

(ert-deftest sx-object-modification ()
  "Test adding things to objects"
  (let ((object '((owner . "me"))))
    (should
     (equal (sx--ensure-owner-in-object 1 object)
            '((owner . "me"))))
    (should
     (equal object '((owner . "me")))))
  (let ((object '((not-owner . "me"))))
    (should
     (equal (sx--ensure-owner-in-object 1 object)
            '((owner . 1) (not-owner . "me"))))
    (should
     (equal object '((owner . 1) (not-owner . "me")))))
  (let ((object '((comments . (something)))))
    (should
     (equal (sx--add-comment-to-object "comment" object)
            '((comments . (something "comment")))))
    (should
     (equal object '((comments . (something "comment"))))))
  (let ((object '((not-comments . (something)))))
    (should
     (equal (sx--add-comment-to-object "comment" object)
            '((comments . ("comment")) (not-comments . (something)))))
    (should
     (equal object '((comments . ("comment")) (not-comments . (something))))))
  (let ((object '((not-answers . (something)))))
    (should
     (equal (sx--add-answer-to-question-object "answer" object)
            '((answers . ("answer")) (not-answers . (something)))))
    (should
     (equal object '((answers . ("answer")) (not-answers . (something))))))
  (let ((object '((answers . (something)))))
    (should
     (equal (sx--add-answer-to-question-object "answer" object)
            '((answers . (something "answer")))))
    (should
     (equal object '((answers . (something "answer")))))))


;;; question-mode
(ert-deftest sx-display-question ()
  ;; Check it doesn't error.
  (sx-display-question (elt sx-test-data-questions 0))
  ;; Check it does error.
  (should-error
   (sx-display-question sx-test-data-questions))
  (should-error
   (sx-display-question sx-test-data-questions nil 1)))

(ert-deftest sx-question-mode--fill-and-fontify ()
  "Check complicated questions are filled correctly."
  (with-temp-buffer
    (sx-question-mode--insert-markdown
     "Creating an account on a new site requires you to log into that site using *the same credentials you used on existing sites.* For instance, if you used the Stack Exchange login method, you'd...

1. Click the \"Log in using Stack Exchange\" button:

  ![image][1]

2. Enter your username and password (yes, even if you *just did this* to log into, say, Stack Overflow) and press the \"Log In\" button:

  [link][2]

3. Confirm the creation of the new account:

  [![image-in-link][3]](emacs.stackexchange.com)

        some code block
        some code block
        some code block
        some code block
        some code block
        some code block

  [1]: http://i.stack.imgur.com/ktFTs.png
  [2]: http://i.stack.imgur.com/5l2AY.png
  [3]: http://i.stack.imgur.com/22myl.png")
    (when sx-question-mode-use-images
      (should (overlays-in (point-min) (point-max)))
      (should (= 2 (length (overlays-in (point-min) (point-max))))))
    (if sx-question-mode-use-images
        (should
         (equal
          (buffer-substring-no-properties (point-min) (point-max))
          "Creating an account on a new site requires you to log into that site
using *the same credentials you used on existing sites.* For instance,
if you used the Stack Exchange login method, you'd...

1. Click the \"Log in using Stack Exchange\" button:


¶



2. Enter your username and password (yes, even if you *just did this*
   to log into, say, Stack Overflow) and press the \"Log In\" button:

link

3. Confirm the creation of the new account:


¶



        some code block
        some code block
        some code block
        some code block
        some code block
        some code block
        
  [1]: http://i.stack.imgur.com/ktFTs.png
  [2]: http://i.stack.imgur.com/5l2AY.png
  [3]: http://i.stack.imgur.com/22myl.png"))
      (should
       (equal
        (buffer-substring-no-properties (point-min) (point-max))
        "Creating an account on a new site requires you to log into that site
using *the same credentials you used on existing sites.* For instance,
if you used the Stack Exchange login method, you'd...

1. Click the \"Log in using Stack Exchange\" button:

image

2. Enter your username and password (yes, even if you *just did this*
   to log into, say, Stack Overflow) and press the \"Log In\" button:

link

3. Confirm the creation of the new account:

image-in-link

        some code block
        some code block
        some code block
        some code block
        some code block
        some code block
        
  [1]: http://i.stack.imgur.com/ktFTs.png
  [2]: http://i.stack.imgur.com/5l2AY.png
  [3]: http://i.stack.imgur.com/22myl.png")))))
