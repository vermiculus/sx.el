(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
         (unintern symbol)))))

;;; Tests
(defvar stack-test-data-dir
  (expand-file-name
   "data-samples/"
   (or (file-name-directory load-file-name) "./"))
  "")

(defun stack-test-sample-data (method &optional directory)
  (let ((file (concat (when directory (concat directory "/"))
                      stack-test-data-dir
                      method ".el")))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (buffer-string))))))

(setq
 stack-core-remaining-api-requests-message-threshold 50000
 debug-on-error t
 stack-core-silent-requests nil
 user-emacs-directory "."

 stack-test-data-questions
 (stack-test-sample-data "questions")
 stack-test-data-sites
 (stack-test-sample-data "sites"))

(require 'stack-core)
(require 'stack-question)
(require 'stack-question-list)
(require 'cl-lib)

(ert-deftest test-basic-request ()
  "Test basic request functionality"
  (should (stack-core-make-request "sites")))

(ert-deftest test-question-retrieve ()
  "Test the ability to receive a list of questions."
  (should (stack-question-get-questions 'emacs)))

(ert-deftest test-bad-request ()
  "Test a method given a bad set of keywords"
  (should-error
   (stack-core-make-request "questions" '(()))))

(ert-deftest test-tree-filter ()
  "`stack-core-filter-data'"
  ;; flat
  (should
   (equal
    '((1 . t) (2 . [1 2]) (3))
    (stack-core-filter-data '((0 . 3) (1 . t) (a . five) (2 . [1 2])
                              ("5" . bop) (3) (p . 4))
                            '(1 2 3))))
  ;; complex
  (should
   (equal
    '((1 . [a b c])
      (2 . [((a . 1) (c . 3))
            ((a . 4) (c . 6))])
      (3 . peach))
    (stack-core-filter-data '((1 . [a b c])
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
    (stack-core-filter-data [((1 . 2) (2 . 3) (3 . 4))
                             ((1 . a) (2 . b) (3 . c) (5 . seven))
                             ((should-not-go))
                             ((1 . alpha) (2 . beta))]
                            '(1 2 3)))))

(ert-deftest test-filters ()
  (let ((stack-cache-directory (make-temp-file "stack-test" t)))
    (should-error (stack-filter-store "names must be symbols"
                                      "this is a filter"))
    ;; basic use
    (should (equal '((test . "filter"))
                   (stack-filter-store 'test "filter")))
    ;; aggregation
    (should (equal '((test2 . "filter2") (test . "filter"))
                   (stack-filter-store 'test2 "filter2")))
    ;; mutation
    (should (equal '((test2 . "filter2") (test . "filter-test"))
                   (stack-filter-store 'test "filter-test")))
    ;; clean up (note: the file should exist)
    (delete-file
     (stack-cache-get-file-name
      stack-filter-cache-file))))

(defmacro line-should-match (regexp)
  ""
  `(let ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
     (message "Line here is: %S" line)
     (should (string-match ,regexp line))))

(ert-deftest question-list-display ()
  (cl-letf (((symbol-function #'stack-core-make-request)
             (lambda (&rest _) stack-test-data-questions)))
    (list-questions nil)
    (switch-to-buffer "*question-list*")
    (goto-char (point-min))
    (should (equal (buffer-name) "*question-list*"))
    (line-should-match
     "^   1   0 Focus-hook: attenuate colours when losing focus [ 0-9]+[ydhms] ago \\[frames\\] \\[hooks\\] \\[focus\\]")
    (stack-question-list-next 5)
    (line-should-match
     "^   0   1 Babel doesn&#39;t wrap results in verbatim [ 0-9]+[ydhms] ago \\[org-mode\\]")
    (call-interactively 'stack-question-list-display-question)
    (should (equal (buffer-name) "*stack-question*"))
    (switch-to-buffer "*question-list*")
    (stack-question-list-previous 4)
    (line-should-match
     "^   2   1 &quot;Making tag completion table&quot; Freezes/Blocks -- how to disable [ 0-9]+[ydhms] ago \\[autocomplete\\]")))
