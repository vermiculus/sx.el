(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))

;;; Tests

(setq stack-core-remaining-api-requests-message-threshold 50000)
(setq debug-on-error t)

(require 'stack-core)
(require 'stack-question)

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

(ert-deftest test-data-filter-1 ()
  "Test the meta-convenience function -- flat structure"
  (should
   (equal
    '((1 . t) (2 . [1 2]) (3))
    (stack-core-filter-data '((0 . 3)
			      (1 . t)
			      (a . five)
			      (2 . [1 2])
			      ("5" . bop)
			      (3)
			      (p . 4))
			    '(1 2 3)))))

(ert-deftest test-data-filter-2 ()
  "Test the meta-convenience function -- complex structure"
  (should
   (equal
    '([()])
    (stack-core-filter-data '((0 . 3)
			      (1 . t)
			      (a . five)
			      (2 . [1 2])
			      ("5" . bop)
			      (3)
			      (p . 4))
			    '(1 2 3)))))
