(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))

;;; Tests

(setq stack-core-remaining-api-requests-message-threshold 1000000000)
(setq debug-on-error t)

(require 'stack-core)
(require 'stack-question)

(setq stack-core-remaining-api-requests-message-threshold 50000)

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
