(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))

;;; Tests

(require 'stack-core)
(require 'stack-question)

(setq stack-core-remaining-api-requests-message-threshold 50000)

(ert-deftest test-question-retrieve ()
  (should (stack-question-get-questions 'emacs)))

(ert-deftest test-bad-request ()
  (should-error
   (stack-core-make-request "questions" '(()))))
