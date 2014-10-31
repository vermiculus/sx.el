(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))

;;; Tests

(add-to-list 'load-path ".")

(require 'stack-core)
(require 'stack-question)

(ert-deftest test-question-retrieve ()
  (should (stack-question-get-questions 'emacs)))

(ert-deftest test-bad-request ()
  (should-error
   (stack-core-make-request "questions" '(()))))
