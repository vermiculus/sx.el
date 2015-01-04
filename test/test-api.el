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

(ert-deftest test-method-get-all ()
  "Tests sx-method interface to `sx-request-all-items'"
  (should (< 250 (length (sx-method-call 'sites :get-all t)))))
