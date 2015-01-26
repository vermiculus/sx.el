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

(ert-deftest request-get-url ()
  (should (sx-request-get-url "http://google.com"))
  (should-error (sx-request-get-url "http://github.com/Bruce-Connor/does-not-exist"))
  (when sx-question-mode-use-images
    (should
     ;; If image is not recognized, this returns nil.
     (create-image (sx-request-get-url "https://raw.githubusercontent.com/vermiculus/sx.el/master/list-and-question.png")
                   'imagemagick t
                   :width sx-question-mode-image-max-width)))
  ;; In case imagemacgick is not available, let's try png so we at
  ;; least test the function.
  (when (image-type-available-p 'png)
    (should
     (create-image (sx-request-get-url "https://raw.githubusercontent.com/vermiculus/sx.el/master/list-and-question.png")
                   'png t
                   :width sx-question-mode-image-max-width))))

(ert-deftest request-get-data ()
  (should-error (sx-request-get-data "tags/emacs-does-not-exist"))
  (let ((emacs-tags (sx-request-get-data 'tags/emacs)))
    (should (> (length emacs-tags) 450))
    (should (not (cl-remove-if #'stringp emacs-tags)))))
