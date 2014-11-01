(defmacro stack-test-get-sample-data (method &optional directory)
  (with-current-buffer
      (find-file-noselect
       (concat "data-samples/"
	       (when directory (concat directory "/"))
	       method ".el"))
    (eval (read (buffer-string)))))

(setq stack-test-data-questions
      (stack-test-get-sample-data "questions")
      stack-test-data-sites
      (stack-test-get-sample-data "sites"))
