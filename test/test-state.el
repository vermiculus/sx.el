(defmacro with-question-data (cell id &rest body)
  (declare (indent 2))
  `(let ((,cell '((question_id . ,id)
                  (site_par . "emacs")
                  (last_activity_date . 1234123456))))
     ,@body))

(ert-deftest test-question-mark-read ()
  "00ccd139248e782cd8316eff65c26aed838c7e46"
  (should
   (with-question-data q nil
     (sx-question--mark-read q))))
