(defmacro with-question-data (cell id &rest body)
  (declare (indent 2))
  `(let ((,cell '((question_id . ,id)
                  (site_par . "emacs")
                  (last_activity_date . 1234123456))))
     ,@body))

(ert-deftest test-question-mark-read ()
  "00ccd139248e782cd8316eff65c26aed838c7e46"
  (with-question-data q 10
    ;; Check basic logic.
    (should (sx-question--mark-read q))
    (should (sx-question--read-p q))
    (should (not (setcdr (assq 10 (cdr (assoc "emacs" sx-question--user-read-list))) nil)))
    ;; Don't freak out because the cdr was nil.
    (should (not (sx-question--read-p q)))
    (should (sx-question--mark-read q)))
  (should
   (with-question-data q nil
     ;; Don't freak out because question_id was nil.
     (sx-question--mark-read q))))

