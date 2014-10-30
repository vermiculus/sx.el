;;; Tests

(setq *t (stack-core-make-request "questions"))

(prog1 t (prin1 (elt (stack-core-parse-questions *t) 0)
		#'insert))
