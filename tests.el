;;; Tests

(add-to-list 'load-path ".")
(require 'stack-core)

(setq *t (stack-core-make-request "questions"))

(prog1 t (prin1 (elt (stack-core-parse-questions *t) 0)
		#'insert))

(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))
