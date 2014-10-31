;;; Tests

(add-to-list 'load-path ".")

(require 'stack-core)
(require 'stack-question)

(setq
 stack-tmp (stack-question-get-questions 'emacs)
 stack-tmp-2 (stack-question-get-questions 'emacs 2))

(prog1 nil
  (stack-message "%S" stack-tmp)
  (stack-message "%S" stack-tmp-2))

(defun -stack--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "stack-" (symbol-name symbol))
	 (unintern symbol)))))
