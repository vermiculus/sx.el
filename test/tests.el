
;;; SX Settings
(defun -sx--nuke ()
  (interactive)
  (mapatoms
   (lambda (symbol)
     (if (string-prefix-p "sx-" (symbol-name symbol))
         (unintern symbol)))))

(setq
 sx-initialized t
 sx-request-remaining-api-requests-message-threshold 50000
 debug-on-error t
 url-show-status nil
 user-emacs-directory "."
 sx-test-base-dir (file-name-directory (or load-file-name "./")))


;;; Test Data
(defvar sx-test-data-dir
  (expand-file-name "data-samples/" sx-test-base-dir))

(defun sx-test-sample-data (method &optional directory)
  (let ((file (concat (when directory (concat directory "/"))
                      sx-test-data-dir
                      method ".el")))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (buffer-string))))))

(setq
 sx-test-data-questions
 (sx-test-sample-data "questions")
 sx-test-data-sites
 (sx-test-sample-data "sites"))


;;; General Settings
(setq
 package-user-dir (expand-file-name
                   (format "../../.cask/%s/elpa" emacs-version)
                   sx-test-data-dir))

(package-initialize)

(require 'sx-load)

(defun sx-load-test (test)
  (load-file
   (format "%s/test-%s.el"
           sx-test-base-dir
           (symbol-name test))))

(setq sx-test-enable-messages nil)

(defun sx-test-message (message &rest args)
  (when sx-test-enable-messages
    (apply #'message message args)))

(mapc #'sx-load-test
      '(api macros printing util search))
