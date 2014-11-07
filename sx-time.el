;;; sx-time.el --- time for stack-mode

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <sallred@calamity.tcs.com>
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'time-date)

(defvar sx-time-seconds-to-string
  ;; (LIMIT NAME VALUE)
  ;; We use an entry if the number of seconds in question is less than
  ;; LIMIT, but more than the previous entry's LIMIT.
  '((100      "s"  1)
    (6000     "m"  60.0)
    (108000   "h"  3600.0)
    (34560000 "d"  86400.0)
    (nil      "y"  31557600.0))
  "Auxiliary variable used by `sx-time-since'.")

(defun sx-time-since (time)
  "Convert the time interval since TIME (in seconds) to a short string."
  (let ((delay (- (time-to-seconds) time)))
    (concat
     (if (> 0 delay) "-" "")
     (if (= 0 delay) "0s"
       (setq delay (abs delay))
       (let ((sts sx-time-seconds-to-string) here)
         (while (and (car (setq here (pop sts)))
                     (<= (car here) delay)))
         (concat (format "%.0f" (/ delay (car (cddr here))))
                 (cadr here)))))))

(provide 'sx-time)
;;; sx-time.el ends here
