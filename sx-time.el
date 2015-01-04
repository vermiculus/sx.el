;;; sx-time.el --- time -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sean Allred

;; Author: Sean Allred <code@seanallred.com>

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

(defcustom sx-time-indicators
  '((second . "s")
    (minute . "m")
    (hour   . "h")
    (day    . "D")
    (month  . "M")
    (year   . "Y"))
  "Alist for time period indicator used in `sx-time-seconds-to-string'."
  :type '(alist :key-type symbol :value-type string)
  :group 'sx
  :options '(second minute hour day month year))

(defun sx-time-seconds-to-string ()
  "Auxiliary value used by `sx-time-since'."
  ;; (LIMIT NAME VALUE)
  ;; We use an entry if the number of seconds in question is less than
  ;; LIMIT, but more than the previous entry's LIMIT.
  ;; For instance, if time is less than 100 sec, we write it in seconds;
  ;; if it is between 100 and 6000 sec, we use minutes.
  ;; VALUE is the actual number of seconds which NAME represents.
  `((100      ,(cdr (assoc 'second sx-time-indicators)) 1)
    (6000     ,(cdr (assoc 'minute sx-time-indicators)) 60.0)
    (108000   ,(cdr (assoc 'hour   sx-time-indicators)) 3600.0)
    (3456000  ,(cdr (assoc 'day    sx-time-indicators)) 86400.0)
    (31622400 ,(cdr (assoc 'month  sx-time-indicators)) 2628000.0)
    (nil      ,(cdr (assoc 'year   sx-time-indicators)) 31557600.0)))

(defun sx-time-since (time)
  "Convert the time interval since TIME (in seconds) to a short string."
  (let ((delay (- (float-time) time)))
    (concat
     (if (> 0 delay) "-" "")
     (if (= 0 delay) "0s"
       (setq delay (abs delay))
       (let ((sts (sx-time-seconds-to-string)) here)
         (while (and (car (setq here (pop sts)))
                     (<= (car here) delay)))
         (concat (format "%.0f" (/ delay (car (cddr here))))
                 (cadr here)))))))

(defcustom sx-time-date-format-year "%H:%M %e %b %Y"
  "Format used for dates on a past year.
See also `sx-time-date-format'."
  :type 'string
  :group 'sx)

(defcustom sx-time-date-format "%H:%M - %d %b"
  "Format used for dates on this year.
See also `sx-time-date-format-year'."
  :type 'string
  :group 'sx)

(defun sx-time-seconds-to-date (seconds)
  "Return the integer SECONDS as a date string."
  (let ((time (seconds-to-time seconds)))
    (format-time-string
     (if (string= (format-time-string "%Y")
                  (format-time-string "%Y" time))
         sx-time-date-format
       sx-time-date-format-year)
     time)))

(provide 'sx-time)
;;; sx-time.el ends here
