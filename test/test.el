;;; test.el --- Tests for ts.el                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements

(require 'ts)

(require 'org-element)

;;;; Tests

;;;;; Accessors

(ert-deftest ts-dow ()
  (should (equal (ts-dow (ts-now))
                 (string-to-number (format-time-string "%w")))))
(ert-deftest ts-day-abbr ()
  (should (equal (ts-day-abbr (ts-now))
                 (format-time-string "%a"))))
(ert-deftest ts-day-name ()
  (should (equal (ts-day-name (ts-now))
                 (format-time-string "%A"))))
(ert-deftest ts-month ()
  (should (equal (ts-month (ts-now))
                 (string-to-number (format-time-string "%m")))))
(ert-deftest ts-month-abbr ()
  (should (equal (ts-month-abbr (ts-now))
                 (format-time-string "%b"))))
(ert-deftest ts-month-name ()
  (should (equal (ts-month-name (ts-now))
                 (format-time-string "%B"))))
(ert-deftest ts-day ()
  (should (equal (ts-day (ts-now))
                 (string-to-number (format-time-string "%d")))))
(ert-deftest ts-year ()
  (should (equal (ts-year (ts-now))
                 (string-to-number (format-time-string "%Y")))))
(ert-deftest ts-hour ()
  (should (equal (ts-hour (ts-now))
                 (string-to-number (format-time-string "%H")))))
(ert-deftest ts-minute ()
  (should (equal (ts-minute (ts-now))
                 (string-to-number (format-time-string "%M")))))
(ert-deftest ts-second ()
  (should (equal (ts-second (ts-now))
                 (string-to-number (format-time-string "%S")))))
(ert-deftest ts-tz-offset ()
  (should (equal (ts-tz-offset (ts-now))
                 (format-time-string "%z"))))
(ert-deftest ts-tz-abbr ()
  (should (equal (ts-tz-abbr (ts-now))
                 (format-time-string "%Z"))))
(ert-deftest ts-unix ()
  ;; Theoretically this test could fail if run extremely close to a second boundary, but it's probably good enough.
  (should (equal (floor (ts-unix (ts-now)))
                 (string-to-number (format-time-string "%s")))))

;;;;; Adjustors

;;;;;; Non-destructive

(ert-deftest ts-adjust ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-adjust 'year 1 ts))
    (should (equal (ts-year ts) (1+ year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-adjust 'year -1 ts))
    (should (equal (ts-year ts) (1- year)))))

(ert-deftest ts-inc ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-inc 'year 1 ts))
    (should (equal (ts-year ts) (1+ year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-inc 'year 2 ts))
    (should (equal (ts-year ts) (+ 2 year)))))

(ert-deftest ts-dec ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-dec 'year 1 ts))
    (should (equal (ts-year ts) (1- year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-dec 'year 2 ts))
    (should (equal (ts-year ts) (- year 2)))))

;;;;;; Destructive

(ert-deftest ts-adjustf ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-adjustf ts 'year 1)
    (should (equal (ts-year ts) (1+ year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-adjustf ts 'year -1)
    (should (equal (ts-year ts) (1- year))))
  (let* ((ts (ts-now))
         (now-unix (ts-unix ts)))
    (ts-adjustf ts 'hour -1 'day +2)
    (should (equal (floor (ts-unix ts))
                   (floor (+ now-unix
                             (* 60 60 47)))))))

(ert-deftest ts-incf ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-incf (ts-year ts))
    (should (equal (ts-year ts) (1+ year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-incf (ts-year ts) 2)
    (should (equal (ts-year ts) (+ 2 year)))))

(ert-deftest ts-decf ()
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-decf (ts-year ts))
    (should (equal (ts-year ts) (1- year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-decf (ts-year ts) 2)
    (should (equal (ts-year ts) (- year 2)))))

;;;;; Comparators

(ert-deftest ts-in ()
  (let* ((beg (ts-parse "Sun, 2019-08-04 00:00:00 -0500"))
         (end (ts-parse "Sat, 2019-08-10 23:59:59 -0500"))
         (check-ts (ts-parse "Fri, 2019-08-09 10:00:34 -0500")))
    (should (ts-in beg end check-ts)))
  (let* ((beg (ts-parse "Sun, 2019-08-04 00:00:00 -0500"))
         (end (ts-parse "Sat, 2019-08-10 23:59:59 -0500"))
         (check-ts (ts-parse "2017-08-09 10:00:34 -0500")))
    (should-not (ts-in beg end check-ts))))

(ert-deftest ts= ()
  (let* ((now (ts-now))
         (b (copy-ts now)))
    (should (ts= now b)))
  ;; In this test, A and B are not `ts=' to NOW, because NOW's Unix timestamp is a float, and A
  ;; and B are created from the timestamp parts, which effectively rounds the timestamp to n.0.
  ;; So rather than compare the newly created timestamp to NOW, we compare it to a copy of itself.
  (let* ((now (ts-fill (ts-now)))
         (a (progn
              (pcase-let* (((cl-struct ts second minute hour day month year) now))
                (make-ts :year year :month month :day day :hour hour :minute minute :second second))))
         (b (copy-ts a)))
    (should (ts= a b))))

(ert-deftest ts< ()
  (let (a b)
    (setq a (ts-now))
    (sleep-for 1)
    (setq b (ts-now))
    (should (ts< a b)))
  (let* ((now (ts-now))
         (past (copy-ts now)))
    (ts-decf (ts-year past))
    (should (ts< past now))))

(ert-deftest ts<= ()
  (let (a b)
    (setq a (ts-now))
    (sleep-for 1)
    (setq b (ts-now))
    (should (ts<= a b)))
  (let* ((now (ts-now))
         (past (copy-ts now)))
    (should (ts<= now past))
    (ts-decf (ts-year past))
    (should (ts<= past now))))

(ert-deftest ts> ()
  (let (a b)
    (setq a (ts-now))
    (sleep-for 1)
    (setq b (ts-now))
    (should (ts> b a)))
  (let* ((now (ts-now))
         (past (copy-ts now)))
    (ts-decf (ts-year past))
    (should (ts> now past))))

(ert-deftest ts>= ()
  (let (a b)
    (setq a (ts-now))
    (sleep-for 1)
    (setq b (ts-now))
    (should (ts>= b a)))
  (let* ((now (ts-now))
         (past (copy-ts now)))
    (should (ts>= now past))
    (ts-decf (ts-year past))
    (should (ts>= now past))))

;;;;; Difference

(ert-deftest ts-difference ()
  ;; We test the difference by subtracting one day.  This should avoid leap day and leap second issues.
  (let* ((a (ts-now))
         (b (ts-adjust 'day -1 a)))
    (should (equal 86400 (floor (ts-difference a b))))))

(ert-deftest ts-diff ()
  ;; We test the diff by subtracting one day.  This should avoid leap day and leap second issues.
  (let* ((a (ts-now))
         (b (ts-adjust 'day -1 a)))
    (should (equal 86400 (floor (ts-diff a b))))))

;;;;; Duration

(ert-deftest ts-human-format-duration ()
  (let* ((now (ts-now))
         (past (ts-adjust 'day -400 'hour -2 'minute -1 'second -5 now)))
    (should (equal (ts-human-format-duration (ts-difference now past))
                   "1 years, 35 days, 2 hours, 1 minutes, 5 seconds"))
    (should (equal (ts-human-format-duration (ts-difference now past) 'abbr)
                   "1y35d2h1m5s"))))

;;;;; Formatting

(ert-deftest ts-format ()
  (let ((ts (make-ts :year 2019 :month 7 :day 27 :hour 19 :minute 48 :second 08)))
    ;; Following the pattern in the function:
    (should (equal (ts-format ts) "2019-07-27 19:48:08 -0500"))
    (should (equal (ts-format "%Y" ts) "2019"))
    (should (ts-format "%Y"))
    (should (equal (ts-format nil ts) "2019-07-27 19:48:08 -0500"))
    (should (ts-format))))

;;;;; Parsing

(ert-deftest ts-parse ()
  (let ((ts (ts-parse "sat 8 dec 2018")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 0))
    (should (eq (ts-M ts) 0))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse "sat 8 dec 2018 12:12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 12))))

(ert-deftest ts-parse-fill ()
  (should-error (ts-parse-fill nil "sat 8 dec 2018"))
  (let ((ts (ts-parse-fill 'begin "sat 8 dec 2018")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 0))
    (should (eq (ts-M ts) 0))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-fill 'end "sat 8 dec 2018")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 23))
    (should (eq (ts-M ts) 59))
    (should (eq (ts-S ts) 59)))
  (let ((ts (ts-parse-fill nil "sat 8 dec 2018 12:12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 12)))
  (let ((ts (ts-parse-fill 'begin "sat 8 dec 2018 12:12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 12)))
  (let ((ts (ts-parse-fill 'end "sat 8 dec 2018 12:12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 12)))
  (let ((ts (ts-parse-fill 'begin "sat 8 dec 2018 12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-fill 'end "sat 8 dec 2018 12:12")))
    (should (eq (ts-Y ts) 2018))
    (should (eq (ts-m ts) 12))
    (should (eq (ts-d ts) 8))
    (should (eq (ts-dow ts) 6))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    ;; NOTE: When the second value is not present in the string, it's set to 0,
    ;; even when FILL is `end'.  In a way this seems wrong, but on the other hand,
    ;; "12:12" as a plain time value is assumed to refer to the moment it becomes
    ;; 12:12, which means 0 seconds.
    (should (eq (ts-S ts) 0))))

(ert-deftest ts-parse-org ()
  ;; NOTE: Not sure how to best handle loading `org-parse-time-string'.  Calling (require 'ts)
  ;; shouldn't cause Org to be loaded, so the user will probably have to do that.
  (require 'org)
  (let* ((org-ts-string "<2015-09-24 Thu .+1d>"))
    (should (equal 1443070800.0 (ts-unix (ts-parse-org org-ts-string))))))

(ert-deftest ts-parse-org-fill ()
  (should-error (ts-parse-org-fill nil "<2015-09-24 Thu .+1d>"))
  (let ((ts (ts-parse-org-fill 'begin "<2015-09-24 Thu .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 0))
    (should (eq (ts-M ts) 0))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-org-fill 'end "<2015-09-24 Thu .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 23))
    (should (eq (ts-M ts) 59))
    (should (eq (ts-S ts) 59)))
  (let ((ts (ts-parse-org-fill 'begin "<2015-09-24 Thu 12:12 .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-org-fill 'end "<2015-09-24 Thu 12:12 .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-org-fill 'begin "<2015-09-24 Thu 12:12:12 .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 0)))
  (let ((ts (ts-parse-org-fill 'end "<2015-09-24 Thu 12:12:12 .+1d>")))
    (should (eq (ts-Y ts) 2015))
    (should (eq (ts-m ts) 9))
    (should (eq (ts-d ts) 24))
    (should (eq (ts-dow ts) 4))
    (should (eq (ts-H ts) 12))
    (should (eq (ts-M ts) 12))
    (should (eq (ts-S ts) 0))))

(ert-deftest ts-parse-org-element ()
  (let ((org-ts '(timestamp (:type active
                                   :raw-value "<2015-09-24 Thu .+1d>"
                                   :year-start 2015 :month-start 9 :day-start 24
                                   :hour-start nil :minute-start nil
                                   :year-end 2015 :month-end 9 :day-end 24
                                   :hour-end nil :minute-end nil
                                   :begin 230314 :end 230335 :post-blank 0
                                   :repeater-type restart :repeater-value 1 :repeater-unit day))))
    (should (equal 1443070800.0 (ts-unix (ts-parse-org-element org-ts)))))
  (let ((org-ts-string "<2015-09-24 Thu .+1d>"))
    (with-temp-buffer
      (delay-mode-hooks
        (org-mode)
        (save-excursion
          (insert org-ts-string))
        (should (->> (org-element-context)
                     (ts-parse-org-element)
                     (ts-unix)
                     (equal 1443070800.0)))))))

;;;;; Other

(ert-deftest ts-apply ()
  (let* ((now (ts-now))
         (then (ts-apply :year 1970 now))
         (expected-difference (- (ts-year now) 1970)))
    (should (= (- (ts-year now) (ts-year then))
               expected-difference))))

(ert-deftest ts-now ()
  "Ensure `ts-now' returns what appears to be the current time."
  (should (equal (floor (ts-unix (ts-now)))
                 (floor (string-to-number (format-time-string "%s"))))))

(ert-deftest ts-update ()
  (let* ((ts (ts-now))
         (one-year-ago ts))
    (setf one-year-ago (ts-fill ts))
    (setf (ts-year one-year-ago) (1- (ts-year one-year-ago)))
    (setf one-year-ago (ts-update one-year-ago))
    ;; The difference between the new and old timestamps should be one of
    ;; these two values, depending on whether a leap day is involved.
    ;; FIXME: I guess this test will fail if a leap second is involved.  If that
    ;; ever actually causes the test to fail, it should be easy to fix then.
    (should (or (equal 31536000 (floor (ts-difference ts one-year-ago)))
                (equal 31622399 (floor (ts-difference ts one-year-ago)))))))

;;;; Footer

(provide 'ts-test)

;;; test.el ends here
