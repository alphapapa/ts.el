(require 'ts)

(ert-deftest ts-now ()
  "Ensure `ts-now' returns what appears to be the current time."
  ;; FIXME: This just tests for non-nil.
  (should (ts-unix (ts-now))))

(ert-deftest ts-format ()
  "Ensure `ts-format' formats."
  ;; FIXME: This just tests for non-nil.
  (should (ts-format nil (ts-now))))

(ert-deftest ts-update ()
  "Test `ts-update'."
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

(ert-deftest ts-adjust ()
  "Test `ts-adjust'."
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (setf ts (ts-adjust 'year 1 ts))
    (should (equal (ts-year ts) (1+ year)))))

(ert-deftest ts-difference ()
  "Test `ts-difference'."
  ;; We test the difference by subtracting one day.  This should avoid leap day and leap second issues.
  (let* ((a (ts-now))
         (b (ts-adjust 'day -1 a)))
    (should (equal 86400 (floor (ts-difference a b))))))

(ert-deftest ts-incf ()
  ""
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-incf (ts-year ts))
    (should (equal (ts-year ts) (1+ year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-incf (ts-year ts) 2)
    (should (equal (ts-year ts) (+ 2 year)))))

(ert-deftest ts-decf ()
  ""
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-decf (ts-year ts))
    (should (equal (ts-year ts) (1- year))))
  (let* ((ts (ts-now))
         (year (ts-year ts)))
    (ts-decf (ts-year ts) 2)
    (should (equal (ts-year ts) (- year 2)))))

;; TODO: Other comparator tests

(ert-deftest ts< ()
  ""
  (let (a b)
    (setq a (ts-now))
    (sleep-for 1)
    (setq b (ts-now))
    (should (ts< a b)))
  (let* ((now (ts-now))
         (past (copy-ts now)))
    (ts-decf (ts-year past))
    (should (ts< past now))))


(ert-deftest ts-parse ()
  ""
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

(ert-deftest ts-human-format-duration ()
  ""
  (let* ((now (ts-now))
         (past (ts-adjust 'day -400 'hour -2 'minute -1 'second -5 now))
         (human-duration (ts-human-format-duration (ts-difference now past))))
    (should (equal human-duration "1 years, 35 days, 2 hours, 1 minutes, 5 seconds"))))
