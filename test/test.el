(ert-deftest ts-now ()
  "Ensure `ts-now' returns what appears to be the current time."
  ;; FIXME: This just tests for non-nil.
  (should (ts-unix (ts-now))))

(ert-deftest ts-format ()
  "Ensure `ts-format' formats."
  ;; FIXME: This just tests for non-nil.
  (should (ts-format nil (ts-now))))

(ert-deftest ts-update ()
  ""
  (let* ((ts (ts-now))
         (old-unix (ts-unix ts)))
    (ts-fill ts)
    (cl-incf (ts-year ts))
    (ts-update ts)
    (should-not (equal old-unix (ts-unix ts)))))

(ert-deftest ts-adjust ()
  ""
  (let* ((ts (ts-now))
         (old-year (ts-year ts)))
    (ts-adjust "+1y" ts)
    (ts-fill ts 'force)
    (should (equal (ts-year ts) (1+ old-year)))))

(ert-deftest ts-difference ()
  ""
  (let* ((a (ts-now))
         (b (copy-ts a)))
    (ts-fill b)
    (cl-incf (ts-year b))
    (ts-update b)
    (ts-difference b a)))

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

(ert-deftest ts-human-duration ()
  ""
  (let* ((now (ts-now))
         (past (ts-adjust 'day -400 'hour -2 'minute -1 'second -5 now))
         (human-duration (ts-human-duration (ts-difference now past))))
    (should (equal human-duration "1 years, 35 days, 2 hours, 1 minutes, 5 seconds"))))
