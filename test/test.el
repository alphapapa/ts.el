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
