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
