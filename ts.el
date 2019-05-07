;;; ts.el --- Date-time library  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net
;; URL: http://github.com/alphapapa/ts.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (dash "2.14.1") (s "1.12.0"))
;; Keywords: date time timestamp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package is designed to ease manipulation of dates, times, and timestamps in Emacs.

;; A struct `ts' is defined, which represents a timestamp.  All manipulation is done internally
;; using Unix timestamps.  Accessors are used to retrieve values such as month, day, year, etc. from
;; a timestamp, and these values are cached in the struct once accessed, to avoid repeatedly having
;; to call `format-time-string'.  If a slot is modified, the timestamp's internal Unix timestamp
;; should be updated with `ts-update'.

;;; License:

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

;;; Code:

;;;; Requirements

(require 'dash)
(require 's)

;;;; Variables

(defvar ts-default-format "%Y-%m-%d %H:%M:%S %z"
  "Default format for `ts-format'.")

;;;; Functions and Macros

(cl-defmacro ts-defstruct (&rest args)
  "Like `cl-defstruct', but with additional slot options.

Additional slot options and values:

`:accessor-init': a sexp that initializes the slot in the
accessor if the slot is nil.  The symbol `struct' will be bound
to the current struct.

`:aliases': A list of symbols which will be aliased to the slot
accessor, prepended with the struct name (e.g. a struct `ts' with
slot `year' and alias `y' would create an alias `ts-y')."
  (declare (indent defun))
  ;; FIXME: Compiler warnings about accessors defined multiple times.  Not sure if we can fix this
  ;; except by ignoring warnings.
  (let ((struct-name (car args))
        (struct-slots (cdr args)))
    `(prog1
         ,(macroexpand `(cl-defstruct ,struct-name ,@struct-slots))
       ,@(cl-loop for slot in struct-slots
                  for pos from 1
                  when (listp slot)
                  collect (-let* (((sname sdefault . soptions) slot)
                                  (accessor-name (intern (concat (symbol-name struct-name) "-" (symbol-name sname))))
                                  (accessor-docstring (format "Access slot \"%s\" of `%s' struct CL-X."
                                                              sname struct-name))
                                  (struct-pred (intern (concat (symbol-name struct-name) "-p")))
                                  (accessor-init (plist-get soptions :accessor-init))
                                  (aliases (plist-get soptions :aliases)))
                            `(progn
                               ;; Copied from macro expansion of `cl-defstruct'.
                               (cl-defsubst ,accessor-name (cl-x)
                                 ,accessor-docstring
                                 ;; FIXME: side-effect-free is probably not true here, but what about error-free?
                                 ;;  (declare (side-effect-free error-free))
                                 (or (,struct-pred cl-x)
                                     (signal 'wrong-type-argument
                                             (list ',struct-name cl-x)))
                                 ,(when accessor-init
                                    `(unless (aref cl-x ,pos)
                                       (let ((struct cl-x))
                                         (aset cl-x ,pos ,accessor-init))))
                                 ;; NOTE: It's essential that this `aref' form be last so
                                 ;; the gv-setter works in the compiler macro.
                                 (aref cl-x ,pos))
                               ;; Aliases
                               ,@(cl-loop for alias in aliases
                                          for alias = (intern (concat (symbol-name struct-name) "-" (symbol-name alias)))
                                          collect `(defalias ',alias ',accessor-name))
                               ;; TODO: Setter
                               ;; ,(when (plist-get soptions :reset)
                               ;;    `(gv-define-setter ,accessor-name (ts value)
                               ;;       `(progn
                               ;;          (aset ,ts ,,pos ,value)
                               ;;          (setf (ts-unix ts) ni))))
                               ))))))

;;;; Structs

;; TODO: When a field is changed, the unix/internal slot needs to be updated.  On the other hand,
;; maybe not.  Maybe `ts-adjust' should be the only way to adjust them.  Otherwise, updating the
;; unix/internal time value when a slot is changed gets really complicated, and it might not be
;; worth it.  Or, at least, not in the initial version.

(ts-defstruct ts
  (hour nil
        :accessor-init (string-to-number (format-time-string "%H" (ts-unix struct)))
        :aliases (H)
        :constructor "%H"
        :type integer)
  (minute nil
          :accessor-init (string-to-number (format-time-string "%M" (ts-unix struct)))
          :aliases (min M)
          :constructor "%M"
          :type integer)
  (second nil
          :accessor-init (string-to-number (format-time-string "%S" (ts-unix struct)))
          :aliases (sec S)
          :constructor "%S"
          :type integer)
  (day nil
       :accessor-init (string-to-number (format-time-string "%d" (ts-unix struct)))
       :aliases (day-of-month-num dom d)
       :constructor "%d"
       :type integer)
  (month nil
         :accessor-init (string-to-number (format-time-string "%m" (ts-unix struct)))
         :aliases (month-num moy m)
         :constructor "%m"
         :type integer)
  (year nil
        :accessor-init (string-to-number (format-time-string "%Y" (ts-unix struct)))
        :aliases (Y)
        :constructor "%Y"
        :type integer)

  (dow nil
       :accessor-init (string-to-number (format-time-string "%w" (ts-unix struct)))
       :aliases (day-of-week-num)
       :constructor "%w"
       :type integer)
  (day-abbr nil
            :accessor-init (format-time-string "%a" (ts-unix struct))
            :aliases (day-of-week-abbr)
            :constructor "%a")
  (day-name nil
            :accessor-init (format-time-string "%A" (ts-unix struct))
            :aliases (day-of-week-name)
            :constructor "%A")
  ;; (doe nil
  ;;      :accessor-init (days-between (format-time-string "%Y-%m-%d 00:00:00" (ts-unix struct))
  ;;                                   "1970-01-01 00:00:00")
  ;;      :aliases (day-of-epoch))
  (doy nil
       :accessor-init (string-to-number (format-time-string "%j" (ts-unix struct)))
       :aliases (day-of-year)
       :constructor "%j"
       :type integer)

  (woy nil
       :accessor-init (string-to-number (format-time-string "%V" (ts-unix struct)))
       :aliases (week week-of-year)
       :constructor "%V"
       :type integer)

  (month-abbr nil
              :accessor-init (format-time-string "%b" (ts-unix struct))
              :aliases (b)
              :constructor "%b")
  (month-name nil
              :accessor-init (format-time-string "%B" (ts-unix struct))
              :aliases (B)
              :constructor "%B")

  (tz-abbr nil
           :accessor-init (format-time-string "%Z" (ts-unix struct))
           :constructor "%Z")
  (tz-offset nil
             :accessor-init (format-time-string "%z" (ts-unix struct))
             :constructor "%z")
  ;; MAYBE: Add tz-offset-minutes

  (internal nil
            :accessor-init (apply #'encode-time (decode-time (ts-unix struct))))
  (unix nil
        :accessor-init (pcase-let* (((cl-struct ts second minute hour day month year) cl-x))
                         (if (and second minute hour day month year)
                             (float-time (encode-time second minute hour day month year))
                           (float-time)))))

;;;; Functions

(defsubst ts-parse (string)
  "Return new `ts' struct, parsing STRING with `parse-time-string'."
  (let ((parsed (parse-time-string string)))
    ;; Fill nil values
    (cl-loop for i from 0 to 5
             when (null (nth i parsed))
             do (setf (nth i parsed) 0))
    (->> parsed
         (apply #'encode-time)
         float-time
         (make-ts :unix))))

(defsubst ts-parse-org (org-ts)
  "Return timestamp object for Org timestamp element ORG-TS.
Element should be like one parsed by `org-element', the first
element of which is `timestamp'.  Assumes timestamp is not a
range."
  ;; I wish `pcase-let*' supported plists, which I guess is to say, I wish `map-elt' supported
  ;; plists.
  (setq org-ts (cadr org-ts))
  (ts-update (make-ts :year (plist-get org-ts :year-start)
                      :month (plist-get org-ts :month-start)
                      :day (plist-get org-ts :day-start)
                      :hour (or (plist-get org-ts :hour-start) 0)
                      :minute (or (plist-get org-ts :minute-start) 0)
                      :second 0)))

(defsubst ts-now ()
  "Return `ts' struct set to now."
  (make-ts :unix (float-time)))

(defsubst ts-format (&optional format-string ts)
  "Format timestamp TS with `format-time-string' according to FORMAT-STRING.
If FORMAT-STRING is nil, use the value of `ts-default-format'.
If TS is nil, use the current time."
  (format-time-string (or format-string ts-default-format)
                      (when ts
                        (ts-unix ts))))

(defsubst ts-update (ts)
  "Return timestamp TS after updating its Unix timestamp from its other slots.
Non-destructive.  To be used after setting slots with,
e.g. `ts-fill'."
  (pcase-let* (((cl-struct ts second minute hour day month year) ts))
    (make-ts :unix (float-time (encode-time second minute hour day month year)))))

(defmacro ts-define-fill ()
  "Define `ts-fill' function that fills all applicable slots of `ts' object from its `unix' slot."
  (let* ((slots (->> (cl-struct-slot-info 'ts)
                     (--select (and (not (member (car it) '(unix internal cl-tag-slot)))
                                    (plist-get (cddr it) :constructor)))

                     (--map (list (intern (concat ":" (symbol-name (car it))))
                                  (cddr it)))))
         (keywords (-map #'car slots))
         (constructors (->> slots
                            (--map (plist-get (cadr it) :constructor))
                            -non-nil))
         (types (--map (plist-get (cadr it) :type) slots))
         (format-string (s-join "\f" constructors))
         (value-conversions (cl-loop for type in types
                                     for keyword in keywords
                                     for i from 0
                                     for val = `(nth ,i time-values)
                                     append (list keyword (pcase type
                                                            ('integer `(string-to-number ,val))
                                                            (_ val))))))
    ;; MAYBE: Construct the record manually?  Probably not worth it, but might eke out a bit more speed.
    `(defun ts-fill (ts)
       "Return TS having filled all slots from its Unix timestamp.
This is non-destructive."
       (let ((time-values (split-string (format-time-string ,format-string (ts-unix ts)) "\f")))
         (make-ts :unix (ts-unix ts) ,@value-conversions)))))
(ts-define-fill)

(defsubst ts-reset (ts)
  "Return TS with all slots cleared except `unix'.
Non-destructive.  The same as:

    (make-ts :unix (ts-unix ts))"
  (make-ts :unix (ts-unix ts)))

;; FIXME: This fails, and I'm not sure if it's a limitation of gvs or if I did something wrong:
;;   (ts-incf (ts-moy (ts-now)))

(defun ts-difference (a b)
  "Return difference between timestamps A and B."
  (- (ts-unix a) (ts-unix b)))

(cl-defun ts-human-duration (seconds)
  "Return plist describing duration SECONDS in years, days, hours, minutes, and seconds.
This is a simple calculation that does not account for leap
years, leap seconds, etc."
  (cl-macrolet ((dividef (place divisor)
                         ;; Divide PLACE by DIVISOR, set PLACE to the remainder, and return the quotient.
                         `(prog1 (/ ,place ,divisor)
                            (setf ,place (% ,place ,divisor)))))
    (let* ((seconds (floor seconds))
           (years (dividef seconds 31536000))
           (days (dividef seconds 86400))
           (hours (dividef seconds 3600))
           (minutes (dividef seconds 60)))
      (list :years years :days days :hours hours :minutes minutes :seconds seconds))))

(cl-defun ts-human-format-duration (seconds &optional abbreviate)
  "Return human-formatted string describing duration SECONDS.
If ABBREVIATE is non-nil, return a shorter version, without
spaces.  This is a simple calculation that does not account for
leap years, leap seconds, etc."
  (cl-macrolet ((format> (place)
                         ;; When PLACE is greater than 0, return formatted string using its symbol name.
                         `(when (> ,place 0)
                            (format "%d%s%s" ,place
                                    (if abbreviate "" " ")
                                    (if abbreviate
                                        (substring ,(symbol-name place) 0 1)
                                      ,(symbol-name place)))))
                (join-places (&rest places)
                             ;; Return string joining the names and values of PLACES.
                             `(->> (list ,@(cl-loop for place in places
                                                    collect `(format> ,place)))
                                   -non-nil
                                   (s-join (if abbreviate "" ", ")))))
    (-let* (((&plist :years :days :hours :minutes :seconds) (ts-human-duration seconds)))
      (join-places years days hours minutes seconds))))

;;;;; Adjustors

;; These functions are very cool, and they may make the adjust function unnecessary, because you can
;; do something like (ts-adjust 'moy 120 (ts-now)) and get a timestamp 10 years in the future.

;;;;;; Non-destructive

;; These non-destructive versions take the slot symbol as an argument and the object last, and they
;; return the timestamp object rather than the new slot value, making them suitable for use in
;; threading macros when the initial form is a sexp rather than a value or variable.

(defun ts-adjust (&rest adjustments)
  "Return new timestamp having applied ADJUSTMENTS to TS.
ADJUSTMENTS should be a series of alternating SLOTS and VALUES by
which to adjust them.  For example, this form returns a new
timestamp that is 47 hours into the future:

  (ts-adjust 'hour -1 'dow +2 (ts-now))

Since the timestamp argument is last, it's suitable for use in a
threading macro."
  (declare (advertised-calling-convention (&rest adjustments ts) nil))
  (let* ((ts (-last-item adjustments))
         (adjustments (nbutlast adjustments))
         (ts (ts-fill ts)))
    (cl-loop for (slot change) on adjustments by #'cddr
             do (cl-incf (cl-struct-slot-value 'ts slot ts) change))
    (make-ts :unix (ts-unix (ts-update ts)))))

(defsubst ts-inc (slot value ts)
  "Return a new timestamp based on TS with its SLOT incremented by VALUE.
SLOT should be specified as a plain symbol, not a keyword."
  (setq ts (ts-fill ts))
  (cl-incf (cl-struct-slot-value 'ts slot ts) value)
  (make-ts :unix (ts-unix (ts-update ts))))

(defsubst ts-dec (slot value ts)
  "Return a new timestamp based on TS with its SLOT decremented by VALUE.
SLOT should be specified as a plain symbol, not a keyword."
  (setq ts (ts-fill ts))
  (cl-decf (cl-struct-slot-value 'ts slot ts) value)
  (make-ts :unix (ts-unix (ts-update ts))))

;;;;;; Generalized variables

;; These destructive versions act like `cl-incf'.  They are slightly less suitable for use in
;; threading macros, because it's not possible to do, e.g. this:

;;   (-> (ts-now)
;;       (ts-adjustf 'day 1))

;; ...because `ts-now' doesn't return a generalized variable.  But this still works:

;;   (let ((ts (ts-now)))
;;     (-> ts (ts-adjustf 'dom 1)))

;;  TODO: Look at `cl-incf' implementation, consider whether we should imitate it.

(defmacro ts-adjustf (ts &rest adjustments)
  "Return timestamp TS having applied ADJUSTMENTS.
This function is destructive, as it calls `setf' on TS.

ADJUSTMENTS should be a series of alternating SLOTS and VALUES by
which to adjust them.  For example, this form adjusts a timestamp
to 47 hours into the future:

  (let ((ts (ts-now)))
    (ts-adjustf ts 'hour -1 'dow +2))"
  ;; MAYBE: Is it possible to make this kind of macro work in a threading macro by taking its TS
  ;; argument last?  It only seems to work if the TS is a symbol rather than a form, because of how
  ;; generalized variables work, but that makes it less useful and more error-prone.
  `(progn
     ;; We use the accessor functions rather than `cl-struct-slot-value', because it's slightly
     ;; faster to use the accessors, even though `cl-struct-slot-value' is supposed to be
     ;; byte-compiled to essentially the same thing (although it's possible I'm doing something
     ;; wrong).
     (setf ,ts (ts-fill ,ts))
     ,@(cl-loop for (slot change) on adjustments by #'cddr
                for accessor = (intern (concat "ts-" (symbol-name (cadr slot))))
                collect `(cl-incf (,accessor ,ts) ,change))
     (setf ,ts (ts-update ,ts))))

(cl-defmacro ts-incf (place &optional (value 1))
  "Increment timestamp PLACE by VALUE (default 1), update its Unix timestamp, and return the new value of PLACE."
  `(progn
     (setf ,(cadr place) (ts-fill ,(cadr place)))
     (prog1
         (cl-incf ,place ,value)
       (setf ,(cadr place)
             (make-ts :unix (ts-unix
                             (ts-update
                              ,(cadr place))))))))

(cl-defmacro ts-decf (place &optional (value 1))
  "Decrement timestamp PLACE by VALUE (default 1), update its Unix timestamp, and return the new value of PLACE."
  `(progn
     (setf ,(cadr place) (ts-fill ,(cadr place)))
     (prog1
         (cl-decf ,place ,value)
       (setf ,(cadr place)
             (make-ts :unix (ts-unix
                             (ts-update
                              ,(cadr place))))))))

;;;;; Comparators

(defun ts= (a b)
  "Return non-nil if timestamp A is the same as timestamp B.
Compares only the timestamps' `unix' slots."
  (= (ts-unix a) (ts-unix b)))

(defun ts< (a b)
  "Return non-nil if timestamp A is less than timestamp B."
  (< (ts-unix a) (ts-unix b)))

(defun ts<= (a b)
  "Return non-nil if timestamp A is <= timestamp B."
  (<= (ts-unix a) (ts-unix b)))

(defun ts> (a b)
  "Return non-nil if timestamp A is greater than timestamp B."
  (> (ts-unix a) (ts-unix b)))

(defun ts>= (a b)
  "Return non-nil if timestamp A is >= timestamp B."
  (>= (ts-unix a) (ts-unix b)))

;;;; Footer

(provide 'ts)

;;; ts.el ends here
