;;; ts.el --- Date-time library  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net
;; URL: http://github.com/alphapapa/ts.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (dash "2.14.1"))
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

(require 'org)

(require 'dash)

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
  (dom nil
       :accessor-init (string-to-number (format-time-string "%d" (ts-unix struct)))
       :aliases (d)
       :constructor "%d"
       :type integer)
  (moy nil
       :accessor-init (string-to-number (format-time-string "%m" (ts-unix struct)))
       :aliases (m month-of-year)
       :constructor "%m"
       :type integer)
  (year nil
        :accessor-init (string-to-number (format-time-string "%Y" (ts-unix struct)))
        :aliases (Y)
        :constructor "%Y"
        :type integer)

  (dow nil
       :accessor-init (string-to-number (format-time-string "%w" (ts-unix struct)))
       :aliases (day-of-week)
       :constructor "%w"
       :type integer)
  (day nil
       :accessor-init (format-time-string "%a" (ts-unix struct))
       :aliases (day-abbr)
       :constructor "%a")
  (day-full nil
            :accessor-init (format-time-string "%A" (ts-unix struct))
            :aliases (day-name)
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

  (mon nil
       :accessor-init (format-time-string "%b" (ts-unix struct))
       :aliases (month-abbr)
       :constructor "%b")
  (month nil
         :accessor-init (format-time-string "%B" (ts-unix struct))
         :aliases (month-name)
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
        :accessor-init (pcase-let* (((cl-struct ts second minute hour dom moy year) cl-x))
                         (if (and second minute hour dom moy year)
                             (float-time (encode-time second minute hour dom moy year))
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

(defun ts-now ()
  "Return `ts' struct set to now."
  (make-ts :unix (float-time)))

(defun ts-format (format-string &optional ts)
  "Format timestamp TS with `format-time-string' according to FORMAT-STRING."
  (format-time-string (or format-string ts-default-format)
                      (if ts
                          (ts-unix ts)
                        (ts-unix (ts-now)))))

(defun ts-update (ts)
  "Return timestamp TS after updating its Unix timestamp from its other slots.
To be used after setting slots."
  (pcase-let* (((cl-struct ts second minute hour dom moy year) ts))
    (setf (ts-unix ts) (float-time (encode-time second minute hour dom moy year)))
    ts))

(defmacro ts-define-fill ()
  "Define `ts-fill' function that fills all applicable slots of `ts' object from its `unix' slot."
  (let* ((slots (->> (cl-struct-slot-info 'ts)
                     (--select (and (not (member (car it) '(unix internal cl-tag-slot)))
                                    (plist-get (cddr it) :constructor)))

                     (--map (list (intern (concat ":" (symbol-name (car it))))
                                  (cddr it)))))
         (keywords (-map #'first slots))
         (constructors (->> slots
                            (--map (plist-get (cadr it) :constructor))
                            -non-nil))
         (types (--map (plist-get (cadr it) :type) slots))
         (format-string (s-join "\f" constructors)))
    `(defun ts-fill (ts)
       "Fill all slots of timestamp TS from Unix timestamp and return TS.
If FORCE is non-nil, update already-filled slots."
       (let* ((time-values (split-string (format-time-string ,format-string (ts-unix ts)) "\f"))
              (args (cl-loop for type in ',types
                             for tv in time-values
                             for keyword in ',keywords
                             append (list keyword (pcase type
                                                    ('integer (string-to-number tv))
                                                    (_ tv))))))
         (apply #'make-ts :unix (ts-unix ts) args)))))
(ts-define-fill)

(defmacro ts-define-reset ()
  "Define `ts-reset' function that resets all applicable slots of `ts' object from its `unix' slot."
  ;; MAYBE: Just make a new ts object, copying the unix slot.
  (let ((slots (->> (cl-struct-slot-info 'ts)
                    (-map #'car)
                    (--select (not (member it '(unix internal cl-tag-slot)))))))
    `(defun ts-reset (ts)
       "Reset all slots of timestamp TS except `unix'.
Allows the slots to be recomputed after updating `unix'."
       ,@(cl-loop for slot in slots
                  for accessor = (intern (concat "ts-" (symbol-name slot)))
                  collect `(setf (,accessor ts) nil))
       ts)))
(ts-define-reset)

;; FIXME: This fails, and I'm not sure if it's a limitation of gvs or if I did something wrong:
;;   (ts-incf (ts-moy (ts-now)))

(defun ts-adjust (period ts)
  "Adjust timestamp TS by PERIOD and return TS.
PERIOD should be understood by `org-read-date', e.g. \"+1d\"."
  ;; FIXME: `org-read-date' doesn't accept very many formats.  Should either write our own version
  ;; or use GNU date.
  (cl-incf (ts-unix ts) (ts-period-secs period))
  ts)

(defun ts-period-secs (period)
  "Return seconds represented by PERIOD.
PERIOD should be understood by `org-read-date'."
  ;; FIXME: This gives inconsistent results.  I guess we'll have to calculate it another way.
  (let* ((future-time (float-time (org-read-date nil t period))))
    (- future-time (float-time))))

(defun ts-difference (a b)
  "Return difference between timestamps A and B."
  (- (ts-unix a) (ts-unix b)))

;;;;; Generalized variables

;; These incf and decf functions are very cool, and they may make the adjust function unnecessary,
;; because you can do something like (ts-incf (ts-moy ts) 120) and the timestamp is set to 10 years
;; in the future.

;;  TODO: Look at `cl-incf' implementation, consider whether we should imitate it.

(cl-defmacro ts-incf (field &optional (value 1))
  "Increment timestamp FIELD by VALUE (default 1) and update Unix timestamp."
  `(progn
     (ts-fill ,(cadr field))
     (prog1
         (cl-incf ,field ,value)
       (ts-update ,(cadr field))
       (ts-reset ,(cadr field)))))

(cl-defmacro ts-decf (field &optional (value 1))
  "Decrement timestamp FIELD by VALUE (default 1) and update Unix timestamp."
  `(progn
     (ts-fill ,(cadr field))
     (prog1
         (cl-decf ,field ,value)
       (ts-update ,(cadr field))
       (ts-reset ,(cadr field)))))

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
