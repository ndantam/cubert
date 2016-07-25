(in-package :cubert)

;; Notes: CL Ignores leap seconds (for better or worse).  This should
;; make calculation time delta's straighforward.

(defparameter +day-seconds+ (* 60 60 24))


;;;;;;;;;;;;;;;;
;;; ISO 8601 ;;;
;;;;;;;;;;;;;;;;
;;;
;;; Date: YYYY-MM-DD
;;;
;;; Date and Local Time:  YYYY-MM-DDTHH:MM
;;;                       YYYY-MM-DDTHH:MM:SS
;;;
;;; Date and UTC Time:    YYYY-MM-DDTHH:MM:SSZ
;;;                       YYYY-MM-DDTHH:MM:SS+0
;;;
;;; Date and Time: YYYY-MM-DDTHH:MM:SS+HH
;;;                YYYY-MM-DDTHH:MM:SS+HH:MM
;;;
;;;

(defun local-time-zone ()
  (multiple-value-bind (sec min hour date mon year dow dst tz)
      (decode-universal-time 0)
    (declare (ignore sec min hour date mon year dow dst))
    tz))

(defun iso-date (&optional universal-time time-zone)
  (multiple-value-bind (sec min hour date mon year)
      (decode-universal-time (or universal-time
                                 (get-universal-time))
                             time-zone)
    (declare (ignore sec min hour))
    (format nil "~A-~2,'0d-~2,'0d" year mon date)))

(defun iso-date-time (&optional universal-time time-zone)
  (multiple-value-bind (sec min hour date mon year dow dst tz)
      (decode-universal-time (or universal-time
                                 (get-universal-time))
                             time-zone)
    (declare (ignore dow dst tz))
    (format nil "~A-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~A"
            year mon date hour min sec
            (if time-zone
              (if (zerop time-zone) "Z"
                  (format nil "+~A" time-zone))
              ""))))

(defparameter +iso-time-zone-regex+
  `(:alternation
    ;; UTC Zulu
    (:register (:alternation "z" "Z"))
    (:sequence
     ;; TZ hour
     (:register  (:sequence (:alternation "+" "-")
                            (:greedy-repetition 1 2 :digit-class)))
     ;; Optional TZ Minute
     (:greedy-repetition
      0 1
      (:sequence
       ":"
       (:register  (:greedy-repetition 0 2 :digit-class)))))))

(defparameter +iso-time-regex+
  `(:sequence
    ;; hour
    (:register (:sequence :digit-class :digit-class))
    ":"
    ;; min
    (:register (:sequence :digit-class :digit-class))
    ;; maybe seconds
    (:greedy-repetition
     0 1
     (:sequence
      ":"
      (:register (:sequence :digit-class :digit-class))))
    ;; timezone
    (:greedy-repetition
     0 1
     ,+iso-time-zone-regex+)))

(defparameter +iso-date-regex+
  `(:sequence
    ;; year
    (:register (:sequence :digit-class :digit-class :digit-class :digit-class))
    (:regex "-?")
    ;; month
    (:register (:sequence :digit-class :digit-class))
    (:regex "-?")
    ;; day
    (:register (:sequence :digit-class :digit-class))))

(defparameter +iso-date-time-regex+
  `(:sequence
    ;; start
    :start-anchor
    (:regex "\\s*")
    ;; year
    ,+iso-date-regex+
    ;; Time
    (:greedy-repetition
     0 1
     (:sequence (:regex "\\s*T?\\s*")
                ,+iso-time-regex+))))

(defparameter +iso-date-scanner+
  (ppcre:create-scanner +iso-date-time-regex+))

(defun parse-iso-date (string)
  (ppcre:register-groups-bind (year month date hour min sec tz-zulu tz-hour tz-min)
      (+iso-date-scanner+ string)
    (if (and year month date)
        (flet ((maybe-parse (string)
                 (if string
                     (parse-integer string)
                     0)))
          (values (maybe-parse sec)
                  (maybe-parse min)
                  (maybe-parse hour)
                  (parse-integer date)
                  (parse-integer month)
                  (parse-integer year)
                  (cond (tz-zulu 0)
                        (tz-hour (+ (parse-integer tz-hour)
                                    (/ (maybe-parse tz-min) 60))))))
          (error "not an iso date"))))

(defun encode-iso-date (string)
  (multiple-value-call #'encode-universal-time
    (parse-iso-date string)))

(defun time-delta (time &key
                  (seconds 0) (minutes 0) (hours 0)
                  (days 0) (weeks 0) (months 0) (years 0))
  (if (or (not (zerop years)) (not (zerop months)))
     ;; let lisp work out months and years
     (multiple-value-bind (sec min hour date month year)
         (decode-universal-time time)
       (let* ((month* (+ month months -1))
              (year* (if (and (<= 0 month*) (>= 11 month*))
                         ;; don't need to overflow to years
                         (+ year years)
                         ;; overflow extra months to years
                         (+ year  years
                           (truncate (/ month* 12))
                           (if (< month* 0) -1 0)))))
         ;(format t "year*: ~A, month*: ~A" year* month*)
         (time-delta (encode-universal-time sec min hour date
                                            (+ (rem month* 12)
                                               (if (< month* 0) 13 1))
                                            year*)
                     :seconds seconds
                     :minutes minutes
                     :hours hours
                     :days days
                     :weeks weeks)))
     ;; this one is easy
     (+ time
        seconds
        (* 60 minutes)
        (* 60 60 hours)
        (* 60 60 24 days)
        (* 60 60 24 7 weeks))))

(defun time-day (time)
  (multiple-value-bind (sec min hour date mon year day)
      (decode-universal-time time)
    (declare (ignore sec min hour date mon year))
    day))

(defun time-date (time)
  (multiple-value-bind (sec min hour date)
      (decode-universal-time time)
    (declare (ignore sec min hour ))
    date))
