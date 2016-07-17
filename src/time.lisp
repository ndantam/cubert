(in-package :cubert)

;; Notes: CL Ignores leap seconds (for better or worse).  This should
;; make calculation time delta's straighforward.

(defparameter +day-seconds+ (* 60 60 24))

(defun iso-date (&optional (utime (get-universal-time)))
  (multiple-value-bind (sec min hour date mon year)
      (decode-universal-time utime)
    (declare (ignore sec min hour))
    (format nil "~A-~2,'0d-~2,'0d" year mon date)))


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

(defun parse-iso-date (string &optional error-value)
  (handler-case
      (let ((year (parse-integer string :start 0 :end 4))
            (month (parse-integer string :start 5 :end 7))
            (date (parse-integer string :start 8 :end 10)))
        (encode-universal-time 0 0 0 date month year))
    (error () error-value)))
