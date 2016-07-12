(in-package :cubert)

;; Notes: CL Ignores leap seconds (for better or worse).  This should
;; make calculation time delta's straighforward.



(defparameter +day-seconds+ (* 60 60 24))

(defun run-command (cmd)
  (format t "~&~{~A~^ ~}~%" cmd)
  (multiple-value-bind (output error-output exit-code)
      (uiop/run-program:run-program cmd
                                    ;:ignore-error-status t
                                    :output *standard-output*
                                    :error-output *error-output*)
    (declare (ignore output error-output))
    exit-code))

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

(defun subdir0 (top sub)
  (concatenate 'string
               top
               (if (eq (elt top (1- (length top))) #\/) "" "/" )
               sub))


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



;; Save daily for the most recent 7 days
;; Save weekly for the most recent four weeks (sunday)
;; Save Monthly (1st) for 1 year
(defun maybe-rotate (dir &optional now)
  (format t "~&Checking directory: ~A... "
          dir)
  (let* ((iso-string (car (last (pathname-directory dir))))
         (iso-time (parse-iso-date iso-string)))
    (if iso-time
      (let ((last-week (time-delta now :days -8))
            (last-month (time-delta now :months -1))
            (last-year (time-delta now :years -1)))
        (when (> iso-time last-week)
          ;; save last week
          (format t "skipping, this week")
          (return-from maybe-rotate))
        (when (and
               (> iso-time last-month)
               (= 6 (time-day iso-time)))
          ;; save last weeklys
          (format t "skipping, weekly")
          (return-from maybe-rotate))
        (when (and
               (> iso-time last-year)
               (= 1 (time-date iso-time)))
          ;; save last years monthlys
          (format t "skipping, monthly")
          (return-from maybe-rotate))
        ;; now we delete
        (format t "stale, removing~%")
        (format t "~&rm -rf ~A" dir))
      (format t "not a back, moving on"))))


(defun rotate-bk (dir &optional (now (get-universal-time)))
  (mapc (lambda (dir)
          (maybe-rotate dir now))
        (directory (subdir dir "*"))))


(defun backup0 (target-dir source-dir
               &key (current-subdir "current")
               verbose ssh)
  (let* ((now (get-universal-time))
         (yesterday-dir (subdir target-dir
                                 (iso-date now)))
         (rsync-dst-dir (subdir target-dir current-subdir)))
    ;; backup the backup
    (sb-ext:run-program "rm"
                        `("-rf"
                          ,@(when verbose '("--verbose"))
                          ,yesterday-dir)
                        :input t
                        :output t
                        :search t)
    (sb-ext:run-program "cp"
                        `("-al"
                          ,@(when verbose '("--verbose"))
                          ,rsync-dst-dir
                          ,yesterday-dir)
                        :input t
                        :output t
                        :search t)
    ;; backup the data
    (let ((cmd "rsync")
          (args `("--recursive"
                  ,@(when ssh '("--rsh=ssh"))
                  ,@(when verbose '("--verbose"))
                  "--perms"
                  "--one-file-system"
                  "--cvs-exclude"
                  "--numeric-ids"
                  "--times"
                  "--delete"
                  "--owner"
                  "--group"
                  "--acls"
                  ;"--backup"
                  ;,(concatenate 'string "--backup-dir=" rsync-bkup-dir)
                  ,source-dir
                  ,rsync-dst-dir)))
      (format t "~&~A ~A" cmd args)
      (finish-output)
      (sb-ext:run-program cmd args
                          :input t
                          :output t
                          :search t))
      ;(sb-ext:run-program cmd args :output t :output t))
  (rotate-bk target-dir)))


(defun subdir (pathname subdirectory)
  (let ((pathname (pathname pathname)))
    ;; ensure not a file
    (assert (null (pathname-name pathname)))
    (merge-pathnames (make-pathname :directory (append (pathname-directory pathname)
                                                       (ensure-list subdirectory)))
                     pathname)))

(defun snapshot (destination &key
                               destination-host
                               (iso-date (iso-date))
                               )
  (assert (null destination-host))
  (let ((current (subdir destination "current"))
        (today (subdir destination iso-date)))
    (unless (probe-file current)
      (run-command (list "sudo" "btrfs" "subvolume" "create" (namestring current))))
    (unless (probe-file today)
      (run-command (list "sudo" "btrfs" "subvolume" "snapshot"
                         (namestring current)
                         (namestring today))))))

(defun sync (source destination
             &key
               destination-host
               (iso-date (iso-date)))
  (assert (null destination-host))
  (run-command `("rsync" ,@source
                         ,(namestring (subdir destination iso-date))
                         "--archive"
                         "--delete"
                         "--recursive"
                         "--progress")))

(defun backup (source destination
               &key
                 source-host
                 destination-host
               (iso-date (iso-date)))
  (let ((source (loop for s in (ensure-list source)
                   for n = (namestring s)
                   collect (if source-host
                               (concatenate 'string source-host ":" s)
                               s))))
    (snapshot destination
              :destination-host destination-host
              :iso-date iso-date)
    (sync source destination
          :destination-host destination-host
          :iso-date iso-date)
    ;; Perform the backup
    ;; Rotate old backups
    ))
