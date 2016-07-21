(in-package :cubert)



(defun sync (source destination
             &key
               (type (probe-type destination))
               (verbose t)
               destination-host)
  (assert (null destination-host))
  (run-command `("rsync" "--archive"
                         "--delete"
                         ,@(when (type-cow-p type)
                                 '("--inplace"))
                         ,@(when verbose '("--verbose"))
                         "--"
                         ,@source
                         ,(subdir-string destination "current"))))

(defun backup (source destination
               &key
                 source-host
                 destination-host
               (iso-date (iso-date)))
  (let ((source (loop for s in (ensure-list source)
                   for n = (namestring s)
                   collect (if source-host
                               (concatenate 'string source-host ":" s)
                               s)))
        (type (probe-type destination)))
    ;; Take a snapshot
    (snapshot destination
              :type type
              :iso-date iso-date)
    ;; Perform the backup
    (sync source destination
          :type type
          :destination-host destination-host)
    ;; Flush to disk
    (fs-sync)
    ;; Rotate old backups
    ))


;; ;; Save daily for the most recent 7 days
;; ;; Save weekly for the most recent four weeks (sunday)
;; ;; Save Monthly (1st) for 1 year
;; (defun maybe-rotate (dir &optional now)
;;   (format t "~&Checking directory: ~A... "
;;           dir)
;;   (let* ((iso-string (car (last (pathname-directory dir))))
;;          (iso-time (encode-iso-date iso-string)))
;;     (if iso-time
;;       (let ((last-week (time-delta now :days -8))
;;             (last-month (time-delta now :months -1))
;;             (last-year (time-delta now :years -1)))
;;         (when (> iso-time last-week)
;;           ;; save last week
;;           (format t "skipping, this week")
;;           (return-from maybe-rotate))
;;         (when (and
;;                (> iso-time last-month)
;;                (= 6 (time-day iso-time)))
;;           ;; save last weeklys
;;           (format t "skipping, weekly")
;;           (return-from maybe-rotate))
;;         (when (and
;;                (> iso-time last-year)
;;                (= 1 (time-date iso-time)))
;;           ;; save last years monthlys
;;           (format t "skipping, monthly")
;;           (return-from maybe-rotate))
;;         ;; now we delete
;;         (format t "stale, removing~%")
;;         (format t "~&rm -rf ~A" dir))
;;       (format t "not a back, moving on"))))


;; (defun rotate-bk (dir &optional (now (get-universal-time)))
;;   (mapc (lambda (dir)
;;           (maybe-rotate dir now))
;;         (directory (subdir dir "*"))))


;; (defun backup0 (target-dir source-dir
;;                &key (current-subdir "current")
;;                verbose ssh)
;;   (let* ((now (get-universal-time))
;;          (yesterday-dir (subdir target-dir
;;                                  (iso-date now)))
;;          (rsync-dst-dir (subdir target-dir current-subdir)))
;;     ;; backup the backup
;;     (sb-ext:run-program "rm"
;;                         `("-rf"
;;                           ,@(when verbose '("--verbose"))
;;                           ,yesterday-dir)
;;                         :input t
;;                         :output t
;;                         :search t)
;;     (sb-ext:run-program "cp"
;;                         `("-al"
;;                           ,@(when verbose '("--verbose"))
;;                           ,rsync-dst-dir
;;                           ,yesterday-dir)
;;                         :input t
;;                         :output t
;;                         :search t)
;;     ;; backup the data
;;     (let ((cmd "rsync")
;;           (args `("--recursive"
;;                   ,@(when ssh '("--rsh=ssh"))
;;                   ,@(when verbose '("--verbose"))
;;                   "--perms"
;;                   "--one-file-system"
;;                   "--cvs-exclude"
;;                   "--numeric-ids"
;;                   "--times"
;;                   "--delete"
;;                   "--owner"
;;                   "--group"
;;                   "--acls"
;;                   ;"--backup"
;;                   ;,(concatenate 'string "--backup-dir=" rsync-bkup-dir)
;;                   ,source-dir
;;                   ,rsync-dst-dir)))
;;       (format t "~&~A ~A" cmd args)
;;       (finish-output)
;;       (sb-ext:run-program cmd args
;;                           :input t
;;                           :output t
;;                           :search t))
;;       ;(sb-ext:run-program cmd args :output t :output t))
;;   (rotate-bk target-dir)))
