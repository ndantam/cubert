(in-package :cubert)

;;;;;;;;;;;;;;;;
;;; Commands ;;;
;;;;;;;;;;;;;;;;

(defun run-command (cmd &key
                          (echo t)
                          (output *standard-output*)
                          (error-output *error-output*)
                          ignore-error-status)
  (when echo
    (format t "~&~{~A~^ ~}~%" cmd))
  (multiple-value-bind (output error-output exit-code)
      (uiop/run-program:run-program cmd
                                    :ignore-error-status ignore-error-status
                                    :output output
                                    :error-output error-output
                                    :output *standard-output*
                                    :error-output *error-output*)
    (declare (ignore output error-output))
    exit-code))


(defun sudo-command (cmd &key
                          (echo t)
                          (output *standard-output*)
                          (error-output *error-output*)
                           ignore-error-status)
  (run-command (cons "sudo" cmd)
               :echo echo
               :output output
               :error-output error-output
               :ignore-error-status ignore-error-status))

;;;;;;;;;;;;;;;;;;
;;; Filesystem ;;;
;;;;;;;;;;;;;;;;;;

(defun subdir (pathname subdirectory)
  (let ((pathname (pathname pathname)))
    ;; ensure not a file
    (assert (null (pathname-name pathname)))
    (merge-pathnames (make-pathname :directory (append (pathname-directory pathname)
                                                       (ensure-list subdirectory)))
                     pathname)))

(defun subdir-string (pathname subdirectory)
  (namestring (subdir pathname subdirectory)))

;;;;;;;;;;;;;;;;;;
;;; Snapshots  ;;;
;;;;;;;;;;;;;;;;;;

(defun probe-type (place)
  (cond
    ((btrfs-p place)
     :btrfs)
    (t
     :hardlink)))

(defun type-cow-p (type)
  (or (eq type :btrfs)
      (eq type :zfs)))

(defun btrfs-p (place)
  (zerop (sudo-command `("btrfs" "filesystem" "label"
                                 ,(namestring place))
                       :output nil
                       :error-output nil
                       :echo nil
                       :ignore-error-status t)))

(defun snapshot-btrfs (source destination)
  (if (probe-file source)
      ;; snapshot source to destination
      (unless (probe-file destination)
        (sudo-command (list "btrfs" "subvolume" "snapshot"
                            (namestring source)
                            (namestring destination))))
      ;; create source
      (sudo-command (list "btrfs" "subvolume" "create" (namestring source)))))

(defun snapshot-hardlink (source destination)
  (when (and (probe-file source)
             (not (probe-file destination)))
    (run-command `("cp" "--archive"
                        "--link"
                        "--"
                        ,(namestring source)
                        ,(namestring destination)))))

(defun snapshot (destination &key
                               (type (probe-type destination))
                               (iso-date (iso-date)))
  (let ((current (subdir destination "current"))
        (today (subdir destination iso-date)))
    (ecase type
      (:btrfs
       (snapshot-btrfs current today))
      (:hardlink
       (snapshot-hardlink current today)))))
