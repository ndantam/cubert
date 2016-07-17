(in-package :cubert)

;;;;;;;;;;;;;;;;
;;; Commands ;;;
;;;;;;;;;;;;;;;;
(defun run-command (cmd)
  (format t "~&~{~A~^ ~}~%" cmd)
  (multiple-value-bind (output error-output exit-code)
      (uiop/run-program:run-program cmd
                                    ;:ignore-error-status t
                                    :output *standard-output*
                                    :error-output *error-output*)
    (declare (ignore output error-output))
    exit-code))



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
