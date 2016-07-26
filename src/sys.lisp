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
    (format t "~&~{~A~^ ~}~%" cmd)
    (finish-output))
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

(defun fs-sync (&optional pathname)
  (sudo-command `("sync" ,@(when pathname
                                 (list "--file-system" (namestring pathname))))))

(defun directory-p (pathname)
  (null (pathname-name (pathname pathname))))

(defun visit-pathname (pathname function)
  (labels ((rec (pathname)
             (let ((recurse (funcall function pathname)))
               (when (and recurse (directory-p pathname))
                 (map nil #'rec
                      (directory (subdir pathname :wild)))))))
    (rec (pathname pathname))))

(defun find-pathname (root &key
                             name
                             name-regex
                             recurse
                             function
                             (collect t)
                             (file t)
                             (directory t))
  (let ((result nil)
        (name-regex (if (or (stringp name-regex)
                            (consp name-regex))
                        (ppcre:create-scanner name-regex)
                        name-regex)))
    (visit-pathname root (lambda (pathname)
                           (let* ((is-dir (directory-p pathname))
                                  (current-name (if is-dir
                                                    (car (last (pathname-directory pathname)))
                                                    (pathname-name pathname))))
                             (if (and
                                  ;; name
                                  (or (null name)
                                      (string= current-name name))
                                  (or (null name-regex)
                                      (ppcre:scan name-regex current-name))
                                  ;; type
                                  (if is-dir
                                      directory
                                      file))
                                 ;; Match
                                 (progn
                                   (when collect
                                     (push pathname result))
                                   (when function
                                     (funcall function pathname))
                                   recurse)
                                 ;; No match
                                 is-dir))))
    result))



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

(defun snapshot-btrfs (source destination &key
                                            readonly)
  (if (probe-file source)
      ;; snapshot source to destination
      (unless (probe-file destination)
        (sudo-command `("btrfs" "subvolume" "snapshot"
                                ,@(when readonly '("-r"))
                                ,(namestring source)
                                ,(namestring destination))))
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
       (snapshot-btrfs current today :readonly t))
      (:hardlink
       (snapshot-hardlink current today)))))
