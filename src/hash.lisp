(in-package :cubert)

(defparameter *db-dir* ".cubert")

(defparameter *sha1-dir* "sha1")
(defparameter *ino-dir* "ino")

(defparameter *verbose* t)

(defun file-hash (file)
  (let ((output
         (with-output-to-string (s)
           (uiop/run-program:run-program (list "sha1sum" (namestring file))
                                         :output s
                                         :error-output *error-output*))))
    (subseq output 0 (position #\Space output))))

(defun make-db (pathname)
  (ensure-directories-exist (subdir pathname *db-name*)))

(defun find-db (root)
  (labels ((rec (pathname)
             (or (probe-file (subdir pathname *db-dir*))
                 (let ((parent (parentdir pathname)))
                   (when (equal (truename pathname)
                                (truename parent))
                     (error "Could not find cubert DB above ~A" root))
                   (rec parent)))))
    (rec root)))

(defvar *db-abort* nil)

(defun db-abort ()
  (setq *db-abort* t))

(defun db-check-ino-file (ino truename)
  (unless (= ino (stat-ino (stat truename)))
    (error "Wrong inode of ~A, wanted ~A"
           truename ino)))

(defun verify-match (a b)
  (uiop/run-program:run-program (list "cmp"
                                      (namestring a)
                                      (namestring b))
                                :output *standard-output*
                                :error-output *error-output*))

(defun db-intern (pathname)
  (let* ((verbose *verbose*)
         (db (find-db pathname))
         (inodir (subdir db *ino-dir*))
         (hashdir (subdir db *sha1-dir*)))
    (ensure-directories-exist inodir)
    (ensure-directories-exist hashdir)
    (labels ((ino (file)
               (stat-ino (stat file)))
             (link (old new)
               (when verbose
                 (format t "~&`~A' => `~A'~%" new old))
               (sb-posix:link old new))
             (symlink (old new)
               (when verbose
                 (format t "~&`~A' => `~A'~%" new old))
               (sb-posix:symlink old new))
             (ino-file (ino)
               (merge-pathnames (format nil "~A" ino)
                                inodir))
             (add-ino-file (hash ino-file)
               (symlink (make-pathname :directory (list :relative :up *sha1-dir*)
                                       :name hash)
                        ino-file))
             (intern-existing (file ino hash-file hash)
               (let* ((hash-ino (ino hash-file))
                      (hash-ino-file (ino-file hash-ino)))
                 ;; Link hash-file to the file unless inodes match
                 (unless (= ino hash-ino)
                   (verify-match file hash-file) ; paranoid verify
                   (sb-posix:unlink file)
                   ;; Uh-oh, the file is temporarily missing...
                   (link hash-file file))
                 ;; Ensure that hashfile inode in registered
                 (if-let ((probed (probe-file hash-ino-file)))
                   ;; validate existing ino file
                   (progn
                     (db-check-ino-file hash-ino probed)
                     (unless (equal (namestring probed)
                                    (namestring (truename hash-file)))
                       (error "Unexpected truename of ~A" hash-ino-file)))
                   ;; create no file
                   (add-ino-file hash hash-ino-file))))
             (intern-file (file)
               ;; gracefully abort
               (when *db-abort*
                 (setq *db-abort* nil)
                 (break))
               ;; Intern it
               (let* ((ino (ino file))
                      (ino-file (ino-file ino)))
                 (if-let ((probe-ino (probe-file ino-file)))
                   ;; Inode is already in the DB, verify it
                   (db-check-ino-file ino probe-ino)
                   ;; Hash the file
                   (let* ((hash (file-hash file))
                          (hash-file (merge-pathnames hash hashdir)))
                     (if (probe-file hash-file)
                         ;; have hash in DB, use it
                         (intern-existing file ino hash-file hash)
                         ;; no hash in DB, intern it
                         (progn (link file hash-file)
                                (add-ino-file hash ino-file))))))))
               (visit-files #'intern-file pathname))))
