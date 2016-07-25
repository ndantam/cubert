(in-package :cubert)

(defun gitbase-fsck (root)
  (find-pathname root
                 :name-regex ".*\\.git$"
                 :file nil
                 :directory t :recurse nil
                 :collect nil
                 :function (lambda (pathname)
                             (run-command (list "git" "-C" (namestring pathname) "fsck"
                                                "--full" "--no-dangling")))))
