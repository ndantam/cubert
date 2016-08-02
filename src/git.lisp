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
(defun git-url-path (url)
  (ppcre:register-groups-bind (host path)
      (`(:sequence :start-anchor
                   ;; strip https / username@
                   (:greedy-repetition 0 1 (:regex "https?://"))
                   (:greedy-repetition 0 1 (:regex ".*@"))
                   ;; get host
                   (:regex "([^/:]*)")
                   (:alternation "/" ":")
                   ;; path
                   (:regex "(.*)"))
        url)
    (pathname (concatenate 'string
                           host "/"
                           (ppcre:regex-replace "\\.git$" path "")
                           ".git"))))


(defun %backup-git (root url)
  (let* ((root (pathname root))
         (rel-path (git-url-path url))
         (abs-path (merge-pathnames rel-path root))
         (namestring (namestring abs-path)))
    (assert (null (pathname-name root)))
    (if (probe-file abs-path)
        ;; exists, update
        (run-command (list  "git" "-C" namestring "fetch" "--prune"))
        ;; does not exist clone
        (run-command (list  "git" "clone" "--mirror" url namestring )))))

(defun backup-git (root urls)
  (loop for url in (ensure-list urls)
     do (%backup-git root url)))
