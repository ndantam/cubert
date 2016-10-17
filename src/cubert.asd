(asdf:defsystem cubert
  :description "Backup Script"
  :depends-on ("alexandria" "cl-ppcre")
  ;; Keep these components in sync with Makefile.am
  :components ((:file "package")
               (:file "time" :depends-on ("package"))
               (:file "sys" :depends-on ("package"))
               (:file "bkrotate" :depends-on ("sys" "time" "package"))
               (:file "hash" :depends-on ("sys"))
               (:file "git" :depends-on ("sys" "package"))))
