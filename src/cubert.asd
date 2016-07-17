(asdf:defsystem cubert
  :description "Backup Script"
  :depends-on ("alexandria")
  ;; Keep these components in sync with Makefile.am
  :components ((:file "package")
               (:file "build" :depends-on ("package"))
               (:file "time" :depends-on ("package"))
               (:file "sys" :depends-on ("package"))
               (:file "bkrotate" :depends-on ("sys" "time" "package"))))
