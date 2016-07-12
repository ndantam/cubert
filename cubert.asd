(asdf:defsystem cubert
  :description "Backup Script"
  :depends-on ("alexandria")
  ;; Keep these components in sync with Makefile.am
  :components ((:file "package")
               (:file "bkrotate" :depends-on ("package"))))
