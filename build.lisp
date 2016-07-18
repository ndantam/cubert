(require :asdf)

;; Add source directory to ASDF registry
(pushnew (pathname "./src/")
         asdf:*central-registry*
         :test #'equal)

(require :cubert)

#+sbcl
(let ((compression nil))
  #+sb-core-compression
  (setq compression t)
  (sb-ext:save-lisp-and-die "cubert"
                            :executable t
                            :compression compression))

#+ecl
(asdf:make-build :cubert
                 :type
                 :program :monolithic t
                 :epilogue-code '(si:top-level)
                 :move-here t)

#+ecl
(ext:quit)
