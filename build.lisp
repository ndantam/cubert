(require :asdf)

;; Add source directory to ASDF registry
(pushnew (pathname "./src/")
         asdf:*central-registry*
         :test #'equal)

(require :cubert)

#+sbcl
(cubert::save-core "cubert")

#+ecl
(asdf:make-build :cubert
                 :type
                 :program :monolithic t
                 :epilogue-code '(si:top-level)
                 :move-here t)

#+ecl
(ext:quit)
