(in-package :cubert)

#+sbcl
(defun save-core (name)
  (let ((compression nil))
  #+sb-core-compression
  (setq compression t)
  (sb-ext:save-lisp-and-die name
                            :executable t
                            :compression compression)))


#+ecl
(defun save-core (name)
  (c:build-program name)
  (ext:quit))
