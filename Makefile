cubert:
	sbcl --eval '(require :cubert)' \
	     --eval '(sb-ext:save-lisp-and-die "cubert" :executable t)'
