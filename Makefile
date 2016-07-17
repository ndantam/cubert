cubert_SOURCES := \
	src/package.lisp \
	src/bkrotate.lisp  \
	src/time.lisp  \
	src/sys.lisp  \
	src/cubert.asd

cubert: $(cubert_SOURCES)
	sbcl --eval '(require :cubert)' \
	     --eval '(sb-ext:save-lisp-and-die "cubert" :executable t)'

clean:
	rm -f cubert

.PHONY: clean
