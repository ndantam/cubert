cubert_SOURCES := package.lisp bkrotate.lisp cubert.asd

cubert: $(cubert_SOURCES)
	sbcl --eval '(require :cubert)' \
	     --eval '(sb-ext:save-lisp-and-die "cubert" :executable t)'

clean:
	rm -f cubert

.PHONY: clean
