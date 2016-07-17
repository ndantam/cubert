cubert_SOURCES := \
	src/package.lisp \
	src/bkrotate.lisp  \
	src/time.lisp  \
	src/sys.lisp  \
	src/cubert.asd

LISP = sbcl --load
# LISP = ecl -load

cubert: $(cubert_SOURCES)
	$(LISP) build.lisp


clean:
	rm -f cubert

.PHONY: clean
