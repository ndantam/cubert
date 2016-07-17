cubert_SOURCES := \
	build.lisp \
	src/package.lisp \
	src/bkrotate.lisp  \
	src/time.lisp  \
	src/sys.lisp  \
	src/cubert.asd

LISP = sbcl --load
# LISP = ecl -load

cubert: $(cubert_SOURCES) Makefile
	if test -f src/asdf-output/cubert; then rm src/asdf-output/cubert; fi
	$(LISP) build.lisp
	if test -f src/asdf-output/cubert; then mv src/asdf-output/cubert .; fi


clean:
	rm -f cubert

.PHONY: clean
