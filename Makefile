PREFIX ?= /usr/local
DESTDIR ?= $(PREFIX)

LISP = sbcl --load
# LISP = ecl -load


cubert_SOURCES := \
	build.lisp \
	src/package.lisp \
	src/bkrotate.lisp  \
	src/time.lisp  \
	src/sys.lisp  \
	src/cubert.asd

cubert: $(cubert_SOURCES) Makefile
	if test -f src/asdf-output/cubert; then rm src/asdf-output/cubert; fi
	$(LISP) build.lisp
	if test -f src/asdf-output/cubert; then mv src/asdf-output/cubert .; fi


install: cubert
	install -t $(DESTDIR)/bin cubert

uninstall:
	rm -f  $(PREFIX)/bin/cubert

clean:
	rm -f cubert

.PHONY: clean install uninstall
