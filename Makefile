
.PHONY: all test test-sbcl test-ecl doc

all: test-sbcl doc

test: test-sbcl test-ecl

# NB: the suite needs the rc files (they load quicklisp), so no
# --no-userinit / --norc here.  The fake flags (--warnings,
# --without-pluto) must come AFTER the real options — sbcl stops
# processing toplevel options at the first one it doesn't recognize.
test-sbcl:
	cd tests; sbcl --eval '(progn (load "test-pluto.lisp") (sb-ext:exit))' --warnings --without-pluto

test-ecl:
	cd tests; ecl --eval '(progn (load "test-pluto.lisp") (si:quit))'

doc:
	pandoc --toc --toc-depth=4 -s -f markdown -t html5 -o ./docs/pluto-documentation.html ./tests/pluto-results.md
