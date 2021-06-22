
.PHONY: all test doc

all: test-sbcl doc

test-sbcl:
	cd tests; sbcl --no-userinit --eval '(progn (load "test-pluto.lisp") (sb-ext:exit))' --warnings --without-pluto

test-ecl:
	cd tests; ecl --norc --eval '(progn (load "test-pluto.lisp") (quit))'

test-clisp:
	cd tests; clisp test-pluto.lisp

test-abcl:
	cd tests; abcl --noinit --eval '(progn (load "test-pluto.lisp") (exit))'

doc:
	pandoc --toc --toc-depth=4 -s -f markdown -t html5 -o ./docs/pluto-documentation.html ./tests/pluto-results.md

