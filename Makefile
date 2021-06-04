.PHONY: all test doc

all: test-sbcl doc

test-sbcl:
	cd tests; sbcl --eval '(progn (load "test-pluto.lisp") (sb-ext:exit))' --warnings --without-pluto

test-clisp:
	cd tests; clisp -i ~/.clisprc.lisp test-pluto.lisp

test-ecl:
	cd tests; ecl --eval '(progn (load "test-pluto.lisp") (quit))'

doc:
	pandoc -f markdown -t html5 -o ./docs/pluto-documentation.html ./tests/pluto-results.md

