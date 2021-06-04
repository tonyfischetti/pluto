.PHONY: all test doc

all: test-sbcl doc

test-sbcl:
	cd tests; sbcl --eval '(progn (load "test-pluto.lisp") (sb-ext:exit))' --warnings --without-pluto

# test-clisp:

doc:
	pandoc -f markdown -t html5 -o ./docs/pluto-documentation.html ./tests/pluto-results.md

