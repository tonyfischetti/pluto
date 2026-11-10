
.PHONY: all test test-sbcl test-ecl test-compile doc

all: test-sbcl doc

# sbcl LAST: whichever suite runs last rewrites tests/pluto-results.md,
# and the committed docs are rendered by sbcl's printer (ecl's lays
# out the code blocks differently -- 80 lines of churn for nothing)
test: test-compile test-ecl test-sbcl

# compile pluto.lisp in a COLD image: no rc files, nothing preloaded.
# The suites load pluto from source first and only then let asdf
# compile it, so a macro used before its definition (or any other
# order-of-definition slip) is invisible here and fatal on a fresh
# machine -- which is how one got to the work computer in Nov 2026.
# Fails on any full WARNING (failure-p), not on style warnings.
test-compile:
	sbcl --noinform --no-userinit --non-interactive \
	  --eval '(multiple-value-bind (fasl warnings-p failure-p) (compile-file "pluto.lisp" :output-file "/tmp/pluto-cold-compile.fasl") (declare (ignore fasl warnings-p)) (when failure-p (format t "~%cold compile of pluto.lisp FAILED (see the warnings above)~%") (sb-ext:exit :code 1)))' \
	  --eval '(progn (format t "cold compile of pluto.lisp: clean~%") (sb-ext:exit :code 0))'

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
