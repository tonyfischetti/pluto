# `def-test-doc`: an amnesia document

*You wrote a test harness, forgot how it worked, rediscovered that it was
better than you remembered, and then rebuilt its core around a technique
you'd been meaning to try since you read about it in a library in
Catalonia. This document exists so the next forgetting is cheaper.*

-----

## 1. The big idea

`def-test-doc` is a **literate test harness**: one stream of declarations
in `test-pluto.lisp` produces, from the same source,

1. **the test suite** — run on every `make test-sbcl`, dies loudly
   (exit 1) if anything fails, and
2. **the documentation** — `pluto-results.md`, rendered to
   `docs/pluto-documentation.html` by pandoc (`make doc`), where every
   example shown is *known to work* because it just ran.

This is the same philosophy as Python doctests or Rust doc-tests, arrived
at independently: documentation that lies is worse than no documentation,
so examples should be executed, and the canonical example of a function
*is* a perfectly good test of it.

The obvious tension: docs want one pretty, curated example per function;
a serious test suite wants many ugly edge cases nobody should ever read.
The design resolves this by making the two roles **orthogonal
capabilities** that any entry can have in any combination:

- `markdown-able` — renders into the documentation
- `test-able` — runs as a test (with an *output type*: does the
  assertion look at what it `returns`, its `stdout`, or its `stderr`?)
- `bench-able` — benchmark (currently: prints a banner; looping is
  still TODO)

So a curated doc example is `markdown-able` + `test-able`; an ugly
regression test is `test-able` only (by convention, indented two extra
spaces in `test-pluto.lisp`); a prose paragraph is `markdown-able` only
(`def-raw-markdown`). Coverage and beauty stop competing.

## 2. The pipeline

```
test-pluto.lisp
  (load "../pluto.lisp")          ; zero-dep core, loaded raw
  (load "def-test-doc.lisp")      ; the harness (this doc's subject)
  (start-test/doc :title "Pluto")
  (def-test/doc-section ...)      ; pushes section-header elements
  (def-test/doc-test ...)         ; pushes test elements  ← the workhorse
  (def-raw-markdown ...)          ; pushes prose elements
  ...                             ; (charon gets quickloaded near the end)
  (end-test/doc)                  ; reverses /all-test-docs/
  (if (run-tests)                 ; every element, in order
    (render-markdown → pluto-results.md)
    (die "at least one test failed"))
```

Everything is an **element object** accumulated into `/all-test-docs/`;
running and rendering are just two different walks over that list.

A `def-test/doc-test` form has five parts:

```lisp
(def-test/doc-test 'str-sub                    ; ① symbol, for headings
  `(markdown-able (test-able returns))         ; ② traits
  'function                                    ; ③ doc: 'function pulls the
                                               ;    docstring; a string is
                                               ;    used verbatim
  (string= test-return-value! "belle")         ; ④ the assertion
  (str-sub "belle and sebastian" 0 5))         ; ⑤ the code under test
```

The code (⑤) runs with stdout/stderr captured and errors trapped; the
assertion (④) is arbitrary Lisp evaluated with four anaphors in scope:

- `test-return-value!` — what ⑤ returned
- `test-stdout!` / `test-stderr!` — what it printed
- `test-error!` — the condition it signalled, or `nil`

That last one means *expected failures are assertable*:
`(and test-error! (null test-return-value!))` is a passing test for code
that ought to blow up — essential for a library whose personality is
die-loudly. A signalling test that *didn't* expect it is a failure (with
diagnostics: returned/stdout/stderr/condition), never a suite crash.

## 3. The former solution: traits as data

Originally, capabilities were an **alist carried in a slot**:

```lisp
(make-instance 'test/doc-test
  :traits '((markdown-able t) (test-able returns)) ...)

(defun has-trait (element atrait)
  (assoc atrait { element 'traits }))

(defmethod run-test :around ((test test/doc-test))
  (if (has-trait test 'test-able)
      (call-next-method)
      (ft (grey "skipping ..."))))

(defmethod to-markdown :around ((test test/doc-element))
  (when (has-trait test 'markdown-able)
    (call-next-method)))
```

This *worked* — the orthogonal-capabilities insight was already right —
but notice what it is: *a hand-rolled reimplementation of method
dispatch*. "Does this object have this capability?" answered by list
search inside `:around` gates, with capability parameters (`returns`,
the bench count) smuggled as alist values and fished out by helpers
(`output-type`). Every new capability meant threading a new key through
scattered checks.

## 4. The technique now: mixins + the MOP

The rebuild makes the type system answer the question instead. Lineage:
Sonya Keene's *Object-Oriented Programming in Common Lisp* (the mixin
style) and *The Art of the Metaobject Protocol* (runtime class
synthesis). Three moves:

**① Capabilities are classes.** Trait parameters become slots:

```lisp
(defclass markdown-able () ())
(defclass test-able  () ((output-type :initarg :output-type :initform nil)))
(defclass bench-able () ((bench-times :initarg :bench-times :initform 1)))
```

**② Membership checks become method applicability.** Each `has-trait`
gate is now just... a method:

```lisp
(defmethod run-test ((test test-able))     ...actually run it...)
(defmethod run-test ((test test/doc-test)) ...print "skipping"...)
(defmethod run-test :around ((test bench-able)) ...banner... (call-next-method))
(defmethod passed-p ((test test-able)) { test 'pass-p })
```

The "skipping" method only fires for tests that *aren't* `test-able`,
because the mixins precede the base class in the precedence list — the
dispatcher performs the check that `has-trait` used to. Same for
markdown: `to-markdown` has a do-nothing method on `test/doc-element`
and a real one on `markdown-able` (which delegates structural rendering
to `render-md`, with the output line as an `:after` method on
`test-able`). `has-trait` and `output-type` no longer exist.

**③ Combination classes are synthesized at runtime.** Users pick trait
combinations freely per test, so the combinations can't be predeclared.
This is where the MOP earns its keep — `ensure-class` is just a
function:

```lisp
(defun %mixed-class (base mixins)          ; mixins first: they must win
  (let ((supers (append mixins (list base))))
    (sb-mop:ensure-class                   ; clos:ensure-class on ECL
      (create-symbol (str-join "+" (mapcar #'symbol-name supers)))
      :direct-superclasses supers)))
```

`%parse-traits` maps the *unchanged* user syntax
`(markdown-able (test-able returns))` onto mixin names + initargs
(`:output-type 'returns`), and `def-test/doc-test` instantiates whatever
class that implies. Ask a running suite what exists and you get:

```
TEST-ABLE+TEST/DOC-TEST                          ; regression tests
MARKDOWN-ABLE+TEST-ABLE+TEST/DOC-TEST            ; doc examples
MARKDOWN-ABLE+TEST-ABLE+BENCH-ABLE+TEST/DOC-TEST ; the -<> bench test
RAW-MARKDOWN  TEST/DOC-TITLE  TEST/DOC-SECTION   ; statically markdown-able
```

Classes that were never asked for are never created.

## 5. Why this maps so well to this project

- The *domain* fact is "an entry has some subset of independent
  capabilities." Mixins are the CLOS-native encoding of exactly that
  fact; the alist was an encoding of the encoding.
- The capability set is **open**: adding, say, a real benchmark runner,
  or a `tty-able` for the terminal-manipulation functions, is now
  "define a class, hang methods on it" — no existing code changes, no
  new keys threaded through gates. Try that with the alist.
- Extension points come free: want per-capability behavior around any
  generic (`run-test`, `passed-p`, `render-md`), it's an `:around`/
  `:after` method away, in the standard method combination you already
  know.
- It's also simply *in character*: pluto is a personal library that uses
  Lisp's big machinery (reader macros, anaphora) wherever the machinery
  fits the shape of the problem. The MOP fits this one.

## 6. Field guide (how to do the common things)

- **Doc example:** traits `` `(markdown-able (test-able returns)) `` —
  keep it pretty; it's going in the manual.
- **Regression/edge test:** traits `` `((test-able returns)) `` and
  indent the whole form two extra spaces. Never appears in docs.
- **Expected error:** assert on `test-error!`.
- **Prose in the docs:** `def-raw-markdown` (it's a `raw-markdown`
  element — statically `markdown-able`).
- **New capability:** defclass the mixin (+ slots for its parameters),
  teach `%parse-traits` its initarg if it has one, hang methods on the
  generics you care about. Done.

## 7. Known warts (so you don't rediscover them the hard way)

- **Class names are order-sensitive**: `(a b)` and `(b a)` traits make
  two behaviorally-identical classes. Harmless; sort the mixins in
  `%mixed-class` if it ever grates.
- **`bench-able` doesn't loop yet** — the `:around` prints a banner and
  proceeds once. Now a clean place to implement it, though.
- **Rendered code is post-reader**: examples using reader macros
  (`{ }`, `?`, `#?`) show their expansion (`PLUTO-GET`,
  gensym-`LET`s...) in the docs, and those gensym names churn between
  runs (spurious one-line diffs in `pluto-results.md`).
- **Sections always render** — a `def-test/doc-section` whose tests are
  all test-only still leaves a header in the docs. Add doc content or
  don't declare a section.
- **`make test-sbcl` passes `--no-userinit`**, but `test-pluto.lisp`
  quickloads charon near the end, which needs your init. Run the suite
  with your normal init until that's reconciled.
