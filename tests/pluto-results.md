---
title: Pluto documentation
...

-----

### about

a common lisp package that's out there



-----

### pluto parameters

fill this out

-----

### formatting


#### FN

> Alias to (format nil ...)\

```{.commonlisp}
(FN "~A ~A" "hello" "goodbye")
```

<small><pre>=> "hello goodbye"</pre></small>



#### FT

> Alias to (format t ...)\

```{.commonlisp}
(FT "~A ~A" "hello" "goodbye")
```

<small><pre>>> "hello goodbye"</pre></small>



-----

### ansi colors and codes

fill this out

-----

### string operations


#### STR+

> Combine (using princ) an arbitrary number of args into one string\

```{.commonlisp}
(STR+ "belle" "and" "sebastian")
```

<small><pre>=> "belleandsebastian"</pre></small>



```{.commonlisp}
(STR+ '(1 2 3) " go")
```

<small><pre>=> "(1 2 3) go"</pre></small>



#### STR-JOIN

> Join STRINGS with DELIM.\

```{.commonlisp}
(STR-JOIN ";" '("one" "two"))
```

<small><pre>=> "one;two"</pre></small>



#### STR-SUB

> Efficient substring of STRING from START to END (optional),\
>   where both can be negative, which means counting from the end.\

```{.commonlisp}
(STR-SUB "belle and sebastian" 0 5)
```

<small><pre>=> "belle"</pre></small>



```{.commonlisp}
(STR-SUB "belle and sebastian" 0 -14)
```

<small><pre>=> "belle"</pre></small>



```{.commonlisp}
(STR-SUB "belle and sebastian" 10)
```

<small><pre>=> "sebastian"</pre></small>



#### STRING->CHAR-LIST

> Make a string a list of single character strings\

```{.commonlisp}
(STRING->CHAR-LIST "belle")
```

<small><pre>=> ("b" "e" "l" "l" "e")</pre></small>



#### SPLIT-STRING->LINES

> Split a string with new lines into a list of strings (one for each line)\

```{.commonlisp}
(SPLIT-STRING->LINES (FORMAT NIL "this~%that~%and the other"))
```

<small><pre>=> ("this" "that" "and the other")</pre></small>



#### REPEAT-STRING

Repeats a string TIMES times
```{.commonlisp}
(REPEAT-STRING "ab" 3)
```

<small><pre>=> "ababab"</pre></small>



-----

### some essential utilities/macros


#### ALAMBDA

> Anaphoric lambda. SELF! is the function\

```{.commonlisp}
(FUNCALL
 (ALAMBDA (X)
   (WHEN (> X 0) (CONS X (SELF! (- X 1)))))
 10)
```

<small><pre>=> (10 9 8 7 6 5 4 3 2 1)</pre></small>



#### FLATTEN

>  Flattens a list (possibly inefficiently)\

```{.commonlisp}
(FLATTEN `(A B (C D (E))))
```

<small><pre>=> (A B C D E)</pre></small>



#### TAKE

> Takes `n` from beginning of `alist` and returns that in a\
>    list. It also returns the remainder of the list (use\
>    `multiple-value-bind` with it\

```{.commonlisp}
(MULTIPLE-VALUE-BIND (ONE TWO) (TAKE `(A B C D E F) 2) (LIST ONE TWO))
```

<small><pre>=> ((A B) (C D E F))</pre></small>



#### GROUP

> Turn a (flat) list into a list of lists of length `n`\

```{.commonlisp}
(GROUP `(A B C D E F) 2)
```

<small><pre>=> ((A B) (C D) (E F))</pre></small>



#### -<>

> Threading macro (put <> where the argument should be)\
>    Stolen from https://github.com/sjl/cl-losh/blob/master/src/control-flow.lisp\

```{.commonlisp}
(-<> "4" (PARSE-INTEGER <>) (SQRT <>))
```

<small><pre>=> 2.0</pre></small>



#### INTERPOSE

> Returns a sequence of the elements of SEQUENCE separated by SEPARATOR.\

```{.commonlisp}
(INTERPOSE 'SEP `(A B C))
```

<small><pre>=> (A SEP B SEP C)</pre></small>



#### WALK-REPLACE-SEXP

> Walks sexpression substituting `oldform` for `newform`.\
>    It works with lists and well as atoms. Checks equality with `test`\
>    (which is #'EQUAL by default)\

```{.commonlisp}
(WALK-REPLACE-SEXP `(+ 1 (* 2 10)) 10 3)
```

<small><pre>=> (+ 1 (* 2 3))</pre></small>



```{.commonlisp}
(WALK-REPLACE-SEXP `(A B (X Y) C) `(X Y) `IT)
```

<small><pre>=> (A B IT C)</pre></small>



#### ALISTP

> Test is something is an alist\

```{.commonlisp}
(ALISTP `((A . 1) (B . 2)))
```

<small><pre>=> T</pre></small>



#### WITH-HASH-ENTRY

> Establishes a lexical environment for referring to the _value_ of\
>    key `akey` on the hash table `ahash` using the anaphor `entry!`.\
>    So, you can setf `entry!` and the hash-table (for that key) will\
>    by modified.\

```{.commonlisp}
(LET ((TMP (MAKE-HASH-TABLE)))
  (WITH-HASH-ENTRY (TMP :COUNT)
    (SETF ENTRY! 42))
  (GETHASH :COUNT TMP))
```

<small><pre>=> 42</pre></small>



#### IF-HASH-ENTRY

> Executes `then` if there's a key `akey` in hash-table `ahash` and\
>    `else` (optional) if not. For convenience, an anaphor `entry!` is\
>    introduced that is setf-able.\

```{.commonlisp}
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH :A TMP) 1)
  (LIST (IF-HASH-ENTRY (TMP :A) :PRESENT :ABSENT)
        (IF-HASH-ENTRY (TMP :B) :PRESENT :ABSENT)))
```

<small><pre>=> (:PRESENT :ABSENT)</pre></small>



#### DELIM

> Makes a string with tabs separating values.\
>    `:what` either :list :listoflist :hash or :alist\
>    `:sep` the (CHARACTER) separator to use (default is tab)\

```{.commonlisp}
(DELIM `("a" "b" "c"))
```

<small><pre>=> "a	b	c"</pre></small>



#### ROUND-TO

> Round `number` to `precision` decimal places. Stolen from somewhere\

```{.commonlisp}
(ROUND-TO 3.14159 2)
```

<small><pre>=> 3.14</pre></small>



#### WITH-TIME

> Anaphoric macro that executes the car of the body and\
>    binds the seconds of execution time (as a float, with\
>    sub-second resolution) to TIME!. Then all the other\
>    forms in the body are executed\

```{.commonlisp}
(WITH-TIME
  (SLEEP 1)
  (FORMAT NIL "time elapsed: ~A" (ROUND TIME!)))
```

<small><pre>=> "time elapsed: 1"</pre></small>



#### TIME-FOR-HUMANS

> Converts SECONDS into minutes, hours, or days (based on magnitude)\

```{.commonlisp}
(TIME-FOR-HUMANS 4)
```

<small><pre>=> "4 seconds"</pre></small>



```{.commonlisp}
(TIME-FOR-HUMANS 4000)
```

<small><pre>=> "1.11 hours"</pre></small>



```{.commonlisp}
(TIME-FOR-HUMANS 191000)
```

<small><pre>=> "2.21 days"</pre></small>



-----

### other abbreviations and shortcuts


-----

### for-each and friends


#### FOR-EACH

> A super-duper imperative looping construct.\
>    It takes either\
>      a filename string    (to be treated as a file and goes line by line)\
>      a pathname           (goes line by line)\
>      a hash-table\
>      a vector\
>      a list\
>      a string             (that goes character by character)\
>      or a stream          (that goes line by line)\
>   It is anaphoric and introduces\
>      index!               (which is a zero indexed counter of which element we are on)\
>      key!                 (the key of the current hash-table entry [only for hash-tables and alists])\
>      value!               (the value of the current element)\
>      this-pass!           (a block that returning from immediately moves to the next iteration)\
>      this-loop!           (a block that returning from exits the loop)\
>   For convenience, (continue!) and (break!) will execute (return-from this-pass!)\
>   and (return-from this-loop!), respectively\
>   If it's a filename, the external format is *pluto-external-format* (:UTF-8 by default)\
>   Oh, it'll die gracefully if Control-C is used during the loops execution.\
>   And, finally, for extra performance, you can call it's subordinate functions directly.\
>   They are... for-each/line, for-each/list, for-each/hash, for-each/vector,\
>   for-each/stream, and for-each/alist\

```{.commonlisp}
(FOR-EACH/LIST '(A B C)
  (FORMAT T "~A -> ~A;" INDEX! VALUE!))
```

<small><pre>>> "1 -> A;2 -> B;3 -> C;"</pre></small>



```{.commonlisp}
(FOR-EACH/LIST '(A B C D E)
  (IF (> INDEX! 2)
      (BREAK!))
  (FORMAT T "~A;" VALUE!))
```

<small><pre>>> "A;B;"</pre></small>



```{.commonlisp}
(FOR-EACH/LIST '(A B C D E)
  (IF (= INDEX! 3)
      (CONTINUE!))
  (FORMAT T "~A;" VALUE!))
```

<small><pre>>> "A;B;D;E;"</pre></small>


If the argument to `for-each` is a string and the file exists,
  `for-each/line` is dispatched. Otherwise, it is treated like a
  character vector

```{.commonlisp}
(FOR-EACH/LINE "somebody.txt"
  (WHEN (> INDEX! 2) (BREAK!))
  (FORMAT T "~A -> ~A;" INDEX! VALUE!))
```

<small><pre>>> "1 -> we gotta celebrate diversity;2 -> in the university;"</pre></small>



```{.commonlisp}
(FOR-EACH "not-a-file.txt"
  (FORMAT T "~A;" VALUE!))
```

<small><pre>>> "n;o;t;-;a;-;f;i;l;e;.;t;x;t;"</pre></small>



```{.commonlisp}
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH/HASH TMP
    (FORMAT T "~A -> ~A;" KEY! VALUE!)))
```

<small><pre>>> "GREEN -> veridian;RED -> cadmium;"</pre></small>



```{.commonlisp}
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH TMP
    (FORMAT T "~A -> ~A;" KEY! VALUE!)))
```

<small><pre>>> "GREEN -> veridian;RED -> cadmium;"</pre></small>



#### FOR-EACH/ALIST

> This works like `for-each/hash` (see documentation for `for-each`)\
>   but it has to be called explicitly (as `for-each/alist`) instead\
>   of relying on `for-each`'s 'dispatch' mechanism.\

```{.commonlisp}
(LET ((TMP (LIST (CONS 'RED "cadmium") (CONS 'GREEN "veridian"))))
  (FOR-EACH/ALIST TMP
    (FORMAT T "~A -> ~A;" KEY! VALUE!)))
```

<small><pre>>> "RED -> cadmium;GREEN -> veridian;"</pre></small>



-----

### error handling


#### OR-DIE

> Anaphoric macro that binds ERROR! to the error\
>    Runs FORM and returns its value; if it signals an error,\
>    ERRFUN (#'die by default) gets funcalled with MESSAGE —\
>    which can reference ERROR!\
>    e.g. (or-die (risky-thing) (fn "no dice: ~A" error!))\
>         (or-die (risky-thing) "hmm" :errfun #'advise)\

```{.commonlisp}
(OR-DIE (/ 3 1) "this message never appears")
```

<small><pre>=> 3</pre></small>



```{.commonlisp}
(OR-DIE (/ 3 0) (FN "the error was: ~A" (TYPE-OF ERROR!)) :ERRFUN #'ADVISE)
```

<small><pre>=> NIL</pre></small>



#### OR-DO

> anaphoric macro that binds ERROR! to the error.\
>    If the body fails, the form ORTHIS gets run.\

```{.commonlisp}
(OR-DO :FALLBACK
  (/ 3 1))
```

<small><pre>=> 3</pre></small>



```{.commonlisp}
(OR-DO :FALLBACK
  (/ 3 0))
```

<small><pre>=> :FALLBACK</pre></small>


Note: the reader macros below necessarily render already-expanded



#### #?

reader macro: `#?<form>` wraps `<form>` in `ignore-errors`
```{.commonlisp}
(IGNORE-ERRORS (/ 3 0))
```

<small><pre>=> NIL</pre></small>



#### ?

reader macro: `? <form> <fallback>` evaluates to `<form>` if it's non-nil, else `<fallback>`
```{.commonlisp}
(LET ((#:G558 (GETHASH :MISSING (MAKE-HASH-TABLE))))
  (IF #:G558
      #:G558
      42))
```

<small><pre>=> 42</pre></small>



-----

### universal indexing operator syntax

`{ thing index }` works on lists, alists, vectors, hash-tables, structs, and CLOS objects — and is setf-able. Successive indexes burrow into nested structures. (The docs below show the `{ }` reader macro already expanded into `PLUTO-GET`)



#### {}

`{ thing index }` — the universal indexing operator
```{.commonlisp}
(PLUTO::PLUTO-GET `(1 2 3) 1)
```

<small><pre>=> 2</pre></small>



```{.commonlisp}
(LET ((TMP `((:A . 1) (:B . 2))))
  (PLUTO::PLUTO-GET TMP :B))
```

<small><pre>=> 2</pre></small>



```{.commonlisp}
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH :RED TMP) "cadmium")
  (PLUTO::PLUTO-GET TMP :RED))
```

<small><pre>=> "cadmium"</pre></small>



```{.commonlisp}
(PLUTO::PLUTO-GET #(#(1 2) #(3 4)) 1 0)
```

<small><pre>=> 3</pre></small>



```{.commonlisp}
(LET ((TMP (VECTOR 1 2 3)))
  (SETF (PLUTO::PLUTO-GET TMP 1) 99)
  TMP)
```

<small><pre>=> #(1 99 3)</pre></small>



-----

### shell and zsh


#### ZSH

> Runs command `acommand` through the shell specified by the global *pluto-shell*\
>    `dry-run` just prints the command (default nil)\
>    `err-fun` takes a function that takes an error code and the STDERR output\
>    `echo` will print the command before running it\
>    `enc` takes a format (default is *pluto-external-format* [which is :UTF-8 by default])\
>    `in` t is inherited STDIN. nil is /dev/null. (default t)\
>    `return-string` t returns the output string. nil inherits stdout and stderr (default t)\
>    `split` will separate the stdout by newlines and return a list (default: nil)\
>    `interactive` will use the '-i' option to make the shell interactive (default: nil)\

```{.commonlisp}
(ZSH "echo hello")
```

<small><pre>=> "hello"</pre></small>



```{.commonlisp}
(MULTIPLE-VALUE-BIND (OUT ERR CODE)
    (ZSH "echo out; echo err >&2")
  (LIST OUT ERR CODE))
```

<small><pre>=> ("out" "err" 0)</pre></small>



```{.commonlisp}
(ZSH "seq 3" :SPLIT T)
```

<small><pre>=> ("1" "2" "3")</pre></small>



```{.commonlisp}
(ZSH "echo hi" :DRY-RUN T)
```

<small><pre>>> "$ echo hi
"</pre></small>



#### Q/SH

> Quotes THING (which gets princ-ed) so that it's safe to\
>    interpolate into a shell command: wraps it in single quotes\
>    and escapes any embedded single quotes. One rule; covers\
>    spaces, globs, `$`, backticks, newlines, etc.\
>    e.g. (sh (fn "cat ~A" (q/sh afilename)))\

```{.commonlisp}
(SH (FN "echo -n ~A" (Q/SH "it's got $pecial \"chars\" & `stuff`")))
```

<small><pre>=> "it's got $pecial \"chars\" & `stuff`"</pre></small>



#### Q/FMT

> Quotes THING (which gets princ-ed) so that it's safe to\
>    embed in a format control string: escapes `~` as `~~`\
>    e.g. (fn (str+ "downloading " (q/fmt afilename) ": ~A%") pct)\

```{.commonlisp}
(FN (Q/FMT "100% done ~ okay"))
```

<small><pre>=> "100% done ~ okay"</pre></small>



-----

### file/filename/directory operations


#### BASENAME

Returns the filename portion of a path
```{.commonlisp}
(BASENAME "/foo/bar/baz.txt")
```

<small><pre>=> "baz.txt"</pre></small>



#### CHANGE-EXTENSION

Returns a pathname with the file extension changed
```{.commonlisp}
(CHANGE-EXTENSION "/foo/bar/baz.txt" "md")
```

<small><pre>=> #P"/foo/bar/baz.md"</pre></small>



#### SIZE-FOR-HUMANS

Formats a byte count for human consumption
```{.commonlisp}
(SIZE-FOR-HUMANS (* 3 (EXPT 2 20)))
```

<small><pre>=> "3M"</pre></small>



```{.commonlisp}
(SIZE-FOR-HUMANS (EXPT 2 31))
```

<small><pre>=> "2.0G"</pre></small>



#### FILE-EXISTS-P

FILE-EXISTS-P and DIRECTORY-EXISTS-P return truthy (the truename) or NIL
```{.commonlisp}
(LIST
 (IF (FILE-EXISTS-P "somebody.txt")
     T
     NIL)
 (IF (DIRECTORY-EXISTS-P "wayward files")
     T
     NIL)
 (IF (FILE-EXISTS-P "no-such-file.txt")
     T
     NIL))
```

<small><pre>=> (T T NIL)</pre></small>



#### PATHNAME->NATIVE

> Converts pathname (or CL namestring) APATH into the exact string\
>    the operating system knows it by — no CL namestring escaping.\
>    This is the string to hand to C functions (through CFFI), etc.\
>    (SBCL backslash-escapes glob characters [* ? \[] in namestrings;\
>    the native namestring is the raw on-disk name. ECL namestrings\
>    are already native — though note that ECL represents filenames\
>    containing glob characters as wild pathnames, which PROBE-FILE\
>    et al. won't accept, the string this returns is still correct)\

```{.commonlisp}
(PATHNAME->NATIVE #P"/foo/bar baz.txt")
```

<small><pre>=> "/foo/bar baz.txt"</pre></small>



-----

### temporary charon tests


#### PARSE-FLOAT

> Similar to PARSE-INTEGER, but parses a floating point value and\
>   returns the value as the specified TYPE (by default\
>   *READ-DEFAULT-FLOAT-FORMAT*). The DECIMAL-CHARACTER (by default #.)\
>   specifies the separator between the integer and decimal parts, and\
>   the EXPONENT-CHARACTER (by default #e, case insensitive) specifies\
>   the character before the exponent. Note that the exponent is only\
>   parsed if RADIX is 10.\

```{.commonlisp}
(PARSE-FLOAT "5.4")
```

<small><pre>=> 5.4</pre></small>



#### Q/RE

> Quote, i.e. prefix with #\\, all non-word characters in STRING.\

```{.commonlisp}
(LIST (STR-DETECT "price: 3x50" "3.50")
      (STR-DETECT "price: 3x50" (Q/RE "3.50")))
```

<small><pre>=> (T NIL)</pre></small>



#### Q/URI

NIL
```{.commonlisp}
(Q/URI "café & bar?q=1")
```

<small><pre>=> "caf%C3%A9%20%26%20bar%3Fq%3D1"</pre></small>



-----

### temporary styx tests


#### STAT-FILESIZE

Size of a file (via `stat`/`lstat`) without opening it
```{.commonlisp}
(STAT-FILESIZE "somebody.txt")
```

<small><pre>=> 3497</pre></small>



#### IS-SYMLINK-P

Is it a symlink? (something portable CL cannot ask)
```{.commonlisp}
(PROGN
 (SH "ln -sf somebody.txt tmp-demo-link.txt")
 (LET ((RES
        (LIST (IS-SYMLINK-P "tmp-demo-link.txt")
              (IS-SYMLINK-P "somebody.txt"))))
   (SH "rm tmp-demo-link.txt")
   RES))
```

<small><pre>=> (T NIL)</pre></small>



#### MD5/STRING

Hashes (via OpenSSL) — also sha256/sha512/ripemd160, also /file variants
```{.commonlisp}
(MD5/STRING "hello")
```

<small><pre>=> "5d41402abc4b2a76b9719d911017c592"</pre></small>



#### SHA256/FILE

File hashing streams in 64KB chunks (never slurps the whole file)
```{.commonlisp}
(SHA256/FILE "somebody.txt")
```

<small><pre>=> "d10edb841b13927faf6dc9032bb9422ab19cc5f9096208c3f67de1da615589a0"</pre></small>



#### WITH-LOCK-FILE

Runs body while flock-holding path (e.g. so a cron script won't run twice); `:wait nil` skips the body instead of blocking
```{.commonlisp}
(LET ((LOCKF "tmp-demo.lock"))
  (LET ((RES
         (WITH-LOCK-FILE (LOCKF)
           (LIST :RAN (ACQUIRE-LOCK-FILE LOCKF :WAIT NIL)))))
    (LET ((HANDLE (ACQUIRE-LOCK-FILE LOCKF :WAIT NIL)))
      (WHEN HANDLE
        (SETQ RES (APPEND RES (LIST :FREE-AGAIN (RELEASE-LOCK-FILE HANDLE))))))
    (DELETE-FILE LOCKF)
    RES))
```

<small><pre>=> (:RAN NIL :FREE-AGAIN T)</pre></small>


