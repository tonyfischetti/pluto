---
title: Pluto documentation
...

-----

### about

a common lisp package that's out there



-----

### pluto parameters

A few special variables steer pluto. Rebind them (or `setq` them at the top of a script) to change the defaults



#### \*PLUTO-EXTERNAL-FORMAT\*

> The external format (character encoding) pluto reads and writes\
>    text with: slurp, slurp-lines, with-a-file, for-each/line, and\
>    the default for `sh`'s `enc`. :UTF-8 unless you say otherwise\

```{.commonlisp}
*PLUTO-EXTERNAL-FORMAT*
```

<small><pre>=> :UTF-8</pre></small>



#### \*PLUTO-CURLY-TEST\*

> The equality test `{ }` uses to find a key in an alist\
>    (default #'equal). Bind it to #'string-equal, say, for\
>    case-insensitive string keys\

```{.commonlisp}
(LET ((*PLUTO-CURLY-TEST* #'STRING-EQUAL))
  (PLUTO::PLUTO-GET `(("a" . 1) ("b" . 2)) "B"))
```

<small><pre>=> 2</pre></small>



#### \*PLUTO-SHELL\*

> The shell `sh` (and `zsh`) run commands through. Resolved when\
>    pluto loads: the PLUTO_SHELL environment variable if it's set,\
>    else the first zsh found in the usual places, else /bin/sh\

```{.commonlisp}
(AND (STRINGP *PLUTO-SHELL*) (PROBE-FILE *PLUTO-SHELL*) T)
```

<small><pre>=> T</pre></small>



#### \*UNIX-EPOCH-DIFFERENCE\*

> The seconds between the common lisp epoch (1900-01-01) and the\
>    unix one (1970-01-01); what universal->unix-time subtracts\

```{.commonlisp}
*UNIX-EPOCH-DIFFERENCE*
```

<small><pre>=> 2208988800</pre></small>



#### \*WHITESPACES\*

> The characters that count as whitespace: a character bag for\
>    string-trim and friends (charon's str-trim uses it)\

```{.commonlisp}
(STRING-TRIM *WHITESPACES* (FN "~C hi ~C~%" #\Tab #\Return))
```

<small><pre>=> "hi"</pre></small>



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

The color functions wrap a `format` control string in a bold ANSI escape and a reset. The escapes themselves are the `+...+` constants (`+red-bold+`, `+reset-terminal-color+`, and the cursor movements `+ansi-escape-up+`, `+ansi-escape-left-all+`, `+ansi-escape-left-one+` that the progress bar leans on). The examples compare against the raw escapes, since the strings themselves don't render here



#### RED

> Returns THESTRING wrapped in bold red (and a reset),\
>    so it prints red on an ANSI terminal. THESTRING is a format\
>    control string and THINGS are its arguments:\
>    e.g. (red "~A of ~A" 1 2)\

```{.commonlisp}
(STRING= (RED "hi") (FN "~C[31;1mhi~C[0m" #\Esc #\Esc))
```

<small><pre>=> T</pre></small>



#### YELLOW

> Returns THESTRING wrapped in bold yellow (and a reset),\
>    so it prints yellow on an ANSI terminal. THESTRING is a format\
>    control string and THINGS are its arguments:\
>    e.g. (yellow "~A of ~A" 1 2)\

```{.commonlisp}
(STRING= (YELLOW "~A of ~A" 1 2) (FN "~C[33;1m1 of 2~C[0m" #\Esc #\Esc))
```

<small><pre>=> T</pre></small>



#### MAKE-ANSI-ESCAPE

> Builds an ANSI escape sequence (ESC[<anum>;<decoration>m) for\
>    the terminal: ANUM is the SGR code (31 is red, 90 grey, 0 is\
>    reset) and DECORATION is bold (the default), underline, or\
>    reversed -- as a symbol or keyword from any package -- or nil\
>    for none. The +...-bold+ constants are made with it\

```{.commonlisp}
(STRING= (MAKE-ANSI-ESCAPE 4 'UNDERLINE) (FN "~C[4;4m" #\Esc))
```

<small><pre>=> T</pre></small>



#### +RESET-TERMINAL-COLOR+

> The escape that turns colors and decorations back off (ESC[0m)\

```{.commonlisp}
(STRING= +RESET-TERMINAL-COLOR+ (FN "~C[0m" #\Esc))
```

<small><pre>=> T</pre></small>



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

> Returns STR repeated TIMES times, as one string\
>    e.g. (repeat-string "ab" 3)  =>  "ababab"\

```{.commonlisp}
(REPEAT-STRING "ab" 3)
```

<small><pre>=> "ababab"</pre></small>



#### STR-PAD

> Pads THING (which gets princ-ed) with PAD until it's WIDTH\
>    characters wide. It never truncates: if THING is already\
>    WIDTH (or wider) it comes back as is\
>    `side` is where the padding goes: :left (default), :right,\
>           or :both (centered; an odd leftover goes on the right)\
>    `pad` is the character to pad with (default is a space);\
>          a one-character string works, too\
>    e.g. (str-pad 2 2 :pad "0")  =>  "02"\

```{.commonlisp}
(STR-PAD "2" 2 :PAD "0")
```

<small><pre>=> "02"</pre></small>



```{.commonlisp}
(FN "~A-~A-~A" 2026 (STR-PAD 11 2 :PAD "0") (STR-PAD 2 2 :PAD "0"))
```

<small><pre>=> "2026-11-02"</pre></small>



```{.commonlisp}
(STR-PAD "name" 8 :SIDE :RIGHT)
```

<small><pre>=> "name    "</pre></small>



```{.commonlisp}
(STR-PAD "name" 9 :SIDE :BOTH :PAD #\-)
```

<small><pre>=> "--name---"</pre></small>



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



#### WITH-TEMP-FILE

> Anaphoric macro: runs BODY with `tempfile!` bound to the path of\
>    a fresh temporary file (from mktemp) and deletes the file\
>    afterwards, however BODY exits. Only defined when :coreutils is\
>    in *features* (the rc files put it there)\

```{.commonlisp}
(WITH-TEMP-FILE
  (BARF TEMPFILE! "hello")
  (SLURP TEMPFILE!))
```

<small><pre>=> "hello"</pre></small>



#### CAPTURE-ALL-OUTPUTS

> Runs BODY with *standard-output* and *error-output* captured and\
>    returns three values: what BODY returned (its first value), what\
>    it printed to stdout, and what it printed to stderr\
>    e.g. (capture-all-outputs (ft "hi") 42)  =>  42, "hi", ""\

```{.commonlisp}
(MULTIPLE-VALUE-LIST
 (CAPTURE-ALL-OUTPUTS
   (FT "to stdout")
   (FORMAT *ERROR-OUTPUT* "to stderr")
   42))
```

<small><pre>=> (42 "to stdout" "to stderr")</pre></small>



#### DISPLAY-TABLE

> Prints a plain-text table to stdout: HEADER is the list of\
>    column titles, DATA the list of rows (each a list with one\
>    element per column), and WIDTH the width of every column. The\
>    cells are princ-ed and left-aligned\

```{.commonlisp}
(DISPLAY-TABLE '("name" "n") '(("ab" 1) ("cd" 2)) 6)
```

<small><pre>>> "|------|------|
| name | n    |
|------|------|
| ab   | 1    |
| cd   | 2    |
|------|------|
"</pre></small>



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



-----

### queries


#### Y-OR-N-DEF

> Asks PROMPT on *query-io* and returns T for y, NIL for n. With\
>    DEFAULT (#\y or #\n) the prompt shows [Y/n] or [y/N] and a bare\
>    Enter picks the default; without one it's plain y-or-n-p\

```{.commonlisp}
(LET ((*QUERY-IO*
       (MAKE-TWO-WAY-STREAM (MAKE-STRING-INPUT-STREAM (FN "~%"))
                            (MAKE-STRING-OUTPUT-STREAM))))
  (Y-OR-N-DEF "proceed?" :DEFAULT #\y))
```

<small><pre>=> T</pre></small>



-----

### other abbreviations and shortcuts


#### Λ

> lambda, in one character: (λ (x) (* x x))\

```{.commonlisp}
(MAPCAR
 (Λ
   (X)
   (* X X))
 `(1 2 3))
```

<small><pre>=> (1 4 9)</pre></small>



-----

### what's the deal with time?

Universal time is common lisp's (seconds since 1900), unix time is everyone else's (since 1970). The pretty-printers read the clock in the local time zone, so the examples build their instants with `encode-universal-time` in local time, too



#### UNIVERSAL->UNIX-TIME

> Converts universal (common lisp time from `(get-universal-time)`\
>    to UNIX time\

```{.commonlisp}
(UNIVERSAL->UNIX-TIME (ENCODE-UNIVERSAL-TIME 0 0 0 1 1 1970 0))
```

<small><pre>=> 0</pre></small>



#### UNIX->UNIVERSAL-TIME

> Converts UNIX time to universal (common lisp time from\
>    `(get-universal-time)`\

```{.commonlisp}
(UNIX->UNIVERSAL-TIME 0)
```

<small><pre>=> 2208988800</pre></small>



#### MAKE-PRETTY-TIME

> Makes a nicely formatted (YYYY-MM-DD HH:MM:SS) from a UNIX time\
>    `just-date` will return just the pretty date\
>    `just-time` will return just the pretty time\
>    `time-sep`  will use the supplied string to separate the hours\
>    minutes and seconds (default ':')\
>    `dt-sep` will use the supplied string to separate the date from the time\
>    (default ' ')\

```{.commonlisp}
(MAKE-PRETTY-TIME (UNIVERSAL->UNIX-TIME (ENCODE-UNIVERSAL-TIME 5 4 3 2 1 2026)))
```

<small><pre>=> "2026-01-02 03:04:05"</pre></small>



```{.commonlisp}
(LET ((THEN (UNIVERSAL->UNIX-TIME (ENCODE-UNIVERSAL-TIME 5 4 3 2 1 2026))))
  (LIST (MAKE-PRETTY-TIME THEN :JUST-DATE T)
        (MAKE-PRETTY-TIME THEN :JUST-TIME T)
        (MAKE-PRETTY-TIME THEN :TIME-SEP "." :DT-SEP "T")))
```

<small><pre>=> ("2026-01-02" "03:04:05" "2026-01-02T03.04.05")</pre></small>



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
>      index!               (which is a counter of which element we are on, starting at 1)\
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



#### PROGRESS

> Prints `INDEX of LIMIT..... [percent%]` (in yellow) to WHERE\
>    (default *error-output*), but only when INDEX is a multiple of\
>    INTERVAL (default 1: every time). NEWLINE-P (default t) ends the\
>    line; nil leaves the cursor on it, for with-loading's spinner\
>    e.g. (for-each things (progress index! (length things) :interval 10) ...)\

```{.commonlisp}
(LET ((OUT (WITH-OUTPUT-TO-STRING (S) (PROGRESS 3 4 :WHERE S))))
  (AND (SEARCH "3 of 4..... [75.00%]" OUT) T))
```

<small><pre>=> T</pre></small>



#### WITH-INTERACTIVE-INTERRUPT-HANDLER

> Runs BODY; if Control-C arrives meanwhile, dies (exit status 1)\
>    printing THE-MESSAGE instead of landing in the debugger. The\
>    for-each family wraps its loops in it. THE-MESSAGE is printed as\
>    data (die's rule), so build it with `fn` if it needs directives\

```{.commonlisp}
(WITH-INTERACTIVE-INTERRUPT-HANDLER "interrupted!"
  42)
```

<small><pre>=> 42</pre></small>



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

> Anaphoric macro that binds ERROR! to the error\
>    Runs FORM and returns its value; if it signals an error,\
>    FALLBACK gets run (and returned) instead\
>    e.g. (or-do (risky-thing) (advise (fn "eh: ~A" error!)))\

```{.commonlisp}
(OR-DO (/ 3 1) :FALLBACK)
```

<small><pre>=> 3</pre></small>



```{.commonlisp}
(OR-DO (/ 3 0) :FALLBACK)
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
(LET ((#:G635 (GETHASH :MISSING (MAKE-HASH-TABLE))))
  (IF #:G635
      #:G635
      42))
```

<small><pre>=> 42</pre></small>



#### Ø

reader macro: `Ø<form>` errors if `<form>` is null, else passes it through
```{.commonlisp}
(LET ((X 42))
  (AIF X IT! (ERROR "its null")))
```

<small><pre>=> 42</pre></small>



-----

### universal indexing operator syntax

`{ thing index }` works on lists, alists, vectors, hash-tables, structs, and CLOS objects — and is setf-able. Successive indexes burrow into nested structures. On a list, an integer index is always a position; any other index is an alist key (so an alist keyed by integers needs a plain `assoc`). (The docs below show the `{ }` reader macro already expanded into `PLUTO-GET`)



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
(PLUTO::PLUTO-GET `((1 2) (3 4)) 1 0)
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


(`sh-simple` and `zsh-simple` are the bare run-program underneath: the command runs, nothing is captured, nothing is signalled)



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

### system

`hostname` and `sys/info` answer for the machine they run on, so only `get-envvar` shows its output here



#### GET-ENVVAR

> The value of the environment variable NAME, or DEFAULT (nil\
>    unless given) when it isn't set\

```{.commonlisp}
(GET-ENVVAR "SURELY_THIS_ENVVAR_DOES_NOT_EXIST" "the-default")
```

<small><pre>=> "the-default"</pre></small>



-----

### command-line arguments


#### PROGRAM/SCRIPT-NAME

> The name of the running script, without its directory: the file\
>    being loaded, or failing that the shell's $_\

```{.commonlisp}
(PROGRAM/SCRIPT-NAME)
```

<small><pre>=> "test-pluto.lisp"</pre></small>



#### DEF-CLI-ARGS

> Defines a command-line interface for a script. SCRIPT-NAME and\
>    AFTER-NAME make the usage line (`Usage: SCRIPT-NAME AFTER-NAME`),\
>    DESCRIPTION sits under it, and every clause in BODY is\
>      (option SHORT LONG DESCRIPTION FORMS...)\
>    FORMS run when SHORT or LONG turns up. A flag that takes a value\
>    reads it from (cadr args!) and must hand the rest on with\
>    (process-args! (cddr args!)) -- or the shorthand\
>    (assign-next-arg! var), which does both; a bare flag hands on\
>    (cdr args!). Combined short flags (-abc) are split up, and\
>    anything that isn't a flag lands in bare-args!.\
>    It defines, in the script's package:\
>      args!          the arguments still to be processed\
>      bare-args!     the non-flag arguments, in order\
>      +usage-text!+  the usage text, built from the clauses\
>      print-usage!   prints it and exits 0\
>      process-args!  runs the clauses over a list of arguments --\
>                     call (process-args! (cdr (cmdargs))) right after\
>    An error inside a clause dies, printing the usage text\
>    e.g. (def-cli-args "frob" "[-v] -s SEP FILES..." "frobs the files"\
>           (option "-h" "--help"    "print usage" (print-usage!))\
>           (option "-v" "--verbose" "say more"    (setq *verbose* t)\
>                                                  (process-args! (cdr args!)))\
>           (option "-s" "--sep"     "separator"   (assign-next-arg! *sep*)))\
>         (process-args! (cdr (cmdargs)))\

```{.commonlisp}
(LET (SEP VERBOSE)
  (DEF-CLI-ARGS "demo"
      "[-v] -s SEP FILES..."
      "a demonstration"
    (OPTION "-h" "--help" "print usage" (PRINT-USAGE!))
    (OPTION "-v" "--verbose" "say more" (SETQ VERBOSE T)
     (PROCESS-ARGS! (CDR ARGS!)))
    (OPTION "-s" "--sep" "separator" (SETQ SEP (CADR ARGS!))
     (PROCESS-ARGS! (CDDR ARGS!))))
  (PROCESS-ARGS! `("-s" "," "a.txt" "-v" "b.txt"))
  (LIST SEP VERBOSE BARE-ARGS!))
```

<small><pre>=> ("," T ("a.txt" "b.txt"))</pre></small>



#### +USAGE-TEXT!+

NIL
```{.commonlisp}
(AND
 (SEARCH (FN "Usage: demo [-v] -s SEP FILES...~%a demonstration")
         +USAGE-TEXT!+)
 (SEARCH "-s, --sep" +USAGE-TEXT!+) T)
```

<small><pre>=> T</pre></small>



-----

### terminal things / terminal manipulation

The cursor movements print escapes (compared against the `+ansi-escape-...+` constants here, since they don't render); the progress bar is shown with `:one-line nil`, so it doesn't try to redraw over the previous line



#### ANSI-UP-LINE

> Moves the cursor up one line on WHERE (default *error-output*)\
>    by printing +ansi-escape-up+\

```{.commonlisp}
(STRING= (WITH-OUTPUT-TO-STRING (S) (ANSI-UP-LINE S)) +ANSI-ESCAPE-UP+)
```

<small><pre>=> T</pre></small>



#### PROGRESS-BAR

> Draws a progress bar for INDEX out of LIMIT on WHERE (default\
>    *error-output*), in yellow: ◖▬▬▬▭▭▭ ◗  50.00%. It's WIDTH cells\
>    wide (default 60) and only drawn when INDEX is a multiple of\
>    INTERVAL (default 1) -- and always when INDEX reaches LIMIT.\
>    ONE-LINE (default t) redraws over the previous bar (up a line,\
>    clear it, start of line) so the bar stays put; nil prints a\
>    fresh line each time. OUT-OF adds [INDEX/LIMIT] after the\
>    percentage. Made for the inside of a for-each, with index!\

```{.commonlisp}
(LET ((OUT
       (WITH-OUTPUT-TO-STRING (S)
         (PROGRESS-BAR 5 10 :WHERE S :WIDTH 10 :ONE-LINE NIL :OUT-OF T))))
  (AND (SEARCH "◖▬▬▬▬▬▭▭▭▭▭ ◗  50.00%  [5/10]" OUT) T))
```

<small><pre>=> T</pre></small>



-----

### file/filename/directory operations


#### BASENAME

> The file part of APATH (name and extension) without the\
>    directory: (basename "/foo/bar/baz.txt") => "baz.txt". A\
>    directory pathname (trailing slash) has no file part, so ""\

```{.commonlisp}
(BASENAME "/foo/bar/baz.txt")
```

<small><pre>=> "baz.txt"</pre></small>



#### CHANGE-EXTENSION

> APATH with its extension swapped for NEW-EXTENSION, as a\
>    pathname: (change-extension "notes.txt" "md") => #P"notes.md"\
>    (nil takes the extension off)\

```{.commonlisp}
(CHANGE-EXTENSION "/foo/bar/baz.txt" "md")
```

<small><pre>=> #P"/foo/bar/baz.md"</pre></small>



#### SIZE-FOR-HUMANS

> ASIZE (in bytes) as a short human-readable string -- 512, 3K,\
>    2.0G: K and M are rounded to whole numbers, G and beyond get one\
>    decimal. Binary units (1K is 1024)\

```{.commonlisp}
(SIZE-FOR-HUMANS (* 3 (EXPT 2 20)))
```

<small><pre>=> "3M"</pre></small>



```{.commonlisp}
(SIZE-FOR-HUMANS (EXPT 2 31))
```

<small><pre>=> "2.0G"</pre></small>



#### FILE-EXISTS-P

> Does PATHSPEC name an existing file -- a file, not a directory?\
>    T or NIL (directory-exists-p and file-or-directory-exists-p are\
>    the companions; those two return the truename)\

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



#### FILE-SIZE

> The size of the file AFILENAME in bytes (file-length on a byte\
>    stream), or as a human-readable string (size-for-humans) with\
>    :human t\

```{.commonlisp}
(LIST (FILE-SIZE "somebody.txt") (FILE-SIZE "somebody.txt" :HUMAN T))
```

<small><pre>=> (3497 "3K")</pre></small>



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



#### -PATH

> PATHONE relative to the directory PATHTWO, as a pathname:\
>    (-path "/a/b/c.txt" "/a/") => #P"b/c.txt". Both must exist, and\
>    PATHONE goes through probe-file, so symlinks are resolved first\

```{.commonlisp}
(-PATH "somebody.txt" "./")
```

<small><pre>=> #P"somebody.txt"</pre></small>



#### ABSOLUTE->RELATIVE

> The absolute APATH made relative to ROOT (default: the current\
>    directory), as a pathname; errors if APATH isn't absolute. Both\
>    must exist (see -path)\

```{.commonlisp}
(ABSOLUTE->RELATIVE (PROBE-FILE "somebody.txt"))
```

<small><pre>=> #P"somebody.txt"</pre></small>



-----

### readtable containment


#### CALL-WITH-PLUTO-READTABLE

> Calls THUNK with *readtable* bound to *pluto-readtable*. It's the\
>    :around-compile hook in charon.asd and styx.asd: their files read\
>    pluto's syntax while the third-party dependencies compile under a\
>    standard readtable (a bare `?` token in a dependency is a reader\
>    error under pluto's)\

```{.commonlisp}
(LET ((*READTABLE* (COPY-READTABLE NIL)))
  (CALL-WITH-PLUTO-READTABLE (LAMBDA () (READ-FROM-STRING "•hi•"))))
```

<small><pre>=> "hi"</pre></small>



-----

### charon: the companion with dependencies

Regexes (cl-ppcre), HTTP (dexador), XML and XPath (cxml, plexippus), JSON (yason), HTML (plump, lquery) and friends, wrapped the pluto way. `request/get` and `request/post` need a network, so they aren't exercised here



#### STR-TRIM

> ASTRING without its leading and trailing whitespace (the\
>    characters in *whitespaces*)\

```{.commonlisp}
(STR-TRIM (FN "~C hi ~%" #\Tab))
```

<small><pre>=> "hi"</pre></small>



#### STR-NORMALIZE

> ASTRING in the unicode normalization FORM -- :nfd (the default),\
>    :nfkd, :nfc, or :nfkc (cl-unicode) -- for comparing strings that\
>    differ only in how their accents are composed\

```{.commonlisp}
(LIST (LENGTH "é") (LENGTH (STR-NORMALIZE "é"))
      (LENGTH (STR-NORMALIZE "é" :FORM :NFC)))
```

<small><pre>=> (1 2 1)</pre></small>



#### RE-COMPILE

> Compiles a regex into a scanner (cl-ppcre:create-scanner) for\
>    when one pattern gets matched many times; every str-* function\
>    (and ~m et al.) takes a scanner in place of a pattern string\

```{.commonlisp}
(LET ((DIGITS (RE-COMPILE "\\d+")))
  (STR-DETECT "abc123" DIGITS))
```

<small><pre>=> T</pre></small>


The regex wrappers put the string first (cl-ppcre puts the pattern first), and each has a one-character alias: `~s` split, `~r` replace, `~ra` replace-all, `~m` match, `~f` filter (subset), `~e` extract, `~c` compile. A pattern may be a string or a compiled scanner



#### STR-SPLIT

> Wrapper around cl-ppcre:split with string first\

```{.commonlisp}
(STR-SPLIT "11/2/2026" "/")
```

<small><pre>=> ("11" "2" "2026")</pre></small>



```{.commonlisp}
(STR-SPLIT "k=v=w" "=" :LIMIT 2)
```

<small><pre>=> ("k" "v=w")</pre></small>



#### STR-REPLACE

> Wrapper around cl-ppcre:regex-replace with string first\

```{.commonlisp}
(STR-REPLACE "2026-11-02" "(\\d+)-(\\d+)-(\\d+)" "\\3/\\2/\\1")
```

<small><pre>=> "02/11/2026"</pre></small>



#### STR-REPLACE-ALL

> Wrapper around cl-ppcre:regex-replace-all with string first\

```{.commonlisp}
(STR-REPLACE-ALL "a-b-c" "-" "_")
```

<small><pre>=> "a_b_c"</pre></small>



#### STR-DETECT

> Returns true if `pattern` matches `astr`\
>    Wrapper around cl-ppcre:scan\

```{.commonlisp}
(LIST (STR-DETECT "price: $3.50" "\\$\\d") (STR-DETECT "abc" "^b"))
```

<small><pre>=> (T NIL)</pre></small>



#### STR-SUBSET

> Returns all elements that match pattern\

```{.commonlisp}
(STR-SUBSET `("apple" "banana" "cherry") "an")
```

<small><pre>=> ("banana")</pre></small>



#### STR-EXTRACT

> If one match, it returns the register group as a string.\
>    If more than one match/register-group, returns a list\
>    of register groups. If there is a match but no register\
>    group, it will still return nil\

```{.commonlisp}
(LIST (STR-EXTRACT "v1.2" "v(\\d)")
      (STR-EXTRACT "Nov 2, 2026" "(\\d+), (\\d+)"))
```

<small><pre>=> ("1" ("2" "2026"))</pre></small>



#### STR-SCAN-TO-STRINGS

> Wrapper around cl-ppcre:scan-to-strings with string first\
>    and only returns the important part (the vector of matches)\

```{.commonlisp}
(LIST (STR-SCAN-TO-STRINGS "a1b2" "(\\d)\\w(\\d)")
      (STR-SCAN-TO-STRINGS "abc" "(z)"))
```

<small><pre>=> (#("1" "2") NIL)</pre></small>



#### PARSE-XML

> Parses the XML in ASTRING into a DOM document (cxml, with the DOM\
>    builder) for xpath to query\

```{.commonlisp}
(XPATH (PARSE-XML "<a><b>1</b><b>2</b></a>") "//b" :TEXT T)
```

<small><pre>=> ("1" "2")</pre></small>



#### XPATH

> Evaluates the XPath expression ANXPATH against DOC (a DOM\
>    document or node) and returns the matching nodes as a list, or\
>    nil when nothing matches. TEXT returns their string values\
>    instead of the nodes. ALL nil returns just the first match (its\
>    string value with TEXT, the raw node-set without). COMPILED-P\
>    says ANXPATH was compiled with xpath-compile\
>    e.g. (xpath doc "//item/title" :text t)\

```{.commonlisp}
(LET ((DOC (PARSE-XML "<a><b>1</b><b>2</b></a>")))
  (LIST (XPATH DOC "//b" :ALL NIL :TEXT T) (XPATH DOC "//zz")))
```

<small><pre>=> ("1" NIL)</pre></small>



#### XPATH-COMPILE

> Compiles an XPath expression (xpath:compile-xpath) for repeated\
>    use; hand the result to xpath with :compiled-p t\

```{.commonlisp}
(LET ((QUERY (XPATH-COMPILE "//b")))
  (XPATH (PARSE-XML "<a><b>1</b></a>") QUERY :COMPILED-P T :TEXT T))
```

<small><pre>=> ("1")</pre></small>



#### USE-XML-NAMESPACE

> Makes ANNS (a namespace URI) the default namespace for xpath\
>    queries: unprefixed element names then match elements in that\
>    namespace, which is what a document with an xmlns= needs\

```{.commonlisp}
(LET ((XPATH::*DYNAMIC-NAMESPACES* XPATH::*DYNAMIC-NAMESPACES*))
  (USE-XML-NAMESPACE "urn:x")
  (XPATH (PARSE-XML "<a xmlns=\"urn:x\"><b>1</b></a>") "//b" :TEXT T))
```

<small><pre>=> ("1")</pre></small>



#### PARSE-XML-FILE

> Parses the XML file AFILE into a DOM document (cxml, with the DOM\
>    builder) for xpath to query\

```{.commonlisp}
(WITH-TEMP-FILE
  (BARF TEMPFILE! "<r><x>ok</x></r>")
  (XPATH (PARSE-XML-FILE TEMPFILE!) "//x" :ALL NIL :TEXT T))
```

<small><pre>=> "ok"</pre></small>



#### HASH-TABLE->JSON

> AHASHTABLE (or anything else yason can encode: lists, strings,\
>    numbers...) as a JSON string\

```{.commonlisp}
(HASH-TABLE->JSON (ALIST->HASH-TABLE `(("a" . 1))))
```

<small><pre>=> "{\"a\":1}"</pre></small>



#### PARSE-JSON-FILE

> Parses the JSON file AFILE (yason:parse): objects become hash\
>    tables and arrays become lists. Errors if the file doesn't exist\

```{.commonlisp}
(WITH-TEMP-FILE
  (BARF TEMPFILE! "{\"a\": [1, 2]}")
  (GETHASH "a" (PARSE-JSON-FILE TEMPFILE!)))
```

<small><pre>=> (1 2)</pre></small>



#### MAKE-OCTET-VECTOR

> A fresh vector of N octets -- element type (unsigned-byte 8)\

```{.commonlisp}
(LET ((V (MAKE-OCTET-VECTOR 2)))
  (SETF (AREF V 0) 1
        (AREF V 1) 2)
  (CONCAT-OCTET-VECTOR V #(3)))
```

<small><pre>=> #(1 2 3)</pre></small>



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

> Quotes THING (which gets princ-ed) for a URL: percent-encodes\
>    whatever isn't safe in a path segment or query string\
>    (quri:url-encode, whose keywords :encoding and :space-to-plus\
>    pass through)\
>    e.g. (fn "https://x.org/search?q=~A" (q/uri "café & bar"))\

```{.commonlisp}
(Q/URI "café & bar?q=1")
```

<small><pre>=> "caf%C3%A9%20%26%20bar%3Fq%3D1"</pre></small>



#### ESCAPE-NAMESTRING/SHELL

> AFILENAME (a namestring) backslash-escaped for the shell: spaces,\
>    quotes, globs, $, &, ;, parens and the rest. This is the SBCL\
>    version, which compensates for SBCL namestrings arriving with ?,\
>    * and [ already escaped. (q/sh -- single quotes, one rule -- is\
>    the newer way to quote anything for the shell)\

```{.commonlisp}
(SH (FN "echo -n ~A" (ESCAPE-NAMESTRING/SHELL "/tmp/it's got spaces!.txt")))
```

<small><pre>=> "/tmp/it's got spaces!.txt"</pre></small>



#### PATHNAME->SHELL

> The pathname APATH as a namestring escaped for the shell\
>    (escape-namestring/shell)\

```{.commonlisp}
(SH (FN "echo -n ~A" (PATHNAME->SHELL #P"/tmp/it's got spaces!.txt")))
```

<small><pre>=> "/tmp/it's got spaces!.txt"</pre></small>



-----

### styx: the syscall tier

What pluto reaches for below portable lisp, through libstyx (C, in `libstyx/`): stat, symlinks, the terminal, file locks, and OpenSSL hashing. `sd-journal` (systemd logging) is linux-only, so it isn't exercised here



#### \*SD-LOG-PRIORITY\*

> The syslog priority sd-journal logs with unless told otherwise:\
>    4 (warning). 0 is emergency, 3 error, 6 informational, 7 debug\

```{.commonlisp}
*SD-LOG-PRIORITY*
```

<small><pre>=> 4</pre></small>



#### STAT-FILESIZE

> The size of AFILENAME in bytes, from stat(2), without opening\
>    the file. With FOLLOW-SYMLINKS nil it's lstat: the size of the\
>    link itself. Errors if it can't be stat-ed\

```{.commonlisp}
(STAT-FILESIZE "somebody.txt")
```

<small><pre>=> 3497</pre></small>



#### IS-SYMLINK-P

> Is AFILENAME a symbolic link? (lstat(2) -- something portable\
>    lisp can't ask.) Errors if it can't be lstat-ed. Give it the\
>    link's name without a trailing slash: with one, the system looks\
>    through the link at the directory\

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

> The MD5 digest of ASTRING as a hex string (OpenSSL). The /file\
>    variants hash a file, streamed in 64KB chunks; the /hexstring\
>    variants hash the bytes a hex string stands for\

```{.commonlisp}
(MD5/STRING "hello")
```

<small><pre>=> "5d41402abc4b2a76b9719d911017c592"</pre></small>



#### MD5/FILE

> The MD5 digest of the file AFILENAME as a hex string, streamed\
>    in 64KB chunks (OpenSSL). Errors if the file can't be read\

```{.commonlisp}
(MD5/FILE "somebody.txt")
```

<small><pre>=> "03d1f29193f99d260ee99b6cac7fd3a4"</pre></small>



#### SHA256/STRING

> The SHA-256 digest of ASTRING as a hex string (OpenSSL)\

```{.commonlisp}
(SHA256/STRING "hello")
```

<small><pre>=> "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"</pre></small>



#### SHA256/HEXSTRING

> The SHA-256 digest, as a hex string, of the BYTES the hex string\
>    ASTRING spells out ("00ff" is two bytes) -- a hash of a hash, say.\
>    Errors on an odd length or a non-hex character\

```{.commonlisp}
(SHA256/HEXSTRING "00ff")
```

<small><pre>=> "06eb7d6a69ee19e5fbdf749018d3d2abfa04bcbd1365db312eb86dc7169389b8"</pre></small>



#### SHA256/FILE

> The SHA-256 digest of the file AFILENAME as a hex string,\
>    streamed in 64KB chunks (OpenSSL). Errors if the file can't be\
>    read\

```{.commonlisp}
(SHA256/FILE "somebody.txt")
```

<small><pre>=> "d10edb841b13927faf6dc9032bb9422ab19cc5f9096208c3f67de1da615589a0"</pre></small>



#### SHA512/STRING

> The SHA-512 digest of ASTRING as a hex string (OpenSSL)\

```{.commonlisp}
(SHA512/STRING "hello")
```

<small><pre>=> "9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca72323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043"</pre></small>



#### SHA512/FILE

> The SHA-512 digest of the file AFILENAME as a hex string,\
>    streamed in 64KB chunks (OpenSSL). Errors if the file can't be\
>    read\

```{.commonlisp}
(SHA512/FILE "somebody.txt")
```

<small><pre>=> "55e28f5838f7c60563990384b60acc2f9a778f8dbc4fd8e8ac77585124c766f8c624baac29fed6e9b9235c64878315c678f3c56e41788a585da267b105071acf"</pre></small>



#### RIPEMD160/STRING

> The RIPEMD-160 digest of ASTRING as a hex string (OpenSSL's\
>    legacy provider, which libstyx loads on demand)\

```{.commonlisp}
(RIPEMD160/STRING "hello")
```

<small><pre>=> "108f07b8382412612c048d07d13f814118445acd"</pre></small>



#### RIPEMD160/HEXSTRING

> The RIPEMD-160 digest, as a hex string, of the bytes the hex\
>    string ASTRING spells out (see sha256/hexstring). Errors on an\
>    odd length or a non-hex character\

```{.commonlisp}
(RIPEMD160/HEXSTRING "00ff")
```

<small><pre>=> "742dcba5d24837058e3fa56f1c36f9878484d1c2"</pre></small>



#### WITH-LOCK-FILE

> Runs BODY while holding an flock on PATH (great for cron\
>    scripts that shouldn't run concurrently). If WAIT is nil and\
>    the lock is already held elsewhere, BODY is skipped and nil\
>    is returned\

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


