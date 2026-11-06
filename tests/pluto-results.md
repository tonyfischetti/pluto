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
(LET ((#:G620 (GETHASH :MISSING (MAKE-HASH-TABLE))))
  (IF #:G620
      #:G620
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


