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



#### SUBSTR

> Efficient substring of STRING from START to END (optional),\
>   where both can be negative, which means counting from the end.\

```{.commonlisp}
(SUBSTR "belle and sebastian" 0 5)
```

<small><pre>=> "belle"</pre></small>



```{.commonlisp}
(SUBSTR "belle and sebastian" 0 -14)
```

<small><pre>=> "belle"</pre></small>



```{.commonlisp}
(SUBSTR "belle and sebastian" 10)
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



#### WITH-TIME

> Anaphoric macro that executes the car of the body and\
>    binds the seconds of execution time to TIME!. Then\
>    all the other forms in the body are executed\

```{.commonlisp}
(WITH-TIME
  (SLEEP 1)
  (FORMAT NIL "time elapsed: ~A" TIME!))
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


#### FILE-SIZE

> Uses `du` to return just the size of the provided file.\
>    `just-bytes` ensures that the size is only counted in bytes (returns integer) [default nil]\

```{.commonlisp}
(FILE-SIZE "interior-of-a-heart.txt")
```

<small><pre>=> "17k"</pre></small>



```{.commonlisp}
(FILE-SIZE "interior-of-a-heart.txt" :JUST-BYTES T)
```

<small><pre>=> 14433</pre></small>



-----

### for-each and friends


#### FOR-EACH

> A super-duper imperative looping construct.\
>    It takes either\
>      a filename string    (to be treated as a file and goes line by line)\
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


