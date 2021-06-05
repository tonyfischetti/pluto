# Pluto documentation


---
### GET-SIZE
<pre>
Uses `du` to return just the size of the provided file.
   `just-bytes` ensures that the size is only counted in bytes (returns integer) [default nil]
</pre>
<br>


```
(GET-SIZE "interior-of-a-heart.txt")
```

Returns:
```
17k
```


<hr size="1">


```
(GET-SIZE "interior-of-a-heart.txt" :JUST-BYTES T)
```

Returns:
```
14433
```



---
### -<>
<pre>
Threading macro (put <> where the argument should be)
   Stolen from https://github.com/sjl/cl-losh/blob/master/src/control-flow.lisp
</pre>
<br>


```
(-<> "4" (PARSE-INTEGER <>) (SQRT <>))
```

Returns:
```
2.0
```



---
### FOR-EACH
<pre>
A super-duper imperative looping construct.
   It takes either
     a filename string    (to be treated as a file and goes line by line)
     a hash-table
     a vector
     a list
     a string             (that goes character by character)
     or a stream          (that goes line by line)
  It is anaphoric and introduces
     `index!`             (which is a zero indexed counter of which element we are on)
     `key!`               (the key of the current hash-table entry [only for hash-tables and alists])
     `value!`             (the value of the current element)
     `this-pass!`         (a block that returning from immediately moves to the next iteration)
     `this-loop!`         (a block that returning from exits the loop)
  For convenience, `(continue!)` and `(break!)` will execute `(return-from this-pass!)`
  and `(return-from this-loop!)`, respectively
  If it's a filename, the external format is *pluto-external-format* (:UTF-8 by default)
  Oh, it'll die gracefully if Control-C is used during the loops execution.
  And, finally, for extra performance, you can call it's subordinate functions directly.
  They are... for-each/line, for-each/list, for-each/hash, for-each/vector,
  for-each/stream, and for-each/alist
</pre>
<br>

for-each/list
```
(FOR-EACH/LIST '(A B C D E) (IF (> INDEX! 2) (BREAK!)) (FORMAT T "~A~%" VALUE!))
```

Output:
```
A
B

```


<hr size="1">

for-each/list
```
(FOR-EACH '(A B C D E) (IF (= INDEX! 3) (CONTINUE!)) (FORMAT T "~A~%" VALUE!))
```

Output:
```
A
B
D
E

```


<hr size="1">

for-each/line
```
(FOR-EACH "somebody.txt" (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

Output:
```
1 -> we gotta celebrate diversity
2 -> in the university

```


<hr size="1">

for-each/hash
```
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH TMP (FORMAT T "~A -> ~A~%" KEY! VALUE!)))
```

Output:
```
RED -> cadmium
GREEN -> veridian

```


