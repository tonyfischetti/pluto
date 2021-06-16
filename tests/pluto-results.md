# Pluto documentation
_A common lisp package that's out there_

heres the documentation

-----

### some essential utilities/macros


#### -<>

```lisp
(-<> "4" (PARSE-INTEGER <>) (SQRT <>))
```

<pre>=> 2.0</pre>



-----

### other abbreviations and shortcuts


#### GET-SIZE

```lisp
(GET-SIZE "interior-of-a-heart.txt")
```

<pre>=> 17k</pre>


<br/>
```lisp
(GET-SIZE "interior-of-a-heart.txt" :JUST-BYTES T)
```

<pre>=> 14433</pre>



-----

### for-each and friends


#### FOR-EACH

```lisp
(FOR-EACH/LIST '(A B C)
  (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

<pre>>> 1 -> A
2 -> B
3 -> C
</pre>


<br/>
```lisp
(FOR-EACH/LIST '(A B C D E)
  (IF (> INDEX! 2)
      (BREAK!))
  (FORMAT T "~A~%" VALUE!))
```

<pre>>> A
B
</pre>


<br/>
```lisp
(FOR-EACH/LIST '(A B C D E)
  (IF (= INDEX! 3)
      (CONTINUE!))
  (FORMAT T "~A~%" VALUE!))
```

<pre>>> A
B
D
E
</pre>


<br/>
```lisp
(FOR-EACH "somebody.txt"
  (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

<pre>>> 1 -> we gotta celebrate diversity
2 -> in the university
</pre>


<br/>
```lisp
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH TMP
    (FORMAT T "~A -> ~A~%" KEY! VALUE!)))
```

<pre>>> GREEN -> veridian
RED -> cadmium
</pre>


