# Pluto documentation
_A common lisp package that's out there_

heres the documentation

-----

# some essential utilities/macros


### -<>

```
(-<> "4" (PARSE-INTEGER <>) (SQRT <>))
```

Returns:
```
2.0
```


-----

# for-each and friends


### FOR-EACH

```
(FOR-EACH/LIST '(A B C)
  (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

Outputs:
```
1 -> A
2 -> B
3 -> C

```


### FOR-EACH

```
(FOR-EACH/LIST '(A B C D E)
  (IF (> INDEX! 2)
      (BREAK!))
  (FORMAT T "~A~%" VALUE!))
```

Outputs:
```
A
B

```


### FOR-EACH

```
(FOR-EACH/LIST '(A B C D E)
  (IF (= INDEX! 3)
      (CONTINUE!))
  (FORMAT T "~A~%" VALUE!))
```

Outputs:
```
A
B
D
E

```


### FOR-EACH

```
(FOR-EACH "somebody.txt"
  (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

Outputs:
```
1 -> we gotta celebrate diversity
2 -> in the university

```


### FOR-EACH

```
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH TMP
    (FORMAT T "~A -> ~A~%" KEY! VALUE!)))
```

Outputs:
```
GREEN -> veridian
RED -> cadmium

```


-----

# other abbreviations and shortcuts


### FILE-SIZE

```
(FILE-SIZE "interior-of-a-heart.txt")
```

Returns:
```
17k
```


### FILE-SIZE

```
(FILE-SIZE "interior-of-a-heart.txt" :JUST-BYTES T)
```

Returns:
```
14433
```

