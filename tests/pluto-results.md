# Pluto documentation


__GET-SIZE__:

_Returns the size as reported by `du -sb`_
```
(GET-SIZE "interior-of-a-heart.txt")
```

Returns:
```
17k
```

---


__GET-SIZE__:

_With the :just-bytes parameter, it'll only return the number of bytes as an integer_
```
(GET-SIZE "interior-of-a-heart.txt" :JUST-BYTES T)
```

Returns:
```
14433
```

---


__FOR-EACH/HASH__:

_hi_
```
(LET ((TMP (MAKE-HASH-TABLE)))
  (SETF (GETHASH 'GREEN TMP) "veridian")
  (SETF (GETHASH 'RED TMP) "cadmium")
  (FOR-EACH TMP
    (FORMAT T "~A -> ~A~%" KEY! VALUE!)))
```

Output:
```
GREEN -> veridian
RED -> cadmium

```

---


__FOR-EACH/LINE__:

_hi_
```
(FOR-EACH/LINE "somebody.txt"
  (FORMAT T "~A -> ~A~%" INDEX! VALUE!))
```

Output:
```
1 -> we gotta celebrate diversity
2 -> in the university

```

---


__FOR-EACH/LIST__:

_hi_
```
(FOR-EACH/LIST '(A B C D E)
  (IF (> INDEX! 2)
      (BREAK!))
  (FORMAT T "~A~%" VALUE!))
```

Output:
```
A
B

```

---


__FOR-EACH/LIST__:

_hi_
```
(FOR-EACH/LIST '(A B C D E)
  (IF (= INDEX! 3)
      (CONTINUE!))
  (FORMAT T "~A~%" VALUE!))
```

Output:
```
A
B
D
E

```

---

