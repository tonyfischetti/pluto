
(load "../pluto.lisp")
(use-package :pluto)

(load "def-test-doc.lisp")

(declaim (optimize (speed 3)))

(start-test/doc :title "Pluto")

; --------------------------------------------------------------- ;

(def-raw-markdown
  (fn "-----~%~%### about~%~%a common lisp package that's out there~%~%"))

; --------------------------------------------------------------- ;

(def-test/doc-section "pluto parameters")

(def-raw-markdown "fill this out")

; --------------------------------------------------------------- ;

(def-test/doc-section "formatting")

(def-test/doc-test 'fn
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hello goodbye")
  (fn "~A ~A" "hello" "goodbye"))

(def-test/doc-test 'ft
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "hello goodbye")
  (ft "~A ~A" "hello" "goodbye"))

; --------------------------------------------------------------- ;

(def-test/doc-section "ansi colors and codes")

(def-raw-markdown "fill this out")

; --------------------------------------------------------------- ;

(def-test/doc-section "string operations")

(def-test/doc-test 'str+
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belleandsebastian")
  (str+ "belle" "and" "sebastian"))

(def-test/doc-test 'str+
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "(1 2 3) go")
  (str+ '(1 2 3) " go"))

(def-test/doc-test 'str-join
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "one;two")
  (str-join ";" '("one" "two")))

(def-test/doc-test 'substr
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belle")
  (substr "belle and sebastian" 0 5))

(def-test/doc-test 'substr
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belle")
  (substr "belle and sebastian" 0 -14))

(def-test/doc-test 'substr
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "sebastian")
  (substr "belle and sebastian" 10))

(def-test/doc-test 'string->char-list
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("b" "e" "l" "l" "e"))
  (string->char-list "belle"))

(def-test/doc-test 'split-string->lines
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! '("this" "that" "and the other"))
  (split-string->lines (format nil "this~%that~%and the other")))


; --------------------------------------------------------------- ;

(def-test/doc-section "some essential utilities/macros")

(def-test/doc-test 'alambda
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! (list 10 9 8 7 6 5 4 3 2 1))
  (funcall (alambda (x) (when (> x 0) (cons x (self! (- x 1))))) 10))

(def-test/doc-test 'flatten
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(A B C D E))
  (flatten `(a b (c d (e)))))

(def-test/doc-test 'take
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `((a b) (c d e f)))
  (multiple-value-bind (one two)
    (take `(a b c d e f) 2)
    (list one two)))

(def-test/doc-test 'group
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `((a b) (c d) (e f)))
  (group `(a b c d e f) 2))


(def-test/doc-test '-<>
  `(markdown-able (test-able returns) (bench-able 5))
  'function
  (= test-return-value! 2)
  (-<> "4"
       (parse-integer <>)
       (sqrt <>)))

(def-test/doc-test 'interpose
  `((test-able returns) markdown-able)
  'function
  (equal test-return-value! `(a sep b sep c))
  (interpose 'sep `(a b c)))

(def-test/doc-test 'with-time
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "time elapsed: 1")
  (with-time
    (sleep 1)
    (format nil "time elapsed: ~A" time!)))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "4 seconds")
  (time-for-humans 4))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "1.11 hours")
  (time-for-humans 4000))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "2.21 days")
  (time-for-humans 191000))

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

(def-test/doc-test 'file-size
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "17k")
  (file-size "interior-of-a-heart.txt"))

(def-test/doc-test 'file-size
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 14433)
  (file-size "interior-of-a-heart.txt" :just-bytes t))

; --------------------------------------------------------------- ;

(def-test/doc-section "for-each and friends")

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "1 -> A;2 -> B;3 -> C;")
  (for-each/list '(a b c)
    (format t "~A -> ~A;" index! value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "1 -> A;2 -> B;3 -> C;")
    (for-each '(a b c)
      (format t "~A -> ~A;" index! value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "A;B;")
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "A;B;")
    (for-each '(a b c d e)
      (if (> index! 2) (break!))
      (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "A;B;D;E;")
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "A;B;D;E;")
    (for-each '(a b c d e)
      (if (= index! 3) (continue!))
      (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `((test-able stdout))
  'function
  (string= test-stdout! "a;b;d;e;")
  (for-each/vector #("a" "b" "c" "d" "e")
    (if (= index! 3) (continue!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "a;b;d;e;")
    (for-each #("a" "b" "c" "d" "e")
      (if (= index! 3) (continue!))
      (format t "~A;" value!)))

(def-raw-markdown
  "If the argument to `for-each` is a string and the file exists,\
  `for-each/line` is dispatched. Otherwise, it is treated like a\
  character vector")

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "1 -> we gotta celebrate diversity;2 -> in the university;")
  (for-each/line "somebody.txt"
    (when (> index! 2) (break!))
    (format t "~A -> ~A;" index! value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
      (string= test-stdout! "1 -> we gotta celebrate diversity;2 -> in the university;")
    (for-each "somebody.txt"
      (when (> index! 2) (break!))
      (format t "~A -> ~A;" index! value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "n;o;t;-;a;-;f;i;l;e;.;t;x;t;")
  (for-each "not-a-file.txt"
    (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (or (string= test-stdout! (fn "GREEN -> veridian;RED -> cadmium;"))
      (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;")))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each/hash tmp
      (format t "~A -> ~A;" key! value!))))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `(markdown-able (test-able stdout))
    'function
    (or (string= test-stdout! (fn "GREEN -> veridian;RED -> cadmium;"))
        (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;")))
    (let ((tmp (make-hash-table)))
      (setf (gethash 'green tmp) "veridian")
      (setf (gethash 'red tmp) "cadmium")
      (for-each tmp
        (format t "~A -> ~A;" key! value!))))

(def-test/doc-test 'for-each/alist
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;"))
  (let ((tmp (list (cons 'red "cadmium")
                   (cons 'green "veridian"))))
    (for-each/alist tmp
      (format t "~A -> ~A;" key! value!))))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

#-clisp
(load "~/quicklisp/setup.lisp")
(ql:quickload :charon :silent t)
(use-package :charon)

(def-test/doc-section "temporary charon tests")

(def-test/doc-test 'parse-float
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 5.4)
  (parse-float "5.4"))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

(ql:quickload :styx :silent t)
(use-package :styx)

(def-test/doc-section "temporary styx tests")

(def-test/doc-test 'stat-filesize
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 14433)
  (stat-filesize "interior-of-a-heart.txt"))


; --------------------------------------------------------------- ;

(end-test/doc)

(if (run-tests)
  (with-a-file "pluto-results.md" :w
    (render-markdown stream!))
  (die "~%at least one test failed"))

