
(ql:quickload :pluto)
(use-package :pluto)

(load "def-test-doc.lisp")

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

(def-test/doc-test '-<>
  `(markdown-able (test-able returns) (bench-able 5))
  'function
  (= test-return-value! 2)
  (-<> "4"
       (parse-integer <>)
       (sqrt <>)))

(def-test/doc-test 'interpose
  `((test-able returns) markdown-able)
  (documentation 'interpose 'function)
  (equal test-return-value! `(a sep b sep c))
  (interpose 'sep `(a b c)))

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

; not markdown able or test able (as a test)
(def-test/doc-test 'file-size
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "17k")
  (file-size "interior-of-a-heart.txt"))

(def-test/doc-test 'file-size
  `(markdown-able (test-able returns))
  nil
  (= test-return-value! 14433)
  (file-size "interior-of-a-heart.txt" :just-bytes t))

; --------------------------------------------------------------- ;

(def-test/doc-section "for-each and friends")

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! (fn "1 -> A~%2 -> B~%3 -> C~%"))
  (for-each/list '(a b c)
    (format t "~A -> ~A~%" index! value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  "test doc"
  (string= test-stdout! (fn "A~%B~%"))
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A~%" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  "test doc"
  (string= test-stdout! (fn "A~%B~%D~%E~%"))
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A~%" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  "for-each/line"
  (string= test-stdout! (fn "1 -> we gotta celebrate diversity~%2 -> in the university~%"))
  (for-each "somebody.txt"
    (format t "~A -> ~A~%" index! value!)))

; TODO: warning about undefined variable: PLUTO:KEY!
; that doesn't happen when using for-each/hash
(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  "for-each/hash"
  (or (string= test-stdout! (fn "GREEN -> veridian~%RED -> cadmium~%"))
      (string= test-stdout! (fn "RED -> cadmium~%GREEN -> veridian~%")))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each tmp
      (format t "~A -> ~A~%" key! value!))))


; --------------------------------------------------------------- ;


(end-test/doc)

(when (run-tests)
  (with-a-file "pluto-results.md" :w
    (render-markdown stream!)))

