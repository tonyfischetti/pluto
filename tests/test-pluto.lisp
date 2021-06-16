
(ql:quickload :pluto)
(use-package :pluto)

(load "def-test-doc.lisp")

(start-tests
  :title "Pluto"
  :tagline "A common lisp package that's out there"
  :doc "heres the documentation")

; --------------------------------------------------------------- ;

; (def-test/doc-section "some essential utilities/macros")
;
; (def-test/doc-test '-<>
;   `(markdown-able (test-able returns) (bench-able 5))
;   "test doc"
;   (= test-return-value! 2)
;   (-<> "4"
;        (parse-integer <>)
;        (sqrt <>)))
;
; (def-raw-markdown "## this is raw markdown")

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

; not markdown able or test able (as a test)
(def-test/doc-tests 'get-size
  ((markdown-able (test-able returns))
    "test doc"
    (string= test-return-value! "17k")
    (get-size "interior-of-a-heart.txt"))
  ((markdown-able (test-able returns))
   "test doc"
   (= test-return-value! 14433)
   (get-size "interior-of-a-heart.txt" :just-bytes t)))

; --------------------------------------------------------------- ;

; (def-test/doc-section "for-each and friends")
;
; (def-test/doc-test 'for-each
;   `(markdown-able (test-able stdout))
;   "test doc"
;   (string= test-stdout! (fn "1 -> A~%2 -> B~%3 -> C~%"))
;   (for-each/list '(a b c)
;     (format t "~A -> ~A~%" index! value!)))
;
; (def-test/doc-test 'for-each
;   `(markdown-able (test-able stdout))
;   "test doc"
;   (string= test-stdout! (fn "A~%B~%"))
;   (for-each/list '(a b c d e)
;     (if (> index! 2) (break!))
;     (format t "~A~%" value!)))
;
; (def-test/doc-test 'for-each
;   `(markdown-able (test-able stdout))
;   "test doc"
;   (string= test-stdout! (fn "A~%B~%D~%E~%"))
;   (for-each/list '(a b c d e)
;     (if (= index! 3) (continue!))
;     (format t "~A~%" value!)))
;
; (def-test/doc-test 'for-each
;   `(markdown-able (test-able stdout))
;   "for-each/line"
;   (string= test-stdout! (fn "1 -> we gotta celebrate diversity~%2 -> in the university~%"))
;   (for-each "somebody.txt"
;     (format t "~A -> ~A~%" index! value!)))
;
; ; TODO: warning about undefined variable: PLUTO:KEY!
; ; that doesn't happen when using for-each/hash
; (def-test/doc-test 'for-each
;   `(markdown-able (test-able stdout))
;   "for-each/hash"
;   (or (string= test-stdout! (fn "GREEN -> veridian~%RED -> cadmium~%"))
;       (string= test-stdout! (fn "RED -> cadmium~%GREEN -> veridian~%")))
;   (let ((tmp (make-hash-table)))
;     (setf (gethash 'green tmp) "veridian")
;     (setf (gethash 'red tmp) "cadmium")
;     (for-each tmp
;       (format t "~A -> ~A~%" key! value!))))


; --------------------------------------------------------------- ;


(end-tests)

(run-tests)

; (when (run-tests)
;   (with-a-file "pluto-results.md" :w
;     (render-markdown stream!)))

