
(ql:quickload :pluto)
(use-package :pluto)

(load "def-test-doc.lisp")


(start-tests
  :title "Pluto"
  :tagline "A common lisp package that's out there"
  :doc "heres the documentation")

; --------------------------------------------------------------- ;

(def-test/doc-section "some essential utilities/macros")

(def-test/doc-test '-<>
  ('doc-able 'run-able 'returns)
  "test doc"
  (= test-return-value! 2)
  (-<> "4"
       (parse-integer <>)
       (sqrt <>)))

; --------------------------------------------------------------- ;

(def-test/doc-section "for-each and friends")

(def-test/doc-test 'for-each
  ('doc-able 'run-able 'stdout)
  "test doc"
  (string= test-stdout! (fn "1 -> A~%2 -> B~%3 -> C~%"))
  (for-each/list '(a b c)
    (format t "~A -> ~A~%" index! value!)))

(def-test/doc-test 'for-each
  ('doc-able 'run-able 'stdout)
  "test doc"
  (string= test-stdout! (fn "A~%B~%"))
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A~%" value!)))

(def-test/doc-test 'for-each
  ('doc-able 'run-able 'stdout)
  "test doc"
  (string= test-stdout! (fn "A~%B~%D~%E~%"))
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A~%" value!)))

(def-test/doc-test 'for-each
  ('doc-able 'run-able 'stdout)
  "for-each/line"
  (string= test-stdout! (fn "1 -> we gotta celebrate diversity~%2 -> in the university~%"))
  (for-each "somebody.txt"
    (format t "~A -> ~A~%" index! value!)))

; TODO: warning about undefined variable: PLUTO:KEY!
; that doesn't happen when using for-each/hash
(def-test/doc-test 'for-each
  ('doc-able 'run-able 'stdout)
  "for-each/hash"
  (or (string= test-stdout! (fn "GREEN -> veridian~%RED -> cadmium~%"))
      (string= test-stdout! (fn "RED -> cadmium~%GREEN -> veridian~%")))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each tmp
      (format t "~A -> ~A~%" key! value!))))

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

(def-test/doc-test 'get-size
  ('doc-able 'run-able 'returns)
  "test doc"
  (string= test-return-value! "17k")
  (get-size "interior-of-a-heart.txt"))

(def-test/doc-test 'get-size
  ('doc-able 'run-able 'returns)
  "test doc"
  (= test-return-value! 14433)
  (get-size "interior-of-a-heart.txt" :just-bytes t))

; --------------------------------------------------------------- ;


(end-tests)

(when (run-tests)
  (with-a-file "pluto-results.md" :w
    (render-markdown stream!)))



; (with-a-file "pluto-results.md" :w
;   (format stream! "# Pluto documentation~%~%")
;   (for-each *all-results*
;     (let ((thefun {value! 'the-function }))
;       (if (gethash thefun /seen-p/)
;         (format stream! •<hr size="1">•)
;         (progn
;           (format stream! "~%---~%### ~A~%" thefun)
;           (format stream!  "<pre>~%~A~%</pre>~%<br>" (documentation thefun 'function))))
;       (setf {/seen-p/ thefun} t)
;       (format stream! •~%~%~A~%```~%~S~%```~%~A~%~%•
;           (if {value! 'doc} {value! 'doc} "")
;           (car {value! 'raw-code })
;           (if (eq {value! 'the-type } 'returns)
;             (fn "~%Returns:~%```~%~A~%```~%" {value! 'return-value })
;             (fn "~%Output:~%```~%~A~%```~%" {value! 'output }))
;           ))))
