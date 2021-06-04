
(ql:quickload :pluto)
(use-package :pluto)

(load "deftest.lisp")

(deftest
  'get-size
  "Returns the size as reported by `du -sb`"
  'returns
  (string= return-value! "17k")
  (get-size "interior-of-a-heart.txt"))

(deftest
  'get-size
  "With the :just-bytes parameter, it'll only return the number of bytes as an integer"
  'returns
  (= return-value! 14433)
  (get-size "interior-of-a-heart.txt" :just-bytes t))

; TODO: warning about undefined variable: PLUTO:KEY!
; that doesn't happen when using for-each/hash
; TODO: fails on clisp?
(deftest
  'for-each/hash
  "hi"
  'prints
  (string= output! (fn "GREEN -> veridian~%RED -> cadmium~%"))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each tmp
      (format t "~A -> ~A~%" key! value!))))

(deftest
  'for-each/line
  "hi"
  'prints
  (string= output! (fn "1 -> we gotta celebrate diversity~%2 -> in the university~%"))
  (for-each/line "somebody.txt"
    (format t "~A -> ~A~%" index! value!)))

(deftest
  'for-each/list
  "hi"
  'prints
  (string= output! (fn "A~%B~%"))
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A~%" value!)))

(deftest
  'for-each/list
  "hi"
  'prints
  (string= output! (fn "A~%B~%D~%E~%"))
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A~%" value!)))


(setq *all-results* (mapcar #'run-test (reverse *all-tests*)))

(with-a-file "pluto-results.md" :w
  (format stream! "# Pluto documentation~%~%")
  (for-each *all-results*
    ; (format stream! "~%__~A__:~%~%^_~A_^~%```~%~S~%```~%~A~%-------~%~%"
    (format stream! "~%__~A__:~%~%_~A_~%```~%~S~%```~%~A~%---~%~%"
        {value! 'the-function}
        {value! 'doc}
        (car {value! 'raw-code })
        (if (eq {value! 'the-type } 'returns)
          (fn "~%Returns:~%```~%~A~%```~%" {value! 'return-value })
          (fn "~%Output:~%```~%~A~%```~%" {value! 'output }))
        )))



