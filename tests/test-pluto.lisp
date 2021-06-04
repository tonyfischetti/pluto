
(ql:quickload :pluto)
(use-package :pluto)

(load "deftest.lisp")

(deftest
  'get-size-regular
  "hi"
  (string= ret-val! "17k")
  (get-size "interior-of-a-heart.txt"))

(deftest
  'get-size-just-bytes
  "hi"
  (= ret-val! 14433)
  (get-size "interior-of-a-heart.txt" :just-bytes t))

; TODO: warning about undefined variable: PLUTO:KEY!
; that doesn't happen when using for-each/hash
(deftest
  'for-each/hash
  "hi"
  (string= output! (fn "GREEN -> veridian~%RED -> cadmium~%"))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each tmp
      (format t "~A -> ~A~%" key! value!))))

(deftest
  'for-each/line
  "hi"
  (string= output! (fn "1 -> we gotta celebrate diversity~%2 -> in the university~%"))
  (for-each/line "somebody.txt"
    (format t "~A -> ~A~%" index! value!)))

(deftest
  'for-each/list-with-break
  "hi"
  (string= output! (fn "A~%B~%"))
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A~%" value!)))

(deftest
  'for-each/list-with-continue
  "hi"
  (string= output! (fn "A~%B~%D~%E~%"))
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A~%" value!)))


(mapcar #'run-test (reverse *all-tests*))

