


(defparameter *all-tests* nil)
(defparameter output! nil)

(defstruct atest
  the-function doc test-closure code-closure)

(defmacro deftest (&body body)
  (with-gensyms (ifun idoc itest)
    `(let ((ifun ,(car body))
           (idoc ,(cadr body)))
       (push
         (make-atest
           :the-function ifun
           :doc idoc
           :test-closure (lambda () ,(caddr body))
           :code-closure
             (lambda () ,(cadddr body)))
         *all-tests*))))



(defun run-test (ateststruct)
  (let* ((output! (funcall (atest-code-closure ateststruct)))
         (test-result (funcall (atest-test-closure ateststruct))))
    (if test-result
      (ft (green "passed: ~A~%" (atest-the-function ateststruct)))
      (ft (red "failed: ~A~%" (atest-the-function ateststruct))))
    test-result))



(deftest
  'get-size
  "hi"
  (string= output! "17k")
  (get-size "interior-of-a-heart.txt"))

(deftest
  'get-size
  "hi"
  (= output! 14433)
  (get-size "interior-of-a-heart.txt" :just-bytes t))

(run-test (car *all-tests*))



