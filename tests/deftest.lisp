
(defparameter *all-tests* nil)
(defparameter return-value! nil)
(defparameter output! nil)
(defparameter tmp nil)
(defparameter *all-results* nil)

(defstruct atest
  the-function doc the-type test-closure code-closure raw-code)

(defstruct aresult
  the-function doc the-type raw-code test-result return-value output)

; TODO has warnings
(defmacro deftest (&body body)
   `(push
     (make-atest
       :the-function ,(car body)
       :doc ,(cadr body)
       :the-type ,(caddr body)
       :test-closure (lambda () ,(cadddr body))
       :code-closure
         (lambda () (progn ,@(cddddr body)))
       :raw-code (quote ,(cddddr body)))
     *all-tests*))

(defmacro capture-ret-and-output (&body body)
  `(let ((*standard-output* (make-string-output-stream)))
     (let ((ret (funcall ,@body)))
       `(,ret ,(get-output-stream-string *standard-output*)))))

(defun run-test (ateststruct)
  (destructuring-bind (return-value! output!)
    (capture-ret-and-output (atest-code-closure ateststruct))
    (let ((test-result (funcall (atest-test-closure ateststruct))))
      (if test-result
        (ft (green "passed: ~A~%" (atest-the-function ateststruct)))
        (ft (red "failed: ~A~%" (atest-the-function ateststruct))))
      (make-aresult
        :the-function (atest-the-function ateststruct)
        :doc (atest-doc ateststruct)
        :the-type (atest-the-type ateststruct)
        :raw-code (atest-raw-code ateststruct)
        :test-result test-result
        :return-value return-value!
        :output output!))))


