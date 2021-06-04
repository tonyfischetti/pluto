
(defparameter *all-tests* nil)
(defparameter ret-val! nil)
(defparameter output! nil)
(defparameter tmp nil)

(defstruct atest
  the-function doc test-closure code-closure)

; TODO has warnings
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
             (lambda () (progn ,@(cdddr body))))
         *all-tests*))))


(defmacro capture-ret-and-output (&body body)
  `(let ((*standard-output* (make-string-output-stream)))
     (let ((ret (funcall ,@body)))
       `(,ret ,(get-output-stream-string *standard-output*)))))

(defun run-test (ateststruct)
  (destructuring-bind (ret-val! output!)
    (capture-ret-and-output (atest-code-closure ateststruct))
    (let ((test-result (funcall (atest-test-closure ateststruct))))
      (if test-result
        (ft (green "passed: ~A~%" (atest-the-function ateststruct)))
        (ft (red "failed: ~A~%" (atest-the-function ateststruct))))
      test-result)))

