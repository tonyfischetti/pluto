

(defparameter *all-doc/tests* nil)
(defparameter /test-counter/ 0)
(defparameter /test-section/ nil)

(defparameter test-return-value! nil)
(defparameter test-stdout! nil)
(defparameter test-stderr! nil)


; TODO: defclass+ with initargs already

(defclass test/doc ()
  ((section :initform /test-section/)
   (doc :initarg :doc :initform "")))

(defclass test/doc-section (test/doc)
  ())

(defclass test/doc-code (test/doc)
  ((test-number :initform (incf /test-counter/))
   (the-function :initarg :the-function)
   (test-closure :initarg :test-closure)
   (code-closure :initarg :code-closure)
   (raw-code :initarg :raw-code)
   (pass-p :initform nil)
   output))

(defclass test/doc-returns (test/doc-code)
  ())

(defclass test/doc-outputs (test/doc-code)
  ())


(defun start-tests ()
  (setq *all-doc/tests* nil)
  (setq /test-counter/ 0)
  (setq /test-section/ 0))

(defun end-tests ()
  (setq *all-doc/tests* (reverse *all-doc/tests*)))

(defun def-test-section (section-name)
  (setq /test-section/ section-name)
  (push (make-instance 'test/doc-section)
        *all-doc/tests*))


(defmacro def-test/doc (&body body)
  (destructuring-bind (the-function doc) body
    `(push
       (make-instance 'test/doc
                      :the-function ,the-function
                      :doc ,doc)
       *all-doc/tests*)))

(defmacro def-test/doc-returns (&body body)
  (destructuring-bind (the-function doc test &rest code) body
    `(push
       (make-instance 'test/doc-returns
                      :the-function ,the-function
                      :doc ,doc
                      :test-closure (lambda () ,test)
                      :raw-code (quote ,code)
                      :code-closure (lambda () ,@code))
       *all-doc/tests*)))

(defmacro def-test/doc-outputs (&body body)
  (destructuring-bind (the-function doc test &rest code) body
    `(push
       (make-instance 'test/doc-outputs
                      :the-function ,the-function
                      :doc ,doc
                      :test-closure (lambda () ,test)
                      :raw-code (quote ,code)
                      :code-closure (lambda () ,@code))
       *all-doc/tests*)))





(defmacro capture-ret-and-output (&body body)
  `(let ((*standard-output* (make-string-output-stream)))
     (let ((ret (funcall ,@body)))
       `(,ret ,(get-output-stream-string *standard-output*)))))

(defgeneric run-test (test/doc))

(defmethod run-test ((test test/doc-section))
  (ft (yellow "testing section: ~A~%" { test 'section })))

(defmethod run-test ((test test/doc-returns))
  (let* ((tmp (funcall { test 'code-closure }))
         (test-return-value! tmp))
    (let ((test-result (funcall { test 'test-closure })))
      (if test-result
        (progn
          (ft (green "passed: ~A~%" { test 'the-function }))
          (setf { test 'pass-p } t)
          (setf { test 'output } tmp))
        (ft (red "failed: ~A~%" { test 'the-function }))))))

(defmethod run-test ((test test/doc-outputs))
  (destructuring-bind (test-return-value! test-stdout!)
    (capture-ret-and-output { test 'code-closure })
    (let ((test-result (funcall { test 'test-closure })))
      (if test-result
        (progn
          (ft (green "passed: ~A~%" { test 'the-function }))
          (setf { test 'pass-p } t)
          (setf { test 'output } test-stdout!))
        (ft (red "failed: ~A~%" { test 'the-function }))))))


(defgeneric to-markdown (test/doc))

(defmethod to-markdown ((test test/doc-section))
  (ft "## ~A~%" { test 'section }))

(defmethod to-markdown ((test test/doc-code))
  (ft "this is a code one~%"))

(defmethod to-markdown ((test test/doc-returns))
  (ft "returns: ~S~%" { test 'output }))

(defmethod to-markdown ((test test/doc-outputs))
  (ft "outputs: ~S~%" { test 'output }))





