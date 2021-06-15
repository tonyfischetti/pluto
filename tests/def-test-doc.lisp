

(defparameter /all-test-docs/ nil)
(defparameter /test-counter/ 0)
(defparameter /test-section/ nil)

(defparameter test-return-value! nil)
(defparameter test-stdout! nil)
(defparameter test-stderr! nil)



;; -----------------------------------
;; classes

; mixins
(defclass run-able () ())
(defclass doc-able () ())
(defclass benchmark-able () ())

(defclass test/doc-element ()
  ((section :initarg :section :initform /test-section/)
   (doc :initarg :doc :initform "")))

(defclass test/doc-title (test/doc-element)
  ((tagline :initarg :tagline)))

(defclass test/doc-section (test/doc-element) ())

(defclass test/doc-test (test/doc-element)
  ((test-number :initform (incf /test-counter/))
   (the-function :initarg :the-function)
   (test-closure :initarg :test-closure)
   (code-closure :initarg :code-closure)
   (raw-code :initarg :raw-code)
   (pass-p :initform nil)
   output))

(defclass test/doc-test-run-able (test/doc-test) ())

(defclass test/doc-test-returns  (test/doc-test-run-able) ())

(defclass test/doc-test-stdout   (test/doc-test-run-able) ())

(defclass test/doc-test-stderr   (test/doc-test-runable) ())


;; -----------------------------------


(defun start-tests (&key title tagline doc)
  (setq /all-test-docs/ nil)
  (setq /test-counter/ 0)
  (push (make-instance 'test/doc-title
                       :section title
                       :tagline tagline
                       :doc doc) /all-test-docs/))

(defun end-tests ()
  (setq /all-test-docs/ (reverse /all-test-docs/)))


;; -----------------------------------
;; "constructors"
(defun def-test/doc-title (title)
  (push (make-instance 'test/doc-title :doc title) /all-test-docs/))

(defun def-test/doc-section (section-name)
  (setq /test-section/ section-name)
  (push (make-instance 'test/doc-section) /all-test-docs/))

(defmacro def-test/doc-test (&body body)
  (destructuring-bind (the-function traits doc test &rest code) body
    `(push (make-instance
             (ecase ,test-type
               ('returns  'test/doc-test-returns)
               ('stdout   'test/doc-test-stdout)
               ('stderr   'test/doc-test-stderr))
             :the-function ,the-function
             :doc ,doc
             :test-closure (lambda () ,test)
             :raw-code (quote ,code)
             :code-closure (lambda () ,@code))
           /all-test-docs/)))
;; -----------------------------------



;; -----------------------------------
;; testing
(defgeneric run-test (runnable))

(defmethod run-test ((test test/doc-title))
  (ft (cyan "beginning tests for ~A~%" { test 'section })))

(defmethod run-test ((test test/doc-section))
  (ft (yellow "testing section: ~A~%" { test 'section })))

(defmethod run-test ((test test/doc-test))
  (multiple-value-bind (test-return-value! test-stdout! test-stderr!)
    (capture-all-outputs { test 'code-closure })
    (let ((test-result (funcall { test 'test-closure })))
      (if test-result
        (progn
          (ft (green "passed: ~A~%" { test 'the-function }))
          (setf { test 'pass-p } t)
          (setf { test 'output }
                `(,test-return-value! ,test-stdout! ,test-stderr!)))
        (ft (red "failed: ~A~%" { test 'the-function }))))))

(defgeneric passed-p  (test/doc-element))
(defmethod passed-p   ((test test/doc-element)) t)
(defmethod passed-p   ((test test/doc-test)) { test 'pass-p })

(defun run-tests ()
  (mapcar #'run-test /all-test-docs/)
  (every #'passed-p /all-test-docs/))
;; -----------------------------------


;; -----------------------------------
;; markdown
(defgeneric to-markdown (test/doc-element &optional stream))

(defmethod to-markdown ((test test/doc-title) &optional (stream t))
  (format stream "# ~A documentation~%_~A_~%~%~A~%"
          { test 'section }
          { test 'tagline }
          { test 'doc }))

(defmethod to-markdown ((test test/doc-section) &optional (stream t))
  (format stream "~%-----~%~%# ~A~%~%" { test 'section }))

(defmethod to-markdown ((test test/doc-test) &optional (stream t))
  (format stream "~%### ~A~%~%" { test 'the-function })
  (format stream "```~%~S~%```~%~%" (car { test 'raw-code })))

(defmethod to-markdown :after ((test test/doc-test-returns)
                               &optional (stream t))
  (format stream "Returns:~%```~%~A~%```~%~%" (car { test 'output })))

(defmethod to-markdown :after ((test test/doc-test-stdout)
                               &optional (stream t))
  (format stream "Outputs:~%```~%~A~%```~%~%" (cadr { test 'output })))

(defmethod to-markdown :after ((test test/doc-test-stderr)
                               &optional (stream t))
  (format stream "Standard error:~%```~%~A~%```~%~%" (caddr { test 'output })))

(defun render-markdown (&optional (stream t))
  (mapcar (lambda (x) (to-markdown x stream)) /all-test-docs/))
;; -----------------------------------



