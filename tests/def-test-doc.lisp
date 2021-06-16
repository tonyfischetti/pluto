

(defparameter /all-test-docs/ nil)
(defparameter /test-counter/ 0)
(defparameter /test-section/ nil)

(defparameter test-return-value! nil)
(defparameter test-stdout! nil)
(defparameter test-stderr! nil)



;; -----------------------------------
;; classes

(defclass test/doc-element ()
  ((traits :initarg :traits)
   (section :initarg :section :initform /test-section/)
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

;; -----------------------------------

(defun has-trait (element atrait)
  (assoc atrait { element 'traits }))

(defun output-type (element)
  (aif (assoc 'test-able { element 'traits })
    (cadr it!)
    nil))

;; -----------------------------------


(defun start-tests (&key title tagline doc)
  (setq /all-test-docs/ nil)
  (setq /test-counter/ 0)
  (push
    (make-instance 'test/doc-title
                   :section title
                   :tagline tagline
                   :doc doc
                   :traits `((markdown-able t)))
    /all-test-docs/))

(defun end-tests ()
  (setq /all-test-docs/ (reverse /all-test-docs/)))


;; -----------------------------------
;; "constructors"
(defun def-test/doc-title (title)
  (push
    (make-instance 'test/doc-title :doc title
                                    :traits '((markdown-able t)))
    /all-test-docs/))

(defun def-test/doc-section (section-name)
  (setq /test-section/ section-name)
  (push
    (make-instance 'test/doc-section :traits '((markdown-able t)))
    /all-test-docs/))

(defmacro def-test/doc-test (&body body)
  (destructuring-bind (the-function traits doc test &rest code) body
    (let ((thetraits (gensym))
          (newone    (gensym)))
      `(let* ((,thetraits nil)
              (,newone (make-instance 'test/doc-test
                                     :the-function ,the-function
                                     :doc ,doc
                                     :test-closure (lambda () ,test)
                                     :raw-code (quote ,code)
                                     :code-closure (lambda () ,@code))))
         (setf ,thetraits (mapcar
                            (lambda (x)
                              (if (listp x) x (list x t)))
                            ,traits))
         ; TODO: the following fails in ecl and clisp
         ; (setf { ,newone 'traits } ,thetraits)
         (setf (slot-value ,newone 'traits) ,thetraits)
         (push ,newone /all-test-docs/)))))

(defmacro def-raw-markdown (astring)
  `(push
     (make-instance 'test/doc-element :doc ,astring
                    :traits `((markdown-able t)))
     /all-test-docs/))

;; -----------------------------------



;; -----------------------------------
;; testing

; TODO: implement
(defmethod run-test :around ((test test/doc-test))
  (if (has-trait test 'test-able)
    (aif (has-trait test 'bench-able)
      (progn
        (ft (magenta "benchmarking: ~S ~A times~%"
                     { test 'the-function }
                     (cadr it!)))
        (call-next-method))
      (call-next-method))
    (ft (grey "skipping test of ~S~%" { test 'the-function }))))

(defmethod run-test ((test test/doc-element))
  t)

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

(defmethod passed-p ((test test/doc-element)) t)
(defmethod passed-p ((test test/doc-test))
  (if (has-trait test 'test-able)
    { test 'pass-p }
    t))

(defun run-tests ()
  (mapcar #'run-test /all-test-docs/)
  (every #'passed-p /all-test-docs/))

;; -----------------------------------


;; -----------------------------------
;; markdown

(defgeneric to-markdown (test/doc-element &optional stream))

(defmethod to-markdown :around ((test test/doc-element) &optional (stream t))
  (when (has-trait test 'markdown-able)
    (call-next-method)))

(defmethod to-markdown ((test test/doc-element) &optional (stream t))
  (format stream "~A~%" { test 'doc }))

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

; TODO: test test-able nil
(defmethod to-markdown :after ((test test/doc-test) &optional (stream t))
  (aif (output-type test)
    (let ((tmp
            (cond ((eq it! 'returns)
                     (list "Returns"   (car { test 'output })))
                  ((eq it! 'stdout)
                     (list "Outputs"   (cadr { test 'output })))
                  ((eq it! 'stderr)
                     (list "Std error" (caddr { test 'output }))))))
      (format stream "~A:~%```~%~A~%```~%~%"
              (car tmp) (cadr tmp)))))

(defun render-markdown (&optional (stream t))
  (mapcar (lambda (x) (to-markdown x stream)) /all-test-docs/)
  (ft (blue "~%rendered markdown~%")))
;; -----------------------------------



