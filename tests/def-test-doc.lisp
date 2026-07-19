
(setq *print-pretty* t)

(defparameter /all-test-docs/ nil)
(defparameter /test-counter/ 0)
(defparameter /test-section/ nil)
(defparameter /last-element-rendered/ nil)

(defparameter test-return-value! nil)
(defparameter test-stdout! nil)
(defparameter test-stderr! nil)
(defparameter test-error! nil)



;; -----------------------------------
;; trait mixins
;;
;; capabilities are mixin classes; whether an element can be
;; tested/rendered/benched is decided by CLOS dispatch, not
;; by checking a list of traits

(defclass markdown-able () ())

(defclass test-able ()
  ((output-type :initarg :output-type :initform nil)))

(defclass bench-able ()
  ((bench-times :initarg :bench-times :initform 1)))

;; -----------------------------------
;; classes

(defclass test/doc-element ()
  ((section :initarg :section :initform /test-section/)
   (doc :initarg :doc :initform "")))

(defclass test/doc-title (markdown-able test/doc-element) ())

(defclass test/doc-section (markdown-able test/doc-element) ())

(defclass raw-markdown (markdown-able test/doc-element) ())

(defclass test/doc-test (test/doc-element)
  ((test-number :initform (incf /test-counter/))
   (the-function :initarg :the-function)
   (test-closure :initarg :test-closure)
   (code-closure :initarg :code-closure)
   (raw-code :initarg :raw-code)
   (pass-p :initform nil)
   output))

;; -----------------------------------
;; programmatic mixin combination (the MOP bit)
;;
;; each test's class is synthesized at runtime from the mixins
;; it asks for: e.g. (markdown-able (test-able returns)) yields
;; the class MARKDOWN-ABLE+TEST-ABLE+TEST/DOC-TEST
;; (mixins precede the base class so their methods win)

(defun %mixed-class (base mixins)
  (if (null mixins)
    (find-class base)
    (let ((supers (append mixins (list base))))
      (#+sbcl sb-mop:ensure-class
       #+ecl  clos:ensure-class
        (create-symbol (str-join "+" (mapcar #'symbol-name supers)))
        :direct-superclasses supers))))

(defun %parse-traits (traits)
  "Traits like (markdown-able (test-able returns) (bench-able 5))
   become (values (markdown-able test-able bench-able)
                  (:output-type returns :bench-times 5))"
  (let ((mixins    nil)
        (initargs  nil))
    (dolist (tr traits)
      (let ((name (if (listp tr) (car tr) tr))
            (val  (if (listp tr) (cadr tr) nil)))
        (push name mixins)
        (when val
          (cond ((eq name 'test-able)
                  (setq initargs (list* :output-type val initargs)))
                ((eq name 'bench-able)
                  (setq initargs (list* :bench-times val initargs)))))))
    (values (nreverse mixins) initargs)))

;; -----------------------------------

(defun process-docstring (astring)
  (when (not (null astring))
    (-<> astring
      (split-string->lines <>)
      (mapcar (lambda (x) (fn "> ~A\\~%" x)) <>)
      (str-join (fn "") <>))))

;; -----------------------------------


(defun start-test/doc (&key title)
  (setq /all-test-docs/ nil)
  (setq /test-counter/ 0)
  (push (make-instance 'test/doc-title :section title)
    /all-test-docs/))

(defun end-test/doc ()
  (setq /all-test-docs/ (reverse /all-test-docs/)))


;; -----------------------------------
;; "constructors"
(defun def-test/doc-title (title)
  (push
    (make-instance 'test/doc-title :doc title)
    /all-test-docs/))

(defun def-test/doc-section (section-name)
  (setq /test-section/ section-name)
  (push
    (make-instance 'test/doc-section)
    /all-test-docs/))

(defmacro def-test/doc-test (&body body)
  (destructuring-bind (the-function traits doc test &rest code) body
    (with-gensyms (mixins initargs)
      `(multiple-value-bind (,mixins ,initargs) (%parse-traits ,traits)
         (push
           (apply #'make-instance (%mixed-class 'test/doc-test ,mixins)
                  :the-function ,the-function
                  :doc (if (symbolp ,doc)
                         (process-docstring
                           (documentation ,the-function ,doc))
                         ,doc)
                  :test-closure (lambda () ,test)
                  :raw-code (quote ,code)
                  :code-closure (lambda () ,@code)
                  ,initargs)
           /all-test-docs/)))))

(defmacro def-raw-markdown (astring)
  `(push
     (make-instance 'raw-markdown :doc ,astring)
     /all-test-docs/))

;; -----------------------------------



;; -----------------------------------
;; testing

(defmethod run-test ((el test/doc-element))
  t)

(defmethod run-test ((el test/doc-title))
  (ft (cyan "beginning tests for ~A~%" { el 'section })))

(defmethod run-test ((el test/doc-section))
  (ft (yellow "testing section: ~A~%" { el 'section })))

; a test that isn't TEST-ABLE gets this method (the TEST-ABLE
; mixin precedes TEST/DOC-TEST in the class precedence list)
(defmethod run-test ((test test/doc-test))
  (ft (grey "skipping test of ~S~%" { test 'the-function })))

; TODO: actually run it BENCH-TIMES times
(defmethod run-test :around ((test bench-able))
  (ft (magenta "benchmarking: ~S ~A times~%"
               { test 'the-function }
               { test 'bench-times }))
  (call-next-method))

(defmethod run-test ((test test-able))
  (let ((outs       (make-string-output-stream))
        (errs       (make-string-output-stream))
        (returned   nil)
        (condition  nil))
    ; if the code signals, that's captured in `condition` (and the
    ; test fails) rather than crashing the whole suite. this also
    ; lets a test _assert_ that its code errors (via `test-error!`)
    (handler-case
      (let ((*standard-output*   outs)
            (*error-output*      errs))
        (setq returned (funcall { test 'code-closure })))
      (error (e) (setq condition e)))
    (let ((test-return-value!   returned)
          (test-stdout!         (get-output-stream-string outs))
          (test-stderr!         (get-output-stream-string errs))
          (test-error!          condition))
      (let ((test-result
              (handler-case (funcall { test 'test-closure })
                (error (e)
                  (ft (red "error in the test expression itself for ~A: ~A~%"
                           { test 'the-function } e))
                  nil))))
        (if test-result
          (progn
            (ft (green "passed: ~A~%" { test 'the-function }))
            (setf { test 'pass-p } t)
            (setf { test 'output }
                  `(,test-return-value! ,test-stdout! ,test-stderr!)))
          (progn
            (ft (red "failed: ~A (test #~A)~%" { test 'the-function }
                                               { test 'test-number }))
            (ft (red "    returned:  ~S~%" test-return-value!))
            (ft (red "    stdout:    ~S~%" test-stdout!))
            (ft (red "    stderr:    ~S~%" test-stderr!))
            (when test-error!
              (ft (red "    signalled: ~A~%" test-error!)))))))))

(defmethod passed-p ((el test/doc-element)) t)
(defmethod passed-p ((test test-able))
  { test 'pass-p })

(defun run-tests ()
  (mapcar #'run-test /all-test-docs/)
  (let ((failures (remove-if #'passed-p /all-test-docs/)))
    (if failures
      (progn
        (ft (red "~%~A test(s) failed:~%" (length failures)))
        (for-each/list failures
          (ft (red "  #~A ~A~%" { value! 'test-number }
                                { value! 'the-function })))
        nil)
      t)))

;; -----------------------------------


;; -----------------------------------
;; markdown
;;
;; TO-MARKDOWN dispatches on MARKDOWN-ABLE (anything else renders
;; as nothing); RENDER-MD does the structural rendering per class

(defgeneric to-markdown (test/doc-element &optional stream))

(defmethod to-markdown ((el test/doc-element) &optional stream)
  (declare (ignore stream))
  nil)

(defmethod to-markdown ((el markdown-able) &optional (stream t))
  (render-md el stream))

(defgeneric render-md (el stream))

(defmethod render-md ((el test/doc-element) stream)
  (format stream "~A~%" { el 'doc }))

(defmethod render-md ((el test/doc-title) stream)
  (format stream "---~%title: ~A documentation~%...~%~%"
          { el 'section }))

(defmethod render-md ((el test/doc-section) stream)
  (format stream "~%-----~%~%### ~A~%~%" { el 'section }))

(defmethod render-md ((test test/doc-test) stream)
  (let ((thefun { test 'the-function }))
    (if (eq thefun /last-element-rendered/)
      (format stream "~%")
      (format stream "~%#### ~A~%~%~A~%" { test 'the-function }
                                         { test 'doc }))
    (format stream "```{.commonlisp}~%~S~%```~%~%" (car { test 'raw-code }))
    (setq /last-element-rendered/ thefun)))

(defmethod render-md :after ((test test-able) stream)
  (aif { test 'output-type }
    (let ((tmp
            (cond ((eq it! 'returns)
                     (list "=>"   (car { test 'output })))
                  ((eq it! 'stdout)
                     (list ">>"   (cadr { test 'output })))
                  ((eq it! 'stderr)
                     (list "Std error" (caddr { test 'output }))))))
      (format stream "<small><pre>~A ~S</pre></small>~%~%~%"
              (car tmp) (cadr tmp)))))

(defun render-markdown (&optional (stream t))
  (mapcar (lambda (x) (to-markdown x stream)) /all-test-docs/)
  (ft (blue "~%rendered markdown~%")))
;; -----------------------------------
