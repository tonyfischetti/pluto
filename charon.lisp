;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;  Charon                                          ;;
;;    a companion to the pluto package that         ;;
;;    relies on external dependencies               ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;              License: GPL-3                      ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :charon
  ; (:use :common-lisp)
  (:use :common-lisp :pluto)
  ; (:shadowing-import-from #:pluto #:realpath)
  (:import-from :parse-float :parse-float)
  (:export
    ; regular expressions / cl-ppcre wrappers
    :re-compile :str-split :str-replace :str-replace-all :str-detect
    :str-subset :str-extract :str-scan-to-strings :str-trim :~m :~r
    :~ra :~s :~f :~c :~e

    ; terminal things / terminal manipulation
    :with-loading

    ; HTML/XML stuff
    :request/get :request/post :parse-xml :parse-xml-file :xpath
    :xpath-compile :use-xml-namespace :xpath-string

    ; other abbriviations and shortcuts
    :alist->hash-table :hash-table->alist :hash-keys :parse-json
    :hash-table->json :parse-json-file :make-octet-vector
    :concat-octet-vector :parse-html :$$

    ; re-exports
    :parse-float

    ; filename/namestring escaping
    :escape-namestring/shell :escape-namestring/c
    :pathname->shell :pathname->c

    ; ; rework of improve-able pluto functions
    ; :realpath

    ))

(use-package :pluto)
(in-package :charon)

(pushnew :charon *features*)


;---------------------------------------------------------;
; regular expressions / cl-ppcre wrappers ----------------;

(defmacro re-compile (&rest everything)
  `(cl-ppcre:create-scanner ,@everything))

(defmacro str-split (astr sep &rest everything)
  "Wrapper around cl-ppcre:split with string first"
  `(cl-ppcre:split ,sep ,astr ,@everything))

(defmacro str-replace (astr from to &rest everything)
  "Wrapper around cl-ppcre:regex-replace with string first"
  `(cl-ppcre:regex-replace ,from ,astr ,to ,@everything))

(defmacro str-replace-all (astr from to &rest everything)
  "Wrapper around cl-ppcre:regex-replace-all with string first"
  `(cl-ppcre:regex-replace-all ,from ,astr ,to ,@everything))

(defmacro str-detect (astr pattern &rest everything)
  "Returns true if `pattern` matches `astr`
   Wrapper around cl-ppcre:scan"
  `(if (cl-ppcre:scan ,pattern ,astr ,@everything) t nil))

(defun str-subset (anlist pattern)
  "Returns all elements that match pattern"
  (remove-if-not (lambda (x) (str-detect x pattern)) anlist))

(defun str-extract (astr pattern)
  "If one match, it returns the register group as a string.
   If more than one match/register-group, returns a list
   of register groups. If there is a match but no register
   group, it will still return nil"
  (multiple-value-bind (dontneed need)
    (cl-ppcre:scan-to-strings pattern astr)
    (let ((ret (coerce need 'list)))
      (if (= (length ret) 1)
        (car ret) ret))))

(defun str-scan-to-strings (astr pattern)
  "Wrapper around cl-ppcre:scan-to-strings with string first
   and only returns the important part (the vector of matches)"
  (multiple-value-bind (dontneed need)
    (cl-ppcre:scan-to-strings pattern astr)
    need))

(defun str-trim (astring)
  (string-trim *whitespaces* astring))

(defmacro ~m (&rest everything)
  "Alias to str-detect"
  `(str-detect ,@everything))

(defmacro ~r (&rest everything)
  "Alias to str-replace (one)"
  `(str-replace ,@everything))

(defmacro ~ra (&rest everything)
  "Alias to str-replace-all"
  `(str-replace-all ,@everything))

(defmacro ~s (&rest everything)
  "Alias to str-split"
  `(str-split ,@everything))

(defmacro ~f (&rest everything)
  "Alias to str-subset"
  `(str-subset ,@everything))

(defmacro ~c (&rest everything)
  "Alias to re-compile"
  `(re-compile ,@everything))

(defmacro ~e (&rest everything)
  "Alias to str-extract"
  `(str-extract ,@everything))

; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; terminal things / terminal manipulation --------------- ;

; TODO: doesn't work on clisp ?
(defmacro with-loading (&body body)
  "This function runs `body` in a separate thread
   and also starts a thread that displays a spinner.
   When the `body` thread finishes, it kills the
   spinner thread. Here's an example....
   ```
    (for-each `(heaven or las vegas)
      (ft •processing: ~10A~C• value! #\Tab)
      (with-loading
        (sleep 3)))
    ```
    its particularly neat combined with
    ```
    (progress index! 5 :newline-p nil :where *standard-output*)
    (ft •~C• #\Tab #\Tab)
    ``` "
  (let ((long-thread	  (gensym))
        (loading-thread (gensym))
        (the-return     (gensym)))
    `(progn
       (let ((,long-thread
               #+sbcl (sb-thread:make-thread
                        (lambda () ,@body)
                        :name "background-thread")
               #+ecl (mp:process-run-function
                       'background-thread
                       (lambda () ,@body))
               #-(or sbcl ecl) (bt:make-thread
                                 (lambda () ,@body) :name "long-thread")
               )
             (,loading-thread
               #+sbcl (sb-thread:make-thread
                        #'loading-forever
                        :name "loading-thread")
               #+ecl (mp:process-run-function
                       'loading-thead
                       #'loading-forever)
               #-(or sbcl ecl) (bt:make-thread
                                 #'loading-forever :name "loading-thread")
               ))
         (let ((,the-return
                 #+sbcl (sb-thread:join-thread ,long-thread)
                 #+ecl (mp:process-join ,long-thread)
                 #-(or sbcl ecl) (bt:join-thread ,long-thread)
                 ))
           #+sbcl (sb-thread:terminate-thread ,loading-thread)
           #+ecl (mp:process-kill ,loading-thread)
           #-(or sbcl ecl) (bt:destroy-thread ,loading-thread)
           (terpri)
           ,the-return)))))

; ------------------------------------------------------- ;

;; !!! BELOW THIS IS UNTESTED ON CLISP
;; !!! AND DIDN'T PIECEMEAL COMPILE FOR SBCL WARNINGS

; ------------------------------------------------------- ;
; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; HTML/XML stuff ---------------------------------------- ;

(defmacro request/get (&rest everything)
  `(dexador:get ,@everything))

(defmacro request/post (&rest everything)
  `(dexador:post ,@everything))

(defun parse-xml (astring)
  (cxml:parse astring (cxml-dom:make-dom-builder)))

(defun parse-xml-file (afile)
  (cxml:parse-file afile (cxml-dom:make-dom-builder)))

(defun xpath (doc anxpath &key (all t) (compiled-p nil) (text nil))
  (let ((result (if compiled-p
                  (xpath:evaluate-compiled anxpath doc)
                  (xpath:evaluate anxpath doc))))
    (unless (xpath:node-set-empty-p result)
      (if (and all text)
        (mapcar (lambda (x) (xpath:string-value x)) (xpath:all-nodes result))
        (if (and all (not text))
          (xpath:all-nodes result)
          (if (and (not all) text)
            (xpath:string-value result)
            result))))))

(defmacro xpath-compile (&rest everything)
  `(xpath:compile-xpath ,@everything))

(defmacro use-xml-namespace (anns)
  `(setq xpath::*dynamic-namespaces*
         (cons
           (cons nil ,anns)
           xpath::*dynamic-namespaces*)))

(abbr xpath-string xpath:string-value)

; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; other abbriviations and shortcuts --------------------- ;

(abbr alist->hash-table alexandria:alist-hash-table)
(abbr hash-table->alist alexandria:hash-table-alist)
(abbr hash-keys alexandria:hash-table-keys)
(abbr parse-json yason:parse)
(abbr export-json yason:encode)

; TODO: document
(defun hash-table->json (ahashtable)
  (let ((s (make-string-output-stream)))
    (yason:encode ahashtable s)
    (get-output-stream-string s)))

(defun parse-json-file (afile)
  (with-a-file afile :r
    (yason:parse stream!)))

(defmacro make-octet-vector (n)
  `(make-array ,n :element-type '(unsigned-byte 8)))

(defmacro concat-octet-vector (&rest everything)
  `(concatenate '(vector (unsigned-byte 8)) ,@everything))

(abbr parse-html plump:parse)
(abbr $$ lquery:$)

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; filename/namestring escaping -------------------------- ;

#+(or sbcl clisp ecl abcl)
(defun escape-namestring/shell (afilename)
  (-<> afilename
       (~ra <> •\t• •\	•) (~ra <> • • •\ •) (~ra <> •;• •\;•)
       (~ra <> •\^• •\^•) (~ra <> •\$• •\$•) (~ra <> •\\\?• •\?•)
       ; (~ra <> •"• •\"•) (~ra <> •\\\[• •\[•) (~ra <> •\]• •\]•)
       (~ra <> •"• •\"•) (~ra <> •\[• •\[•) (~ra <> •\]• •\]•)
       (~ra <> •\)• •\)•) (~ra <> •\(• •\(•) (~ra <> •'• •\\'•)
       (~ra <> •<• •\<•) (~ra <> •>• •\>•) (~ra <> •!• •\!•)
       (~ra <> •\\\*• •\*•) (~ra <> •\&• •\\&•) (~ra <> •=• •\=•)
       (~ra <> •`• •\\`•) (~ra <> •\|• •\|•)))


#+(or clisp ecl abcl)
(defun escape-namestring/shell (afilename)
  (-<> afilename
       (~ra <> •\\• •\\\•) (~ra <> •\t• •\	•) (~ra <> • • •\ •)
       (~ra <> •;• •\;•) (~ra <> •\^• •\^•) (~ra <> •\$• •\$•)
       (~ra <> •\?• •\?•) (~ra <> •"• •\"•) (~ra <> •\[• •\[•)
       (~ra <> •\]• •\]•) (~ra <> •\)• •\)•) (~ra <> •\(• •\(•)
       (~ra <> •'• •\\'•) (~ra <> •<• •\<•) (~ra <> •>• •\>•)
       (~ra <> •!• •\!•) (~ra <> •\*• •\*•) (~ra <> •\&• •\\&•)
       (~ra <> •=• •\=•) (~ra <> •`• •\\`•) (~ra <> •\|• •\|•)))

#+sbcl
(defun escape-namestring/c (afilename)
  (-<> afilename
       (~ra <> •\\• •\\\•) (~ra <> •\?• •\?•) (~ra <> •\[• •\[•)
       (~ra <> •\\\\\*• •*•) (~ra <> •\\\\\\\[• •[•) (~ra <> •\\\\\\• "")))

#+(or clisp ecl abcl)
(defun escape-namestring/c (afilename)
  (-<> afilename
       (~ra <> •\\\\\*• •*•) (~ra <> •\\\\\\\[• •[•) (~ra <> •\\\\\\• "")))

(defun pathname->shell (apath)
  (escape-namestring/shell (namestring apath)))

(defun pathname->c (apath)
  (escape-namestring/c (namestring apath)))

; ------------------------------------------------------- ;

; ------------------------------------------------------- ;
; some clippings that'll be helpful for reference ------- ;

; (defun dec->hex (something)
;   (fn "~X" something))
;
; (defun hex->dec (something)
;   (parse-integer something :radix 16))
;
; ; needs babel and ironclad
;
; (defun string->octets (something)
;   (babel:string-to-octets something))
;
; (defun octets->string (something)
;   (babel:octets-to-string something))
;
; (defun octets->hex (something)
;   (ironclad:byte-array-to-hex-string something))
;
; (defun hex->octets (something)
;   (ironclad:hex-string-to-byte-array something))
;
; (defun octets->int (something)
;   (ironclad:octets-to-integer something))
;
; (defun int->octets (something)
;   (ironclad:integer-to-octets something))
;
; (defun string->octets->integer (something)
;   (-<> something (string->octets <>) (octets->integer <>)))

; ------------------------------------------------------- ;


; ;---------------------------------------------------------;
; ; experimental logging and reader macros ---------------- ;
;
; ; (defun prettify-time-output (thetimeoutput)
; ;   (subseq thetimeoutput 0 (- (length thetimeoutput) 4)))
; ;
; ; ; TODO: REALLY LOOK INTO THIS BECAUSE THERE ARE A LOT OF WARNINGS AND IT SUCKS
; ; (defun clix-log-verbose (stream char arg)
; ;   ;;;;;; HOW UNHYGENIC IS THIS???!!
; ;   (declare (ignore char))
; ;   (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
; ;                        (get-decoded-time)
; ;     (let ((sexp               (read stream t))
; ;           (thetime            (get-universal-time))
; ;           (thereturnvalue     nil)
; ;           (thetimingresults   nil)
; ;           (daoutputstream     (make-string-output-stream)))
; ;       `(progn
; ;          (with-a-file *clix-log-file* :a
; ;            (format stream!
; ;                    "--------------------~%[~A-~A-~A ~2,'0d:~2,'0d:~2,'0d]~%~%FORM:~%~A~%"
; ;                    ,year ,month ,date ,hour ,minute ,second
; ;                    ; (write-to-string ',sexp))
; ;                    (format nil "λ ~S~%" ',sexp))
; ;            (let ((daoutputstream (make-string-output-stream)))
; ;              (let ((*trace-output* daoutputstream))
; ;                (setq thereturnvalue (progn (time ,sexp))))
; ;                  (setq thetimingresults
; ;                        (prettify-time-output
; ;                          (get-output-stream-string daoutputstream))))
; ;            (format stream! "RETURNED:~%~A~%" thereturnvalue)
; ;            (format stream! "~%~A~%--------------------~%~%~%" thetimingresults)
; ;            (finish-output stream!)
; ;            thereturnvalue)))))
; ;
; ;
; ; ; REALLY LOOK INTO THIS BECAUSE THERE ARE A LOT OF WARNINGS AND IT SUCKS
; ; ; (defun clix-log-just-echo (stream char arg)
; ; ;   ;;;;;; HOW UNHYGENIC IS THIS???!!
; ; ;   (declare (ignore char))
; ; ;   (let ((sexp               (read stream t))
; ; ;     ;       (thetime            (get-universal-time))
; ; ;     ;       (thereturnvalue     nil)
; ; ;     ;       (thetimingresults   nil))
; ; ;       `(progn
; ; ;          (with-a-file *clix-log-file* :a
; ; ;            (format t "~S~%" ',sexp)
; ; ;            (format stream! "~%λ ~S~%" ',sexp)
; ; ;            (let* ((daoutputstream   (make-string-output-stream))
; ; ;                   (*trace-output*   daoutputstream)
; ; ;                   (thereturnvalue   (progn (time ,sexp))))
; ; ;              (finish-output stream!)
; ; ;              ,thereturnvalue))))))
; ;
; ;
; ; (defun clix-log (stream char arg)
; ;   (cond ((= *clix-log-level* 2)    (clix-log-verbose   stream char arg))
; ;         ; ((= *clix-log-level* 1)    (clix-log-just-echo stream char arg))
; ;         (                          nil)))
; ;
; ; (set-dispatch-macro-character #\# #\! #'clix-log)
;
; ;---------------------------------------------------------;


