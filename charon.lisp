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
    :str-normalize
    :q/re

    ; terminal things / terminal manipulation
    :with-loading

    ; HTML/XML stuff
    :request/get :request/post :parse-xml :parse-xml-file :xpath
    :xpath-compile :use-xml-namespace :xpath-string
    :q/uri

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
  "Compiles a regex into a scanner (cl-ppcre:create-scanner) for
   when one pattern gets matched many times; every str-* function
   (and ~m et al.) takes a scanner in place of a pattern string"
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
  "ASTRING without its leading and trailing whitespace (the
   characters in *whitespaces*)"
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

; quotes a string so it can be used _literally_ inside a regex
(abbr q/re cl-ppcre:quote-meta-chars)

(defun str-normalize (astring &key (form :nfd))
  "ASTRING in the unicode normalization FORM -- :nfd (the default),
   :nfkd, :nfc, or :nfkc (cl-unicode) -- for comparing strings that
   differ only in how their accents are composed"
  (let ((code-points
          (cond ((eq form :nfd)  (cl-unicode:normalization-form-d astring))
                ((eq form :nfkd) (cl-unicode:normalization-form-k-d astring))
                ((eq form :nfc)  (cl-unicode:normalization-form-c astring))
                ((eq form :nfkc) (cl-unicode:normalization-form-k-c astring)))))
    (map 'string #'code-char code-points)))

; ------------------------------------------------------- ;



;; !!! BELOW THIS IS UNTESTED ON CLISP
;; !!! AND DIDN'T PIECEMEAL COMPILE FOR SBCL WARNINGS

; ------------------------------------------------------- ;
; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; HTML/XML stuff ---------------------------------------- ;

; (a defun rather than an abbr of quri:url-encode: that has no
; docstring to inherit, and the other q/ functions princ their THING)
(defun q/uri (thing &rest keys)
  "Quotes THING (which gets princ-ed) for a URL: percent-encodes
   whatever isn't safe in a path segment or query string
   (quri:url-encode, whose keywords :encoding and :space-to-plus
   pass through)
   e.g. (fn \"https://x.org/search?q=~A\" (q/uri \"café & bar\"))"
  (apply #'quri:url-encode (fn "~A" thing) keys))

(defmacro request/get (&rest everything)
  "HTTP GET (dexador:get, same arguments): a URI, then dexador's
   keywords (:headers, :basic-auth, :want-stream...). Returns the
   body, the status code, the response headers, the final URI, and
   the stream, as values"
  `(dexador:get ,@everything))

(defmacro request/post (&rest everything)
  "HTTP POST (dexador:post, same arguments): a URI, then :content
   (an alist of form fields, or a string) and dexador's other
   keywords. Returns the body, the status code, the response
   headers, the final URI, and the stream, as values"
  `(dexador:post ,@everything))

(defun parse-xml (astring)
  "Parses the XML in ASTRING into a DOM document (cxml, with the DOM
   builder) for xpath to query"
  (cxml:parse astring (cxml-dom:make-dom-builder)))

(defun parse-xml-file (afile)
  "Parses the XML file AFILE into a DOM document (cxml, with the DOM
   builder) for xpath to query"
  (cxml:parse-file afile (cxml-dom:make-dom-builder)))

(defun xpath (doc anxpath &key (all t) (compiled-p nil) (text nil))
  "Evaluates the XPath expression ANXPATH against DOC (a DOM
   document or node) and returns the matching nodes as a list, or
   nil when nothing matches. TEXT returns their string values
   instead of the nodes. ALL nil returns just the first match (its
   string value with TEXT, the raw node-set without). COMPILED-P
   says ANXPATH was compiled with xpath-compile
   e.g. (xpath doc \"//item/title\" :text t)"
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
  "Compiles an XPath expression (xpath:compile-xpath) for repeated
   use; hand the result to xpath with :compiled-p t"
  `(xpath:compile-xpath ,@everything))

(defmacro use-xml-namespace (anns)
  "Makes ANNS (a namespace URI) the default namespace for xpath
   queries: unprefixed element names then match elements in that
   namespace, which is what a document with an xmlns= needs"
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

(defun hash-table->json (ahashtable)
  "AHASHTABLE (or anything else yason can encode: lists, strings,
   numbers...) as a JSON string"
  (let ((s (make-string-output-stream)))
    (yason:encode ahashtable s)
    (get-output-stream-string s)))

(defun parse-json-file (afile)
  "Parses the JSON file AFILE (yason:parse): objects become hash
   tables and arrays become lists. Errors if the file doesn't exist"
  (with-a-file afile :r
    (yason:parse stream!)))

(defmacro make-octet-vector (n)
  "A fresh vector of N octets -- element type (unsigned-byte 8)"
  `(make-array ,n :element-type '(unsigned-byte 8)))

(defmacro concat-octet-vector (&rest everything)
  "The octet vectors EVERYTHING concatenated into one"
  `(concatenate '(vector (unsigned-byte 8)) ,@everything))

(abbr parse-html plump:parse)
(abbr $$ lquery:$)

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; filename/namestring escaping -------------------------- ;

; sbcl's namestrings arrive pre-escaped (?, *, [) — this variant
; compensates; the ecl variant below escapes from scratch
#+sbcl
(defun escape-namestring/shell (afilename)
  "AFILENAME (a namestring) backslash-escaped for the shell: spaces,
   quotes, globs, $, &, ;, parens and the rest. This is the SBCL
   version, which compensates for SBCL namestrings arriving with ?,
   * and [ already escaped. (q/sh -- single quotes, one rule -- is
   the newer way to quote anything for the shell)"
  (-<> afilename
       (~ra <> •\t• •\	•) (~ra <> • • •\ •) (~ra <> •;• •\;•)
       (~ra <> •\^• •\^•) (~ra <> •\$• •\$•) (~ra <> •\\\?• •\?•)
       ; (~ra <> •"• •\"•) (~ra <> •\\\[• •\[•) (~ra <> •\]• •\]•)
       (~ra <> •"• •\"•) (~ra <> •\[• •\[•) (~ra <> •\]• •\]•)
       (~ra <> •\)• •\)•) (~ra <> •\(• •\(•) (~ra <> •'• •\\'•)
       (~ra <> •<• •\<•) (~ra <> •>• •\>•) (~ra <> •!• •\!•)
       (~ra <> •\\\*• •\*•) (~ra <> •\&• •\\&•) (~ra <> •=• •\=•)
       (~ra <> •`• •\\`•) (~ra <> •\|• •\|•)))


#+ecl
(defun escape-namestring/shell (afilename)
  "AFILENAME (a namestring) backslash-escaped for the shell: spaces,
   quotes, globs, $, &, ;, parens and the rest. This is the ECL
   version, which escapes from scratch (ECL namestrings arrive
   unescaped). (q/sh -- single quotes, one rule -- is the newer way
   to quote anything for the shell)"
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
  "AFILENAME (a namestring) as the exact on-disk name a C function
   expects: undoes the backslashes SBCL puts before ?, * and [ in
   its namestrings. A regex-based predecessor of pluto's
   pathname->native, which does this properly"
  (-<> afilename
       (~ra <> •\\• •\\\•) (~ra <> •\?• •\?•) (~ra <> •\[• •\[•)
       (~ra <> •\\\\\*• •*•) (~ra <> •\\\\\\\[• •[•) (~ra <> •\\\\\\• "")))

#+ecl
(defun escape-namestring/c (afilename)
  "AFILENAME (a namestring) as the exact on-disk name a C function
   expects. ECL namestrings aren't escaped to begin with, so this
   version has little to undo. A regex-based predecessor of pluto's
   pathname->native, which does this properly"
  (-<> afilename
       (~ra <> •\\\\\*• •*•) (~ra <> •\\\\\\\[• •[•) (~ra <> •\\\\\\• "")))

(defun pathname->shell (apath)
  "The pathname APATH as a namestring escaped for the shell
   (escape-namestring/shell)"
  (escape-namestring/shell (namestring apath)))

(defun pathname->c (apath)
  "The pathname APATH as the string a C function expects
   (escape-namestring/c) -- pluto's pathname->native is the
   better tool for this"
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


