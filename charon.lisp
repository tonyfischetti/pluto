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
  (:use :common-lisp :pluto)
  (:import-from :parse-float :parse-float)
  (:export
    ; regular expressions / cl-ppcre wrappers
    :re-compile :str-split :str-replace :str-replace-all :str-detect
    :str-subset :str-extract :str-scan-to-strings :str-trim :~m :~r
    :~ra :~s :~f :~c :~e

    ; terminal things / terminal manipulation
    ; :with-loading

    ))

(use-package :pluto) ;????
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
#-clisp
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
  ; TODO: what does clisp do with with-gensyms?
  ; (with-gensyms (tmp long-thread loading-thread wrapper the-return)
  (let ((tmp (gensym))
        (long-thread (gensym))
        (loading-thread (gensym))
        (wrapper (gensym))
        (the-return (gensym)))
    `(progn
       (defun ,tmp ()
         ,@body)
       (let ((,long-thread      (bt:make-thread #',tmp
                                                :name "long-thread"))
             (,loading-thread   (bt:make-thread #'loading-forever
                                                :name "loading-thread")))
         (setq ,the-return (bt:join-thread ,long-thread))
         (bt:destroy-thread ,loading-thread)
         (terpri)
         ,the-return))))

; ------------------------------------------------------- ;

    (for-each `(heaven or las vegas)
      (ft •processing: ~10A~C• value! #\Tab)
      (with-loading
        (sleep 3)))
