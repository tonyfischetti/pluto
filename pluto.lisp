;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                            ;;
;;  Pluto                                                     ;;
;;    a common lisp library that's out there                  ;;
;;                                                            ;;
;;              Tony Fischetti                                ;;
;;              tony.fischetti@gmail.com                      ;;
;;                                                            ;;
;;              License: GPL-3                                ;;
;;                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clisp (unuse-package :ext)

(defpackage :pluto
  (:use :common-lisp)
  (:export

    ; pluto parameters
    :*pluto-output-stream* :*pluto-log-level* :*pluto-log-file*
    :*pluto-curly-test* :*pluto-external-format* :*pluto-shell*
    :*unix-epoch-difference* :*whitespaces*

    ; formatting
    :fn :ft

    ; ansi colors and codes
    :make-ansi-escape :+reset-terminal-color+ :+magenta-bold+ :+red-bold+
    :+yellow-bold+ :+green-bold+ :+cyan-bold+ :+blue-bold+ :+grey-bold+
    :+ansi-escape-up+ :+ansi-escape-left-all+ :+ansi-escape-left-one+
    :magenta :red :yellow :green :cyan :blue :grey

    ; string operations
    :str+ :str-join :substr :string->char-list :split-string->lines

    ; some essential utilities/macros
    :with-gensyms :mac :nil! :alambda :self! :abbr :flatten :take :group
    :create-symbol :create-keyword :walk-replace-sexp :-<> :<> :aif :it!
    :slurp :slurp-lines :barf :debug-these :with-a-file :stream! :interpose
    :delim :defparams :round-to :advise :alistp :with-hash-entry :entry!
    :if-hash-entry :if-not-hash-entry :capture-all-outputs :with-temp-file
    :tempfile!

    ; queries
    :y-or-n-def

    ; error handling
    :die :or-die :or-do :die-if-null :error!
    ; reader macros
    ; #? Ø ? «

    ; time
    :universal->unix-time :unix->universal-time :get-unix-time
    :make-pretty-time :get-current-time :with-time :time-for-humans :time!

    ; for-each and friends
    :with-interactive-interrupt-handler
    :progress :break! :continue! :index! :value! :key! :for-each/line
    :for-each/list :for-each/hash :for-each/vector :for-each/stream
    :for-each/alist :for-each/call :for-each :forever

    ; shell and zsh
    :zsh :sh :zsh-simple :sh-simple

    ; other abbreviations and shortcuts
    :λ

    ; system
    :hostname :sys/info :get-envvar


    ; command-line arguments
    :program/script-name :cmdargs :def-cli-args :args! :bare-args!
    :+USAGE-TEXT!+ :print-usage! :process-args!

    ; terminal things / terminal manipulation
    :clear-screen
    :get-terminal-columns :ansi-up-line :ansi-left-all :ansi-clear-line
    :ansi-left-one :progress-bar :loading-forever :with-loading :give-choices

    ; filename operations
    :remove-extension :basename :pwd :realpath :file-size

    ;; file-related functions
    :inspect-pathname   ; TODO: TMP!!
    :ls :directory-exists-p :file-exists-p :file-or-directory-exists-p
    :walk-directory :file-find :-path

    ))

(in-package :pluto)

(pushnew :pluto *features*)


;---------------------------------------------------------;
; pluto parameters ---------------------------------------;

(defparameter *pluto-output-stream*    *terminal-io*)
(defparameter *pluto-log-level*        2)
(defparameter *pluto-log-file*         "pluto-log.out")
(defparameter *pluto-curly-test*       #'equal)
; TODO: implementation dependent
(defparameter *pluto-external-format*  #+clisp CHARSET:UTF-8 #-clisp :UTF-8)
(defparameter *pluto-shell*            "/usr/local/bin/zsh")

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

;---------------------------------------------------------;


;---------------------------------------------------------;
; formatting ---------------------------------------------;

(defmacro fn (&rest everything)
  "Alias to (format nil ...)"
  `(format nil ,@everything))

(defmacro ft (&rest everything)
  "Alias to (format t ...)"
  `(format t ,@everything))

;---------------------------------------------------------;


;---------------------------------------------------------;
; ansi colors and codes ----------------------------------;

(defun make-ansi-escape (anum &optional (decoration 'bold))
  (format nil "~c[~A~Am" #\ESC anum (cond
                                      ((eq decoration 'bold)        ";1")
                                      ((eq decoration 'underline)   ";4")
                                      ((eq decoration 'reversed)    ";7")
                                      (t                            ""))))

(defvar +reset-terminal-color+  (make-ansi-escape 0 nil))
(defvar +magenta-bold+          (make-ansi-escape 35))
(defvar +red-bold+              (make-ansi-escape 31))
(defvar +yellow-bold+           (make-ansi-escape 33))
(defvar +green-bold+            (make-ansi-escape 32))
(defvar +cyan-bold+             (make-ansi-escape 36))
(defvar +blue-bold+             (make-ansi-escape 34))
(defvar +grey-bold+             (make-ansi-escape 90))

(defvar +ansi-escape-up+        (format nil "~c[1A" #\ESC))
(defvar +ansi-escape-left-all+  (format nil "~c[500D" #\ESC))
(defvar +ansi-escape-left-one+  (format nil "~c[1D" #\ESC))

(defun do-with-color (acolor thestring &rest everything)
  (apply #'format nil
         (format nil "~A~A~A" acolor thestring +reset-terminal-color+)
         everything))

(defmacro make-color-fun (thename acolor)
  `(defun ,thename (thestring &rest things)
     (apply #'do-with-color ,acolor thestring things)))

(make-color-fun magenta +magenta-bold+)
(make-color-fun red     +red-bold+)
(make-color-fun yellow  +yellow-bold+)
(make-color-fun green   +green-bold+)
(make-color-fun cyan    +cyan-bold+)
(make-color-fun blue    +blue-bold+)
(make-color-fun grey    +grey-bold+)

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; string operations ------------------------------------- ;

(defun str+ (&rest args)
  "Combine (using princ) an arbitrary number of args into one string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun str-join (delim strings)
  "Join STRINGS with DELIM."
  (format nil (format nil "~~{~~A~~^~A~~}" delim) strings))

; TODO: did I take this from somewhere
(defun substr (string start &optional end)
  "Efficient substring of STRING from START to END (optional),
  where both can be negative, which means counting from the end."
  (let ((len (length string)))
    (subseq string
            (if (minusp start) (+ len start) start)
            (if (and end (minusp end)) (+ len end) end))))

; TODO: check all for undocumented

(defun string->char-list (astring)
  "Make a string a list of single character strings"
  (map 'list #'string astring))

(defun split-string->lines (astring)
  "Split a string with new lines into a list of strings (one for each line)"
  (with-input-from-string (s astring)
    (loop for value = (read-line s nil)
          while value collect value)))

; ------------------------------------------------------- ;


;---------------------------------------------------------;
; some essential utilities/macros ------------------------;

; TODO: Do all of these need to be in _this_ section?

(defmacro with-gensyms ((&rest names) &body body)
  "Why mess with the classics"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

; Stolen from "On Lisp"
(defmacro mac (sexp)
  "Let's you do `(mac (anunquotesmacro))`"
  `(pprint (macroexpand-1 ',sexp)))

; Adapted from "On Lisp"
(defmacro nil! (&rest rest)
  "Sets all the arguments to nil"
  (let ((tmp (mapcar (lambda (x) `(setf ,x nil)) rest)))
    `(progn ,@tmp)))

; I forgot where I stole this from
(defmacro alambda (params &body body)
  "Anaphoric lambda. SELF! is the function"
  `(labels ((self! ,params ,@body))
     #'self!))

(defmacro abbr (short long)
  `(defmacro ,short (&rest everything)
     `(,',long ,@everything)))

(defun flatten (alist)
  " Flattens a list (possibly inefficiently)"
  (if alist
    (if (listp (car alist))
      (append (flatten (car alist)) (flatten (cdr alist)))
      (cons (car alist) (flatten (cdr alist))))))

(defun take (alist n &optional (acc nil))
  "Takes `n` from beginning of `alist` and returns that in a
   list. It also returns the remainder of the list (use
   `multiple-value-bind` with it"
  (when (and (> n 0) (null alist)) (error "not enough to take"))
  (if (= n 0)
    (values (nreverse acc) alist)
    (take (cdr alist) (- n 1) (cons (car alist) acc))))

(defun group (alist &optional (n 2) (acc nil))
  "Turn a (flat) list into a list of lists of length `n`"
  (if (null alist)
    (nreverse acc)
    (multiple-value-bind (eins zwei) (take alist n)
      (group zwei n (cons eins acc)))))

(defun create-symbol (&rest args)
  "Interns an string as a symbol."
  (values (intern (string-upcase (apply #'str+ args)))))

(defun create-keyword (&rest args)
  "Interns an UP-cased string as a keyword symbol."
  (values (intern (string-upcase (apply #'str+ args)) :keyword)))

(defun walk-replace-sexp (alist oldform newform &key (test #'equal))
  "Walks sexpression substituting `oldform` for `newform`.
   It works with lists and well as atoms. Checks equality with `test`
   (which is #'EQUAL by default)"
  (if alist
    (let ((thecar (car alist)))
      (if (listp thecar)
        (if (tree-equal thecar oldform :test test)
          (cons newform (walk-replace-sexp (cdr alist) oldform newform :test test))
          (cons (walk-replace-sexp thecar oldform newform :test test)
                (walk-replace-sexp (cdr alist) oldform newform :test test)))
        (let ((rplment (if (funcall test thecar oldform) newform thecar)))
          (cons rplment (walk-replace-sexp (cdr alist) oldform newform :test test)))))))

(defmacro -<> (expr &rest forms)
  "Threading macro (put <> where the argument should be)
   Stolen from https://github.com/sjl/cl-losh/blob/master/src/control-flow.lisp"
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))

(defmacro aif (test then &optional else)
  "Like IF. IT is bound to TEST."
  `(let ((it! ,test))
     (if it! ,then ,else)))

; TODO: IS IT MAYBE BIGGER THAN IT NEEDS TO BE BECAUSE MULTIBYTE?
(defun slurp (path)
  "Reads file at PATH into a single string"
  (with-open-file (stream path :if-does-not-exist :error)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

; TODO: what?
(defun slurp-stream (astream)
  (with-open-stream (s astream)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defun slurp-lines (afilename)
  "Reads lines of a file into a list"
  (with-open-file (tmp afilename :if-does-not-exist :error
                       :external-format *pluto-external-format*)
    (loop for value = (read-line tmp nil)
          while value collect value)))

(defun barf (path contents &key (printfn #'write-string) (overwrite nil))
  "Outputs CONTENTS into filename PATH with function PRINTFN
   (default WRITE-STRING) and appends by default (controllable by
   by boolean OVERWRITE)"
  (with-open-file (stream path :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create)
    (funcall printfn contents stream)))

(defmacro debug-these (&rest therest)
  "Macro that takes an arbitrary number of arguments,
  prints the symbol, and then prints it's evaluated value
  (for debugging)"
  (flet ((debug (this)
      `(format *error-output* "~20S -> ~S~%" ',this ,this)))
    `(progn ,@(mapcar #'debug therest))))

; TODO: use gensyms?
; TODO: implementation specific
(defmacro with-a-file (filename key &body body)
  "Anaphoric macro that binds `stream!` to the stream
   First argument is the filename
   The second argument is one of
     `:w` - write to a file  (clobber if already exists)
     `:a` - append to a file (create if doesn't exist)
     `:r` - read a file      (in text mode)
     `:b` - read a file      (in binary mode [unsigned-byte 8])
    Only provide one of these arguments"
   (let ((dir (cond
                ((eq key :w) :output)       ((eq key :a) :output)
                ((eq key :r) :input)        ((eq key :b) :input)))
         (iex (cond
                ((eq key :w) :supersede)    ((eq key :a) :append)
                ((eq key :r) :append)       ((eq key :b) :append))))
    `(with-open-file (stream! ,filename :direction ,dir :if-exists ,iex
                              ,@(when (eq key :b)
                                  `(':element-type 'unsigned-byte))
                              :if-does-not-exist :create
                              :external-format *pluto-external-format*)
       ,@body)))

(defun interpose (separator list)
  "Returns a sequence of the elements of SEQUENCE separated by SEPARATOR."
  (labels
    ((rec (s acc) (if s
                    (rec (cdr s) (nconc acc (list separator (car s))))
                    acc)))
    (cdr (rec list nil))))

(defun delim (anlist &key (what :list) (sep #\Tab))
  "Makes a string with tabs separating values.
   `:what` either :list :listoflist :hash or :alist
   `:sep` the (CHARACTER) separator to use (default is tab)"
  (labels ((join-with-sep      (x) (str-join (format nil "~C" sep) x))
           (join-with-newlines (x) (str-join (format nil "~C" #\Newline) x)))
    (cond
      ((eq :list what)   (str-join (format nil "~C" sep) anlist))
      ((eq :alist what)  (join-with-newlines (loop for i in anlist
                                                   for key = (car i)
                                                   for val = (cdr i)
                                                   collect (join-with-sep
                                                             (list key val)))))
      ((eq :listoflists what)
                         (join-with-newlines (loop for i in anlist
                                                   collect (join-with-sep i))))
      ((eq :hash what)   (join-with-newlines (loop for key being the hash-keys
                                                     in anlist
                                                   using (hash-value val)
                                                   collect (join-with-sep
                                                             (list key val)))))
      (t                 (error "unsupported type")))))

(defmacro defparams (&body body)
  "Declares the arguments to by special defparameter parameters
   with a value on `nil`"
  (labels ((helper (alist)
              (loop for i in alist collect `(defparameter ,i nil))))
    (let ((tmp (helper body)))
     `(progn  ,@tmp))))

(defun round-to (number precision &optional (what #'round))
  "Round `number` to `precision` decimal places. Stolen from somewhere"
  (let ((div (expt 10 precision)))
    (float (/ (funcall what (* number div)) div))))

(defun advise (message &key (yellow-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* but resumes
   (for use with OR-DIE's ERRFUN)"
   (format *error-output* "~A~A~A~%" (if yellow-p +yellow-bold+ "")
                                     message
                                     (if yellow-p +reset-terminal-color+ "")))

; TODO: more we can test
(defun alistp (something)
  "Test is something is an alist"
  (and (listp something)
       (every #'consp something)))

(defmacro with-hash-entry ((ahash akey) &body body)
  "Establishes a lexical environment for referring to the _value_ of
   key `akey` on the hash table `ahash` using the anaphor `entry!`.
   So, you can setf `entry!` and the hash-table (for that key) will
   by modified."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (symbol-macrolet
         ((entry! (gethash ,thekey ,thehash)))
         ,@body))))

; TODO: GOTTA BE A BETTER WAY!
(defmacro if-hash-entry ((ahash akey) then &optional else)
  "Executes `then` if there's a key `akey` in hash-table `ahash` and
   `else` (optional) if not. For convenience, an anaphor `entry!` is
   introduced that is setf-able."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (with-hash-entry (,thehash ,thekey)
         (if entry! ,then ,else)))))

(defmacro if-not-hash-entry ((ahash akey) then &optional else)
  "Executes `then` if there is _NOT_ key `akey` in hash-table `ahash` and
   `else` (optional) if exists. For convenience, an anaphor `entry!` is
   introduced that is setf-able."
  (with-gensyms (thehash thekey)
    `(let ((,thehash ,ahash)
           (,thekey  ,akey))
       (with-hash-entry (,thehash ,thekey)
         (if (not entry!) ,then ,else)))))

(defun |•-reader| (stream char)
  "Alternate double quote"
  (declare (ignore char))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((char= curr #\•) (push prev chars))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-macro-character #\• #'|•-reader|)

; TODO: document
(defmacro capture-all-outputs (&body body)
  (let ((ret (gensym)))
    `(let ((*standard-output*   (make-string-output-stream))
           (*error-output*      (make-string-output-stream)))
       (let ((,ret (funcall ,@body)))
         (values ,ret
                 (get-output-stream-string *standard-output*)
                 (get-output-stream-string *error-output*))))))

; TODO: document
; change rm to something else
#+coreutils
(defmacro with-temp-file (&body body)
  `(let ((tempfile! (zsh "mktemp")))
     (unwind-protect
       (progn
         ,@body)
         (zsh (fn "rm ~A" tempfile!)))))

; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; Queries ------------------------------------------------;

(defun yond-read-char (&key (default nil))
  (clear-input *query-io*)
  (let ((tmp (read-char *query-io*)))
    (when (and default (char= tmp #\Newline))
      (setq tmp default))
    (clear-input *query-io*)
    tmp))

(defun yond-prompt (prompt &key (default nil))
  (fresh-line *query-io*)
  (format *query-io* "~A ~A " prompt
          (if default
            (cond ((or (char= default #\y) (char= default #\Y)) "[Y/n]")
                  ((or (char= default #\N) (char= default #\n)) "[y/N]")
                  (t "(y or n)"))
            "(y or n)"))
  (finish-output *query-io*))

; TODO: document
(defun y-or-n-def (prompt &key (default nil))
  (if default
    (loop (yond-prompt prompt :default default)
      (case (yond-read-char :default default)
        ((#\Y #\y) (return t))
        ((#\N #\n) (return nil))
        (t (format *query-io* "~&try again~%"))))
    (y-or-n-p prompt)))

; ------------------------------------------------------- ;


;---------------------------------------------------------;
; Error handling -----------------------------------------;

; TODO: implementation specific
(defun die (message &key (status 1) (red-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* and quits with a STATUS (default 1)"
  (format *error-output* "~A~A~A~%" (if red-p +red-bold+ "")
                                      (fn message)
                                    (if red-p +reset-terminal-color+ ""))
  #+sbcl            (sb-ext:quit :unix-status status)
  #+(or ecl clisp)  (ext:exit status)
  #+abcl            (ext:exit :status status))

(defmacro or-die ((message &key (errfun #'die)) &body body)
  "anaphoric macro that binds ERROR! to the error
   It takes a MESSAGE with can include ERROR! (via
   (format nil...) for example) It also takes ERRFUN
   which it will FUNCALL with the MESSAGE. The default
   is to DIE, but you can, for example, PRINC instead"
  `(handler-case
     (progn
       ,@body)
     (error (error!)
       (funcall ,errfun (format nil "~A" ,message)))))

(defmacro or-do (orthis &body body)
  "anaphoric macro that binds ERROR! to the error.
   If the body fails, the form ORTHIS gets run."
  `(handler-case
     (progn
       ,@body)
      (error (error!)
        ,orthis)))

(defmacro die-if-null (avar &rest therest)
  "Macro to check if any of the supplied arguments are null"
  (let ((whole (cons avar therest)))
    `(loop for i in ',whole
           do (unless (eval i)
                (die (format nil "Fatal error: ~A is null" i))))))

(defun ignore-the-errors-wrapper (stream char arg)
  (declare (ignore char))
  (declare (ignore arg))
  (let ((sexp (read stream t)))
    `(ignore-errors ,sexp)))

(set-dispatch-macro-character #\# #\? #'ignore-the-errors-wrapper)

(defun |ensure-not-null| (stream char)
  "Reader macro to check if symbol is null,
   otherwise, pass it on"
  (declare (ignore char))
  (let ((sexp (read stream t)))
    `(progn
       (aif (eval ',sexp)
            it!
            (error "its null")))))

(set-macro-character #\Ø #'|ensure-not-null|)

(defun |if-null->this| (stream char)
  "Reader macro that takes two s-expressions.
   If the first evaluates to not null, it is returned.
   If the first evaluates to null, the second s-expression is returned"
  (declare (ignore char))
  (let ((sexp (read stream t))
        (replacement (read stream t))
        (res  (gensym)))
    `(let ((,res ,sexp))
       (if ,res ,res ,replacement))))

(set-macro-character #\? #'|if-null->this|)

(defun |«-reader| (stream char)
  "Examples:
     « (/ 3 1) or die error! »        ; returns 3
     « (/ 3 0) or warn error! »       ; stderrs error, continues, and returns NIL
     « (/ 3 0) or die error! »        ; dies with error message
     « 3 or die error! »              ; returns 3
     « nil or die error! »            ; dies because atom preceding `or` is NIL
     « 3 or do (format t •no~%•)! »   ; returns 3
     « nil or do (format t •no~%•) »  ; prints 'no'"
  (declare (ignore char))
  (let ((err-mess     "« reader macro not written to specification")
        (ender        "»")
        (before       (read stream))
        (theor        (read stream))
        (theoperator  (read stream))
        (after        (read stream))
        (theend-p     (symbol-name (read stream))))
    ; syntax error checking
    (unless (string= theend-p ender) (die err-mess))
    (unless (string= (symbol-name theor) "OR") (die err-mess))
    (cond
      ((consp before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(or-die (,after) ,before))
         ((string= "WARN" (symbol-name theoperator))
           `(or-die (,after :errfun #'advise) ,before))
         ((string= "DO" (symbol-name theoperator))
           `(or-do ,after ,before))))
      ((atom before)
       (cond
         ((string= "DIE" (symbol-name theoperator))
           `(if ,before ,before (die ,after)))
         ((string= "WARN" (symbol-name theoperator))
           `(if ,before ,before (advise ,after)))
         ((string= "DO" (symbol-name theoperator))
           `(if ,before ,before ,after)))))))

(set-macro-character #\« #'|«-reader|)

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; what's the deal with T I M E ? ------------------------ ;

(defun universal->unix-time (universal-time)
  "Converts universal (common lisp time from `(get-universal-time)`
   to UNIX time"
  (- universal-time *unix-epoch-difference*))

(defun unix->universal-time (unix-time)
  "Converts UNIX time to universal (common lisp time from
   `(get-universal-time)`"
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Get current UNIX time"
  (universal->unix-time (get-universal-time)))

(defun make-pretty-time (a-unix-time &key (just-date nil) (just-time nil)
                                          (time-sep ":") (dt-sep " "))
  "Makes a nicely formatted (YYYY-MM-DD HH?:MM:SS) from a UNIX time
   `just-date` will return just the pretty date
   `just-time` will return just the pretty time
   `time-sep`  will use the supplied string to separate the hours
   minutes and seconds (default ':')
   `dt-sep` will use the supplied string to separate the date from the time
   (default ' ')"
  (let ((thisuniversaltime (unix->universal-time a-unix-time)))
    (multiple-value-bind (second minute hour date month year)
      (decode-universal-time thisuniversaltime)
      (if (and (not just-date) (not just-time))
        (format nil "~d-~2,'0d-~2,'0d~A~d~A~2,'0d~A~2,'0d"
                year month date dt-sep hour TIME-SEP minute TIME-SEP second)
        (if just-date
          (format nil "~d-~2,'0d-~2,'0d" year month date)
          (format nil "~d~A~2,'0d~A~2,'0d"
                  hour TIME-SEP minute TIME-SEP second))))))

(defun get-current-time (&key (just-date nil) (just-time nil)
                              (time-sep ":") (dt-sep " "))
  "Uses `make-pretty-time` to get the current datetime"
  (make-pretty-time (-<> (get-universal-time) universal->unix-time)
                    :just-date just-date :just-time just-time
                    :time-sep time-sep :dt-sep dt-sep))

(defmacro with-time (&body aform)
  "Anaphoric macro that executes the car of the body and
   binds the seconds of execution time to TIME!. Then
   all the other forms in the body are executed"
  (let ((began      (gensym))
        (ended      (gensym))
        (ret        (gensym)))
    `(let (,began ,ended time!)
       (setq ,began (get-universal-time))
       (setq ,ret ,(car aform))
       (setq ,ended (get-universal-time))
       (setq time! (- ,ended ,began))
       ,@(cdr aform)
       ,ret)))

(defun time-for-humans (seconds)
  "Converts SECONDS into minutes, hours, or days (based on magnitude)"
  (cond
    ((> seconds 86400)        (format nil "~$ days" (/ seconds 86400)))
    ((> seconds 3600)         (format nil "~$ hours" (/ seconds 3600)))
    ((> seconds 60)           (format nil "~$ minutes" (/ seconds 60)))
    ((< seconds 60)           (format nil "~A seconds" seconds))))

;---------------------------------------------------------;


;---------------------------------------------------------;
; for-each and friends  ----------------------------------;

(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *error-output*)
                       (newline-p t))
  (when (= 0 (mod index interval))
    (format where (yellow "~A of ~A..... [~$%]"
                          index limit (* 100 (/ index limit))))
    (when newline-p (format where "~%"))))

(defmacro break! ()
  "For use with `for-each`
   It's short for `(return-from this-loop!"
  `(return-from this-loop!))

(defmacro continue! ()
  "For use with `for-each`
   It's short for `(return-from this-pass!"
  `(return-from this-pass!))

; TODO: implementation dependent
(defmacro with-interactive-interrupt-handler (the-message &body body)
  `(handler-case
     (progn
       ,@body)
       (#+sbcl    sb-sys:interactive-interrupt
        #+ecl     ext:interactive-interrupt
        #+clisp   system::simple-interrupt-condition
        #+ccl     ccl:interrupt-signal-condition
        #+allegro	excl:interrupt-signal
         () (die ,the-message))))

; TODO: external format doesn't work on clisp
(defmacro for-each/line (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((resolved-fn            (gensym))
        (instream               (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
       (let ((index!            0)
             (value!            nil)
             (,resolved-fn      ,a-thing))
         (declare (ignorable value!))
         (with-open-file (,instream ,resolved-fn :if-does-not-exist :error
                                    :external-format *pluto-external-format*)
           (block this-loop!
              (loop for value! = (read-line ,instream nil)
                    while value! do (progn
                                      (incf index!)
                                      (block this-pass! ,@body)))))))))

(defmacro for-each/list (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-list         (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
      (let ((index!       0)
             (value!      nil)
             (,the-list   ,a-thing))
         (declare (ignorable value!))
         (block this-loop!
                (dolist (value! ,the-list)
                  (incf index!)
                  (block this-pass! ,@body)))))))

(defmacro for-each/hash (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-hash         (gensym))
        (tmp              (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
       (let ((index!      0)
             (key!        nil)
             (value!      nil)
             (,the-hash   ,a-thing))
         (declare (ignorable value!))
         (declare (ignorable key!))
         (block this-loop!
                (loop for ,tmp being the hash-keys of ,the-hash
                      do (progn (incf index!)
                                (setq key! ,tmp)
                                (setq value! (gethash key! ,the-hash))
                                (block this-pass! ,@body))))))))

(defmacro for-each/vector (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((the-vector       (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
      (let ((index!       0)
            (value!      nil)
            (,the-vector ,a-thing))
         (declare (ignorable value!))
         (block this-loop!
                (loop for value! across ,the-vector
                      do (progn
                           (incf index!)
                           (block this-pass! ,@body))))))))

; TODO: use unwind-protect?
(defmacro for-each/stream (the-stream &body body)
  "(see documentation for `for-each`)"
  (let ((instream               (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
       (let ((index!            0)
             (value!            nil)
             (,instream         ,the-stream))
           (declare (ignorable value!))
           (block this-loop!
              (loop for value! = (read-line ,instream nil)
                    while value! do (progn
                                      (incf index!)
                                      (block this-pass! ,@body))))))))

(defmacro for-each/alist (aalist &body body)
  "This works like `for-each/hash` (see documentation for `for-each`)
  but it has to be called explicitly (as `for-each/alist`) instead
  of relying on `for-each`'s 'dispatch' mechanism."
  (let ((tmp          (gensym))
        (resolved     (gensym)))
    `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
      (let ((index!         0)
            (key!           nil)
            (value!         nil)
            (,resolved      ,aalist))
         (declare (ignorable value!))
         (declare (ignorable key!))
         (block this-loop!
                (loop for ,tmp in ,resolved
                      do (progn
                           (incf index!)
                           (setq key! (car ,tmp))
                           (setq value! (cdr ,tmp))
                           (block this-pass! ,@body))))))))

(defmacro for-each/call (aclosure &body body)
  "This works like `for-each` (see documentation for it) but
   due to differences, it is not automatically dispatched so
   if always needs to be called explicitly). It's only
   argument (besides the body) is a closure that is repeatedly
   `FUNCALL`ed and terminates when the closure returns NIL"
  `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
     (let ((index!      0)
           (value!      nil))
       (declare (ignorable value!))
       (block this-loop!
              (loop for value! = (funcall ,aclosure)
                    while value!
                    do (progn (incf index!)
                              (block this-pass! ,@body)))))))

; TODO: weird sbcl compiler notes
(defmacro for-each (a-thing &body body)
  "A super-duper imperative looping construct.
   It takes either
     a filename string    (to be treated as a file and goes line by line)
     a hash-table
     a vector
     a list
     a string             (that goes character by character)
     or a stream          (that goes line by line)
  It is anaphoric and introduces
     index!               (which is a zero indexed counter of which element we are on)
     key!                 (the key of the current hash-table entry [only for hash-tables and alists])
     value!               (the value of the current element)
     this-pass!           (a block that returning from immediately moves to the next iteration)
     this-loop!           (a block that returning from exits the loop)
  For convenience, (continue!) and (break!) will execute (return-from this-pass!)
  and (return-from this-loop!), respectively
  If it's a filename, the external format is *pluto-external-format* (:UTF-8 by default)
  Oh, it'll die gracefully if Control-C is used during the loops execution.
  And, finally, for extra performance, you can call it's subordinate functions directly.
  They are... for-each/line, for-each/list, for-each/hash, for-each/vector,
  for-each/stream, and for-each/alist"
  (let ((tmp (gensym)))
    `(let ((,tmp ,a-thing))
      (cond
        ((and (stringp ,tmp) (probe-file ,tmp))
           (for-each/line ,tmp ,@body))
        (t
          (progn
            (etypecase ,tmp
              (hash-table     (for-each/hash      ,tmp      ,@body))
              (vector         (for-each/vector    ,tmp      ,@body))
              (list           (for-each/list      ,tmp      ,@body))
              (stream         (for-each/stream    ,tmp      ,@body)))))))))

(defmacro forever (&body body)
  "Performed BODY forever. Must be terminated by
   RETURN-FROM NIL, or, simple RETURN
   Simple wrapper around `(loop (progn ,@body))`"
  `(with-interactive-interrupt-handler "~%Loop aborted. Bailing out.~%"
     (block nil (loop (progn ,@body)))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; universal indexing operator syntax -------------------- ;

(defun |{-reader| (stream char)
  (declare (ignore char))
  (let ((inbetween nil))
    (let ((chars nil))
      (do ((prev (read-char stream) curr)
           (curr (read-char stream) (read-char stream)))
          ((char= curr #\}) (push prev chars))
        (push prev chars))
      (setf inbetween (coerce (nreverse chars) 'string)))
    (let ((leido (read-from-string (fn "(~A)" inbetween))))
      `(pluto-get ,@leido))))

(defmethod get-at ((this list) that)
  (cond ((alistp this)        (cdr (assoc that this :test *pluto-curly-test*)))
        (t                    (nth that this))))

(defmethod get-at ((this vector) that)
  (aref this that))

(defmethod get-at ((this hash-table) that)
  (gethash that this))

(defmethod get-at ((this structure-object) that)
  (slot-value this that))

(defmethod get-at ((this standard-object) that)
  (slot-value this that))

; TODO: this
; (defmethod get-at ((this RUNE-DOM::DOCUMENT) that)
;   (xpath this that))

(set-macro-character #\{ #'|{-reader|)

(defun (setf get-at) (new this that)
  (cond
    ((simple-vector-p this)         (setf (svref this that) new))
    ((vectorp this)                 (setf (aref this that) new))
    ((hash-table-p this)            (setf (gethash that this) new))
    ((alistp this)                  (setf (cdr (assoc that this
                                                      :test *pluto-curly-test*))
                                          new))
    ((listp this)                   (setf (nth that this) new))
    ((typep this 'structure-object) (setf (slot-value this that) new))
    ((typep this 'standard-object)  (setf (slot-value this that) new))
    ))

(defmacro suc-apply (afun &rest rest)
  (let ((built      nil)
        (thing      (car rest))
        (thefirst   (cadr rest))
        (therest    (cddr rest)))
    (setq built (reduce (lambda (x y) `(,afun ,x ,y)) therest
                        :initial-value `(,afun ,thing ,thefirst)))
    built))

(defmacro pluto-get (x &rest rest)
  `(suc-apply get-at ,x ,@rest))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; shell and zsh ----------------------------------------- ;

; workarounds for clisp and abcl
(defun %slurp-stream-lines (astream)
  (loop for this = (read-line astream nil)
        while this collect this))

(defun %reconstruct-stream (lines)
  (let ((s (make-string-output-stream)))
    (for-each/list lines
      (format s "~A~%" value!))
    (substr (get-output-stream-string s) 0 -1)))

; TODO: a whole bunch
; TODO: implementation dependent
#+sbcl
(defun zsh (acommand &key (dry-run        nil)
                          (err-fun        #'(lambda (code stderr)
                                              (error (format nil "~A (~A)" stderr code))))
                          (echo           nil)
                          (enc            *pluto-external-format*)
                          (in             t)
                          (return-string  t)
                          (split          nil)
                          (interactive    nil))
  "Runs command `acommand` through the shell specified by the global *pluto-shell*
   `dry-run` just prints the command (default nil)
   `err-fun` takes a function that takes an error code and the STDERR output
   `echo` will print the command before running it
   `enc` takes a format (default is *pluto-external-format* [which is :UTF-8 by default])
   `in` t is inherited STDIN. nil is /dev/null. (default t)
   `return-string` t returns the output string. nil inherits stdout (default t)
   `split` will separate the stdout by newlines and return a list (default: nil)
   `interactive` will use the '-i' option to make the shell interactive (default: nil)"
  (flet ((strip (astring)
    (if (string= "" astring)
      astring
      (subseq astring 0 (- (length astring) 1)))))
    (when (or echo dry-run)
      (format t "$ ~A~%" acommand))
    (unless dry-run
      (let* ((arglist     `(,(if interactive "-ic" "-c")
                             ,(fn "~A;~A" acommand (if interactive "exit" ""))))
             (outs        (if return-string (make-string-output-stream) t))
             (errs        (make-string-output-stream))
             (theprocess
               (sb-ext:run-program *pluto-shell* arglist
                                   :input in :output outs :error errs
                                   :external-format enc))
             (retcode
               (sb-ext:process-exit-code theprocess)))
        (when (> retcode 0)
          (funcall err-fun retcode (strip (get-output-stream-string errs))))
        (when return-string
          (values (if split
                    (split-string->lines (get-output-stream-string outs))
                    (strip (get-output-stream-string outs)))
                  (strip (get-output-stream-string errs))
                  retcode))))))

; TODO: fill in doc string
#+ecl
(defun zsh (acommand &key (dry-run        nil)
                          (err-fun        #'(lambda (code stderr)
                                              (error (format nil "~A (~A)" stderr code))))
                          (echo           nil)
                          (return-string  t)
                          (split          nil)
                          (interactive  nil))
  (flet ((strip (astring)
    (if (string= "" astring)
      astring
      (subseq astring 0 (- (length astring) 1)))))
    (when (or echo dry-run)
      (format t "$ ~A~%" acommand))
    (unless dry-run
      (let* ((arglist     `(,(if interactive "-ic" "-c")
                             ,(fn "~A;~A" acommand (if interactive "exit" ""))))
             (outs        (if return-string (make-string-output-stream) t))
             (errs        (make-string-output-stream)))
        (multiple-value-bind (procstream retcode process)
              (ext:run-program *pluto-shell* arglist
                               :output outs :error errs)
              (ext:external-process-wait process)
              (when (> retcode 0)
                (funcall err-fun retcode (strip (get-output-stream-string errs))))
              (when return-string
                (values (if split
                          (split-string->lines (get-output-stream-string outs))
                          (strip (get-output-stream-string outs)))
                        (strip (get-output-stream-string errs))
                        retcode)))))))

#+ clisp
(defun zsh (acommand &key (dry-run      nil)
                          (err-fun      #'error)
                          (echo         nil)
                          (split        nil)
                          (interactive  nil))
  "Runs command `acommand` through the ZSH shell specified by the global *pluto-shell*
   `dry-run` just prints the command (default nil)
   `err-fun` takes a function that takes an error code and the STDERR output
   `echo` will print the command before running it
   `split` will separate the stdout by newlines and return a list (default: nil)
   `interactive` will use the '-i' option to make the shell interactive (default: nil)"
  (when (or echo dry-run)
    (format t "$ ~A~%" acommand))
  (unless dry-run
    (let* ((arglist `(,(if interactive "-ic" "-c")
                       ,(fn "~A;~A" acommand (if interactive "exit" "")))))
      (or-die ((fn "error <~A> with shell command <~A>" error! acommand)
               :errfun error)
        (with-open-stream
          (s (ext:run-program *pluto-shell*
                              :arguments arglist
                              :output :stream))
          (if split
            (%slurp-stream-lines s)
            (%reconstruct-stream (%slurp-stream-lines s))))))))

; TODO: write documentation
#+abcl
(defun zsh (acommand &key (dry-run        nil)
                          (err-fun        #'(lambda (code stderr)
                                              (error (format nil "~A (~A)" stderr code))))
                          (echo           nil)
                          (in             t)
                          (return-string  t)
                          (split          nil)
                          (interactive    nil))
  (when (or echo dry-run)
    (format t "$ ~A~%" acommand))
  (unless dry-run
    (let* ((arglist     `(,(if interactive "-ic" "-c")
                           ,(fn "~A;~A" acommand (if interactive "exit" ""))))
           (theprocess
             (system:run-program *pluto-shell* arglist :input in)))
      (system:process-wait theprocess)
      (let ((retcode (system:process-exit-code theprocess))
            (outstream (system:process-output theprocess))
            (errstream (system:process-error theprocess)))
        (when (> retcode 0)
          (funcall err-fun retcode "returns non 0 exit code"))
        (when return-string
          (values (if split
                    (%slurp-stream-lines outstream)
                    (%reconstruct-stream (%slurp-stream-lines outstream)))
                  retcode))))))

; TODO: have a not-implememted error

#+(or sbcl ecl clisp abcl)
(setf (fdefinition 'sh) #'zsh)

; TODO document
(defmacro sh-simple (acommand)
  #+sbcl
  `(sb-ext:run-program *pluto-shell* `("-c" ,,acommand))
  #+ecl
  `(ext:run-program *pluto-shell* `("-c" ,,acommand))
  #+clisp
  `(ext:run-program *pluto-shell* :arguments `("-c" ,,acommand))
  )

#+(or sbcl ecl clisp)
(abbr zsh-simple sh-simple)

; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; other abbreviations and shortcuts --------------------- ;

(defmacro λ (&body body)
  `(lambda ,@body))

(defun %remove-after-first-whitespace (astring)
  (let ((pos1 (position-if (lambda (x) (member x *whitespaces*)) astring)))
    (substr astring 0 pos1)))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; system -------------------------------------------------;

; TODO: USE UIOP FOR THESE?!

(defun hostname ()
  #+clisp (%remove-after-first-whitespace (machine-instance))
  #+(or abcl clasp clozure cmucl ecl genera
        lispworks mcl mezzano mkcl sbcl scl xcl) (machine-instance))

; TODO: beef out
; TODO: distro
(defun sys/info ()
  (let ((kernel         (zsh "uname -s"))
        (os             (zsh "uname -o"))
        (hostname       (hostname))
        (architecture   (zsh "uname -m")))
    (let ((info
            `((:kernel . ,(cond ((string= kernel "Linux")  :linux)
                               ((string= kernel "Darwin") :darwin)
                               (t                         :unknown)))
              (:os . ,(cond ((string= os "GNU/Linux") :gnu/linux)
                            ((string= os "Darwin")     :darwin)
                            ((string= os "Android")    :android)
                            (t                        :unknown)))
              (:hostname . ,hostname)
              (:architecture . ,(cond ((search "x86" architecture)   :x86)
                                      ((search "arm" architecture)   :arm))))))
      (when (eq (cdr (assoc :os info))  :gnu/linux)
        (push `(:distro . ,(create-keyword (zsh "lsb_release -a 2> /dev/null | head -n 1 | awk '{ print $3 }'")))
              info))
      info)))

; TODO: write documentation
; TODO: implementation dependent
; TODO: SBCL DOESN't get columns
; TODO: https://stackoverflow.com/questions/44236376/how-do-i-get-the-list-of-all-environment-variables-available-in-a-lisp-process
(defun get-envvar (name &optional default)
    #+cmu
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-cmu
    (or
     #+sbcl (sb-ext:posix-getenv name)
     #+(or abcl clasp clisp ecl xcl) (ext:getenv name)
     #+allegro (sys:getenv name)
     #+lispworks (lispworks:environment-variable name)
     default))

; ------------------------------------------------------- ;




;---------------------------------------------------------;
; command line arguments ---------------------------------;

; TODO: document
; TODO: implementation-dependent? with the envvar thing?
(defun program/script-name ()
  (basename (if *load-truename*
              (namestring *load-truename*)
              (get-envvar "_"))))

; TODO: implementation dependent
; TODO: get program name in clisp
(defun cmdargs ()
  "A multi-implementation function to return argv (program name is CAR)"
  (let ((tmp (or
               #+sbcl sb-ext:*posix-argv*
               #+ecl (ext:command-args)
               #+clisp (cons "program_name" ext:*args*)
               #+lispworks system:*line-arguments-list*
               #+cmu extensions:*command-line-words*
               nil
               )))
    (cons (program/script-name) (cdr tmp))))

(defun %ext-flag-match-p (astring)
  (let ((as-chars (coerce astring 'list)))
    (and (char= (car as-chars) #\-)
         (> (length as-chars) 2)
         (every #'alpha-char-p (cdr as-chars)))))

(defmacro def-cli-args (script-name after-name description &body body)
  (let* ((switches       (mapcar #'second    body))
         (longswitches   (mapcar #'third     body))
         (descriptions   (mapcar #'fourth    body))
         (flag-lines     (mapcar (lambda (x y z)
                                   (format nil "  ~A, ~17A~A~%" x y z))
                                 switches longswitches descriptions))
         (tmp (mapcar (lambda (x)
                        `((or
                            (string= current ,(second x)) (string= current ,(third x)))
                          (progn
                            (or-die ((format nil
                                             "Fatal error processing ~A flag (~A)~%~%~A"
                                             ,(second x) error! +USAGE-TEXT!+))
                              ,@(nthcdr 4 x)))))
                      body)))
  `(progn
     (defparameter args!        nil)
     (defparameter bare-args!   nil)
     (defparameter +USAGE-TEXT!+ nil)
     (macrolet ((assign-next-arg! (avar)
       `(progn (setq ,avar (cadr args!)) (process-args! (cddr args!)))))
       (or-die ("invalid arguments")
         (setq +USAGE-TEXT!+
           (format nil "Usage: ~A ~A~%~A~%~%~A"
                   ,script-name ,after-name ,description
                   (format nil "~{~A~}" (list ,@flag-lines))))
         (defun print-usage! ()
           (format t "~A" +USAGE-TEXT!+)
           (die "" :status 0))
         (defun process-args! (args)
           (setq args! args)
           (if (null args!)
             (setq bare-args! (reverse bare-args!))
             (let ((current (car args!)))
               (cond
                 ((%ext-flag-match-p current)
                   (process-args! (append
                                    (mapcar (lambda (x) (format nil "-~A" x))
                                            (cdr (string->char-list current)))
                                    (cdr args))))
                 ,@tmp
                 (t
                   (progn
                     (setq bare-args! (cons current bare-args!))
                     (process-args! (cdr args!)))))))))))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; terminal things / terminal manipulation --------------- ;

; :TODO: implementation dependent
(defun clear-screen ()
  "A multi-implementation function to clear the terminal screen"
   #+sbcl     (sb-ext:run-program "/bin/sh" (list "-c" "clear")
                                  :input nil :output *standard-output*)
   #+clisp    (ext:shell "clear")
   #+ecl      (si:system "clear")
   #+clozure  (ccl:run-program "/bin/sh" (list "-c" "clear")
                               :input nil :output *standard-output*))

; TODO: SBCL doesn't have the COLUMNS environment variable. fix it
(defun get-terminal-columns ()
  "Retrieves the number of columns in terminal by querying
   `$COLUMNS` environment variable. Returns
   (values num-of-columns t) if successful and (values 200 nil)
   if not"
  (let ((raw-res (ignore-errors (parse-integer
                                  #+sbcl (zsh "echo $COLUMNS")
                                  #-sbcl (get-envvar "COLUMNS" "80")
                                  ))))
    (if raw-res (values raw-res t) (values 200 nil))))

; TODO: is there are better DRY way?
(defun ansi-up-line (&optional (where *error-output*))
  (format where "~A" +ansi-escape-up+))

(defun ansi-left-all (&optional (where *error-output*))
  (format where "~A" +ansi-escape-left-all+))

(defun ansi-clear-line (&optional (where *error-output*))
  (ansi-left-all)
  (format where "~A" (make-string (get-terminal-columns) :initial-element #\Space)))

(defun ansi-left-one (&optional (where *error-output*))
  (format where "~A" +ansi-escape-left-one+))

; TODO: what if it's interactive
; TODO: test on different implementations
(defun progress-bar (index limit &key (interval 1)
                                      (where *error-output*)
                                      (width 60)
                                      (one-line t)
                                      (out-of nil))
  (when (or (= index limit) (and (= 0 (mod index interval))))
    (let* ((perc-done (/ index limit))
           (filled    (round (* perc-done width))))
      (when one-line
        (ansi-up-line     where)
        (ansi-clear-line  where)
        (ansi-left-all    where))
      (format where (yellow "~&|~A~A| ~$%~A"
              (make-string filled :initial-element #\=)
              (make-string (max 0 (- width filled)) :initial-element #\Space)
              (float (* 100 perc-done))
              (if out-of (fn "~C~A/~A" #\Tab index limit) "")))
      (force-output where))))

(defun loading-forever ()
  (let ((counter -1))
    (forever
      (incf counter)
      (setq counter (mod counter 4))
      (let ((rune (case counter
                    (0  "-")
                    (1  "\\")
                    (2  "|")
                    (t  "/"))))
        (format t "~A" rune)
        (force-output)
        (ansi-left-one *standard-output*)
        (sleep 0.1)))))

; TODO: only sbcl and ecl
; TODO: update doc string
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
               ; (bt:make-thread (lambda () ,@body) :name "long-thread")
               )
             (,loading-thread
               #+sbcl (sb-thread:make-thread
                        #'loading-forever
                        :name "loading-thread")
               #+ecl (mp:process-run-function
                       'loading-thead
                       #'loading-forever)
               ; (bt:make-thread #'loading-forever :name "loading-thread")
               ))
         (let ((,the-return
                 #+sbcl (sb-thread:join-thread ,long-thread)
                 #+ecl (mp:process-join ,long-thread)
                 ; (bt:join-thread ,long-thread)
                 ))
           #+sbcl (sb-thread:terminate-thread ,loading-thread)
           #+ecl (mp:process-kill ,loading-thread)
           ; (bt:destroy-thread ,loading-thread)
           (terpri)
           ,the-return)))))

#-(or sbcl ecl)
(defmacro with-loading (&body body)
  `(warn "only implemented for sbcl and ecl"))

(defun give-choices (choices &key (limit 37)
                                  (num-p nil)
                                  (mode :table)
                                  (sep nil))
  "Uses `smenu` (must be installed) to give the user some choices in
   a list (princs the elements). The user's choice(s) are returned
   unless they Control-C, in which case it return `nil`. You can also
   use '/' to search through the choices!
   It's (smenu) is very flexible and this function offers a lot
   of optional keyword parameters
   `limit` sets the limit of choices (and presents a scroll bar)
   `num-p` if true, puts a number next to the choices for easy
           selection (default nil)
   `mode` :table (default), :columns, :lines, and nil
   `sep` if not nil, it will allow the user to select multiple choices (with
         't') and this string will separate them all"
  (let ((tmpvar   (fn "tmp~A"   (get-unix-time)))
        (xchoice  (fn "'~A'"    (str-join "'\\n'" choices)))
        (xmode    (case mode
                    (:columns   "-c")
                    (:table     "-t")
                    (:lines     "-l")
                    (otherwise  ""))))
    (let ((response
            (zsh (fn "~A=$(echo -e \"~A\" | smenu ~A -n~A ~A ~A); echo $~A"
                     tmpvar xchoice (if num-p "-N" "")
                     limit xmode
                     (if sep (fn "-T '~A'" sep) "")
                     tmpvar) :echo nil)))
      (if (string= response "") nil response))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; file/filename/directory operations -------------------- ;

; TODO: document
(defun basename (apath)
  (file-namestring apath))

#-clisp
(defun pwd ()
  (let ((tmp
          #+(or abcl genera mezzano xcl) (truename *default-pathname-defaults*)
          #+clisp (ext:default-directory)
          #+(or clasp ecl) (ext:getcwd)
          #+clozure (ccl:current-directory)
          #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))))
    (namestring tmp)))

#+clisp
(defun pwd ()
  (namestring (ext:default-directory)))

; #+coreutils
(defun realpath (apath &key (expand-symlinks t) (relative-to nil) (all-existing t))
  "Prints resolved path. Needs coreutils and :coreutils must be in *features*
   `expand-symlinks` boolean (default t)
   `relative-to` print resolved path relative to supplied path
   `all-existing` requires that all the directories/files exist (default t)
                  when false, all but the last component must exist.
   All paths must be escaped (if escaping is needed)"
  (unless (probe-file apath)
    (error "no such file"))
  (let ((command (fn "realpath ~A ~A ~A ~A"
                     (if expand-symlinks "" "-s")
                     (if all-existing "-e" "")
                     apath
                     (if relative-to
                       (fn "--relative-to=~A" (realpath relative-to)) ""))))
    (nth-value 0 (zsh command))))

; TODO: escape thing!!!
; #+coreutils
(defun file-size (afile &key (just-bytes nil))
  "Uses `du` to return just the size of the provided file.
   `just-bytes` ensures that the size is only counted in bytes (returns integer)
                [default nil]
   REQUIRES THAT :coreutils is in *features* (and requires coreutils)"
  (let ((result
          (%remove-after-first-whitespace
            (zsh (format nil "du ~A ~A" (if just-bytes "-sb" "") afile)))))
    (if just-bytes
      (nth-value 0 (parse-integer result))
      result)))

; ------------------------------------------------------- ;

; ------------------------------------------------------- ;
; file-related functions -------------------------------- ;

;;;;;
;;;;; STEALING FROM CL-FAD
;;;;;

(defun inspect-pathname (apathname)
  (format *error-output* "received:              ~S~%" apathname)
  ; (format *error-output* "probe file:            ~S~%" (probe-file apathname))
  (format *error-output* "pathname device:       ~S~%" (pathname-device apathname))
  (format *error-output* "pathname host:         ~S~%" (pathname-host apathname))
  (format *error-output* "pathname version:      ~S~%" (pathname-version apathname))
  (format *error-output* "pathname directory:    ~S~%" (pathname-directory apathname))
  (format *error-output* "pathname name:         ~S~%" (pathname-name apathname))
  (format *error-output* "pathname type:         ~S~%" (pathname-type apathname))
  (format *error-output* "truename:              ~S~%" (truename apathname))
  (format *error-output* "namestring:            ~S~%" (namestring apathname))
  (format *error-output* "wild pathname?:        ~S~%" (wild-pathname-p apathname))
  (format *error-output* "directory:             ~S~%" (directory apathname))
  (format *error-output* "directory namestring   ~S~%" (directory-namestring apathname))
  (format *error-output* "file author:           ~S~%" (file-author apathname))
  (format *error-output* "file namestring:       ~S~%" (file-namestring apathname))
  (format *error-output* "file write date:       ~S~%" (file-write-date apathname))
  (format *error-output* "host namestring:       ~S~%" (host-namestring apathname))
  (format *error-output* "enough namestring:     ~S~%" (enough-namestring apathname)))


(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
   a directory, PATHSPEC otherwise.  It is irrelevant whether file or
   directory designated by PATHSPEC does actually exist."
  (and
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

; to export (untested)
(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
  form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
   the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
   sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

; export
; TODO: ignore dot file option
(defun ls (&optional (dirname "./")  (follow-symlinks t))
        "Returns a fresh list of pathnames corresponding to all files within
         the directory named by the non-wild pathname designator DIRNAME.
         The pathnames of sub-directories are returned in directory form -
         see PATHNAME-AS-DIRECTORY.

         If FOLLOW-SYMLINKS is true, then the returned list contains
         truenames (symlinks will be resolved) which essentially means that it
         might also return files from *outside* the directory.  This works on
         all platforms.

         When FOLLOW-SYMLINKS is NIL, it should return the actual directory
         contents, which might include symlinks.  Currently this works on SBCL
         and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #-(or sbcl ccl)
  (unless follow-symlinks
    (error "implementation doesn't support _not_ following symlinks"))
  #+(or :ecl :clasp)
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-(or :ecl :clasp)
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl) (directory wildcard)
    #+:lispworks (directory wildcard :link-transparency follow-symlinks)
    #+(or :openmcl :digitool) (directory wildcard :directories t :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool :clasp)
  (error "LIST-DIRECTORY not implemented"))


(defun file-or-directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
   exists and returns its truename if this is the case, NIL otherwise.
   The truename is returned in `canonical' form, i.e. the truename of a
   directory is returned as if by PATHNAME-AS-DIRECTORY."
  ; #+ecl
  ; (if (or
  ;       (search "*" (namestring pathspec))
  ;       (search "?" (namestring pathspec))
  ;       (search "\\" (namestring pathspec)))
  ;   (error "ecl doesn't support '?', '*', or '\\' in the path"))
  #+(or :sbcl :lispworks :openmcl :ecl :digitool clasp) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or
             (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     (truename directory-form))))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool :clasp)
  (error "FILE-EXISTS-P not implemented"))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
   exists and if it is a directory.  Returns its truename if this is the
   case, NIL otherwise.  The truename is returned in directory form as if
   by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (file-or-directory-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

(defun file-exists-p (pathspec)
  (let ((tmp (file-or-directory-exists-p pathspec)))
    (and tmp (not (directory-exists-p pathspec)))))

; TODO: same options as ls? check symlink ability
; TODO: test if input is actually a directory
(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t))
                                       (follow-symlinks t))
  "Recursively applies the function FN to all files within the
   directory named by the non-wild pathname designator DIRNAME and all of
   its sub-directories.  FN will only be applied to files for which the
   function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
   TEST are applied to directories as well.  If DIRECTORIES
   is :DEPTH-FIRST, FN will be applied to the directory's contents first.
   If DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the directory's
   content will be skipped. IF-DOES-NOT-EXIST must be one of :ERROR
   or :IGNORE where :ERROR means that an error will be signaled if the
   directory DIRNAME does not exist.  If FOLLOW-SYMLINKS is T, then your
   callback will receive truenames.  Otherwise you should get the actual
   directory contents, which might include symlinks.  This might not be
   supported on all platforms.  See LS."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (ls name follow-symlinks))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (ls name follow-symlinks))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (ls name follow-symlinks))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (file-or-directory-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (file-or-directory-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))

; TODO: document
(defun file-find (apathname &key (the-type nil) (test (constantly t)))
  (let (to-return)
    (walk-directory apathname #'(lambda (x) (push x to-return))
                    :directories :depth-first
                    :test test)
    (reverse to-return)))

; TODO: mention that it always FOLLOWS SYMLINKS
(defun -path (pathone pathtwo)
  (enough-namestring (probe-file pathone) (directory-exists-p  pathtwo)))

; ------------------------------------------------------- ;

