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

(defpackage :pluto
  (:use :common-lisp)
  (:export

    ; pluto parameters
    :*pluto-output-stream* :*pluto-log-level* :*pluto-log-file*
    :*pluto-curly-test* :*pluto-external-format* :*pluto-shell*
    :*unix-epoch-difference* :*whitespaces* :*pluto-spinner-frames*
    :*pluto-readtable* :call-with-pluto-readtable

    ; formatting
    :fn :ft :q/fmt

    ; ansi colors and codes
    :make-ansi-escape :+reset-terminal-color+ :+magenta-bold+ :+red-bold+
    :+yellow-bold+ :+green-bold+ :+cyan-bold+ :+blue-bold+ :+grey-bold+
    :+ansi-escape-up+ :+ansi-escape-left-all+ :+ansi-escape-left-one+
    :+ansi-escape-clear-line+ :+ansi-escape-hide-cursor+
    :+ansi-escape-show-cursor+
    :magenta :red :yellow :green :cyan :blue :grey

    ; string operations
    :str+ :str-join :str-sub :string->char-list :split-string->lines
    :repeat-string :str-pad

    ; some essential utilities/macros
    :with-gensyms :mac :nil! :alambda :self! :abbr :flatten :take :group
    :create-symbol :create-keyword :walk-replace-sexp :-<> :<> :aif :it!
    :slurp :slurp-lines :barf :debug-these :with-a-file :stream! :interpose
    :delim :defparams :round-to :advise :alistp :with-hash-entry :entry!
    :if-hash-entry :if-not-hash-entry :capture-all-outputs :with-temp-file
    :tempfile! :display-table

    ; queries
    :y-or-n-def

    ; error handling
    :die :or-die :or-do :die-if-null :error!
    ; reader macros
    ; #? Ø ?

    ; time
    :universal->unix-time :unix->universal-time :get-unix-time
    :make-pretty-time :get-current-time :with-time :time-for-humans :time!

    ; threads
    :make-thread :join-thread :make-mutex :with-mutex

    ; for-each and friends
    :with-interactive-interrupt-handler
    :progress :break! :continue! :index! :value! :key! :for-each/line
    :for-each/list :for-each/hash :for-each/vector :for-each/stream
    :for-each/alist :for-each/call :for-each/parallel :for-each :forever

    ; shell and zsh
    :zsh :sh :zsh-simple :sh-simple :q/sh

    ; other abbreviations and shortcuts
    :λ

    ; system
    :hostname :sys/info :get-envvar

    ; command-line arguments
    :program/script-name :cmdargs :def-cli-args :args! :bare-args!
    :+USAGE-TEXT!+ :print-usage! :process-args! :assign-next-arg!

    ; terminal things / terminal manipulation
    :clear-screen
    :get-terminal-columns :ansi-up-line :ansi-left-all :ansi-clear-line
    :ansi-left-one :ansi-up-lines :progress-bar :loading-forever
    :with-loading :give-choices
    :with-status-lines :lines! :status! :done! :failed!

    ; file/filename/directory operations
    :basename :pwd :realpath :size-for-humans :file-size
    :inspect-pathname :ls :directory-exists-p :file-exists-p
    :file-or-directory-exists-p :walk-directory :-path :+path
    :absolute->relative :change-extension :file-find :pathname->native

    ))

(in-package :pluto)

(pushnew :pluto *features*)

#-(or sbcl ecl)
(warn "pluto is only developed and tested on SBCL and ECL")


;---------------------------------------------------------;
; pluto parameters ---------------------------------------;

(defparameter *pluto-output-stream*    *terminal-io*
  "Where pluto's own output would go. Not used by anything yet:
   a knob left over from the logging experiment (see the
   commented-out reader macros at the bottom of charon)")
(defparameter *pluto-log-level*        2
  "How chatty the logging would be. Not used by anything yet
   (see *pluto-output-stream*)")
(defparameter *pluto-log-file*         "pluto-log.out"
  "Where the logging would write. Not used by anything yet
   (see *pluto-output-stream*)")
(defparameter *pluto-curly-test*       #'equal
  "The equality test `{ }` uses to find a key in an alist
   (default #'equal). Bind it to #'string-equal, say, for
   case-insensitive string keys")
(defparameter *pluto-external-format*  :UTF-8
  "The external format (character encoding) pluto reads and writes
   text with: slurp, slurp-lines, barf, with-a-file, for-each/line,
   and the default for `sh`'s `enc`. :UTF-8 unless you say otherwise")
(defun %resolve-shell ()
  "Resolves the shell that `sh` (et al.) will use:
   the PLUTO_SHELL environment variable (if set and non-empty),
   then well-known zsh locations, then /bin/sh"
  (let ((from-env (or #+sbcl (sb-ext:posix-getenv "PLUTO_SHELL")
                      #+ecl  (ext:getenv "PLUTO_SHELL"))))
    (or
      (and from-env (string/= from-env "") from-env)
      (find-if #'probe-file '("/opt/homebrew/bin/zsh" "/usr/local/bin/zsh"
                              "/usr/bin/zsh" "/bin/zsh"))
      "/bin/sh")))

(defparameter *pluto-shell* (%resolve-shell)
  "The shell `sh` (and `zsh`) run commands through. Resolved when
   pluto loads: the PLUTO_SHELL environment variable if it's set,
   else the first zsh found in the usual places, else /bin/sh")

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0)
  "The seconds between the common lisp epoch (1900-01-01) and the
   unix one (1970-01-01); what universal->unix-time subtracts")

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout)
  "The characters that count as whitespace: a character bag for
   string-trim and friends (charon's str-trim uses it)")

(defparameter *pluto-spinner-frames* #("-" "\\" "|" "/")
  "The frames of the spinner that loading-forever (so with-loading)
   and with-status-lines show, cycled ten times a second: a vector
   of one-column strings. Braille is the usual upgrade:
   #(\"⠋\" \"⠙\" \"⠹\" \"⠸\" \"⠼\" \"⠴\" \"⠦\" \"⠧\" \"⠇\" \"⠏\")")

;---------------------------------------------------------;


;---------------------------------------------------------;
; formatting ---------------------------------------------;

(defmacro fn (&rest everything)
  "Alias to (format nil ...)"
  `(format nil ,@everything))

(defmacro ft (&rest everything)
  "Alias to (format t ...)"
  `(format t ,@everything))

(defun q/fmt (thing)
  "Quotes THING (which gets princ-ed) so that it's safe to
   embed in a format control string: escapes `~` as `~~`
   e.g. (fn (str+ \"downloading \" (q/fmt afilename) \": ~A%\") pct)"
  (with-output-to-string (out)
    (loop for ch across (format nil "~A" thing)
          do (write-char ch out)
          when (char= ch #\~) do (write-char #\~ out))))

;---------------------------------------------------------;


;---------------------------------------------------------;
; ansi colors and codes ----------------------------------;

(defun make-ansi-escape (anum &optional (decoration 'bold))
  "Builds an ANSI escape sequence (ESC[<anum>;<decoration>m) for
   the terminal: ANUM is the SGR code (31 is red, 90 grey, 0 is
   reset) and DECORATION is bold (the default), underline, or
   reversed -- as a symbol or keyword from any package -- or nil
   for none. The +...-bold+ constants are made with it"
  ; compared by name: an EQ against pluto's own symbols meant
  ; 'underline from a script (another package) silently matched
  ; nothing, so only the default bold ever worked from outside
  (format nil "~c[~A~Am" #\ESC anum
          (cond
            ((null decoration)                      "")
            ((string-equal decoration "bold")       ";1")
            ((string-equal decoration "underline")  ";4")
            ((string-equal decoration "reversed")   ";7")
            (t                                      ""))))

(defvar +reset-terminal-color+  (make-ansi-escape 0 nil)
  "The escape that turns colors and decorations back off (ESC[0m)")
(defvar +magenta-bold+          (make-ansi-escape 35)
  "The escape for bold magenta (ESC[35;1m)")
(defvar +red-bold+              (make-ansi-escape 31)
  "The escape for bold red (ESC[31;1m)")
(defvar +yellow-bold+           (make-ansi-escape 33)
  "The escape for bold yellow (ESC[33;1m)")
(defvar +green-bold+            (make-ansi-escape 32)
  "The escape for bold green (ESC[32;1m)")
(defvar +cyan-bold+             (make-ansi-escape 36)
  "The escape for bold cyan (ESC[36;1m)")
(defvar +blue-bold+             (make-ansi-escape 34)
  "The escape for bold blue (ESC[34;1m)")
(defvar +grey-bold+             (make-ansi-escape 90)
  "The escape for bold grey (ESC[90;1m -- 'bright black')")

(defvar +ansi-escape-up+        (format nil "~c[1A" #\ESC)
  "The escape that moves the cursor up one line (ESC[1A)")
(defvar +ansi-escape-left-all+  (format nil "~c[500D" #\ESC)
  "The escape that moves the cursor 500 columns left, which is to
   say to the start of the line (ESC[500D)")
(defvar +ansi-escape-left-one+  (format nil "~c[1D" #\ESC)
  "The escape that moves the cursor one column left (ESC[1D)")
(defvar +ansi-escape-clear-line+   (format nil "~c[2K" #\ESC)
  "The escape that blanks the whole current line (ESC[2K) without
   moving the cursor")
(defvar +ansi-escape-hide-cursor+  (format nil "~c[?25l" #\ESC)
  "The escape that hides the cursor (ESC[?25l) -- show it again
   before you're done, or the terminal stays that way")
(defvar +ansi-escape-show-cursor+  (format nil "~c[?25h" #\ESC)
  "The escape that shows the cursor again (ESC[?25h)")

(defun do-with-color (acolor thestring &rest everything)
  "Wraps THESTRING (a format control string, EVERYTHING its
   arguments) in the escape ACOLOR and a reset"
  (apply #'format nil
         (format nil "~A~A~A" acolor thestring +reset-terminal-color+)
         everything))

(defmacro make-color-fun (thename acolor)
  "Defines THENAME as a function that colors a string with ACOLOR
   (see `red` et al. below)"
  `(defun ,thename (thestring &rest things)
     ,(format nil "Returns THESTRING wrapped in bold ~(~A~) (and a reset),
   so it prints ~(~A~) on an ANSI terminal. THESTRING is a format
   control string and THINGS are its arguments:
   e.g. (~(~A~) \"~~A of ~~A\" 1 2)" thename thename thename)
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
(defun str-sub (string start &optional end)
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

(defun repeat-string (str times)
  "Returns STR repeated TIMES times, as one string
   e.g. (repeat-string \"ab\" 3)  =>  \"ababab\""
  (fn "~{~A~}" (loop for i from 1 to times collect str)))

(defun str-pad (thing width &key (side :left) (pad #\Space))
  "Pads THING (which gets princ-ed) with PAD until it's WIDTH
   characters wide. It never truncates: if THING is already
   WIDTH (or wider) it comes back as is
   `side` is where the padding goes: :left (default), :right,
          or :both (centered; an odd leftover goes on the right)
   `pad` is the character to pad with (default is a space);
         a one-character string works, too
   e.g. (str-pad 2 2 :pad \"0\")  =>  \"02\""
  (let ((astring (format nil "~A" thing))
        (padchar (character pad)))
    (ecase side
      (:left    (format nil "~v,,,v@A" width padchar astring))
      (:right   (format nil "~v,,,vA"  width padchar astring))
      (:both    (let* ((needed (max 0 (- width (length astring))))
                       (left   (floor needed 2)))
                  (str+ (make-string left :initial-element padchar)
                        astring
                        (make-string (- needed left)
                                     :initial-element padchar)))))))

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
  "Defines SHORT as an abbreviation for LONG.
   If LONG names a macro, SHORT becomes a macro, too.
   If LONG names a function, SHORT becomes a bona-fide
   function alias (so it can be FUNCALLed, MAPCARed, etc.).
   Note that SHORT is bound to LONG's definition at the
   time ABBR runs; redefining LONG later won't change SHORT"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cond
       ((macro-function ',long)
         (setf (macro-function ',short) (macro-function ',long)))
       ((fboundp ',long)
         (setf (fdefinition ',short) (fdefinition ',long)))
       (t
         (error "can't abbreviate ~A: it names neither a macro nor a function"
                ',long)))
     ',short))

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

(defun slurp (path)
  "Reads file at PATH into a single string
   (note: FILE-LENGTH counts octets, so with multibyte encodings
    the buffer can be larger than the number of characters
    actually read; READ-SEQUENCE returns the true end)"
  (with-open-file (stream path :if-does-not-exist :error
                          :external-format *pluto-external-format*)
    (let ((data (make-string (file-length stream))))
      (subseq data 0 (read-sequence data stream)))))

; TODO: what?
(defun slurp-stream (astream)
  (with-open-stream (s astream)
    (let ((data (make-string (file-length s))))
      (subseq data 0 (read-sequence data s)))))

(defun slurp-lines (afilename)
  "Reads lines of a file into a list"
  (with-open-file (tmp afilename :if-does-not-exist :error
                       :external-format *pluto-external-format*)
    (loop for value = (read-line tmp nil)
          while value collect value)))

(defun barf (path contents &key (printfn #'write-string) (overwrite nil))
  "Outputs CONTENTS into filename PATH with function PRINTFN
   (default WRITE-STRING), appending unless OVERWRITE is true;
   creates the file if it doesn't exist. Writes text in
   *pluto-external-format*, like slurp reads it"
  (with-open-file (stream path :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create
                          :external-format *pluto-external-format*)
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
    Only provide one of these arguments
    The writing modes create the file if it doesn't exist;
    the reading modes error"
   (let ((dir (cond
                ((eq key :w) :output)       ((eq key :a) :output)
                ((eq key :r) :input)        ((eq key :b) :input)))
         (iex (cond
                ((eq key :w) :supersede)    ((eq key :a) :append)
                ((eq key :r) :append)       ((eq key :b) :append)))
         (idne (if (or (eq key :w) (eq key :a)) :create :error)))
    `(with-open-file (stream! ,filename :direction ,dir :if-exists ,iex
                              :if-does-not-exist ,idne
                              ; external-format is a text-mode
                              ; affair; :b gets real octets
                              ,@(if (eq key :b)
                                  `(:element-type '(unsigned-byte 8))
                                  `(:external-format *pluto-external-format*)))
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

(defmacro capture-all-outputs (&body body)
  "Runs BODY with *standard-output* and *error-output* captured and
   returns three values: what BODY returned (its first value), what
   it printed to stdout, and what it printed to stderr
   e.g. (capture-all-outputs (ft \"hi\") 42)  =>  42, \"hi\", \"\""
  ; (it used to FUNCALL its body -- a function and its arguments --
  ; which the name never suggested; nothing called it that way)
  (let ((ret (gensym)))
    `(let ((*standard-output*   (make-string-output-stream))
           (*error-output*      (make-string-output-stream)))
       (let ((,ret (progn ,@body)))
         (values ,ret
                 (get-output-stream-string *standard-output*)
                 (get-output-stream-string *error-output*))))))

; (this used to be #+coreutils, but mktemp and rm are everywhere; it's
; realpath that needs GNU coreutils)
(defmacro with-temp-file (&body body)
  "Anaphoric macro: runs BODY with `tempfile!` bound to the path of
   a fresh temporary file (from mktemp) and deletes the file
   afterwards, however BODY exits"
  `(let ((tempfile! (sh "mktemp")))
     (unwind-protect
       (progn
         ,@body)
         (sh (fn "rm ~A" tempfile!)))))

(defun display-table (header data width)
  "Prints a plain-text table to stdout: HEADER is the list of
   column titles, DATA the list of rows (each a list with one
   element per column), and WIDTH the width of every column. The
   cells are princ-ed and left-aligned"
  (let ((num-cols (length header)))
    (ft (fn "~~{|~~{~~~AA|~~}~~%~~}" width)
        `(,(loop for i from 1 to num-cols collect (repeat-string "-" width))))
    (ft (fn "~~{|~~{ ~~~AA|~~}~~%~~}" (- width 1)) `(,header))
    (ft (fn "~~{|~~{~~~AA|~~}~~%~~}" width)
        `(,(loop for i from 1 to num-cols collect (repeat-string "-" width))))
    (ft (fn "~~{|~~{ ~~~AA|~~}~~%~~}" (- width 1)) data)
    (ft (fn "~~{|~~{~~~AA|~~}~~%~~}" width)
        `(,(loop for i from 1 to num-cols collect (repeat-string "-" width))))))

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

(defun y-or-n-def (prompt &key (default nil))
  "Asks PROMPT on *query-io* and returns T for y, NIL for n. With
   DEFAULT (#\\y or #\\n) the prompt shows [Y/n] or [y/N] and a bare
   Enter picks the default; without one it's plain y-or-n-p"
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

(defun die (message &key (status 1) (red-p t))
  "Prints MESSAGE to *ERROR-OUTPUT* and quits with a STATUS
   (default 1). MESSAGE is printed as-is — never interpreted as
   a format control string, so a stray `~` (in, say, an error
   message we're relaying) can't crash die while dying. Build
   fancy messages with `fn` (and `q/fmt`) first"
  (format *error-output* "~A~A~A~%" (if red-p +red-bold+ "")
                                      message
                                    (if red-p +reset-terminal-color+ ""))
  #+sbcl  (sb-ext:quit :unix-status status)
  #+ecl   (ext:exit status))

(defmacro or-die (form message &key (errfun '#'die))
  "Anaphoric macro that binds ERROR! to the error
   Runs FORM and returns its value; if it signals an error,
   ERRFUN (#'die by default) gets funcalled with MESSAGE —
   which can reference ERROR!
   e.g. (or-die (risky-thing) (fn \"no dice: ~A\" error!))
        (or-die (risky-thing) \"hmm\" :errfun #'advise)"
  `(handler-case
     ,form
     (error (error!)
       (declare (ignorable error!))
       (funcall ,errfun (format nil "~A" ,message)))))

(defmacro or-do (form fallback)
  "Anaphoric macro that binds ERROR! to the error
   Runs FORM and returns its value; if it signals an error,
   FALLBACK gets run (and returned) instead
   e.g. (or-do (risky-thing) (advise (fn \"eh: ~A\" error!)))"
  `(handler-case
     ,form
     (error (error!)
       (declare (ignorable error!))
       ,fallback)))

(defmacro die-if-null (avar &rest therest)
  "Macro to check if any of the supplied arguments are null"
  `(progn
     ,@(mapcar (lambda (v)
                 `(unless ,v
                    (die (format nil "Fatal error: ~A is null" ',v))))
               (cons avar therest))))

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
    `(aif ,sexp
          it!
          (error "its null"))))

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

; non-terminating-p is T: `?` only fires at the START of a token,
; so scheme-style predicate names in foreign code (fast-http's
; `match?`, say) still read as plain symbols — with a terminating
; `?`, quickloading such a library with pluto's readtable active
; (i.e. compiling it from a cold fasl cache) was a reader error
(set-macro-character #\? #'|if-null->this| t)

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
  "Makes a nicely formatted (YYYY-MM-DD HH:MM:SS) from a UNIX time
   `just-date` will return just the pretty date
   `just-time` will return just the pretty time
   `time-sep`  will use the supplied string to separate the hours
   minutes and seconds (default ':')
   `dt-sep` will use the supplied string to separate the date from the time
   (default ' ')"
  (let ((thisuniversaltime (unix->universal-time a-unix-time)))
    (multiple-value-bind (second minute hour date month year)
      (decode-universal-time thisuniversaltime)
      ; (the hour is padded like everything else now; it used to be
      ; the one bare ~d, so timestamps before 10am sorted wrong)
      (if (and (not just-date) (not just-time))
        (format nil "~d-~2,'0d-~2,'0d~A~2,'0d~A~2,'0d~A~2,'0d"
                year month date dt-sep hour TIME-SEP minute TIME-SEP second)
        (if just-date
          (format nil "~d-~2,'0d-~2,'0d" year month date)
          (format nil "~2,'0d~A~2,'0d~A~2,'0d"
                  hour TIME-SEP minute TIME-SEP second))))))

(defun get-current-time (&key (just-date nil) (just-time nil)
                              (time-sep ":") (dt-sep " "))
  "Uses `make-pretty-time` to get the current datetime"
  (make-pretty-time (-<> (get-universal-time) universal->unix-time)
                    :just-date just-date :just-time just-time
                    :time-sep time-sep :dt-sep dt-sep))

(defmacro with-time (&body aform)
  "Anaphoric macro that executes the car of the body and
   binds the seconds of execution time (as a float, with
   sub-second resolution) to TIME!. Then all the other
   forms in the body are executed"
  (let ((began      (gensym))
        (ended      (gensym)))
    `(let (,began ,ended time!)
       (setq ,began (get-internal-real-time))
       ,(car aform)
       (setq ,ended (get-internal-real-time))
       (setq time! (float (/ (- ,ended ,began)
                             internal-time-units-per-second)))
       ,@(cdr aform))))

(defun time-for-humans (seconds)
  "Converts SECONDS into minutes, hours, or days (based on magnitude)"
  (cond
    ((>= seconds 86400)       (format nil "~$ days" (/ seconds 86400)))
    ((>= seconds 3600)        (format nil "~$ hours" (/ seconds 3600)))
    ((>= seconds 60)          (format nil "~$ minutes" (/ seconds 60)))
    (t                        (format nil "~A seconds" seconds))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; threads ----------------------------------------------- ;

; the two blessed implementations' threads under one set of names
; (pluto can't lean on bordeaux-threads: zero deps). with-loading,
; for-each/parallel and with-status-lines are built on these

(defun make-thread (fn &key (name "pluto-thread"))
  "Starts FN (a function of no arguments) in a new thread and
   returns the thread; join-thread waits for it and hands back FN's
   value. The new thread sees the global values of special
   variables, not the caller's dynamic bindings -- pass what it
   needs lexically"
  #+sbcl (sb-thread:make-thread fn :name name)
  #+ecl  (mp:process-run-function name fn))

(defun join-thread (thread)
  "Waits for THREAD (from make-thread) to finish and returns what
   its function returned"
  #+sbcl (sb-thread:join-thread thread)
  #+ecl  (mp:process-join thread))

(defun %terminate-thread (thread)
  #+sbcl (sb-thread:terminate-thread thread)
  #+ecl  (mp:process-kill thread))

(defun make-mutex (&key (name "pluto-mutex"))
  "A mutex, for with-mutex: whatever two threads must never do at
   once (update a shared list, a hash table, a counter) is done
   holding it. Not recursive: taking it again from the thread that
   holds it is an error, not a no-op"
  #+sbcl (sb-thread:make-mutex :name name)
  #+ecl  (mp:make-lock :name name))

(defmacro with-mutex ((mutex) &body body)
  "Runs BODY holding MUTEX (from make-mutex) -- waiting first if
   another thread has it -- and releases it however BODY exits.
   Every thread touching the shared thing must go through the same
   mutex; a write that skips it is still a race
   e.g. (with-mutex (m) (incf count))"
  #+sbcl `(sb-thread:with-mutex (,mutex) ,@body)
  #+ecl  `(mp:with-lock (,mutex) ,@body))

; ------------------------------------------------------- ;


;---------------------------------------------------------;
; for-each and friends  ----------------------------------;

(declaim (inline progress))
(defun progress (index limit &key (interval 1) (where *error-output*)
                       (newline-p t))
  "Prints `INDEX of LIMIT..... [percent%]` (in yellow) to WHERE
   (default *error-output*), but only when INDEX is a multiple of
   INTERVAL (default 1: every time). NEWLINE-P (default t) ends the
   line; nil leaves the cursor on it, for with-loading's spinner
   e.g. (for-each things (progress index! (length things) :interval 10) ...)"
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

(defmacro with-interactive-interrupt-handler (the-message &body body)
  "Runs BODY; if Control-C arrives meanwhile, dies (exit status 1)
   printing THE-MESSAGE instead of landing in the debugger. The
   for-each family wraps its loops in it. THE-MESSAGE is printed as
   data (die's rule), so build it with `fn` if it needs directives"
  `(handler-case
     (progn
       ,@body)
       (#+sbcl    sb-sys:interactive-interrupt
        #+ecl     ext:interactive-interrupt
         () (die ,the-message))))

(defmacro for-each/line (a-thing &body body)
  "(see documentation for `for-each`)"
  (let ((resolved-fn            (gensym))
        (instream               (gensym)))
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
    `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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
   due to differences, it is not automatically dispatched, so
   it always needs to be called explicitly. Its only argument
   (besides the body) is a closure that is repeatedly FUNCALLed;
   the loop ends when the closure returns NIL"
  `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
     (let ((index!      0)
           (value!      nil))
       (declare (ignorable value!))
       (block this-loop!
              (loop for value! = (funcall ,aclosure)
                    while value!
                    do (progn (incf index!)
                              (block this-pass! ,@body)))))))

(defun %run-in-parallel (items limit fn)
  "The engine of for-each/parallel: calls FN with (index item) for
   every item of ITEMS, LIMIT at a time (nil: all at once), and
   returns the results in ITEMS' order. Every item runs even when
   some fail; then the first error is re-signalled"
  (check-type limit (or null (integer 1)))
  (let* ((items    (coerce items 'vector))
         (n        (length items))
         (results  (make-array n :initial-element nil))
         (errors   (make-array n :initial-element nil))
         (next     0)
         (mutex    (make-mutex :name "for-each/parallel"))
         (workers  (min n (or limit n))))
    (flet ((worker ()
             (loop
               (let ((i (with-mutex (mutex)
                          (when (< next n) (prog1 next (incf next))))))
                 (unless i (return))
                 (handler-case
                   (setf (aref results i) (funcall fn (1+ i) (aref items i)))
                   (error (e) (setf (aref errors i) e)))))))
      (mapc #'join-thread
            (loop repeat workers
                  collect (make-thread #'worker :name "for-each/parallel"))))
    (let ((failed (remove nil errors)))
      (when (plusp (length failed))
        (error "for-each/parallel: ~A of ~A tasks errored -- the first: ~A"
               (length failed) n (aref failed 0))))
    (coerce results 'list)))

(defmacro for-each/parallel ((a-list &key limit) &body body)
  "Like for-each/list, but every element's BODY runs in its own
   thread -- at most LIMIT of them at a time (default: all at once)
   -- and, unlike the other for-eaches, it returns the list of
   BODY's values, in A-LIST's order. index! and value! are there as
   usual; continue! ends that element's body (its value is nil);
   break! has nothing to break out of. If an element's body errors,
   every element still runs, then the first error is re-signalled
   -- wrap the body in or-do if a failure should be a result instead.
   BODY runs in a fresh thread, so the caller's dynamic bindings (a
   let of a special, a rebound *standard-output*) aren't visible
   e.g. (for-each/parallel (urls :limit 4) (fetch value!))"
  `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
     (%run-in-parallel ,a-list ,limit
                       (lambda (index! value!)
                         (declare (ignorable index! value!))
                         (block this-pass! ,@body)))))

; TODO: weird sbcl compiler notes
(defmacro for-each (a-thing &body body)
  "A super-duper imperative looping construct.
   It takes either
     a filename string    (to be treated as a file and goes line by line)
     a pathname           (goes line by line)
     a hash-table
     a vector
     a list
     a string             (that goes character by character)
     or a stream          (that goes line by line)
  It is anaphoric and introduces
     index!               (which is a counter of which element we are on, starting at 1)
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
              (pathname       (for-each/line      ,tmp      ,@body))
              (hash-table     (for-each/hash      ,tmp      ,@body))
              (vector         (for-each/vector    ,tmp      ,@body))
              (list           (for-each/list      ,tmp      ,@body))
              (stream         (for-each/stream    ,tmp      ,@body)))))))))

(defmacro forever (&body body)
  "Performed BODY forever. Must be terminated by
   RETURN-FROM NIL, or, simple RETURN
   Simple wrapper around `(loop (progn ,@body))`"
  `(with-interactive-interrupt-handler (fn "~%Loop aborted. Bailing out.~%")
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

; on a list, the INDEX decides: an integer is always a position
; (nth), anything else is an alist key. A list of lists looks
; exactly like an alist (every element is a cons), so guessing
; from the list's shape made { '((1 2) (3 4)) 1 } a key lookup.
; (the price: an alist keyed by integers needs a plain `assoc`)
(defmethod get-at ((this list) that)
  ; (one NTH, reached either way: a separate "not an alist" branch
  ; after the integer test made sbcl warn, rightly, that NTH could
  ; never be happy there)
  (if (or (integerp that) (not (alistp this)))
    (nth that this)
    (cdr (assoc that this :test *pluto-curly-test*))))

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
    ; same rule as get-at: integer index -> position
    ((and (listp this)
          (integerp that))          (setf (nth that this) new))
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

(defun %strip-trailing-newline (astring)
  (if (and (> (length astring) 0)
           (char= #\Newline (char astring (- (length astring) 1))))
    (subseq astring 0 (- (length astring) 1))
    astring))

#+sbcl
(defun sh (acommand &key (dry-run        nil)
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
   `return-string` t returns the output string. nil inherits stdout and stderr (default t)
   `split` will separate the stdout by newlines and return a list (default: nil)
   `interactive` will use the '-i' option to make the shell interactive (default: nil)"
  (when (or echo dry-run)
    (format t "$ ~A~%" acommand))
  (unless dry-run
    (let* ((arglist     (if interactive
                          `("-ic" ,(fn "~A;exit" acommand))
                          `("-c"  ,acommand)))
           (outs        (if return-string (make-string-output-stream) t))
           (errs        (if return-string (make-string-output-stream) t))
           (theprocess
             (sb-ext:run-program *pluto-shell* arglist
                                 :input in :output outs :error errs
                                 :external-format enc))
           (retcode
             (sb-ext:process-exit-code theprocess)))
      (let ((errstr (if return-string
                      (%strip-trailing-newline (get-output-stream-string errs))
                      "")))
        (unless (eql 0 retcode)
          (funcall err-fun retcode errstr))
        (when return-string
          (values (if split
                    (split-string->lines (get-output-stream-string outs))
                    (%strip-trailing-newline (get-output-stream-string outs)))
                  errstr
                  retcode))))))

#+ecl
(defun sh (acommand &key (dry-run        nil)
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
   `return-string` t returns the output string. nil inherits stdout and stderr (default t)
   `split` will separate the stdout by newlines and return a list (default: nil)
   `interactive` will use the '-i' option to make the shell interactive (default: nil)"
  (when (or echo dry-run)
    (format t "$ ~A~%" acommand))
  (unless dry-run
    (let* ((arglist     (if interactive
                          `("-ic" ,(fn "~A;exit" acommand))
                          `("-c"  ,acommand)))
           (outs        (if return-string (make-string-output-stream) t))
           (errs        (if return-string (make-string-output-stream) t)))
      (multiple-value-bind (procstream retcode process)
            (ext:run-program *pluto-shell* arglist
                             :input in :output outs :error errs
                             :external-format enc)
            (declare (ignore procstream))
            (ext:external-process-wait process)
            (let ((errstr (if return-string
                            (%strip-trailing-newline (get-output-stream-string errs))
                            "")))
              (unless (eql 0 retcode)
                (funcall err-fun retcode errstr))
              (when return-string
                (values (if split
                          (split-string->lines (get-output-stream-string outs))
                          (%strip-trailing-newline (get-output-stream-string outs)))
                        errstr
                        retcode)))))))

#+(or sbcl ecl)
(setf (fdefinition 'zsh) #'sh)

(defmacro sh-simple (acommand)
  "Runs ACOMMAND through *pluto-shell* with the implementation's
   bare run-program (and its defaults: nothing captured, nothing
   signalled) and returns whatever run-program returns -- a process
   object on SBCL. For output, exit codes, and errors use `sh`"
  #+sbcl
  `(sb-ext:run-program *pluto-shell* `("-c" ,,acommand))
  #+ecl
  `(ext:run-program *pluto-shell* `("-c" ,,acommand))
  )

#+(or sbcl ecl)
(abbr zsh-simple sh-simple)

(defun q/sh (thing)
  "Quotes THING (which gets princ-ed) so that it's safe to
   interpolate into a shell command: wraps it in single quotes
   and escapes any embedded single quotes. One rule; covers
   spaces, globs, `$`, backticks, newlines, etc.
   e.g. (sh (fn \"cat ~A\" (q/sh afilename)))"
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for ch across (fn "~A" thing)
          do (if (char= ch #\')
               (write-string "'\\''" out)
               (write-char ch out)))
    (write-char #\' out)))

; ------------------------------------------------------- ;


; ------------------------------------------------------- ;
; other abbreviations and shortcuts --------------------- ;

(defmacro λ (&body body)
  "lambda, in one character: (λ (x) (* x x))"
  `(lambda ,@body))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; system -------------------------------------------------;

; TODO: USE UIOP FOR THESE?!

(defun hostname ()
  "The machine's hostname (machine-instance)"
  (machine-instance))

; TODO: beef out
; TODO: distro
(defun sys/info ()
  "An alist describing the machine, from uname:
     (:kernel . :linux | :darwin | :unknown)
     (:os . :gnu/linux | :darwin | :android | :unknown)
     (:hostname . \"...\")
     (:architecture . :x86 | :arm)
   plus, on GNU/Linux, (:distro . :debian ...) from lsb_release
   e.g. (eq { (sys/info) :os } :darwin)"
  (let ((kernel         (sh "uname -s"))
        (os             (sh "uname -o"))
        (hostname       (hostname))
        (architecture   (sh "uname -m")))
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
        (push `(:distro . ,(create-keyword (sh "lsb_release -a 2> /dev/null | head -n 1 | awk '{ print $3 }'")))
              info))
      info)))

; TODO: SBCL DOESN't get columns
; TODO: https://stackoverflow.com/questions/44236376/how-do-i-get-the-list-of-all-environment-variables-available-in-a-lisp-process
(defun get-envvar (name &optional default)
  "The value of the environment variable NAME, or DEFAULT (nil
   unless given) when it isn't set"
  (or
    #+sbcl (sb-ext:posix-getenv name)
    #+ecl  (ext:getenv name)
    default))

; ------------------------------------------------------- ;




;---------------------------------------------------------;
; command line arguments ---------------------------------;

; TODO: implementation-dependent? with the envvar thing?
(defun program/script-name ()
  "The name of the running script, without its directory: the file
   being loaded, or failing that the shell's $_"
  (basename (if *load-truename*
              (namestring *load-truename*)
              (get-envvar "_"))))

(defun cmdargs ()
  "Returns argv (program name is CAR)"
  (let ((tmp (or
               #+sbcl sb-ext:*posix-argv*
               #+ecl (ext:command-args)
               nil
               )))
    (cons (program/script-name) (cdr tmp))))

(defun %ext-flag-match-p (astring)
  (let ((as-chars (coerce astring 'list)))
    (and (char= (car as-chars) #\-)
         (> (length as-chars) 2)
         (every #'alpha-char-p (cdr as-chars)))))

; def-cli-args defines these with defparameter, but code that refers
; to them can get compiled before that runs (a def-cli-args inside a
; function, say) -- declaring them special up front keeps the
; compiler from flagging such references as undefined variables
(declaim (special args! bare-args! +USAGE-TEXT!+))

(defmacro def-cli-args (script-name after-name description &body body)
  "Defines a command-line interface for a script. SCRIPT-NAME and
   AFTER-NAME make the usage line (`Usage: SCRIPT-NAME AFTER-NAME`),
   DESCRIPTION sits under it, and every clause in BODY is
     (option SHORT LONG DESCRIPTION FORMS...)
   FORMS run when SHORT or LONG turns up. A flag that takes a value
   reads it from (cadr args!) and must hand the rest on with
   (process-args! (cddr args!)) -- or the shorthand
   (assign-next-arg! var), which does both; a bare flag hands on
   (cdr args!). Combined short flags (-abc) are split up, and
   anything that isn't a flag lands in bare-args!.
   It defines, in the script's package:
     args!          the arguments still to be processed
     bare-args!     the non-flag arguments, in order
     +usage-text!+  the usage text, built from the clauses
     print-usage!   prints it and exits 0
     process-args!  runs the clauses over a list of arguments --
                    call (process-args! (cdr (cmdargs))) right after
   An error inside a clause dies, printing the usage text
   e.g. (def-cli-args \"frob\" \"[-v] -s SEP FILES...\" \"frobs the files\"
          (option \"-h\" \"--help\"    \"print usage\" (print-usage!))
          (option \"-v\" \"--verbose\" \"say more\"    (setq *verbose* t)
                                                 (process-args! (cdr args!)))
          (option \"-s\" \"--sep\"     \"separator\"   (assign-next-arg! *sep*)))
        (process-args! (cdr (cmdargs)))"
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
                            (or-die (progn ,@(nthcdr 4 x))
                                    (format nil
                                            "Fatal error processing ~A flag (~A)~%~%~A"
                                            ,(second x) error! +USAGE-TEXT!+)))))
                      body)))
  `(progn
     (defparameter args!        nil
       "The command-line arguments process-args! still has to look at;
        inside a def-cli-args clause, (cadr args!) is the flag's value")
     (defparameter bare-args!   nil
       "The command-line arguments that weren't flags, in order
        (complete once process-args! returns)")
     (defparameter +USAGE-TEXT!+ nil
       "The usage text def-cli-args built from its clauses
        (print-usage! prints it; an error in a clause dies with it)")
     (macrolet ((assign-next-arg! (avar)
       `(progn (setq ,avar (cadr args!)) (process-args! (cddr args!)))))
       (or-die
         (progn
           (setq +USAGE-TEXT!+
           (format nil "Usage: ~A ~A~%~A~%~%~A"
                   ,script-name ,after-name ,description
                   (format nil "~{~A~}" (list ,@flag-lines))))
         (defun print-usage! ()
           "Prints +usage-text!+ to stdout and exits with status 0"
           (format t "~A" +USAGE-TEXT!+)
           (die "" :status 0))
         (defun process-args! (args)
           "Runs def-cli-args' clauses over ARGS (normally (cdr (cmdargs))):
            flags run their forms, combined short flags (-abc) are split
            up, and everything else is collected into bare-args!"
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
                     (process-args! (cdr args!)))))))))
         "invalid arguments")))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; terminal things / terminal manipulation --------------- ;

(defun clear-screen ()
  "Clears the terminal screen"
   #+sbcl  (sb-ext:run-program "/bin/sh" (list "-c" "clear")
                               :input nil :output *standard-output*)
   #+ecl   (si:system "clear"))

(defun get-terminal-columns ()
  "Retrieves the number of columns in the terminal: via styx's
   TIOCGWINSZ ioctl if styx is loaded, else by querying the
   `$COLUMNS` environment variable. Returns
   (values num-of-columns t) if successful and (values 200 nil)
   if not"
  ; pluto can't depend on styx (the dependency points the other
  ; way) so the delegation is by runtime lookup
  (let ((styx-cols (and (find-package "STYX")
                        (find-symbol "TERMINAL-COLUMNS" "STYX"))))
    (if (and styx-cols (fboundp styx-cols))
      (multiple-value-bind (cols real-p) (funcall styx-cols)
        (if real-p (values cols t) (values 200 nil)))
      ; COLUMNS is a shell variable, usually not exported; the
      ; zsh shell-out is a poor man's ioctl (zsh sets COLUMNS
      ; from its own tty) for when styx isn't around
      (let ((raw-res (ignore-errors (parse-integer
                                      #+sbcl (sh "echo $COLUMNS")
                                      #-sbcl (get-envvar "COLUMNS" "80")
                                      ))))
        ; zsh answers 0 when it has no terminal to measure —
        ; that's a failure, not a zero-width terminal
        (if (and raw-res (plusp raw-res))
          (values raw-res t)
          (values 200 nil))))))

; TODO: is there are better DRY way?
(defun ansi-up-line (&optional (where *error-output*))
  "Moves the cursor up one line on WHERE (default *error-output*)
   by printing +ansi-escape-up+"
  (format where "~A" +ansi-escape-up+))

(defun ansi-up-lines (n &optional (where *error-output*))
  "Moves the cursor up N lines on WHERE (default *error-output*):
   ESC[<n>A. Does nothing for an N of 0"
  (when (plusp n)
    (format where "~C[~AA" #\ESC n)))

(defun ansi-left-all (&optional (where *error-output*))
  "Moves the cursor to the start of the line on WHERE (default
   *error-output*) by printing +ansi-escape-left-all+"
  (format where "~A" +ansi-escape-left-all+))

(defun ansi-clear-line (&optional (where *error-output*))
  "Blanks the current line on WHERE (default *error-output*): back
   to the start of the line, then as many spaces as the terminal is
   wide (get-terminal-columns)"
  ; (the start-of-line escape used to go to *error-output* no
  ; matter what WHERE was)
  (ansi-left-all where)
  (format where "~A" (make-string (get-terminal-columns) :initial-element #\Space)))

(defun ansi-left-one (&optional (where *error-output*))
  "Moves the cursor one column left on WHERE (default *error-output*)
   by printing +ansi-escape-left-one+"
  (format where "~A" +ansi-escape-left-one+))

; TODO: what if it's interactive
; TODO: test on different implementations
(defun progress-bar (index limit &key (interval 1)
                                      (where *error-output*)
                                      (width 60)
                                      (one-line t)
                                      (out-of nil))
  "Draws a progress bar for INDEX out of LIMIT on WHERE (default
   *error-output*), in yellow: ◖▬▬▬▭▭▭ ◗  50.00%. It's WIDTH cells
   wide (default 60) and only drawn when INDEX is a multiple of
   INTERVAL (default 1) -- and always when INDEX reaches LIMIT.
   ONE-LINE (default t) redraws over the previous bar (up a line,
   clear it, start of line) so the bar stays put; nil prints a
   fresh line each time. OUT-OF adds [INDEX/LIMIT] after the
   percentage. Made for the inside of a for-each, with index!"
  (when (or (= index limit) (and (= 0 (mod index interval))))
    (let* ((perc-done (/ index limit))
           (filled    (round (* perc-done width))))
      (when one-line
        (ansi-up-line     where)
        (ansi-clear-line  where)
        (ansi-left-all    where))
      (format where (yellow "~& ◖~A~A ◗  ~$% ~A"
              (make-string filled :initial-element #\▬)
              (make-string (max 0 (- width filled)) :initial-element #\▭)
              (float (* 100 perc-done))
              (if out-of (fn " [~A/~A]" index limit) "")))
      (force-output where))))

(defun loading-forever ()
  "Spins a - \\ | / cursor on *standard-output*, ten frames a second,
   forever: with-loading runs it in a thread alongside its body and
   kills it when the body is done. Dies gracefully on Control-C
   (it's a `forever`)"
  (let ((counter -1))
    (forever
      (incf counter)
      (let ((rune (aref *pluto-spinner-frames*
                        (mod counter (length *pluto-spinner-frames*)))))
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
    `(let ((,long-thread    (make-thread (lambda () ,@body) :name "background-thread"))
           (,loading-thread (make-thread #'loading-forever :name "loading-thread")))
       (let ((,the-return (join-thread ,long-thread)))
         (%terminate-thread ,loading-thread)
         (terpri)
         ,the-return))))

#-(or sbcl ecl)
(defmacro with-loading (&body body)
  `(warn "only implemented for sbcl and ecl"))

; -------------------
;; status lines: homebrew-style live progress -- one line per thing
;; being worked on, each with its own spinner until it's done. The
;; one rule: only the renderer thread prints; the workers (status!,
;; done!, failed!) just change the lines' state, under the mutex

(defstruct (%status-lines (:conc-name %sl-))
  keys texts states mutex where live-p key-width columns (frame 0) stop-p renderer)

(defun %styx-call (name default &rest args)
  "Calls styx's function NAME with ARGS when styx is loaded (pluto
   can't depend on it: the arrows point the other way); DEFAULT
   otherwise"
  (let ((f (and (find-package "STYX") (find-symbol name "STYX"))))
    (if (and f (fboundp f))
      (values (apply f args))
      default)))

(defun %stream-fd (stream)
  "The file descriptor behind STREAM when it's one of the standard
   streams, nil otherwise (a string stream, say)"
  #+sbcl
  (let ((s (if (typep stream 'synonym-stream)
             (symbol-value (synonym-stream-symbol stream))
             stream)))
    (when (typep s 'sb-sys:fd-stream)
      (return-from %stream-fd (sb-sys:fd-stream-fd s))))
  (cond ((eq stream *standard-output*) 1)
        ((eq stream *error-output*)    2)
        ((eq stream *terminal-io*)     1)
        (t                             nil)))

(defun %terminal-stream-p (stream)
  "Is STREAM a terminal? (styx asks the fd; without styx the
   standard streams are assumed to be one)"
  (let ((fd (%stream-fd stream)))
    (and fd (%styx-call "TTY-P" t fd) t)))

(defun %key-string (key)
  (if (symbolp key) (string-downcase (symbol-name key)) (fn "~A" key)))

(defun %make-status-lines (keys where)
  (let* ((keys   (coerce keys 'vector))
         (n      (length keys))
         (rows   (%styx-call "TERMINAL-ROWS" 24))
         ; animation needs a terminal with room for every line;
         ; otherwise lines are printed plainly as they finish
         (live-p (and (%terminal-stream-p where) (< n rows))))
    (make-%status-lines
      :keys      keys
      :texts     (make-array n :initial-element "")
      :states    (make-array n :initial-element :running)
      :mutex     (make-mutex :name "status-lines")
      :where     where
      :live-p    live-p
      :key-width (reduce #'max (map 'list (lambda (k) (length (%key-string k))) keys)
                         :initial-value 0)
      :columns   (multiple-value-bind (cols real-p) (get-terminal-columns)
                   (if real-p cols 80)))))

(defun %line-index (board key)
  (or (position key (%sl-keys board) :test *pluto-curly-test*)
      (error "status lines: there's no line for ~S" key)))

(defun %mark (state frame)
  (case state
    (:done   "✓")
    (:failed "✗")
    (t       (aref *pluto-spinner-frames*
                   (mod frame (length *pluto-spinner-frames*))))))

(defun %render-status-line (board i &key color-p)
  "One line as it should look right now -- mark, key (padded), text
   -- cut to the terminal's width"
  (let* ((state (aref (%sl-states board) i))
         (mark  (%mark state (%sl-frame board)))
         (mark  (if color-p
                  (case state
                    (:done   (green "~A" mark))
                    (:failed (red "~A" mark))
                    (t       (yellow "~A" mark)))
                  mark))
         (rest  (fn "~vA  ~A" (%sl-key-width board)
                    (%key-string (aref (%sl-keys board) i))
                    (aref (%sl-texts board) i)))
         (room  (max 0 (- (%sl-columns board) 2))))
    (fn "~A ~A" mark (if (> (length rest) room) (subseq rest 0 room) rest))))

(defun %render-status-frame (board)
  "Every line, each cleared then redrawn, newline-terminated: what
   the renderer writes each tick, after moving back up over the
   last frame"
  (with-output-to-string (s)
    (dotimes (i (length (%sl-keys board)))
      (format s "~A~A~%" +ansi-escape-clear-line+
              (%render-status-line board i :color-p t)))))

(defun %status-lines-redraw (board &key first-p)
  (let ((where (%sl-where board)))
    (unless first-p
      (ansi-up-lines (length (%sl-keys board)) where))
    (write-string (%render-status-frame board) where)
    (force-output where)))

(defun %status-lines-start (board)
  (when (%sl-live-p board)
    (let ((where (%sl-where board)))
      (write-string +ansi-escape-hide-cursor+ where)
      (with-mutex ((%sl-mutex board))
        (%status-lines-redraw board :first-p t))
      (setf (%sl-renderer board)
            (make-thread
              (lambda ()
                (loop until (%sl-stop-p board)
                      do (sleep 0.1)
                         (with-mutex ((%sl-mutex board))
                           (incf (%sl-frame board))
                           (%status-lines-redraw board))))
              :name "status-lines")))))

(defun %status-lines-stop (board)
  (when (%sl-live-p board)
    (setf (%sl-stop-p board) t)
    (join-thread (%sl-renderer board))
    (with-mutex ((%sl-mutex board))
      (%status-lines-redraw board))              ; the final frame
    (write-string +ansi-escape-show-cursor+ (%sl-where board))
    (force-output (%sl-where board))))

(defun %line-update (board key &key (text nil text-p) (state nil state-p))
  (let ((i (%line-index board key)))
    (with-mutex ((%sl-mutex board))
      (when text-p  (setf (aref (%sl-texts board) i) (fn "~A" text)))
      (when state-p (setf (aref (%sl-states board) i) state))
      ; no terminal, no redrawing: a line is printed once, when it
      ; finishes
      (when (and state-p (not (%sl-live-p board)))
        (format (%sl-where board) "~A~%" (%render-status-line board i))
        (force-output (%sl-where board))))
    nil))

(defmacro with-status-lines ((keys &key (where '*error-output*)) &body body)
  "Homebrew-style live progress: one line per key in KEYS (symbols
   or strings), each with a spinner until the body marks it with
   done! or failed!; status! changes a line's text meanwhile. A
   renderer thread redraws the lines in place ten times a second on
   WHERE (default *error-output*), so the body (and any threads it
   starts -- for-each/parallel, say) must not print there itself.
   Binds `lines!` to the board for the three macros. When the body
   ends, however it ends, the final state is left on screen and the
   cursor sits below it: the place for whatever comes next. Without
   a terminal (a pipe, cron) or with more keys than rows, nothing
   animates: each line is printed once, as it finishes
   e.g. (with-status-lines ('(zsh pluto))
          (for-each/parallel ('(zsh pluto))
            (status! value! \"fetching...\")
            (done! value! (fetch-and-describe value!))))"
  `(let ((lines! (%make-status-lines ,keys ,where)))
     (%status-lines-start lines!)
     (unwind-protect
       (progn ,@body)
       (%status-lines-stop lines!))))

(defmacro status! (key text)
  "Changes the text on KEY's line in the enclosing with-status-lines;
   its spinner keeps going"
  `(%line-update lines! ,key :text ,text))

(defmacro done! (key text)
  "Finishes KEY's line in the enclosing with-status-lines: a green ✓
   replaces the spinner, TEXT the text"
  `(%line-update lines! ,key :text ,text :state :done))

(defmacro failed! (key text)
  "Finishes KEY's line in the enclosing with-status-lines: a red ✗
   replaces the spinner, TEXT the text"
  `(%line-update lines! ,key :text ,text :state :failed))

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
        (xchoice  (q/sh (str-join (string #\Newline) choices)))
        (xmode    (case mode
                    (:columns   "-c")
                    (:table     "-t")
                    (:lines     "-l")
                    (otherwise  ""))))
    (let ((response
            (sh (fn "~A=$(echo ~A | smenu ~A -n~A ~A ~A); echo $~A"
                     tmpvar xchoice (if num-p "-N" "")
                     limit xmode
                     (if sep (fn "-T ~A" (q/sh sep)) "")
                     tmpvar) :echo nil)))
      (if (string= response "") nil response))))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; file/filename/directory operations -------------------- ;

(defun basename (apath)
  "The file part of APATH (name and extension) without the
   directory: (basename \"/foo/bar/baz.txt\") => \"baz.txt\". A
   directory pathname (trailing slash) has no file part, so \"\""
  (file-namestring apath))

(defparameter %%ylimit (expt 2 80))
(defparameter %%zlimit (expt 2 70))
(defparameter %%elimit (expt 2 60))
(defparameter %%plimit (expt 2 50))
(defparameter %%tlimit (expt 2 40))
(defparameter %%glimit (expt 2 30))
(defparameter %%mlimit (expt 2 20))
(defparameter %%klimit (expt 2 10))

(defun size-for-humans (asize) ; have block size?
  "ASIZE (in bytes) as a short human-readable string -- 512, 3K,
   2.0G: K and M are rounded to whole numbers, G and beyond get one
   decimal. Binary units (1K is 1024)"
  (cond
    ((> asize %%ylimit)     (fn "~1$Y" (/ asize %%ylimit)))
    ((> asize %%zlimit)     (fn "~1$Z" (/ asize %%zlimit)))
    ((> asize %%elimit)     (fn "~1$E" (/ asize %%elimit)))
    ((> asize %%plimit)     (fn "~1$P" (/ asize %%plimit)))
    ((> asize %%tlimit)     (fn "~1$T" (/ asize %%tlimit)))
    ((> asize %%glimit)     (fn "~1$G" (/ asize %%glimit)))
    ((> asize %%mlimit)     (fn "~AM" (round (/ asize %%mlimit))))
    ((> asize %%klimit)     (fn "~AK" (round (/ asize %%klimit))))
    (t                      (fn "~A" asize))))

(defun file-size (afilename &key (human nil))
  "The size of the file AFILENAME in bytes (file-length on a byte
   stream), or as a human-readable string (size-for-humans) with
   :human t"
  (with-open-file (s afilename :element-type '(unsigned-byte 8))
    (if human (size-for-humans (file-length s)) (file-length s))))

(defun pwd ()
  "The current working directory as a namestring, trailing slash
   included"
  (let ((tmp
          #+ecl  (ext:getcwd)
          #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))))
    (namestring tmp)))

(defun pathname->native (apath)
  "Converts pathname (or CL namestring) APATH into the exact string
   the operating system knows it by — no CL namestring escaping.
   This is the string to hand to C functions (through CFFI), etc.
   (SBCL backslash-escapes glob characters [* ? \\[] in namestrings;
   the native namestring is the raw on-disk name. ECL namestrings
   are already native — though note that ECL represents filenames
   containing glob characters as wild pathnames, which PROBE-FILE
   et al. won't accept, the string this returns is still correct)"
  #+sbcl (sb-ext:native-namestring apath)
  #-sbcl (namestring apath))

(defun realpath (apath &key (expand-symlinks t) (relative-to nil) (all-existing t))
  "The resolved, absolute path of APATH as a string, from GNU
   realpath (so it needs GNU coreutils: the :coreutils feature,
   which the rc files declare and clix's `make doctor` verifies)
   `expand-symlinks` nil keeps symlinks as they are (-s)
   `relative-to` a directory: the result is relative to it
   `all-existing` t (default) insists every path component exist
                  (-e); nil lets the last one be missing -- moot
                  while APATH itself has to exist for this to run
   APATH is quoted for the shell here, so spaces and such are fine
   Errors if APATH doesn't exist"
  (unless (member :coreutils *features*)
    (error "realpath: needs GNU coreutils (the :coreutils feature isn't set)"))
  (unless (probe-file apath)
    (error "no such file"))
  (let ((command (fn "realpath ~A ~A ~A ~A"
                     (if expand-symlinks "" "-s")
                     (if all-existing "-e" "")
                     (q/sh apath)
                     (if relative-to
                       (fn "--relative-to=~A" (q/sh (realpath relative-to))) ""))))
    (nth-value 0 (sh command))))

(defun inspect-pathname (apathname)
  "Prints everything common lisp will say about APATHNAME to
   *error-output* -- device, host, directory, name, type, truename,
   namestrings, wildness, directory listing, write date... -- for
   working out what a pathname actually is"
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

;;;;;
;;;;; STEALING FROM CL-FAD
;;;;;

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC (a pathname designator) does not designate
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
  "Does PATHSPEC name an existing file -- a file, not a directory?
   T or NIL (directory-exists-p and file-or-directory-exists-p are
   the companions; those two return the truename)"
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


(defun -path (pathone pathtwo)
  "PATHONE relative to the directory PATHTWO, as a pathname:
   (-path \"/a/b/c.txt\" \"/a/\") => #P\"b/c.txt\". Both must exist, and
   PATHONE goes through probe-file, so symlinks are resolved first"
  (pathname (enough-namestring (probe-file pathone)
                               (directory-exists-p  pathtwo))))

(defun +path (pathone pathtwo)
  "The directory PATHONE with PATHTWO appended, as a pathname -- the
   inverse of -path: (+path \"/a/\" \"b/c.txt\") => #P\"/a/b/c.txt\".
   PATHONE must exist"
  (merge-pathnames pathtwo (directory-exists-p pathone)))

(defun absolute->relative (apath &optional root)
  "The absolute APATH made relative to ROOT (default: the current
   directory), as a pathname; errors if APATH isn't absolute. Both
   must exist (see -path)"
  (when (eq :RELATIVE (car (pathname-directory apath)))
    (error "pathname is not absolute"))
  (-path apath (if root root (pwd))))

(defun change-extension (apath new-extension)
  "APATH with its extension swapped for NEW-EXTENSION, as a
   pathname: (change-extension \"notes.txt\" \"md\") => #P\"notes.md\"
   (nil takes the extension off)"
  (make-pathname :type new-extension
                 :defaults apath))

; pluto can't use charon's `~m` (the dependency points the other
; way), and referencing it here just compiled into a call to a
; function that never exists; regex support arrives with charon
; (cl-ppcre) at runtime, so look it up then
(defun %regex-matches-p (regex astring)
  (let ((scan (and (find-package "CL-PPCRE")
                   (find-symbol "SCAN" "CL-PPCRE"))))
    (unless (and scan (fboundp scan))
      (error "file-find: :regex/:inv-regex needs charon (cl-ppcre) loaded"))
    (funcall scan regex astring)))

; TODO: document
(defun file-find (apathname &key (type nil) (test (constantly t)) (ext nil)
                            (regex nil) (inv-regex nil) (full t))
  "ext implies type file
   regex is for full path if full is t (default)
   (:regex/:inv-regex require charon to be loaded)"
  (let (to-return)
    (walk-directory apathname #'(lambda (x) (push x to-return))
                    :directories :depth-first
                    :test test)
    ; the last visit (car, pre-reverse) is the root as-typed;
    ; canonicalize it — unless :test filtered everything out,
    ; in which case an empty find is just an empty list
    (when to-return
      (setf (car to-return) (probe-file (car to-return))))
    (unless full (setq to-return (mapcar #'absolute->relative to-return)))
    (setq to-return (reverse to-return))
    (when ext (setq type :file))
    (when type
      (setq to-return
            (cond ((eq type :file) (remove-if-not #'file-exists-p to-return))
                  ((eq type :dir)  (remove-if-not #'directory-exists-p to-return)))))
    (when ext
      (setq to-return (remove-if-not
                        (lambda (x) (string= (pathname-type x) ext))
                        to-return)))
    (when regex
      (setq to-return (remove-if-not
                        (lambda (x) (%regex-matches-p regex (namestring x)))
                        to-return)))
    (when inv-regex
      (setq to-return (remove-if
                        (lambda (x) (%regex-matches-p inv-regex (namestring x)))
                        to-return)))
    to-return))

; ------------------------------------------------------- ;
; readtable containment ---------------------------------- ;

; by this point every pluto reader macro (•, {, ?, Ø, #?) is installed
; in the current (global) readtable — give that readtable a name so
; code compiled under a DIFFERENT readtable can still ask for it.
; charon.asd and styx.asd use the helper as :around-compile, so their
; files always compile under pluto's readtable while third-party deps
; compile under a standard one (a bare `?` token in a dep — cxml's
; content-model `(? ...)` clauses, e.g. — is a reader error under
; pluto's readtable).
(defparameter *pluto-readtable* *readtable*
  "The readtable with pluto's reader macros (•, {, ?, Ø, #?)
   installed, captured as pluto finished loading -- for reading
   pluto syntax when *readtable* has been rebound to a standard
   one (see call-with-pluto-readtable)")

(defun call-with-pluto-readtable (thunk)
  "Calls THUNK with *readtable* bound to *pluto-readtable*. It's the
   :around-compile hook in charon.asd and styx.asd: their files read
   pluto's syntax while the third-party dependencies compile under a
   standard readtable (a bare `?` token in a dependency is a reader
   error under pluto's)"
  (let ((*readtable* *pluto-readtable*))
    (funcall thunk)))

; ------------------------------------------------------- ;

