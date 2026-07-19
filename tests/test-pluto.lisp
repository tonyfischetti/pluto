
(load "../pluto.lisp")
(use-package :pluto)

(load "def-test-doc.lisp")

(declaim (optimize (speed 3)))

(start-test/doc :title "Pluto")

; --------------------------------------------------------------- ;

(def-raw-markdown
  (fn "-----~%~%### about~%~%a common lisp package that's out there~%~%"))

; --------------------------------------------------------------- ;

(def-test/doc-section "pluto parameters")

(def-raw-markdown "fill this out")

  ; *pluto-shell* should always resolve to a real executable
  (def-test/doc-test '*pluto-shell*
    `((test-able returns))
    ""
    (and (stringp test-return-value!)
         (probe-file test-return-value!)
         t)
    *pluto-shell*)

; --------------------------------------------------------------- ;

(def-test/doc-section "formatting")

(def-test/doc-test 'fn
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hello goodbye")
  (fn "~A ~A" "hello" "goodbye"))

(def-test/doc-test 'ft
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "hello goodbye")
  (ft "~A ~A" "hello" "goodbye"))

; --------------------------------------------------------------- ;

(def-test/doc-section "ansi colors and codes")

(def-raw-markdown "fill this out")

  (def-test/doc-test 'green
    `((test-able returns))
    'function
    (and (search "hi" test-return-value!)
         (search (string #\ESC) test-return-value!))
    (green "hi"))

; --------------------------------------------------------------- ;

(def-test/doc-section "string operations")

(def-test/doc-test 'str+
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belleandsebastian")
  (str+ "belle" "and" "sebastian"))

(def-test/doc-test 'str+
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "(1 2 3) go")
  (str+ '(1 2 3) " go"))

(def-test/doc-test 'str-join
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "one;two")
  (str-join ";" '("one" "two")))

(def-test/doc-test 'str-sub
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belle")
  (str-sub "belle and sebastian" 0 5))

(def-test/doc-test 'str-sub
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "belle")
  (str-sub "belle and sebastian" 0 -14))

(def-test/doc-test 'str-sub
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "sebastian")
  (str-sub "belle and sebastian" 10))

(def-test/doc-test 'string->char-list
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("b" "e" "l" "l" "e"))
  (string->char-list "belle"))

(def-test/doc-test 'split-string->lines
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! '("this" "that" "and the other"))
  (split-string->lines (format nil "this~%that~%and the other")))

  (def-test/doc-test 'str-sub
    `((test-able returns))
    'function
    (string= test-return-value! "lle")
    (str-sub "belle" -3))

  (def-test/doc-test 'str-join
    `((test-able returns))
    'function
    (string= test-return-value! "")
    (str-join ";" nil))

(def-test/doc-test 'repeat-string
  `(markdown-able (test-able returns))
  "Repeats a string TIMES times"
  (string= test-return-value! "ababab")
  (repeat-string "ab" 3))


; --------------------------------------------------------------- ;

(def-test/doc-section "some essential utilities/macros")

(def-test/doc-test 'alambda
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! (list 10 9 8 7 6 5 4 3 2 1))
  (funcall (alambda (x) (when (> x 0) (cons x (self! (- x 1))))) 10))

(def-test/doc-test 'flatten
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(A B C D E))
  (flatten `(a b (c d (e)))))

(def-test/doc-test 'take
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `((a b) (c d e f)))
  (multiple-value-bind (one two)
    (take `(a b c d e f) 2)
    (list one two)))

  ; taking more than the list holds should error
  (def-test/doc-test 'take
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (take `(a b) 5))

(def-test/doc-test 'group
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `((a b) (c d) (e f)))
  (group `(a b c d e f) 2))


(def-test/doc-test '-<>
  `(markdown-able (test-able returns) (bench-able 5))
  'function
  (= test-return-value! 2)
  (-<> "4"
       (parse-integer <>)
       (sqrt <>)))

(def-test/doc-test 'interpose
  `((test-able returns) markdown-able)
  'function
  (equal test-return-value! `(a sep b sep c))
  (interpose 'sep `(a b c)))

  ; regression: multibyte characters used to leave junk
  ; padding at the end of the slurped string
  (def-test/doc-test 'slurp
    `((test-able returns))
    'function
    (string= test-return-value! (fn "héllo wörld — ünïcode~%"))
    (let ((tmpf "tmp-slurp-roundtrip.txt"))
      (barf tmpf (fn "héllo wörld — ünïcode~%") :overwrite t)
      (let ((contents (slurp tmpf)))
        (delete-file tmpf)
        contents)))

  ; slurping a file that doesn't exist should error
  (def-test/doc-test 'slurp
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (slurp "this-file-hopefully-does-not-exist.txt"))

(def-test/doc-test 'walk-replace-sexp
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(+ 1 (* 2 3)))
  (walk-replace-sexp `(+ 1 (* 2 10)) 10 3))

(def-test/doc-test 'walk-replace-sexp
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(a b it c))
  (walk-replace-sexp `(a b (x y) c) `(x y) `it))

(def-test/doc-test 'alistp
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (alistp `((a . 1) (b . 2))))

  (def-test/doc-test 'alistp
    `((test-able returns))
    'function
    (null test-return-value!)
    (alistp `(1 2 3)))

(def-test/doc-test 'with-hash-entry
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 42)
  (let ((tmp (make-hash-table)))
    (with-hash-entry (tmp :count)
      (setf entry! 42))
    (gethash :count tmp)))

(def-test/doc-test 'if-hash-entry
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(:present :absent))
  (let ((tmp (make-hash-table)))
    (setf (gethash :a tmp) 1)
    (list (if-hash-entry (tmp :a) :present :absent)
          (if-hash-entry (tmp :b) :present :absent))))

  (def-test/doc-test 'if-not-hash-entry
    `((test-able returns))
    'function
    (equal test-return-value! `(:has-entry :no-entry))
    (let ((tmp (make-hash-table)))
      (setf (gethash :a tmp) 1)
      (list (if-not-hash-entry (tmp :a) :no-entry :has-entry)
            (if-not-hash-entry (tmp :b) :no-entry :has-entry))))

(def-test/doc-test 'delim
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! (fn "a~Cb~Cc" #\Tab #\Tab))
  (delim `("a" "b" "c")))

  (def-test/doc-test 'delim
    `((test-able returns))
    'function
    (string= test-return-value! (fn "A~C1~%B~C2" #\Tab #\Tab))
    (delim `((a . 1) (b . 2)) :what :alist))

(def-test/doc-test 'round-to
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3.14)
  (round-to 3.14159 2))

(def-test/doc-test 'debug-these
  `((test-able stderr))
  'function
  (and (search "TMP" test-stderr!) (search "84" test-stderr!))
  (let ((tmp 42))
    (debug-these tmp (* tmp 2))))

(def-test/doc-test 'with-time
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "time elapsed: 1")
  (with-time
    (sleep 1)
    (format nil "time elapsed: ~A" (round time!))))

  ; regression: time! is a float with sub-second resolution now
  (def-test/doc-test 'with-time
    `((test-able returns))
    'function
    (and (floatp test-return-value!)
         (< 0.05 test-return-value! 1.0))
    (with-time
      (sleep 0.1)
      time!))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "4 seconds")
  (time-for-humans 4))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "1.11 hours")
  (time-for-humans 4000))

(def-test/doc-test 'time-for-humans
  `((test-able returns) markdown-able)
  'function
  (string= test-return-value! "2.21 days")
  (time-for-humans 191000))

  ; regression: boundary values used to fall through and return NIL
  (def-test/doc-test 'time-for-humans
    `((test-able returns))
    'function
    (string= test-return-value! "1.00 minutes")
    (time-for-humans 60))

  (def-test/doc-test 'time-for-humans
    `((test-able returns))
    'function
    (string= test-return-value! "1.00 hours")
    (time-for-humans 3600))

  (def-test/doc-test 'time-for-humans
    `((test-able returns))
    'function
    (string= test-return-value! "1.00 days")
    (time-for-humans 86400))

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

; (def-test/doc-test 'file-size
;   `(markdown-able (test-able returns))
;   'function
;   (string= test-return-value! "17k")
;   (file-size "interior-of-a-heart.txt"))
;
; (def-test/doc-test 'file-size
;   `(markdown-able (test-able returns))
;   'function
;   (= test-return-value! 14433)
;   (file-size "interior-of-a-heart.txt" :just-bytes t))

; --------------------------------------------------------------- ;

(def-test/doc-section "for-each and friends")

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "1 -> A;2 -> B;3 -> C;")
  (for-each/list '(a b c)
    (format t "~A -> ~A;" index! value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "1 -> A;2 -> B;3 -> C;")
    (for-each '(a b c)
      (format t "~A -> ~A;" index! value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "A;B;")
  (for-each/list '(a b c d e)
    (if (> index! 2) (break!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "A;B;")
    (for-each '(a b c d e)
      (if (> index! 2) (break!))
      (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "A;B;D;E;")
  (for-each/list '(a b c d e)
    (if (= index! 3) (continue!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "A;B;D;E;")
    (for-each '(a b c d e)
      (if (= index! 3) (continue!))
      (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `((test-able stdout))
  'function
  (string= test-stdout! "a;b;d;e;")
  (for-each/vector #("a" "b" "c" "d" "e")
    (if (= index! 3) (continue!))
    (format t "~A;" value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
    (string= test-stdout! "a;b;d;e;")
    (for-each #("a" "b" "c" "d" "e")
      (if (= index! 3) (continue!))
      (format t "~A;" value!)))

(def-raw-markdown
  "If the argument to `for-each` is a string and the file exists,\
  `for-each/line` is dispatched. Otherwise, it is treated like a\
  character vector")

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "1 -> we gotta celebrate diversity;2 -> in the university;")
  (for-each/line "somebody.txt"
    (when (> index! 2) (break!))
    (format t "~A -> ~A;" index! value!)))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `((test-able stdout))
    'function
      (string= test-stdout! "1 -> we gotta celebrate diversity;2 -> in the university;")
    (for-each "somebody.txt"
      (when (> index! 2) (break!))
      (format t "~A -> ~A;" index! value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! "n;o;t;-;a;-;f;i;l;e;.;t;x;t;")
  (for-each "not-a-file.txt"
    (format t "~A;" value!)))

(def-test/doc-test 'for-each
  `(markdown-able (test-able stdout))
  'function
  (or (string= test-stdout! (fn "GREEN -> veridian;RED -> cadmium;"))
      (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;")))
  (let ((tmp (make-hash-table)))
    (setf (gethash 'green tmp) "veridian")
    (setf (gethash 'red tmp) "cadmium")
    (for-each/hash tmp
      (format t "~A -> ~A;" key! value!))))

  ; auto-"dispatch" variant
  (def-test/doc-test 'for-each
    `(markdown-able (test-able stdout))
    'function
    (or (string= test-stdout! (fn "GREEN -> veridian;RED -> cadmium;"))
        (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;")))
    (let ((tmp (make-hash-table)))
      (setf (gethash 'green tmp) "veridian")
      (setf (gethash 'red tmp) "cadmium")
      (for-each tmp
        (format t "~A -> ~A;" key! value!))))

(def-test/doc-test 'for-each/alist
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! (fn "RED -> cadmium;GREEN -> veridian;"))
  (let ((tmp (list (cons 'red "cadmium")
                   (cons 'green "veridian"))))
    (for-each/alist tmp
      (format t "~A -> ~A;" key! value!))))

; --------------------------------------------------------------- ;

(def-test/doc-section "error handling")

(def-test/doc-test 'or-die
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3)
  (or-die ("this message never appears")
    (/ 3 1)))

(def-test/doc-test 'or-die
  `(markdown-able (test-able returns))
  'function
  (and (null test-return-value!)
       (search "DIVISION-BY-ZERO" test-stderr!))
  (or-die ((fn "the error was: ~A" (type-of error!))
           :errfun #'advise)
    (/ 3 0)))

(def-test/doc-test 'or-do
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3)
  (or-do :fallback (/ 3 1)))

(def-test/doc-test 'or-do
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! :fallback)
  (or-do :fallback (/ 3 0)))

  (def-test/doc-test 'advise
    `((test-able stderr))
    'function
    (search "be careful" test-stderr!)
    (advise "be careful"))

(def-raw-markdown
  (fn "Note: the reader macros below necessarily render already-expanded~%~%"))

(def-test/doc-test '|#?|
  `(markdown-able (test-able returns))
  "reader macro: `#?<form>` wraps `<form>` in `ignore-errors`"
  (null test-return-value!)
  #?(/ 3 0))

(def-test/doc-test '|?|
  `(markdown-able (test-able returns))
  "reader macro: `? <form> <fallback>` evaluates to `<form>` if it's non-nil, else `<fallback>`"
  (= test-return-value! 42)
  ?(gethash :missing (make-hash-table)) 42)

  ; Ø is possibly getting excised, so: tests but no docs
  (def-test/doc-test '|Ø|
    `((test-able returns))
    "reader macro: `Ø<form>` errors if `<form>` is null, else passes it through"
    (= test-return-value! 42)
    (let ((x 42)) Øx))

  (def-test/doc-test '|Ø|
    `((test-able returns))
    "reader macro: `Ø<form>` errors if `<form>` is null, else passes it through"
    (and test-error! (null test-return-value!))
    (let ((x nil)) Øx))

; --------------------------------------------------------------- ;

(def-test/doc-section "universal indexing operator syntax")

(def-raw-markdown
  (fn "`{ thing index }` works on lists, alists, vectors, hash-tables, structs, and CLOS objects — and is setf-able. Successive indexes burrow into nested structures. (The docs below show the `{ }` reader macro already expanded into `PLUTO-GET`)~%~%"))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  "`{ thing index }` — the universal indexing operator"
  (= test-return-value! 2)
  { `(1 2 3) 1 })

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 2)
  (let ((tmp `((:a . 1) (:b . 2))))
    { tmp :b }))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (string= test-return-value! "cadmium")
  (let ((tmp (make-hash-table)))
    (setf (gethash :red tmp) "cadmium")
    { tmp :red }))

; note: a list-of-lists can't be nth-indexed with { } because
; every element is a cons, so GET-AT dispatches to alist/assoc
(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 3)
  { #(#(1 2) #(3 4)) 1 0 })

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (equalp test-return-value! #(1 99 3))
  (let ((tmp (vector 1 2 3)))
    (setf { tmp 1 } 99)
    tmp))

; --------------------------------------------------------------- ;

(def-test/doc-section "shell and zsh")

(def-test/doc-test 'zsh
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hello")
  (zsh "echo hello"))

(def-test/doc-test 'zsh
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("out" "err" 0))
  (multiple-value-bind (out err code)
    (zsh "echo out; echo err >&2")
    (list out err code)))

(def-test/doc-test 'zsh
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1" "2" "3"))
  (zsh "seq 3" :split t))

(def-test/doc-test 'zsh
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout! (fn "$ echo hi~%"))
  (zsh "echo hi" :dry-run t))

  ; a command with a non-zero exit code should signal
  ; (with the code in the condition)
  (def-test/doc-test 'zsh
    `((test-able returns))
    'function
    (and test-error! (search "42" (fn "~A" test-error!)))
    (zsh "exit 42"))

; system stuff (test-able only: results are machine-dependent)

  (def-test/doc-test 'hostname
    `((test-able returns))
    'function
    (and (stringp test-return-value!)
         (> (length test-return-value!) 0))
    (hostname))

  (def-test/doc-test 'get-envvar
    `((test-able returns))
    'function
    (search "/" test-return-value!)
    (get-envvar "HOME"))

  (def-test/doc-test 'get-envvar
    `((test-able returns))
    'function
    (string= test-return-value! "the-default")
    (get-envvar "SURELY_THIS_ENVVAR_DOES_NOT_EXIST" "the-default"))

; --------------------------------------------------------------- ;

(def-test/doc-section "file/filename/directory operations")

(def-test/doc-test 'basename
  `(markdown-able (test-able returns))
  "Returns the filename portion of a path"
  (string= test-return-value! "baz.txt")
  (basename "/foo/bar/baz.txt"))

(def-test/doc-test 'change-extension
  `(markdown-able (test-able returns))
  "Returns a pathname with the file extension changed"
  (string= (namestring test-return-value!) "/foo/bar/baz.md")
  (change-extension "/foo/bar/baz.txt" "md"))

(def-test/doc-test 'size-for-humans
  `(markdown-able (test-able returns))
  "Formats a byte count for human consumption"
  (string= test-return-value! "3M")
  (size-for-humans (* 3 (expt 2 20))))

(def-test/doc-test 'size-for-humans
  `(markdown-able (test-able returns))
  ""
  (string= test-return-value! "2.0G")
  (size-for-humans (expt 2 31)))

(def-test/doc-test 'file-exists-p
  `(markdown-able (test-able returns))
  "FILE-EXISTS-P and DIRECTORY-EXISTS-P return truthy (the truename) or NIL"
  (equal test-return-value! `(t t nil))
  (list (if (file-exists-p "somebody.txt") t nil)
        (if (directory-exists-p "wayward files") t nil)
        (if (file-exists-p "no-such-file.txt") t nil)))

  (def-test/doc-test 'file-size
    `((test-able returns))
    'function
    (= test-return-value! 5)
    (let ((tmpf "tmp-file-size.txt"))
      (barf tmpf "12345" :overwrite t)
      (let ((s (file-size tmpf)))
        (delete-file tmpf)
        s)))

  (def-test/doc-test 'pwd
    `((test-able returns))
    'function
    (search "tests" test-return-value!)
    (pwd))

  (def-test/doc-test 'ls
    `((test-able returns))
    'function
    (and (listp test-return-value!)
         (member "somebody.txt" test-return-value!
                 :key #'file-namestring :test #'string=)
         t)
    (ls "./"))

  (def-test/doc-test 'file-find
    `((test-able returns))
    'function
    (= (length test-return-value!) 2)
    (progn
      (zsh "mkdir -p tmp-walk/sub && touch tmp-walk/a.txt tmp-walk/sub/b.txt tmp-walk/c.md")
      (let ((found (file-find "tmp-walk" :ext "txt")))
        (zsh "rm -rf tmp-walk")
        found)))

  (def-test/doc-test '-path
    `((test-able returns))
    'function
    (string= (namestring test-return-value!) "somebody.txt")
    (-path "somebody.txt" "./"))

  (def-test/doc-test '+path
    `((test-able returns))
    'function
    (and (search "tests" (namestring test-return-value!))
         (search "foo.txt" (namestring test-return-value!)))
    (+path "./" "foo.txt"))

; --------------------------------------------------------------- ;

; more regression tests (test-able only; these never render into the docs)

  ; regression: command output without a trailing newline
  ; used to lose its last character
  (def-test/doc-test 'zsh
    `((test-able returns))
    'function
    (string= test-return-value! "foo")
    (zsh "echo -n foo"))

  (def-test/doc-test 'zsh
    `((test-able returns))
    'function
    (string= test-return-value! "foo")
    (zsh "echo foo"))

  ; regression: die-if-null used to EVAL its arguments,
  ; which broke on lexical variables
  (def-test/doc-test 'die-if-null
    `((test-able returns))
    'function
    (and (null test-error!) (eq test-return-value! :survived))
    (let ((x 5) (y "hi"))
      (die-if-null x y)
      :survived))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

#-clisp
(load "~/quicklisp/setup.lisp")
(ql:quickload :charon :silent t)
(use-package :charon)

(def-test/doc-section "temporary charon tests")

(def-test/doc-test 'parse-float
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 5.4)
  (parse-float "5.4"))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; (ql:quickload :styx :silent t)
; (use-package :styx)
;
; (def-test/doc-section "temporary styx tests")
;
; (def-test/doc-test 'stat-filesize
;   `(markdown-able (test-able returns))
;   'function
;   (= test-return-value! 14433)
;   (stat-filesize "interior-of-a-heart.txt"))


; --------------------------------------------------------------- ;

(end-test/doc)

(if (run-tests)
  (with-a-file "pluto-results.md" :w
    (render-markdown stream!))
  (die "~%at least one test failed"))

