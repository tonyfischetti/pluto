
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

(def-raw-markdown
  (fn "A few special variables steer pluto. Rebind them (or `setq` them at the top of a script) to change the defaults~%~%"))

(def-test/doc-test '*pluto-external-format*
  `(markdown-able (test-able returns))
  'variable
  (eq test-return-value! :utf-8)
  *pluto-external-format*)

(def-test/doc-test '*pluto-curly-test*
  `(markdown-able (test-able returns))
  'variable
  (= test-return-value! 2)
  (let ((*pluto-curly-test* #'string-equal))
    { `(("a" . 1) ("b" . 2)) "B" }))

(def-test/doc-test '*pluto-shell*
  `(markdown-able (test-able returns))
  'variable
  (eq test-return-value! t)
  (and (stringp *pluto-shell*) (probe-file *pluto-shell*) t))

(def-test/doc-test '*unix-epoch-difference*
  `(markdown-able (test-able returns))
  'variable
  (= test-return-value! 2208988800)
  *unix-epoch-difference*)

(def-test/doc-test '*whitespaces*
  `(markdown-able (test-able returns))
  'variable
  (string= test-return-value! "hi")
  (string-trim *whitespaces* (fn "~C hi ~C~%" #\Tab #\Return)))

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

(def-raw-markdown
  (fn "The color functions wrap a `format` control string in a bold ANSI escape and a reset. The escapes themselves are the `+...+` constants (`+red-bold+`, `+reset-terminal-color+`, and the cursor movements `+ansi-escape-up+`, `+ansi-escape-left-all+`, `+ansi-escape-left-one+` that the progress bar leans on). The examples compare against the raw escapes, since the strings themselves don't render here~%~%"))

(def-test/doc-test 'red
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (string= (red "hi") (fn "~C[31;1mhi~C[0m" #\ESC #\ESC)))

(def-test/doc-test 'yellow
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (string= (yellow "~A of ~A" 1 2) (fn "~C[33;1m1 of 2~C[0m" #\ESC #\ESC)))

(def-test/doc-test 'make-ansi-escape
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (string= (make-ansi-escape 4 'underline) (fn "~C[4;4m" #\ESC)))

(def-test/doc-test '+reset-terminal-color+
  `(markdown-able (test-able returns))
  'variable
  (eq test-return-value! t)
  (string= +reset-terminal-color+ (fn "~C[0m" #\ESC)))

  ; regression: the decoration used to be compared with EQ against
  ; pluto's own symbols, so 'underline from any other package (this
  ; file's, your script's) fell through to "no decoration"
  (def-test/doc-test 'make-ansi-escape
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (and (string= (make-ansi-escape 7 'reversed)  (fn "~C[7;7m" #\ESC))
         (string= (make-ansi-escape 7 :reversed)  (fn "~C[7;7m" #\ESC))
         (string= (make-ansi-escape 0 nil)        (fn "~C[0m"   #\ESC))
         t))

  ; every color function is made by the same macro; spot-check the
  ; rest against their escapes
  (def-test/doc-test 'magenta
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (and (string= (magenta "x") (str+ +magenta-bold+ "x" +reset-terminal-color+))
         (string= (green "x")   (str+ +green-bold+   "x" +reset-terminal-color+))
         (string= (cyan "x")    (str+ +cyan-bold+    "x" +reset-terminal-color+))
         (string= (blue "x")    (str+ +blue-bold+    "x" +reset-terminal-color+))
         (string= (grey "x")    (str+ +grey-bold+    "x" +reset-terminal-color+))
         t))

  ; the docstring the macro writes names the color
  (def-test/doc-test 'red
    `((test-able returns))
    'function
    (search "bold red" test-return-value!)
    (documentation 'red 'function))

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
  'function
  (string= test-return-value! "ababab")
  (repeat-string "ab" 3))

(def-test/doc-test 'str-pad
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "02")
  (str-pad "2" 2 :pad "0"))

; the motivating case: pieces of a parsed date (integers work, too)
(def-test/doc-test 'str-pad
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "2026-11-02")
  (fn "~A-~A-~A" 2026 (str-pad 11 2 :pad "0") (str-pad 2 2 :pad "0")))

(def-test/doc-test 'str-pad
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "name    ")
  (str-pad "name" 8 :side :right))

(def-test/doc-test 'str-pad
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "--name---")
  (str-pad "name" 9 :side :both :pad #\-))

  ; defaults: spaces, on the left
  (def-test/doc-test 'str-pad
    `((test-able returns))
    'function
    (string= test-return-value! "    name")
    (str-pad "name" 8))

  ; never truncates (and exactly-wide comes back as is)
  (def-test/doc-test 'str-pad
    `((test-able returns))
    'function
    (equal test-return-value! `("2026" "ab" "2026" "2026"))
    (list (str-pad "2026" 2 :pad "0")
          (str-pad "ab" 2)
          (str-pad "2026" 2 :side :right)
          (str-pad "2026" 2 :side :both)))

  ; :both with an even leftover is symmetric
  (def-test/doc-test 'str-pad
    `((test-able returns))
    'function
    (string= test-return-value! "  ab  ")
    (str-pad "ab" 6 :side :both))

  ; the pad is ONE character
  (def-test/doc-test 'str-pad
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (str-pad "2" 4 :pad "ab"))

  ; an unknown side is an error (not a silent left-pad)
  (def-test/doc-test 'str-pad
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (str-pad "2" 4 :side :middle))


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

(def-test/doc-test 'barf
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("onetwo" "three"))
  (let ((tmpf "tmp-barf.txt"))
    (barf tmpf "one")
    (barf tmpf "two")
    (let ((appended (slurp tmpf)))
      (barf tmpf "three" :overwrite t)
      (let ((overwritten (slurp tmpf)))
        (delete-file tmpf)
        (list appended overwritten)))))

  ; regression: barf wrote in the implementation's default encoding,
  ; ignoring *pluto-external-format*; é is two bytes in UTF-8, one
  ; in Latin-1
  (def-test/doc-test 'barf
    `((test-able returns))
    'function
    (equal test-return-value! `(2 1))
    (let ((tmpf "tmp-barf-enc.txt"))
      (barf tmpf "é" :overwrite t)
      (let ((utf8 (file-size tmpf)))
        (let ((*pluto-external-format* :latin-1))
          (barf tmpf "é" :overwrite t))
        (let ((latin1 (file-size tmpf)))
          (delete-file tmpf)
          (list utf8 latin1)))))

(def-test/doc-test 'slurp-lines
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("we gotta celebrate diversity" "in the university"))
  (subseq (slurp-lines "somebody.txt") 0 2))

  ; regression: the reading modes used :if-does-not-exist
  ; :create — reading a missing file silently made an empty one
  (def-test/doc-test 'with-a-file
    `((test-able returns))
    'function
    (and test-error!
         (not (probe-file "tmp-should-not-exist.txt")))
    (with-a-file "tmp-should-not-exist.txt" :r
      (read-line stream!)))

  ; regression: :b used element-type 'unsigned-byte (not 8-bit
  ; octets); "we" must read back as (119 101)
  (def-test/doc-test 'with-a-file
    `((test-able returns))
    'function
    (equal test-return-value! `((unsigned-byte 8) 119 101))
    (with-a-file "somebody.txt" :b
      (list (stream-element-type stream!)
            (read-byte stream!)
            (read-byte stream!))))

(def-test/doc-test 'with-temp-file
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hello")
  (with-temp-file
    (barf tempfile! "hello")
    (slurp tempfile!)))

  ; the file is gone afterwards
  (def-test/doc-test 'with-temp-file
    `((test-able returns))
    'function
    (and (stringp (car test-return-value!))
         (null (cadr test-return-value!)))
    (let (path)
      (with-temp-file (setq path tempfile!))
      (list path (probe-file path))))

(def-test/doc-test 'capture-all-outputs
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(42 "to stdout" "to stderr"))
  (multiple-value-list
    (capture-all-outputs
      (ft "to stdout")
      (format *error-output* "to stderr")
      42)))

(def-test/doc-test 'display-table
  `(markdown-able (test-able stdout))
  'function
  (string= test-stdout!
           (fn "|------|------|~%| name | n    |~%|------|------|~%| ab   | 1    |~%| cd   | 2    |~%|------|------|~%"))
  (display-table '("name" "n") '(("ab" 1) ("cd" 2)) 6))

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

; --------------------------------------------------------------- ;

(def-test/doc-section "queries")

; interactive, so the example feeds it a bare Enter, which picks the default
(def-test/doc-test 'y-or-n-def
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (let ((*query-io* (make-two-way-stream (make-string-input-stream (fn "~%"))
                                         (make-string-output-stream))))
    (y-or-n-def "proceed?" :default #\y)))

  ; an explicit answer beats the default, and the prompt shows the default
  (def-test/doc-test 'y-or-n-def
    `((test-able returns))
    'function
    (equal test-return-value! `(nil "[Y/n]"))
    (let* ((out       (make-string-output-stream))
           (*query-io* (make-two-way-stream (make-string-input-stream (fn "n~%")) out)))
      (let ((answer (y-or-n-def "proceed?" :default #\y)))
        (list answer
              (and (search "[Y/n]" (get-output-stream-string out)) "[Y/n]")))))

; --------------------------------------------------------------- ;

(def-test/doc-section "other abbreviations and shortcuts")

(def-test/doc-test 'λ
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(1 4 9))
  (mapcar (λ (x) (* x x)) `(1 2 3)))

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

(def-test/doc-section "what's the deal with time?")

(def-raw-markdown
  (fn "Universal time is common lisp's (seconds since 1900), unix time is everyone else's (since 1970). The pretty-printers read the clock in the local time zone, so the examples build their instants with `encode-universal-time` in local time, too~%~%"))

(def-test/doc-test 'universal->unix-time
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 0)
  (universal->unix-time (encode-universal-time 0 0 0 1 1 1970 0)))

(def-test/doc-test 'unix->universal-time
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 2208988800)
  (unix->universal-time 0))

  ; they're inverses
  (def-test/doc-test 'unix->universal-time
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((now (get-universal-time)))
      (and (= now (unix->universal-time (universal->unix-time now)))
           (= 1700000000 (universal->unix-time (unix->universal-time 1700000000)))
           t)))

  ; the clock: within a second of doing it by hand, and after 2023
  (def-test/doc-test 'get-unix-time
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((ours   (get-unix-time))
          (theirs (- (get-universal-time) *unix-epoch-difference*)))
      (and (<= 0 (- theirs ours) 1)
           (> ours 1700000000)
           t)))

; (an early hour, deliberately: it used to be the one field that
; wasn't zero-padded)
(def-test/doc-test 'make-pretty-time
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "2026-01-02 03:04:05")
  (make-pretty-time (universal->unix-time (encode-universal-time 5 4 3 2 1 2026))))

(def-test/doc-test 'make-pretty-time
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("2026-01-02" "03:04:05" "2026-01-02T03.04.05"))
  (let ((then (universal->unix-time (encode-universal-time 5 4 3 2 1 2026))))
    (list (make-pretty-time then :just-date t)
          (make-pretty-time then :just-time t)
          (make-pretty-time then :time-sep "." :dt-sep "T"))))

  ; asked for both, just-date wins; two digits everywhere at the
  ; far end of a year
  (def-test/doc-test 'make-pretty-time
    `((test-able returns))
    'function
    (equal test-return-value! `("2026-01-02" "1999-12-31 23:59:59"))
    (list (make-pretty-time (universal->unix-time (encode-universal-time 5 4 3 2 1 2026))
                            :just-date t :just-time t)
          (make-pretty-time (universal->unix-time (encode-universal-time 59 59 23 31 12 1999)))))

  ; get-current-time is make-pretty-time of right now, same keywords;
  ; the clock moves, so this checks the shape
  (def-test/doc-test 'get-current-time
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((date (get-current-time :just-date t))
          (time (get-current-time :just-time t :time-sep "h"))
          (both (get-current-time :dt-sep "T")))
      (and (= (length date) 10)
           (char= #\- (char date 4)) (char= #\- (char date 7))
           (= 2 (count #\h time)) (not (find #\: time))
           (search date both) (find #\T both)
           t)))

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
    (equal test-return-value! `("1.00 minutes" "1.00 hours" "1.00 days"))
    (list (time-for-humans 60) (time-for-humans 3600) (time-for-humans 86400)))

  ; zero, and a float, pass through as given
  (def-test/doc-test 'time-for-humans
    `((test-able returns))
    'function
    (equal test-return-value! `("0 seconds" "59.5 seconds" "1.50 minutes"))
    (list (time-for-humans 0) (time-for-humans 59.5) (time-for-humans 90)))

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

(def-test/doc-test 'for-each/parallel
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `((1 a) (2 b) (3 c)))
  (for-each/parallel ('(a b c))
    (list index! value!)))

  ; :limit caps how many run at once; the results still come back in order
  (def-test/doc-test 'for-each/parallel
    `((test-able returns))
    'function
    (equal test-return-value! `((1 4 9 16 25) t))
    (let ((active 0) (peak 0) (m (make-mutex)))
      (list (for-each/parallel ('(1 2 3 4 5) :limit 2)
              (with-mutex (m) (incf active) (setq peak (max peak active)))
              (sleep 0.05)
              (with-mutex (m) (decf active))
              (* value! value!))
            (<= 1 peak 2))))

  ; continue! ends that element's body (its result is nil)
  (def-test/doc-test 'for-each/parallel
    `((test-able returns))
    'function
    (equal test-return-value! `(1 nil 3))
    (for-each/parallel ('(1 2 3))
      (when (= value! 2) (continue!))
      value!))

  ; one element erroring doesn't stop the others; the error comes
  ; once everything has run
  (defparameter /ran/ 0)
  (def-test/doc-test 'for-each/parallel
    `((test-able returns))
    'function
    (and test-error!
         (search "1 of 3" (fn "~A" test-error!))
         (= /ran/ 3))
    (let ((m (make-mutex)))
      (setq /ran/ 0)
      (for-each/parallel ('(1 2 3))
        (with-mutex (m) (incf /ran/))
        (when (= value! 2) (error "two is bad"))
        value!)))

; progress prints in yellow, so the example checks the text inside the escapes
(def-test/doc-test 'progress
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (let ((out (with-output-to-string (s) (progress 3 4 :where s))))
    (and (search "3 of 4..... [75.00%]" out) t)))

  ; only every INTERVAL-th index reports
  (def-test/doc-test 'progress
    `((test-able returns))
    'function
    (equal test-return-value! `("" t))
    (list (with-output-to-string (s) (progress 3 4 :where s :interval 2))
          (and (plusp (length (with-output-to-string (s)
                                (progress 4 4 :where s :interval 2))))
               t)))

(def-test/doc-test 'with-interactive-interrupt-handler
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 42)
  (with-interactive-interrupt-handler "interrupted!"
    42))

; --------------------------------------------------------------- ;

(def-test/doc-section "error handling")

(def-test/doc-test 'or-die
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3)
  (or-die (/ 3 1) "this message never appears"))

(def-test/doc-test 'or-die
  `(markdown-able (test-able returns))
  'function
  (and (null test-return-value!)
       (search "DIVISION-BY-ZERO" test-stderr!))
  (or-die (/ 3 0)
          (fn "the error was: ~A" (type-of error!))
          :errfun #'advise))

(def-test/doc-test 'or-do
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3)
  (or-do (/ 3 1) :fallback))

(def-test/doc-test 'or-do
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! :fallback)
  (or-do (/ 3 0) :fallback))

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

(def-test/doc-test '|Ø|
  `(markdown-able (test-able returns))
  "reader macro: `Ø<form>` errors if `<form>` is null, else passes it through"
  (= test-return-value! 42)
  (let ((x 42)) Øx))

  ; the null case signals
  (def-test/doc-test '|Ø|
    `((test-able returns))
    "reader macro: `Ø<form>` errors if `<form>` is null, else passes it through"
    (and test-error! (null test-return-value!))
    (let ((x nil)) Øx))

; --------------------------------------------------------------- ;

(def-test/doc-section "universal indexing operator syntax")

(def-raw-markdown
  (fn "`{ thing index }` works on lists, alists, vectors, hash-tables, structs, and CLOS objects — and is setf-able. Successive indexes burrow into nested structures. On a list, an integer index is always a position; any other index is an alist key (so an alist keyed by integers needs a plain `assoc`). (The docs below show the `{ }` reader macro already expanded into `PLUTO-GET`)~%~%"))

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

; structs and CLOS objects are indexed by slot name (the symbol)
(defstruct point x y)
(defclass thing ()
  ((name :initarg :name)
   (size :initarg :size)))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 2)
  (let ((p (make-point :x 1 :y 2)))
    { p 'y }))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (string= test-return-value! "cup")
  (let ((o (make-instance 'thing :name "cup" :size 3)))
    { o 'name }))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 10)
  (let ((p (make-point :x 1 :y 2)))
    (setf { p 'x } 10)
    (point-x p)))

  ; setf on a CLOS object, and burrowing: a list of hash tables of structs
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (equal test-return-value! `(9 2))
    (let ((o    (make-instance 'thing :name "cup" :size 3))
          (data (list (let ((h (make-hash-table)))
                        (setf (gethash :home h) (make-point :x 1 :y 2))
                        h))))
      (setf { o 'size } 9)
      (list (slot-value o 'size)
            { data 0 :home 'y })))

  ; a slot that isn't there is an error, not nil
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (and test-error! (null test-return-value!))
    (let ((p (make-point :x 1 :y 2)))
      { p 'z }))

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 3)
  { #(#(1 2) #(3 4)) 1 0 })

(def-test/doc-test '|{}|
  `(markdown-able (test-able returns))
  ""
  (= test-return-value! 3)
  { `((1 2) (3 4)) 1 0 })

  ; regression: a list of lists looks just like an alist (every
  ; element is a cons), so an integer index used to be an assoc
  ; KEY — this returned (2), not the second row
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (equal test-return-value! `(3 4))
    { `((1 2) (3 4)) 1 })

  ; an integer is a position even when the list IS an alist...
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (equal test-return-value! `(:b . 2))
    (let ((tmp `((:a . 1) (:b . 2))))
      { tmp 1 }))

  ; ...and setf follows the same rule
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (equal test-return-value! `((1 2) (:new) (5 6)))
    (let ((tmp (list (list 1 2) (list 3 4) (list 5 6))))
      (setf { tmp 1 } (list :new))
      tmp))

  ; non-integer keys still set through the alist
  (def-test/doc-test '|{}|
    `((test-able returns))
    ""
    (equal test-return-value! `((:a . 1) (:b . 99)))
    (let ((tmp (list (cons :a 1) (cons :b 2))))
      (setf { tmp :b } 99)
      tmp))

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

(def-raw-markdown
  (fn "(`sh-simple` and `zsh-simple` are the bare run-program underneath: the command runs, nothing is captured, nothing is signalled)~%~%"))

  ; sh-simple runs the command (and waits for it), returning
  ; run-program's own value, which differs by implementation
  (def-test/doc-test 'sh-simple
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((marker "tmp-sh-simple-ran.txt"))
      (sh-simple (fn "touch ~A" (q/sh marker)))
      (let ((ran (and (probe-file marker) t)))
        (delete-file marker)
        ran)))

(def-test/doc-test 'q/sh
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "it's got $pecial \"chars\" & `stuff`")
  (sh (fn "echo -n ~A" (q/sh "it's got $pecial \"chars\" & `stuff`"))))

  ; newlines survive the round-trip too
  (def-test/doc-test 'q/sh
    `((test-able returns))
    'function
    (string= test-return-value! (fn "two~%lines"))
    (sh (fn "echo -n ~A" (q/sh (fn "two~%lines")))))

  ; pathnames get princ-ed
  (def-test/doc-test 'q/sh
    `((test-able returns))
    'function
    (string= test-return-value! "/tmp/has spaces/file.txt")
    (sh (fn "echo -n ~A" (q/sh #P"/tmp/has spaces/file.txt"))))

(def-test/doc-test 'q/fmt
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "100% done ~ okay")
  (fn (q/fmt "100% done ~ okay")))

  ; the point: strings with stray ~s survive being embedded in
  ; a control string (die relays error text as-is for the same
  ; reason)
  (def-test/doc-test 'q/fmt
    `((test-able returns))
    'function
    (string= test-return-value! "downloading save~1.dat: 33%")
    (fn (str+ "downloading " (q/fmt "save~1.dat") ": ~A%") 33))

  ; the full point: shell-side file operations on nasty filenames
  (def-test/doc-test 'q/sh
    `((test-able returns))
    'function
    (string= test-return-value! "content")
    (let ((nasty "tmp it's a $file name.txt"))
      (sh (fn "echo -n content > ~A" (q/sh nasty)))
      (let ((res (slurp nasty)))
        (sh (fn "rm ~A" (q/sh nasty)))
        res)))

  ; a command with a non-zero exit code should signal
  ; (with the code in the condition)
  (def-test/doc-test 'zsh
    `((test-able returns))
    'function
    (and test-error! (search "42" (fn "~A" test-error!)))
    (zsh "exit 42"))

  ; regression: non-interactive commands used to get a spurious ";"
  ; appended (which broke commands ending in "&")
  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (and (null test-error!) (string= test-return-value! ""))
    (sh "true &"))

  ; a shell killed by a signal should still trigger the error path
  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (and test-error! t)
    (sh "kill -KILL $$"))

  ; regression: with a non-signaling err-fun, stderr used to be
  ; consumed by the error path and returned as ""
  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (equal test-return-value! `("" "oops" 3))
    (multiple-value-list
      (sh "echo -n oops >&2; exit 3"
          :err-fun (lambda (c e) (declare (ignore c e)) nil))))

(def-raw-markdown
  (fn "`:in` is the command's stdin: `t` inherits ours (the default), `nil` is /dev/null, and a stream or a filename feeds it. `:enc` is the encoding the output is read with~%~%"))

(def-test/doc-test 'sh
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "HI")
  (sh "tr a-z A-Z" :in (make-string-input-stream "hi")))

  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (equal test-return-value! `("" "3497" "3497"))
    (list (sh "cat" :in nil)
          (string-trim " " (sh "wc -c" :in "somebody.txt"))
          (string-trim " " (sh "wc -c" :in #P"somebody.txt"))))

  ; :return-string nil hands stdout and stderr to ours and returns
  ; nothing -- but a failing command still signals
  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (equal test-return-value! `((nil) "3"))
    (list (multiple-value-list (sh "true" :return-string nil))
          (handler-case (sh "exit 3" :return-string nil)
            (error (e) (and (search "3" (fn "~A" e)) "3")))))

  ; :interactive runs the shell with -i (it reads its rc files, so
  ; aliases and functions from there work): $- carries the i flag
  (def-test/doc-test 'sh
    `((test-able returns))
    'function
    (equal test-return-value! `(t nil))
    (flet ((flags-with-i-p (line) (and (every #'alphanumericp line) (find #\i line) t)))
      (list (and (some #'flags-with-i-p (sh "echo $-" :interactive t :split t)) t)
            (flags-with-i-p (sh "echo $-")))))

(def-test/doc-test 'sh
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("é" "é"))
  (list (sh "printf '\\303\\251'")
        (sh "printf '\\351'" :enc :latin-1)))

; --------------------------------------------------------------- ;

(def-test/doc-section "system")

(def-raw-markdown
  (fn "`hostname` and `sys/info` answer for the machine they run on, so only `get-envvar` shows its output here~%~%"))

(def-test/doc-test 'get-envvar
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "the-default")
  (get-envvar "SURELY_THIS_ENVVAR_DOES_NOT_EXIST" "the-default"))

  (def-test/doc-test 'get-envvar
    `((test-able returns))
    'function
    (search "/" test-return-value!)
    (get-envvar "HOME"))

  (def-test/doc-test 'hostname
    `((test-able returns))
    'function
    (and (stringp test-return-value!)
         (> (length test-return-value!) 0))
    (hostname))

  ; an alist; the kernel is one of the two we develop on
  (def-test/doc-test 'sys/info
    `((test-able returns))
    'function
    (and (alistp test-return-value!)
         (member { test-return-value! :kernel } `(:linux :darwin))
         (string= { test-return-value! :hostname } (hostname))
         t)
    (sys/info))

; --------------------------------------------------------------- ;

(def-test/doc-section "command-line arguments")

(def-test/doc-test 'program/script-name
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "test-pluto.lisp")
  (program/script-name))

(def-test/doc-test 'def-cli-args
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("," t ("a.txt" "b.txt")))
  (let (sep verbose)
    (def-cli-args "demo" "[-v] -s SEP FILES..." "a demonstration"
      (option "-h" "--help"    "print usage" (print-usage!))
      (option "-v" "--verbose" "say more"    (setq verbose t)
                                             (process-args! (cdr args!)))
      (option "-s" "--sep"     "separator"   (setq sep (cadr args!))
                                             (process-args! (cddr args!))))
    (process-args! `("-s" "," "a.txt" "-v" "b.txt"))
    (list sep verbose bare-args!)))

(def-test/doc-test '+usage-text!+
  `(markdown-able (test-able returns))
  'variable
  (eq test-return-value! t)
  (and (search (fn "Usage: demo [-v] -s SEP FILES...~%a demonstration") +usage-text!+)
       (search "-s, --sep" +usage-text!+)
       t))

  ; combined short flags split up, and assign-next-arg! is the
  ; shorthand for a flag that takes a value
  (def-test/doc-test 'def-cli-args
    `((test-able returns))
    'function
    (equal test-return-value! `(";" t ("x")))
    (let (sep verbose)
      (def-cli-args "demo2" "" ""
        (option "-v" "--verbose" "say more"  (setq verbose t)
                                             (process-args! (cdr args!)))
        (option "-s" "--sep"     "separator" (assign-next-arg! sep)))
      (process-args! `("-vs" ";" "x"))
      (list sep verbose bare-args!)))

; --------------------------------------------------------------- ;

(def-test/doc-section "threads")

(def-raw-markdown
  (fn "The two blessed implementations' threads under one set of names (pluto can't lean on bordeaux-threads). `for-each/parallel`, `with-loading` and `with-status-lines` are built on them~%~%"))

(def-test/doc-test 'make-thread
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 42)
  (join-thread (make-thread (lambda () (* 6 7)))))

(def-test/doc-test 'with-mutex
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 4000)
  (let ((count 0)
        (m     (make-mutex)))
    (for-each/parallel ('(1 2 3 4))
      (loop repeat 1000 do (with-mutex (m) (incf count))))
    count))

  ; join-thread hands back whatever the function returned, nil included
  (def-test/doc-test 'join-thread
    `((test-able returns))
    'function
    (equal test-return-value! `(nil "done"))
    (list (join-thread (make-thread (lambda () nil)))
          (join-thread (make-thread (lambda () (sleep 0.05) "done") :name "sleeper"))))

; --------------------------------------------------------------- ;

(def-test/doc-section "terminal things / terminal manipulation")

(def-raw-markdown
  (fn "The cursor movements print escapes (compared against the `+ansi-escape-...+` constants here, since they don't render); the progress bar is shown with `:one-line nil`, so it doesn't try to redraw over the previous line~%~%"))

(def-test/doc-test 'ansi-up-line
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (string= (with-output-to-string (s) (ansi-up-line s)) +ansi-escape-up+))

  (def-test/doc-test 'ansi-left-all
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (and (string= (with-output-to-string (s) (ansi-left-all s)) +ansi-escape-left-all+)
         (string= (with-output-to-string (s) (ansi-left-one s)) +ansi-escape-left-one+)))

  ; regression: the start-of-line escape went to *error-output*
  ; whatever WHERE was; now the whole thing lands on WHERE
  (def-test/doc-test 'ansi-clear-line
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let* ((out    (with-output-to-string (s) (ansi-clear-line s)))
           (prefix (length +ansi-escape-left-all+)))
      (and (> (length out) prefix)
           (string= (subseq out 0 prefix) +ansi-escape-left-all+)
           (every (lambda (c) (char= c #\Space)) (subseq out prefix))
           t)))

(def-test/doc-test 'progress-bar
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (let ((out (with-output-to-string (s)
               (progress-bar 5 10 :where s :width 10 :one-line nil :out-of t))))
    (and (search "◖▬▬▬▬▬▭▭▭▭▭ ◗  50.00%  [5/10]" out) t)))

  ; drawn every INTERVAL-th index and always at the end; one-line
  ; mode starts by moving up and clearing
  (def-test/doc-test 'progress-bar
    `((test-able returns))
    'function
    (equal test-return-value! `("" t t))
    (list (with-output-to-string (s) (progress-bar 3 10 :where s :interval 5 :one-line nil))
          (and (search "100.00%" (with-output-to-string (s)
                                   (progress-bar 10 10 :where s :interval 7 :one-line nil)))
               t)
          (let ((out (with-output-to-string (s) (progress-bar 1 2 :where s :width 4))))
            (and (search +ansi-escape-up+ out)
                 (search +ansi-escape-left-all+ out)
                 (search "◖▬▬▭▭ ◗" out)
                 t))))

(def-raw-markdown
  (fn "`with-status-lines` is homebrew-style live progress: one line per key, each with its own spinner until `done!` or `failed!`, redrawn in place by a renderer thread. Without a terminal (as here, writing to a string) nothing animates: each line is printed once, when it finishes~%~%"))

(def-test/doc-test 'with-status-lines
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("✓ zsh    up to date" "✗ pluto  18 to push"))
  (split-string->lines
    (with-output-to-string (s)
      (with-status-lines ('(zsh pluto) :where s)
        (status! 'pluto "fetching...")
        (done!   'zsh   "up to date")
        (failed! 'pluto "18 to push")))))

  ; a frame as the renderer draws it: every line cleared and redrawn,
  ; the marks colored, the spinner on the unfinished one; and a line
  ; is cut to the terminal's width
  (def-test/doc-test 'with-status-lines
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((board (pluto::%make-status-lines '(a bb) (make-string-output-stream))))
      (pluto::%line-update board 'bb :text "done" :state :done)
      (pluto::%line-update board 'a  :text "still going")
      (let ((frame (pluto::%render-status-frame board)))
        (setf (pluto::%sl-columns board) 8)
        (and (= 2 (count #\Newline frame))
             (search +ansi-escape-clear-line+ frame)
             (search (green "~A" "✓") frame)
             (search (yellow "~A" (aref *pluto-spinner-frames* 0)) frame)
             (search "bb  done" frame)
             (search "a   still going" frame)
             (<= (length (pluto::%render-status-line board 0)) 8)
             t))))

  ; a key that isn't one of the lines is an error
  (def-test/doc-test 'with-status-lines
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (with-output-to-string (s)
      (with-status-lines ('(a) :where s)
        (done! 'zz "nope"))))

; --------------------------------------------------------------- ;

(def-test/doc-section "file/filename/directory operations")

(def-test/doc-test 'basename
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "baz.txt")
  (basename "/foo/bar/baz.txt"))

(def-test/doc-test 'change-extension
  `(markdown-able (test-able returns))
  'function
  (string= (namestring test-return-value!) "/foo/bar/baz.md")
  (change-extension "/foo/bar/baz.txt" "md"))

  ; nil takes the extension off
  (def-test/doc-test 'change-extension
    `((test-able returns))
    'function
    (string= (namestring test-return-value!) "/foo/bar/baz")
    (change-extension "/foo/bar/baz.txt" nil))

(def-test/doc-test 'size-for-humans
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "3M")
  (size-for-humans (* 3 (expt 2 20))))

(def-test/doc-test 'size-for-humans
  `(markdown-able (test-able returns))
  ""
  (string= test-return-value! "2.0G")
  (size-for-humans (expt 2 31)))

(def-test/doc-test 'file-exists-p
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(t t nil))
  (list (if (file-exists-p "somebody.txt") t nil)
        (if (directory-exists-p "wayward files") t nil)
        (if (file-exists-p "no-such-file.txt") t nil)))

(def-test/doc-test 'file-size
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(3497 "3K"))
  (list (file-size "somebody.txt")
        (file-size "somebody.txt" :human t)))

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
    (and (search "tests" test-return-value!)
         (char= #\/ (char test-return-value! (1- (length test-return-value!)))))
    (pwd))

  (def-test/doc-test 'inspect-pathname
    `((test-able stderr))
    'function
    (and (search "pathname name:" test-stderr!)
         (search "somebody" test-stderr!))
    (inspect-pathname "somebody.txt"))

(def-test/doc-test 'realpath
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "wayward files/regular")
  (realpath "wayward files/regular" :relative-to "./"))

  ; absolute by default (and agrees with truename); -s keeps a
  ; symlink as itself; a path with spaces survives the shell
  (def-test/doc-test 'realpath
    `((test-able returns))
    'function
    (equal test-return-value! `(t "tmp-rp-link" "somebody.txt"))
    (progn
      (sh "ln -sf somebody.txt tmp-rp-link")
      (let ((res (list (string= (realpath "somebody.txt")
                                (namestring (truename "somebody.txt")))
                       (basename (realpath "tmp-rp-link" :expand-symlinks nil))
                       (basename (realpath "tmp-rp-link")))))
        (sh "rm tmp-rp-link")
        res)))

  ; a missing path is an error
  (def-test/doc-test 'realpath
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (realpath "no-such-file.txt"))

(def-test/doc-test 'pathname->native
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "/foo/bar baz.txt")
  (pathname->native #P"/foo/bar baz.txt"))

  ; a file with glob characters in its name: `ls` hands back its
  ; (possibly escaped) pathname; pathname->native recovers the
  ; exact on-disk name
  (def-test/doc-test 'pathname->native
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (let ((nasty "tmp-star*quest?br[ack].txt"))
      (sh (fn "touch ~A" (q/sh nasty)))
      (let ((result (and (member nasty (ls "./")
                                 :key #'pathname->native
                                 :test (lambda (n full) (search n full)))
                         t)))
        (sh (fn "rm ~A" (q/sh nasty)))
        result)))

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

  ; regression: :regex compiled into a call to charon's ~m macro
  ; as if it were a function — it errored on every single use
  (def-test/doc-test 'file-find
    `((test-able returns))
    'function
    (and (= (length test-return-value!) 1)
         (string= (file-namestring (car test-return-value!))
                  "keep.txt"))
    (progn
      (zsh "mkdir -p tmp-walk2 && touch tmp-walk2/keep.txt tmp-walk2/skip.md")
      (let ((found (file-find "tmp-walk2" :type :file :regex "keep")))
        (zsh "rm -rf tmp-walk2")
        found)))

  ; regression: a find that matches nothing used to die on
  ; (setf (car nil)) instead of returning nil
  (def-test/doc-test 'file-find
    `((test-able returns))
    'function
    (and (null test-error!) (null test-return-value!))
    (progn
      (zsh "mkdir -p tmp-walk-empty")
      (let ((found (file-find "tmp-walk-empty"
                              :test (constantly nil))))
        (zsh "rm -rf tmp-walk-empty")
        found)))

(def-test/doc-test '-path
  `(markdown-able (test-able returns))
  'function
  (string= (namestring test-return-value!) "somebody.txt")
  (-path "somebody.txt" "./"))

  (def-test/doc-test '+path
    `((test-able returns))
    'function
    (and (search "tests" (namestring test-return-value!))
         (search "foo.txt" (namestring test-return-value!)))
    (+path "./" "foo.txt"))

(def-test/doc-test 'absolute->relative
  `(markdown-able (test-able returns))
  'function
  (string= (namestring test-return-value!) "somebody.txt")
  (absolute->relative (probe-file "somebody.txt")))

  ; a relative path isn't absolute
  (def-test/doc-test 'absolute->relative
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (absolute->relative "relative/path.txt"))

; --------------------------------------------------------------- ;

(def-test/doc-section "readtable containment")

(def-test/doc-test 'call-with-pluto-readtable
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hi")
  (let ((*readtable* (copy-readtable nil)))
    (call-with-pluto-readtable (lambda () (read-from-string "•hi•")))))

  ; under a standard readtable the same text is just a symbol
  (def-test/doc-test '*pluto-readtable*
    `((test-able returns))
    'variable
    (and (symbolp (car test-return-value!))
         (string= (cadr test-return-value!) "hi")
         (readtablep *pluto-readtable*))
    (let ((*readtable* (copy-readtable nil)))
      (list (read-from-string "•hi•")
            (let ((*readtable* *pluto-readtable*))
              (read-from-string "•hi•")))))

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
; deps compile under a STANDARD readtable — pluto's is already global
; by now, and a bare `?` token in a dep (cxml's `(? ...)` clauses) is
; a reader error under it.  charon's own file still compiles under
; pluto's readtable via its .asd's :around-compile.
(let ((*readtable* (copy-readtable nil)))
  (ql:quickload :charon :silent t))
(use-package :charon)

(def-test/doc-section "charon: the companion with dependencies")

(def-raw-markdown
  (fn "Regexes (cl-ppcre), HTTP (dexador), XML and XPath (cxml, plexippus), JSON (yason), HTML (plump, lquery) and friends, wrapped the pluto way. `request/get` and `request/post` need a network, so they aren't exercised here~%~%"))

(def-test/doc-test 'str-trim
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "hi")
  (str-trim (fn "~C hi ~%" #\Tab)))

(def-test/doc-test 'str-normalize
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(1 2 1))
  (list (length "é")
        (length (str-normalize "é"))
        (length (str-normalize "é" :form :nfc))))

(def-test/doc-test 're-compile
  `(markdown-able (test-able returns))
  'function
  (eq test-return-value! t)
  (let ((digits (re-compile "\\d+")))
    (str-detect "abc123" digits)))

(def-raw-markdown
  (fn "The regex wrappers put the string first (cl-ppcre puts the pattern first), and each has a one-character alias: `~~s` split, `~~r` replace, `~~ra` replace-all, `~~m` match, `~~f` filter (subset), `~~e` extract, `~~c` compile. A pattern may be a string or a compiled scanner~%~%"))

(def-test/doc-test 'str-split
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("11" "2" "2026"))
  (str-split "11/2/2026" "/"))

(def-test/doc-test 'str-split
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("k" "v=w"))
  (str-split "k=v=w" "=" :limit 2))

  ; empty fields survive in the middle and at the start; trailing
  ; ones are dropped (cl-ppcre's rule, like perl's)
  (def-test/doc-test 'str-split
    `((test-able returns))
    'function
    (equal test-return-value! `(("a" "" "b") ("" "a") ("a" "b" "c")))
    (list (str-split "a,,b,," ",")
          (str-split ",a" ",")
          (str-split "a b   c" "\\s+")))

(def-test/doc-test 'str-replace
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "02/11/2026")
  (str-replace "2026-11-02" "(\\d+)-(\\d+)-(\\d+)" "\\3/\\2/\\1"))

  ; first match only, and a second value says whether anything matched
  (def-test/doc-test 'str-replace
    `((test-able returns))
    'function
    (equal test-return-value! `(("baa" t) ("abc" nil)))
    (list (multiple-value-list (str-replace "aaa" "a" "b"))
          (multiple-value-list (str-replace "abc" "z" "y"))))

(def-test/doc-test 'str-replace-all
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "a_b_c")
  (str-replace-all "a-b-c" "-" "_"))

(def-test/doc-test 'str-detect
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(t nil))
  (list (str-detect "price: $3.50" "\\$\\d")
        (str-detect "abc" "^b")))

  ; cl-ppcre:scan's keywords pass through
  (def-test/doc-test 'str-detect
    `((test-able returns))
    'function
    (null test-return-value!)
    (str-detect "xab" "a" :start 2))

(def-test/doc-test 'str-subset
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("banana"))
  (str-subset `("apple" "banana" "cherry") "an"))

  ; with a compiled scanner
  (def-test/doc-test 'str-subset
    `((test-able returns))
    'function
    (equal test-return-value! `("a1" "c2"))
    (str-subset `("a1" "b" "c2") (re-compile "\\d")))

(def-test/doc-test 'str-extract
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1" ("2" "2026")))
  (list (str-extract "v1.2" "v(\\d)")
        (str-extract "Nov 2, 2026" "(\\d+), (\\d+)")))

  ; no group, no match, and an unmatched optional group
  (def-test/doc-test 'str-extract
    `((test-able returns))
    'function
    (equal test-return-value! `(nil nil ("a" nil "b")))
    (list (str-extract "abc" "b")
          (str-extract "abc" "(z)")
          (str-extract "ab" "(a)(x)?(b)")))

(def-test/doc-test 'str-scan-to-strings
  `(markdown-able (test-able returns))
  'function
  (and (equalp (car test-return-value!) #("1" "2"))
       (null (cadr test-return-value!)))
  (list (str-scan-to-strings "a1b2" "(\\d)\\w(\\d)")
        (str-scan-to-strings "abc" "(z)")))

  ; the one-character aliases are the same thing
  (def-test/doc-test '~s
    `((test-able returns))
    'function
    (equal test-return-value! `(t t t t t t t))
    (list (equal (~s "a/b" "/")         (str-split "a/b" "/"))
          (equal (~r "aa" "a" "b")       (str-replace "aa" "a" "b"))
          (equal (~ra "aa" "a" "b")      (str-replace-all "aa" "a" "b"))
          (eq    (~m "abc" "b")          (str-detect "abc" "b"))
          (equal (~f `("ab" "c") "a")    (str-subset `("ab" "c") "a"))
          (equal (~e "a1" "(\\d)")       (str-extract "a1" "(\\d)"))
          (functionp (~c "a"))))

(def-test/doc-test 'parse-xml
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1" "2"))
  (xpath (parse-xml "<a><b>1</b><b>2</b></a>") "//b" :text t))

(def-test/doc-test 'xpath
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1" nil))
  (let ((doc (parse-xml "<a><b>1</b><b>2</b></a>")))
    (list (xpath doc "//b" :all nil :text t)
          (xpath doc "//zz"))))

(def-test/doc-test 'xpath-compile
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1"))
  (let ((query (xpath-compile "//b")))
    (xpath (parse-xml "<a><b>1</b></a>") query :compiled-p t :text t)))

(def-test/doc-test 'use-xml-namespace
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `("1"))
  (let ((xpath::*dynamic-namespaces* xpath::*dynamic-namespaces*))
    (use-xml-namespace "urn:x")
    (xpath (parse-xml "<a xmlns=\"urn:x\"><b>1</b></a>") "//b" :text t)))

(def-test/doc-test 'parse-xml-file
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "ok")
  (with-temp-file
    (barf tempfile! "<r><x>ok</x></r>")
    (xpath (parse-xml-file tempfile!) "//x" :all nil :text t)))

(def-test/doc-test 'hash-table->json
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "{\"a\":1}")
  (hash-table->json (alist->hash-table `(("a" . 1)))))

(def-test/doc-test 'parse-json-file
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(1 2))
  (with-temp-file
    (barf tempfile! "{\"a\": [1, 2]}")
    (gethash "a" (parse-json-file tempfile!))))

(def-test/doc-test 'make-octet-vector
  `(markdown-able (test-able returns))
  'function
  (and (equalp test-return-value! #(1 2 3))
       (typep test-return-value! `(vector (unsigned-byte 8))))
  (let ((v (make-octet-vector 2)))
    (setf (aref v 0) 1
          (aref v 1) 2)
    (concat-octet-vector v #(3))))

(def-test/doc-test 'parse-float
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 5.4)
  (parse-float "5.4"))

; without q/re, the dot matches anything; with it, only a literal dot
(def-test/doc-test 'q/re
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(t nil))
  (list (str-detect "price: 3x50" "3.50")
        (str-detect "price: 3x50" (q/re "3.50"))))

  (def-test/doc-test 'q/re
    `((test-able returns))
    'function
    (eq test-return-value! t)
    (str-detect "costs $3.50 (about)" (q/re "$3.50 (about)")))

(def-test/doc-test 'q/uri
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "caf%C3%A9%20%26%20bar%3Fq%3D1")
  (q/uri "café & bar?q=1"))

; each impl gets exactly one definition (the overlapping guards used
; to make ECL define-then-clobber it at load); nasty names must
; round-trip through the shell
(def-test/doc-test 'escape-namestring/shell
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "/tmp/it's got spaces!.txt")
  (sh (fn "echo -n ~A"
          (escape-namestring/shell "/tmp/it's got spaces!.txt"))))

(def-test/doc-test 'pathname->shell
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "/tmp/it's got spaces!.txt")
  (sh (fn "echo -n ~A" (pathname->shell #P"/tmp/it's got spaces!.txt"))))

  ; a plain name passes through; SBCL's namestring escaping is
  ; undone (ECL's namestrings never had any)
  (def-test/doc-test 'pathname->c
    `((test-able returns))
    'function
    (equal test-return-value! `("/tmp/a b.txt" "/tmp/a*.txt"))
    (list (pathname->c #P"/tmp/a b.txt")
          #+sbcl (escape-namestring/c "/tmp/a\\*.txt")
          #+ecl  "/tmp/a*.txt"))

; --------------------------------------------------------------- ;
; --------------------------------------------------------------- ;

; build a fresh libstyx (and refresh ~/.lisp, where styx.lisp looks)
(sh "make -C ../libstyx -s install")
; standard readtable for dep compiles — see the charon quickload above
(let ((*readtable* (copy-readtable nil)))
  (ql:quickload :styx :silent t))
(use-package :styx)

(def-test/doc-section "styx: the syscall tier")

(def-raw-markdown
  (fn "What pluto reaches for below portable lisp, through libstyx (C, in `libstyx/`): stat, symlinks, the terminal, file locks, and OpenSSL hashing. `sd-journal` (systemd logging) is linux-only, so it isn't exercised here~%~%"))

(def-test/doc-test '*sd-log-priority*
  `(markdown-able (test-able returns))
  'variable
  (= test-return-value! 4)
  *sd-log-priority*)

(def-test/doc-test 'stat-filesize
  `(markdown-able (test-able returns))
  'function
  (= test-return-value! 3497)
  (stat-filesize "somebody.txt"))

  ; without following, it's the link itself: as long as its target's name
  (def-test/doc-test 'stat-filesize
    `((test-able returns))
    'function
    (equal test-return-value! `(3497 12))
    (progn
      (sh "ln -sf somebody.txt tmp-size-link")
      (let ((res (list (stat-filesize "tmp-size-link")
                       (stat-filesize "tmp-size-link" :follow-symlinks nil))))
        (sh "rm tmp-size-link")
        res)))

(def-test/doc-test 'is-symlink-p
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(t nil))
  (progn
    (sh "ln -sf somebody.txt tmp-demo-link.txt")
    (let ((res (list (is-symlink-p "tmp-demo-link.txt")
                     (is-symlink-p "somebody.txt"))))
      (sh "rm tmp-demo-link.txt")
      res)))

(def-test/doc-test 'md5/string
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "5d41402abc4b2a76b9719d911017c592")
  (md5/string "hello"))

(def-test/doc-test 'md5/file
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "03d1f29193f99d260ee99b6cac7fd3a4")
  (md5/file "somebody.txt"))

(def-test/doc-test 'sha256/string
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value!
           "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
  (sha256/string "hello"))

; the hex string stands for bytes: "00ff" is two of them (and
; regression: an embedded 00 byte used to end the input, strlen-wise)
(def-test/doc-test 'sha256/hexstring
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value!
           "06eb7d6a69ee19e5fbdf749018d3d2abfa04bcbd1365db312eb86dc7169389b8")
  (sha256/hexstring "00ff"))

(def-test/doc-test 'sha256/file
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value!
           "d10edb841b13927faf6dc9032bb9422ab19cc5f9096208c3f67de1da615589a0")
  (sha256/file "somebody.txt"))

(def-test/doc-test 'sha512/string
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value!
           "9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca72323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043")
  (sha512/string "hello"))

(def-test/doc-test 'sha512/file
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value!
           "55e28f5838f7c60563990384b60acc2f9a778f8dbc4fd8e8ac77585124c766f8c624baac29fed6e9b9235c64878315c678f3c56e41788a585da267b105071acf")
  (sha512/file "somebody.txt"))

; RIPEMD160 lives in OpenSSL 3's legacy provider
(def-test/doc-test 'ripemd160/string
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "108f07b8382412612c048d07d13f814118445acd")
  (ripemd160/string "hello"))

(def-test/doc-test 'ripemd160/hexstring
  `(markdown-able (test-able returns))
  'function
  (string= test-return-value! "742dcba5d24837058e3fa56f1c36f9878484d1c2")
  (ripemd160/hexstring "00ff"))

  ; hashing a missing file signals instead of segfaulting
  (def-test/doc-test 'md5/file
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (md5/file "no-such-file.bin"))

  ; so does a hex string that isn't one (odd length here)
  (def-test/doc-test 'sha256/hexstring
    `((test-able returns))
    'function
    (and test-error! (null test-return-value!))
    (sha256/hexstring "abc"))

; whether stdout is a tty depends on how the tests are run,
; but a bogus fd is deterministically not one
(def-test/doc-test 'tty-p
  `((test-able returns))
  'function
  (and (member (car test-return-value!) `(t nil))
       (null (cadr test-return-value!)))
  (list (tty-p) (tty-p 999)))

; real size under a terminal, the (values 24 80 nil) fallback
; when piped — positive integers either way, and the -columns /
; -rows conveniences must agree with terminal-size
(def-test/doc-test 'terminal-size
  `((test-able returns))
  'function
  (destructuring-bind (rows cols real-p cols2 rows2) test-return-value!
    (and (integerp rows) (plusp rows)
         (integerp cols) (plusp cols)
         (member real-p `(t nil))
         (= cols cols2)
         (= rows rows2)))
  (append (multiple-value-list (terminal-size))
          (list (nth-value 0 (terminal-columns))
                (nth-value 0 (terminal-rows)))))

; now that styx is loaded, pluto's get-terminal-columns must
; delegate to the ioctl (agreeing with terminal-columns) instead
; of shelling out; with no terminal it keeps its (values 200 nil)
(def-test/doc-test 'get-terminal-columns
  `((test-able returns))
  'function
  (destructuring-bind (cols real-p styx-cols styx-real-p)
                      test-return-value!
    (if styx-real-p
      (and real-p (= cols styx-cols))
      (and (null real-p) (= cols 200))))
  (append (multiple-value-list (get-terminal-columns))
          (multiple-value-list (terminal-columns))))

; a second acquisition (even in the same process — it's a fresh
; open file description) fails while held, succeeds after release
(def-test/doc-test 'with-lock-file
  `(markdown-able (test-able returns))
  'function
  (equal test-return-value! `(:ran nil :free-again t))
  (let ((lockf "tmp-demo.lock"))
    (let ((res (with-lock-file (lockf)
                 (list :ran (acquire-lock-file lockf :wait nil)))))
      (let ((handle (acquire-lock-file lockf :wait nil)))
        (when handle
          (setq res (append res (list :free-again
                                      (release-lock-file handle))))))
      (delete-file lockf)
      res)))


; --------------------------------------------------------------- ;

(end-test/doc)

(if (run-tests)
  (with-a-file "pluto-results.md" :w
    (render-markdown stream!))
  (die (fn "~%at least one test failed")))

