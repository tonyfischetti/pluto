;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;  Styx                                            ;;
;;    a companion to the pluto and charon packages  ;;
;;    that uses helper C libraries                  ;;
;;    (including libstyx)                           ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;              License: GPL-3                      ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :styx
  (:use :common-lisp :pluto :charon)
  ; (:shadowing-import-from #:pluto #:file-size)
  (:export

    ; systemd
    :*sd-log-priority*
    #+linux :sd-journal

    ; libstyx
    :stat-filesize
    :is-symlink-p
    :tty-p
    :terminal-size
    :terminal-columns
    :terminal-rows
    :md5/string
    :md5/file
    :sha256/string
    :sha256/hexstring
    :sha256/file
    :sha512/string
    :sha512/file
    :ripemd160/string
    :ripemd160/hexstring

    ))

(use-package :pluto)
(use-package :charon)
(in-package :styx)

(pushnew :styx *features*)

(push #P"~/.lisp/" cffi:*foreign-library-directories*)

; ------------------------------------------------------- ;
; systemd ----------------------------------------------- ;

(defparameter *sd-log-priority* 4)

#+linux
(cffi:define-foreign-library libsystemd
  (t (:default "libsystemd")))

#+linux
(cffi:use-foreign-library libsystemd)

#+linux
(cffi:defcfun "sd_journal_send" :int (theformat :string) &rest)

#+linux
(defun sd-journal (message &key (priority *sd-log-priority*)
                                (identifier nil))
  (unless identifier
    (setq identifier (program/script-name)))
  (let ((ret (sd-journal-send (fn "MESSAGE=~A" message)
                              :string (fn "PRIORITY=~A" priority)
                              :string (fn "SYSLOG_IDENTIFIER=~A" identifier)
                              :pointer (cffi:null-pointer))))
    (when (= ret 0)
      t)))

;---------------------------------------------------------;


; ------------------------------------------------------- ;
; libstyx ----------------------------------------------- ;

(cffi:define-foreign-library libstyx
  (:darwin "libstyx.dylib")
  (:unix   "libstyx.so")
  (t (:default "libstyx")))

;; Load by absolute path so the pathname SBCL bakes into a saved core
;; (save-lisp-and-die) is absolute.  A bare name resolves fine at load
;; time (via *foreign-library-directories* / CWD), but SBCL records
;; that same bare name in the core and, on restart, dlopen can't find
;; it (it searches system paths, not ~/.lisp or the CWD) -> debugger.
(cffi:load-foreign-library
  (merge-pathnames #+darwin ".lisp/libstyx.dylib"
                   #-darwin ".lisp/libstyx.so"
                   (user-homedir-pathname)))

; the C side returns malloc'd hex strings (or NULL on error);
; `:free-from-foreign` has cffi free them after conversion
; `%->native` gives C the exact on-disk name (no namestring escapes)

(defun %->native (afilename)
  (if (pathnamep afilename)
    (pathname->native afilename)
    afilename))

; -------------------
;; stat-filesize
(cffi:defcfun "styx_stat_filesize" :int64
  (afilename :string) (follow_symlinks :int))

(defun stat-filesize (afilename &key (follow-symlinks t))
  (let ((ret (styx-stat-filesize (%->native afilename)
                                 (if follow-symlinks 1 0))))
    (if (< ret 0)
      (error "stat-filesize: cannot stat ~A" afilename)
      ret)))

; -------------------
;; is-symlink-p
(cffi:defcfun "styx_stat_is_symlink_p" :int (afilename :string))

(defun is-symlink-p (afilename)
  (let ((ret (styx-stat-is-symlink-p (%->native afilename))))
    (if (< ret 0)
      (error "is-symlink-p: cannot lstat ~A" afilename)
      (if (= ret 0) nil t))))
; if the namestring has a slash at the end, it doesn't work properly
; so we need to check if it's a directory and then strip the trailing
; slash


; -------------------
;; tty-p
(cffi:defcfun "styx_isatty" :int (fd :int))

(defun tty-p (&optional (fd 1))
  "Is file descriptor FD (default 1 [stdout]) attached to a
   terminal? Use it to suppress ANSI colors / progress bars when
   output is piped or redirected. A closed or bogus FD is simply
   not a terminal (nil) — no error"
  (= 1 (styx-isatty fd)))


; -------------------
;; terminal-size (and -columns / -rows)
(cffi:defcfun "styx_terminal_size" :int
  (fd :int) (rows :pointer) (cols :pointer))

(defun %terminal-size (fd)
  (cffi:with-foreign-objects ((rows :int) (cols :int))
    (when (= 0 (styx-terminal-size fd rows cols))
      (cons (cffi:mem-ref rows :int) (cffi:mem-ref cols :int)))))

(defun terminal-size ()
  "Asks the terminal its size with the TIOCGWINSZ ioctl, trying
   stdout, stderr, then stdin (redirecting one of them shouldn't
   blind us). Returns (values rows columns t), or the venerable
   default (values 24 80 nil) if no terminal is attached (or it
   reports zero size, as some do under exotic conditions)"
  (loop for fd in '(1 2 0)
        for size = (%terminal-size fd)
        when (and size (plusp (car size)) (plusp (cdr size)))
          do (return (values (car size) (cdr size) t))
        finally (return (values 24 80 nil))))

(defun terminal-columns ()
  "The terminal's width: (values columns real-p) — see TERMINAL-SIZE"
  (multiple-value-bind (rows cols real-p) (terminal-size)
    (declare (ignore rows))
    (values cols real-p)))

(defun terminal-rows ()
  "The terminal's height: (values rows real-p) — see TERMINAL-SIZE"
  (multiple-value-bind (rows cols real-p) (terminal-size)
    (declare (ignore cols))
    (values rows real-p)))


; -------------------
;; md5
(cffi:defcfun "styx_md5_string" (:string :free-from-foreign t)
  (astring :string))
(cffi:defcfun "styx_md5_file" (:string :free-from-foreign t)
  (afilename :string))

(defun md5/string (astring)
  (or (styx-md5-string astring)
      (error "md5/string: hashing failed")))

(defun md5/file (afilename)
  (or (styx-md5-file (%->native afilename))
      (error "md5/file: cannot hash ~A" afilename)))


; -------------------
;; sha256
(cffi:defcfun "styx_sha256_string" (:string :free-from-foreign t)
  (astring :string))
(cffi:defcfun "styx_sha256_hexstring" (:string :free-from-foreign t)
  (astring :string))
(cffi:defcfun "styx_sha256_file" (:string :free-from-foreign t)
  (afilename :string))

(defun sha256/string (astring)
  (or (styx-sha256-string astring)
      (error "sha256/string: hashing failed")))

(defun sha256/hexstring (astring)
  (or (styx-sha256-hexstring astring)
      (error "sha256/hexstring: invalid hex string ~S" astring)))

(defun sha256/file (afilename)
  (or (styx-sha256-file (%->native afilename))
      (error "sha256/file: cannot hash ~A" afilename)))


; -------------------
;; sha512
(cffi:defcfun "styx_sha512_string" (:string :free-from-foreign t)
  (astring :string))
(cffi:defcfun "styx_sha512_file" (:string :free-from-foreign t)
  (afilename :string))

(defun sha512/string (astring)
  (or (styx-sha512-string astring)
      (error "sha512/string: hashing failed")))

(defun sha512/file (afilename)
  (or (styx-sha512-file (%->native afilename))
      (error "sha512/file: cannot hash ~A" afilename)))


; -------------------
;; ripemd160
(cffi:defcfun "styx_ripemd160_string" (:string :free-from-foreign t)
  (astring :string))
(cffi:defcfun "styx_ripemd160_hexstring" (:string :free-from-foreign t)
  (astring :string))

(defun ripemd160/string (astring)
  (or (styx-ripemd160-string astring)
      (error "ripemd160/string: hashing failed")))

(defun ripemd160/hexstring (astring)
  (or (styx-ripemd160-hexstring astring)
      (error "ripemd160/hexstring: invalid hex string ~S" astring)))

;---------------------------------------------------------;

