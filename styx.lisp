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
    :acquire-lock-file
    :release-lock-file
    :with-lock-file
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
  ; the unversioned .so only exists with the dev package installed;
  ; ordinary machines just have the .so.0 runtime
  (t (:or (:default "libsystemd") "libsystemd.so.0")))

; a machine without systemd at all (docker containers, e.g.) shouldn't
; lose the whole styx system over its journal bindings — sd-journal
; degrades to a call-time error instead
#+linux
(defvar *libsystemd-loaded-p*
  (and (ignore-errors (cffi:use-foreign-library libsystemd)) t))

#+linux
(cffi:defcfun "sd_journal_send" :int (theformat :string) &rest)

#+linux
(defun sd-journal (message &key (priority *sd-log-priority*)
                                (identifier nil))
  (unless *libsystemd-loaded-p*
    (error "sd-journal: libsystemd could not be loaded on this machine"))
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
; `%claim-c-string` copies them lispward then has *libstyx*
; free them (styx_free) — cffi's foreign-free is free(3) on
; sbcl but a different allocator on ecl, where freeing
; C-malloc'd memory with it segfaults
; `%->native` gives C the exact on-disk name (no namestring escapes)

(cffi:defcfun "styx_free" :void (p :pointer))

(defun %claim-c-string (ptr)
  (unless (cffi:null-pointer-p ptr)
    (unwind-protect
      (cffi:foreign-string-to-lisp ptr)
      (styx-free ptr))))

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
;; advisory file locking (flock)
(cffi:defcfun "styx_flock_acquire" :int
  (path :string) (exclusive :int) (nonblocking :int))
(cffi:defcfun "styx_flock_release" :int (fd :int))

(defun acquire-lock-file (path &key (exclusive t) (wait t))
  "Acquires an advisory lock (flock) on PATH (creating the file
   if needed) and returns a lock handle. If WAIT is nil and
   somebody else holds the lock, returns nil immediately instead
   of blocking. Errors if the lock file can't be opened. The
   underlying fd is O_CLOEXEC: processes spawned while holding
   the lock don't inherit it"
  (let ((ret (styx-flock-acquire (%->native path)
                                 (if exclusive 1 0)
                                 (if wait 0 1))))
    (cond ((= ret -2) nil)
          ((< ret 0)  (error "acquire-lock-file: cannot lock ~A" path))
          (t          ret))))

(defun release-lock-file (handle)
  "Releases a lock acquired with ACQUIRE-LOCK-FILE"
  (if (= 0 (styx-flock-release handle))
    t
    (error "release-lock-file: cannot release lock")))

(defmacro with-lock-file ((path &key (exclusive t) (wait t)) &body body)
  "Runs BODY while holding an flock on PATH (great for cron
   scripts that shouldn't run concurrently). If WAIT is nil and
   the lock is already held elsewhere, BODY is skipped and nil
   is returned"
  (let ((handle (gensym "LOCK-HANDLE")))
    `(let ((,handle (acquire-lock-file ,path
                                       :exclusive ,exclusive
                                       :wait ,wait)))
       (when ,handle
         (unwind-protect
           (progn ,@body)
           (release-lock-file ,handle))))))


; -------------------
;; md5
(cffi:defcfun "styx_md5_string" :pointer (astring :string))
(cffi:defcfun "styx_md5_file" :pointer (afilename :string))

(defun md5/string (astring)
  (or (%claim-c-string (styx-md5-string astring))
      (error "md5/string: hashing failed")))

(defun md5/file (afilename)
  (or (%claim-c-string (styx-md5-file (%->native afilename)))
      (error "md5/file: cannot hash ~A" afilename)))


; -------------------
;; sha256
(cffi:defcfun "styx_sha256_string" :pointer (astring :string))
(cffi:defcfun "styx_sha256_hexstring" :pointer (astring :string))
(cffi:defcfun "styx_sha256_file" :pointer (afilename :string))

(defun sha256/string (astring)
  (or (%claim-c-string (styx-sha256-string astring))
      (error "sha256/string: hashing failed")))

(defun sha256/hexstring (astring)
  (or (%claim-c-string (styx-sha256-hexstring astring))
      (error "sha256/hexstring: invalid hex string ~S" astring)))

(defun sha256/file (afilename)
  (or (%claim-c-string (styx-sha256-file (%->native afilename)))
      (error "sha256/file: cannot hash ~A" afilename)))


; -------------------
;; sha512
(cffi:defcfun "styx_sha512_string" :pointer (astring :string))
(cffi:defcfun "styx_sha512_file" :pointer (afilename :string))

(defun sha512/string (astring)
  (or (%claim-c-string (styx-sha512-string astring))
      (error "sha512/string: hashing failed")))

(defun sha512/file (afilename)
  (or (%claim-c-string (styx-sha512-file (%->native afilename)))
      (error "sha512/file: cannot hash ~A" afilename)))


; -------------------
;; ripemd160
(cffi:defcfun "styx_ripemd160_string" :pointer (astring :string))
(cffi:defcfun "styx_ripemd160_hexstring" :pointer (astring :string))

(defun ripemd160/string (astring)
  (or (%claim-c-string (styx-ripemd160-string astring))
      (error "ripemd160/string: hashing failed")))

(defun ripemd160/hexstring (astring)
  (or (%claim-c-string (styx-ripemd160-hexstring astring))
      (error "ripemd160/hexstring: invalid hex string ~S" astring)))

;---------------------------------------------------------;

