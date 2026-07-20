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
  (t (:default "libstyx")))

(cffi:use-foreign-library libstyx)

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
;; terminal things

(cffi:defcfun "styx_isatty" :int (fd :int))

(defun tty-p (&optional (fd 1))
  "Is file descriptor FD (default 1 [stdout]) attached to a
   terminal? Use this to suppress ANSI colors/progress bars
   when output is piped or redirected"
  (= 1 (styx-isatty fd)))

(cffi:defcfun "styx_terminal_columns" :int (fd :int))
(cffi:defcfun "styx_terminal_rows" :int (fd :int))

(defun terminal-columns ()
  "Asks the terminal its width with the TIOCGWINSZ ioctl
   (trying stdout, stderr, then stdin). Returns (values cols t),
   or (values 80 nil) if no terminal is attached"
  (loop for fd in '(1 2 0)
        for res = (styx-terminal-columns fd)
        when (> res 0) do (return (values res t))
        finally (return (values 80 nil))))

(defun terminal-rows ()
  "Like TERMINAL-COLUMNS but for the terminal's height
   (fallback is (values 24 nil))"
  (loop for fd in '(1 2 0)
        for res = (styx-terminal-rows fd)
        when (> res 0) do (return (values res t))
        finally (return (values 24 nil))))


; -------------------
;; advisory file locking (flock)

(cffi:defcfun "styx_flock_acquire" :int
  (path :string) (exclusive :int) (nonblocking :int))
(cffi:defcfun "styx_flock_release" :int (fd :int))

(defun acquire-lock-file (path &key (exclusive t) (wait t))
  "Acquires an advisory lock (flock) on PATH (creating the file if
   needed) and returns a lock handle. If WAIT is NIL and somebody
   else holds the lock, returns NIL immediately instead of blocking.
   Errors if the lock file can't be opened."
  (let ((ret (styx-flock-acquire (%->native path)
                                 (if exclusive 1 0)
                                 (if wait 0 1))))
    (cond ((= ret -2)  nil)
          ((< ret 0)   (error "acquire-lock-file: cannot lock ~A" path))
          (t           ret))))

(defun release-lock-file (handle)
  "Releases a lock acquired with ACQUIRE-LOCK-FILE"
  (if (= 0 (styx-flock-release handle))
    t
    (error "release-lock-file: cannot release lock")))

(defmacro with-lock-file ((path &key (exclusive t) (wait t)) &body body)
  "Runs BODY while holding an flock on PATH (great for cron scripts
   that shouldn't run concurrently). If WAIT is NIL and the lock is
   already held elsewhere, BODY is skipped and NIL is returned."
  (let ((handle (gensym)))
    `(let ((,handle (acquire-lock-file ,path
                                       :exclusive ,exclusive
                                       :wait ,wait)))
       (when ,handle
         (unwind-protect
           (progn ,@body)
           (release-lock-file ,handle))))))


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

