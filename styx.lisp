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
  (:shadowing-import-from #:pluto #:file-size)
  (:export

    ; systemd
    :*sd-log-priority*
    :sd-journal

    ; libstyx
    :stat-filesize
    :is-symlink-p
    :md5/string
    :md5/file
    :sha256/string
    :sha256/hexstring
    :sha256/file
    :sha512/string
    :sha512/file
    :ripemd160/string
    :ripemd160/hexstring

    ; rework of improve-able pluto functions
    :file-size

    ))

(use-package :pluto)
(use-package :charon)
(in-package :styx)

(pushnew :styx *features*)

(push #P"~/.lisp/" cffi:*foreign-library-directories*)

; ------------------------------------------------------- ;
; systemd ----------------------------------------------- ;

(defparameter *sd-log-priority* 4)

(cffi:define-foreign-library libsystemd
  (t (:default "libsystemd")))

(cffi:use-foreign-library libsystemd)

(cffi:defcfun "sd_journal_send" :int (theformat :string) &rest)

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

; -------------------
;; stat-filesize
(cffi:defcfun "styx_stat_filesize" :int64 (afilename :string) (follow_symlinks :int) &rest)

; TODO: everything
; TODO: check files
(defun stat-filesize (afilename &key (follow-symlinks t))
  (when (pathnamep afilename)
    (setq afilename (escape-namestring/c (namestring afilename))))
  (let ((ret (styx-stat-filesize afilename (if follow-symlinks 1 0))))
    (if (< ret 0)
      (error "something went wrong")
      ret)))

; -------------------
;; is-symlink-p
(cffi:defcfun "styx_stat_is_symlink_p" :int (afilename :string) &rest)

; TODO: everything
; TODO: check files
(defun is-symlink-p (afilename)
  (when (pathnamep afilename)
    (setq afilename (escape-namestring/c (namestring afilename))))
  (let ((ret (styx-stat-is-symlink-p afilename)))
    (if (< ret 0)
      (error "something went wrong")
      (if (= ret 0) nil t))))
; if the namestring has a slash at the end, it doesn't work properly
; so we need to check if it's a directory and then strip the trailing
; slash


; -------------------
;; md5
(cffi:defcfun "styx_md5_string" :string (astring :string) &rest)
(cffi:defcfun "styx_md5_file" :string (afilename :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun md5/string (astring)
  (styx-md5-string astring))

(defun md5/file (afilename)
  (when (pathnamep afilename)
    (setq afilename (escape-namestring/c (namestring afilename))))
  (styx-md5-file afilename))


; -------------------
;; sha256
(cffi:defcfun "styx_sha256_string" :string (astring :string) &rest)
(cffi:defcfun "styx_sha256_hexstring" :string (astring :string) &rest)
(cffi:defcfun "styx_sha256_file" :string (afilename :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun sha256/string (astring)
  (styx-sha256-string astring))

(defun sha256/hexstring (astring)
  (styx-sha256-hexstring astring))

(defun sha256/file (afilename)
  (when (pathnamep afilename)
    (setq afilename (escape-namestring/c (namestring afilename))))
  (styx-sha256-file afilename))


; -------------------
;; sha512
(cffi:defcfun "styx_sha512_string" :string (astring :string) &rest)
(cffi:defcfun "styx_sha512_file" :string (afilename :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun sha512/string (astring)
  (styx-sha512-string astring))

(defun sha512/file (afilename)
  (when (pathnamep afilename)
    (setq afilename (escape-namestring/c (namestring afilename))))
  (styx-sha512-file afilename))


; -------------------
;; ripemd160
(cffi:defcfun "styx_ripemd160_string" :string (astring :string) &rest)
(cffi:defcfun "styx_ripemd160_hexstring" :string (astring :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun ripemd160/string (astring)
  (styx-ripemd160-string astring))

(defun ripemd160/hexstring (astring)
  (styx-ripemd160-hexstring astring))

;---------------------------------------------------------;


;---------------------------------------------------------;
; rework of improve-able pluto functions ---------------- ;

; TODO: document
(defun file-size (afile &key (just-bytes nil))
  (if just-bytes
    (stat-filesize afile)
    (size-for-humans (stat-filesize afile))))

;---------------------------------------------------------;
