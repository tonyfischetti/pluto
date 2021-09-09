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
  (:export

    ; systemd
    :*sd-log-priority*
    :sd-journal

    ; libstyx
    :stat-filesize
    :md5

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
(cffi:defcfun "styx_stat_filesize" :uint64 (afilename :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun stat-filesize (afilename)
  (styx-stat-filesize (realpath (pathname afilename))))


; ; -------------------
; ;; mv
; (cffi:defcfun "styx_mv" :int (old :string) (new :string) &rest)
;
; ; TODO: everything
; ; TODO: check files, return values, etc...
; (defun mv (old new)
;   (styx-mv (realpath old) new))


; ; -------------------
; ;; cp
; (cffi:defcfun "styx_cp" :int (old :string) (new :string) &rest)
;
; ; TODO: everything
; ; TODO: check files, return values, etc...
; (defun cp (old new)
;   (styx-cp (realpath old) new))
; ; the return is the bytes (?)


; -------------------
;; md5
(cffi:defcfun "styx_md5" :string (afilename :string) &rest)

; TODO: everything
; TODO: check files, return values, etc...
(defun md5 (afilename)
  (styx-md5 (realpath afilename)))

;---------------------------------------------------------;

