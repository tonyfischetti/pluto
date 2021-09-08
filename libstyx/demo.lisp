#!/usr/local/bin/lispscript

  (ql:quickload :pluto)
(ql:quickload :cffi :silent t)

  (use-package :pluto)
(use-package :cffi)

(push #P"/home/tony/Desktop/lisp-cffi/styx/" cffi:*foreign-library-directories*)

(define-foreign-library libstyx
  (t (:default "libstyx")))

(use-foreign-library libstyx)


(defcfun "get_filesize" :int (afilename :string))

(defcfun "copy_file" :int (fn1 :string) (fn2 :string))

(defcfun "get_envvar" :string (ask :string))

(ft "~A~%" (get-envvar "PATH"))

; (write-to-journal "demo.lisp" 4 (fn "pwd is ~A~%" (pwd)))
;
; (write-to-journal "demo.lisp" 4 (fn "pwd is ~A~%" (pwd)))

(ft "~S~%" (basename (namestring *load-truename*)))

(ft "~S~%" (get-envvar "_"))




; --------------------------------------------------------------- ;;
;;;; FILESIZE

(time (file-size "libstyx.so" :just-bytes t))
(time (get-filesize "libstyx.so"))

(time (loop for i from 1 to 100 do (get-filesize "libstyx.so")))

(time (loop for i from 1 to 100 do (file-size "libstyx.so")))

; MUCH FASTER IN C!!
; --------------------------------------------------------------- ;;


; --------------------------------------------------------------- ;;
;;;; get envvar

(time (loop for i from 1 to 1000 do (get-envvar "HOME")))
(time (loop for i from 1 to 1000 do (pluto:get-envvar "HOME")))

; slightly faster using posix binding in SBCL
; --------------------------------------------------------------- ;;


; --------------------------------------------------------------- ;;
;;;; ls

(ls)
(zsh "ls" :split t)
(time (loop for i from 1 to 100 do (ls)))
(time (loop for i from 1 to 100 do (zsh "ls" :split t)))

; the new ls is much better
; --------------------------------------------------------------- ;;







