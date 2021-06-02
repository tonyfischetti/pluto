;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;;  Charon                                          ;;
;;    a companion to the pluto package that         ;;
;;    relies on external dependencies               ;;
;;                                                  ;;
;;              Tony Fischetti                      ;;
;;              tony.fischetti@gmail.com            ;;
;;                                                  ;;
;;              License: GPL-3                      ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage :charon
  (:use :common-lisp)
  (:import-from :parse-float :parse-float)
  (:export :tmp123))

(in-package :charon)

(pushnew :charon *features*)


;---------------------------------------------------------;
; tmp ----------------------------------------------------;

(defun tmp123 ()
  (ft (yellow "this is just a test~%")))

;---------------------------------------------------------;

