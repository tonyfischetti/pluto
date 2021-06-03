
(asdf:defsystem :charon
  :description "A companion to pluto (with external dependencies)"
  :author "Tony Fischetti"
  :license "GPL-3"

  ; TODO: do I really need all of these?
  :depends-on (
               ; of course
               :pluto

               ; the venerable alexadria
               ; https://gitlab.common-lisp.net/alexandria/alexandria
               :alexandria

               ; ya tu sabes
               ; https://edicl.github.io/cl-ppcre/
               :cl-ppcre

               ; https://github.com/soemraws/parse-float
               :parse-float

               ; awesome json parser
               ; https://github.com/phmarek/yason
               :yason

               ; portable threading
               ; https://github.com/sionescu/bordeaux-threads
               :bordeaux-threads

               ; Plexippus XPATH library
               ; https://common-lisp.net/project/plexippus-xpath/
               :xpath

               ; lenient HTML parser
               ; https://github.com/Shinmera/plump
               :plump

               ; dope jquery like thing for plump
               ; https://shinmera.github.io/lquery/
               :lquery

               ; XML parser of choice
               ; https://common-lisp.net/project/cxml/
               :cxml

               ;; !!!!
               ;; everything below this doesn't work on clisp YET
               ; fails:
               ;        drakma (needs clisp to be compiled with CFFI requires
               ;        CLISP compiled with dynamic FFI support.)
               ; [package cffi-sys]
               ; *** - CFFI requires CLISP compiled with dynamic FFI support.

               ; HTTP client of choice
               ; https://edicl.github.io/drakma/
               ; :drakma
               )

  :components ((:file "charon")))
