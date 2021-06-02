
(asdf:defsystem :charon
  :description "A companion to pluto (with external dependencies)"
  :author "Tony Fischetti"
  :license "GPL-3"

  ; TODO: do I really need all of these?
  :depends-on (:pluto :alexandria :cl-ppcre :parse-float :yason)

  ; fails: drakma
  ;        cxml (because of puri)
  ;        quri (because of split-sequence)

               ; ; of course
               ; :pluto
               ;
               ; ; the venerable alexadria
               ; ; https://gitlab.common-lisp.net/alexandria/alexandria
               ; :alexandria
               ;
               ; ; ya tu sabes
               ; ; https://edicl.github.io/cl-ppcre/
               ; :cl-ppcre
               ;
               ; ; http://quickdocs.org/parse-float/
               ; :parse-float
               ;
               ; ;; DO I NEED THESE
               ;
               ; ;; HTTP client of choice
               ; ;; https://edicl.github.io/drakma/
               ; :drakma)
               ;
               ; ; XML parser of choice
               ; ; https://common-lisp.net/project/cxml/
               ; TODO: PURI isn't working on clisp
               ; :cxml
               ;
               ; ; Plexippus XPATH library
               ; ; https://common-lisp.net/project/plexippus-xpath/
               ; :xpath
               ;
               ; ; lenient HTML parser
               ; ; https://github.com/Shinmera/plump
               ; :plump

               ; TODO: wrong link
               ; dope jquery like thing for plump
               ; https://github.com/Shinmera/plump
               ; :lquery

               ; ; URI encoding/decoding
               ; ; https://github.com/fukamachi/quri
               ; :quri
               ;
               ; ; awesome json parser
               ; ; https://github.com/phmarek/yason
               ; :yason)

  :components ((:file "charon")))
