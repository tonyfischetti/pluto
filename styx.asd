
(asdf:defsystem :styx
  ; :description "A companion to pluto (and charon) using a shared C libraries (including libstyx)"
  :author "Tony Fischetti <tony.fischetti@gmail.com>"
  :homepage "https://github.com/tonyfischetti/pluto"
  :version "0.0.1"
  :license "GPL-3"

  ; TODO: do I really need all of these?
  :depends-on (
               ; of course
               :pluto

               ; of course
               :charon

               ; foreign function interface
               :cffi
               )

  :components ((:file "styx")))
