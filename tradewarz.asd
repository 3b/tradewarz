(defsystem #:tradewarz
  :name "tradewarz" 
  :author "Michael Fiano <axedcode@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A war game"
  :depends-on (lispbuilder-sdl
               lispbuilder-sdl-image
               cl-opengl
               cl-glu
               bordeaux-threads
               alexandria
               split-sequence)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "vector")
               (:file "matrix")
               (:file "display")
               (:file "input")
               (:file "model")
               (:file "tile-hex")
               (:file "map")
               (:file "scene")
               (:file "engine")))
