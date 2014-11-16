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
               cl-fad
               bordeaux-threads)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "display")
               (:file "input")
               (:file "shape")
               (:file "entity")
               (:file "map")
               (:file "scene")
               (:file "engine")))
