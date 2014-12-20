(defsystem #:tradewarz
  :name "tradewarz" 
  :author "Michael Fiano <axedcode@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A war game"
  :depends-on (sdl2
               sdl2kit
               png-read
               cl-opengl
               cl-glu
               alexandria
               split-sequence)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "vector")
               (:file "matrix")
               (:file "display")
               (:file "input")
               (:file "obj-file")
               (:file "model")
               (:file "tile-hex")
               (:file "map")
               (:file "frame")
               (:file "node")
               (:file "scene")
               (:file "engine")))
