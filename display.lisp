(in-package :tradewarz)

(defclass display ()
  ((width :accessor width
          :initarg :width
          :initform 800)
   (height :accessor height
           :initarg :height
           :initform 600)
   (title :reader title
          :initform "TradeWarz")
   (fps :reader fps
        :initform 60)))

(defmethod initialize-instance :after ((object display) &key)
  (sdl:window (width object)
              (height object)
              :flags sdl:sdl-opengl
              :resizable t
              :double-buffer t
              :title-caption (title object)
              :icon-caption (title object))
  (configure object))

(defmethod configure ((object display))
  (setf cl-opengl-bindings:*gl-get-proc-address*
        #'sdl-cffi::sdl-gl-get-proc-address
        (sdl:frame-rate) (fps object))
  (gl:viewport 0 0 (width object) (height object))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50.0 (/ (width object) (height object)) 1 1000.0)
  (glu:look-at 0 150 -500 0 0 0 0 0 -1)
  (gl:translate 220 -400 -100)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :texture-2d :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha))
