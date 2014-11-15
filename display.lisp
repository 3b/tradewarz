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
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 (width object) (height object))
  (gl:ortho 0 (width object) (height object) 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))
