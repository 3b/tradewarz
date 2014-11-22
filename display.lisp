(in-package :tradewarz)

(defclass display ()
  ((width :reader width
          :initarg :width
          :initform 800)
   (height :reader height
           :initarg :height
           :initform 600)
   (title :reader title
          :initform "TradeWarz")
   (fps :reader fps
        :initform 60)))

(defmethod initialize-instance :after ((object display) &key)
  (sdl:window (width object)
              (height object)
              :title-caption (title object)
              :icon-caption (title object)
              :opengl t
              :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
              :fps (make-instance 'sdl:fps-fixed
                                  :target-frame-rate (fps object)))
  (configure object))

(defmethod configure ((object display))
  (sdl:enable-key-repeat 1 20)
  (sdl:show-cursor nil)
  (gl:viewport 0 0 (width object) (height object))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 (/ (width object) (height object)) 1 1000.0)
  (glu:look-at 0 150 -300 0 0 0 0 0 -1)
  (gl:enable :cull-face :texture-2d :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha))
