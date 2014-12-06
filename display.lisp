(in-package :tradewarz)

(defclass display ()
  ((width :reader width
          :initarg :width
          :initform 1920)
   (height :reader height
           :initarg :height
           :initform 1080)
   (title :reader title
          :initform "TradeWarz")
   (fps :reader fps
        :initform nil)))

(defmethod initialize-instance :after ((object display) &key)
  (sdl:set-gl-attribute :sdl-gl-multisamplebuffers 1)
  (sdl:set-gl-attribute :sdl-gl-multisamplesamples 4)
  (sdl:window (width object)
              (height object)
              :title-caption (title object)
              :icon-caption (title object)
              :opengl t
              :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1))
              )
  (configure object))

(defmethod configure ((object display))
  (setf (sdl:frame-rate) (fps object))
  (sdl:show-cursor nil)
  (gl:viewport 0 0 (width object) (height object))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 (/ (width object) (height object)) 1 1000.0)
  (gl:matrix-mode :modelview)
  (glu:look-at 0 -150 250 0 0 0 0 1 0)
  (gl:enable
    :texture-2d
    :blend
    :depth-test
    :multisample
    :color-material
    :lighting
    :light0
    :normalize)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:material :front :shininess 50)
  (gl:light :light0 :position '(1 1 1 0))
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 1))
