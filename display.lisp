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
  (gl:matrix-mode :modelview)
  (glu:look-at 0 -150 250 0 0 0 0 1 0)
  (gl:enable :texture-2d :blend :depth-test :color-material :lighting :light1 :normalize)
;  (%gl:hint :perspective-correction-hint :nicest)
;  (%gl:color-material :front :ambient-and-diffuse)
;  (gl:material :front :specular '(1 1 1 1))
;  (gl:material :front :shininess 100)
  (gl:light :light1 :position '(1 1 1 0))
  (gl:light :light1 :diffuse '(1 1 1 1))
;  (gl:shade-model :smooth)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 1))
