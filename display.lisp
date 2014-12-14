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
        :initform nil)
   (camera :reader camera
           :initform (make-instance 'camera))
   (gl-settings :reader gl-settings
                :initform '((:sdl-gl-doublebuffer 1)
                            (:sdl-gl-accelerated-visual 1)
                            (:sdl-gl-multisamplebuffers 1)
                            (:sdl-gl-multisamplesamples 4)))))

(defclass camera ()
  ((eye :reader eye
        :initarg :eye
        :initform (make-vector 0 -150 250))
   (center :reader center
           :initarg :center
           :initform (make-vector 0 0 0))
   (up :reader up
       :initarg :up
       :initform (make-vector 0 1 0))))

(defmethod make-window :before ((display display))
  "Configure OpenGL attributes before creating a window"
  (dolist (setting (gl-settings display))
    (apply #'sdl:set-gl-attribute setting)))

(defmethod make-window ((display display))
  "Create a window with an OpenGL context"
  (sdl:window (width display)
              (height display)
              :title-caption (title display)
              :icon-caption (title display)
              :opengl t))

(defmethod make-window :after ((display display))
  "Configure SDL and OpenGL after creating a window"
  (configure display 'sdl)
  (configure display 'opengl))

(defmethod configure ((display display) (api (eql 'sdl)))
  "Configure SDL settings"
  (setf (sdl:frame-rate) (fps display))
  (sdl:show-cursor nil))

(defmethod configure ((display display) (api (eql 'opengl)))
  "Configure OpenGL settings"
  (gl:viewport 0 0 (width display) (height display))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 (/ (width display) (height display)) 1 1000.0)
  (gl:matrix-mode :modelview)
  (aim (camera display))
  (gl:enable
    :texture-2d
    :blend
    :depth-test
    :multisample
    :color-material
    :lighting
    :light0)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:material :front :shininess 50)
  (gl:light :light0 :position '(1 1 1 0))
  (gl:shade-model :smooth)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 1))

(defmethod aim ((camera camera))
  "Set up the camera"
  (let ((look-at (list (eye camera) (center camera) (up camera))))
    (apply #'glu:look-at (flatten (mapcar #'vector->list look-at)))))
