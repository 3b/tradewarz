(in-package :tradewarz)

(defclass display (gl-window)
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
           :initform (make-instance 'camera))))

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

(defmethod initialize-instance :before ((display display) &key)
  (dolist (setting '((:multisamplebuffers 1)
                     (:multisamplesamples 4)))
    (apply #'sdl2:gl-set-attr setting)))

(defmethod initialize-instance :after ((display display) &key &allow-other-keys)
  "Configure SDL and OpenGL after creating a window"
  (configure display 'sdl)
  (configure display 'opengl))

(defmethod configure ((display display) (api (eql 'sdl)))
  "Configure SDL settings"
  (sdl2:gl-set-swap-interval (or (fps display) 0))
  (setf (idle-render display) t))

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
    :cull-face
    :light0)
  (gl:front-face :cw)
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

(defmethod render ((display display))
  (gl:clear :color-buffer :depth-buffer)
  (when-let ((scene (current-scene)))
    (step-frame scene)
    (update-scene)))

(defmethod close-window ((display display))
  (format t "Quitting game~%")
  (call-next-method))
