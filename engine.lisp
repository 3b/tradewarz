(in-package :tradewarz)

(defvar *game* nil)

(defclass game ()
  ((display :reader display
            :initform (make-instance 'display))
   (debugp :accessor debugp
           :initform nil)
   (textures :reader textures
             :initform (make-hash-table :test 'equal))
   (scene :accessor scene
          :initarg :scene
          :initform nil)))

(defun make-game ()
  (sdl:with-init ()
    (setf *game* (make-instance 'game))
    (setf (debugp *game*) t)
    (make-window (display *game*))
    (load-scene :name "demo")
    (define-events)))

(defun toggle-debugging ()
  (setf (debugp *game*) (not (debugp *game*)))
  (load-scene :name "demo"))

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:state state
                      :scancode scancode
                      :key key
                      :mod-key mod-key
                      :unicode unicode)
     (key-down key state mod-key scancode unicode))
    (:key-up-event (:state state
                    :scancode scancode
                    :key key
                    :mod-key mod-key
                    :unicode unicode)
     (key-up key state mod-key scancode unicode))
    (:mouse-button-down-event (:button button :state state :x x :y y)
     (mouse-down button state x y))
    (:mouse-button-up-event (:button button :state state :x x :y y)
     (mouse-up button state x y))
    (:idle ()
     (restartable (main-loop)))))

(defun main-loop ()
  (gl:clear :color-buffer :depth-buffer)
  (step-frame (current-scene))
  (update-scene)
  (gl:flush)
  (sdl:update-display))

(defun start-game ()
  (bt:make-thread #'make-game :name "tradewarz"))

(defun profile ()
  (sb-profile:unprofile)
  (sb-profile:reset)
  (sb-profile:profile "TRADEWARZ")
  (make-game)
  (sb-profile:report))
