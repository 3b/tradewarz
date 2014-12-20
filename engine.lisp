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

(defun start-game ()
  (sdl2.kit:start)
  (setf *game* (make-instance 'game))
  (setf (debugp *game*) t)
  (load-scene *game* :name "demo"))

(defun toggle-debugging ()
  (setf (debugp *game*) (not (debugp *game*)))
  (load-scene *game* :name "demo"))

(defun profile ()
  (sb-profile:unprofile)
  (sb-profile:reset)
  (sb-profile:profile "TRADEWARZ")
  (start-game)
  (sb-profile:report)
  (sb-profile:unprofile))
