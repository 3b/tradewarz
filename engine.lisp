(in-package :tradewarz)

(defvar *game* nil)

(defclass game ()
  ((display :reader display
            :initform (make-instance 'display))
   (textures :reader textures
             :initform (make-hash-table :test 'equal))
   (scene :accessor scene
          :initarg :scene
          :initform nil)))

(defun make-game ()
  (setf *game* (make-instance 'game)))

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:state state
                      :scancode scancode
                      :key key
                      :mod-key mod-key
                      :unicode unicode)
     (key-down key state mod-key scancode unicode))
    (:idle ()
     (restartable (main-loop)))))

(defun main-loop ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (generate-map)
  (gl:with-pushed-matrix
    (gl:translate 0 0 -1)
    (draw-entity :alien-small -300 400))
  (gl:flush)
  (sdl:update-display))

(defun start-game ()
  (bt:make-thread
    #'(lambda ()
        (sdl:with-init ()
          (make-game)
          (make-scene :name "demo")
          (define-events)))
    :name "tradewarz"))
