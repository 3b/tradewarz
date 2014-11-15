(in-package :tradewarz)

(defvar *game* nil)

(defclass game ()
  ((display :reader display
            :initform (make-instance 'display))
   (textures :accessor textures
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
  (gl:clear :color-buffer-bit)
  (generate-map)
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
