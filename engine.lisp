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
  (gl:with-pushed-matrix
    (gl:translate 30 30 0)
    (gl:translate (* 20 (sin (/ (sdl:sdl-get-ticks) 100.0)))
                  (* 20 (cos (/ (sdl:sdl-get-ticks) 100.0)))
                  0)
    (draw-entity :alien-big))
  (gl:with-pushed-matrix
    (gl:translate 300 60 0)
    (gl:translate (* 20 (sin (/ (sdl:sdl-get-ticks) 250.0)))
                  (* 20 (cos (/ (sdl:sdl-get-ticks) 250.0)))
                  0)
    (draw-entity :alien-small))
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
