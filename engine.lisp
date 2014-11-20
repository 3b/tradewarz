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
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (update-entities)
  (gl:flush)
  (sdl:update-display))

(defun start-game ()
  (bt:make-thread
    #'(lambda ()
        (sdl:with-init ()
          (make-game)
          (make-scene :name "demo")
          (make-entity :alien-small
                       :layer :mob)
          (move (get-entity 0 :layer :mob) '(0 0 -32))
          (rotate (get-entity 0 :layer :mob) '(90 0 0))
          (define-events)))
    :name "tradewarz"))
