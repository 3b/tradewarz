(in-package :tradewarz)

(defparameter *display* (make-instance 'display))
(defparameter *assets*
  '("alien"))
(defparameter *entities* nil)

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
     (restartable (draw)))))

(defun draw ()
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
  (gl:flush)
  (sdl:update-display))

(defun clean-up ())

(defun start-game ()
  (sdl:with-init ()
    (create-display)
    (load-assets)
    (define-events)
    (clean-up)))

(defun tradewarz ()
  (bt:make-thread #'start-game :name "tradewarz"))
