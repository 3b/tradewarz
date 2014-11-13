(in-package :tradewarz)

(defparameter *display* (make-instance 'display))
(defparameter *assets*
  '("alien"))
(defparameter *entities* nil)

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:idle ()
     (restartable (draw)))))

(defun draw ()
  (gl:clear :color-buffer-bit)
  (draw-entity :alien-small)
  (draw-entity :hex)
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
