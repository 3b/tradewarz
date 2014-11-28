(in-package :tradewarz)

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (case key
    (:sdl-key-escape (sdl:push-quit-event))
    (:sdl-key-left (gl:translate 5 0 0))
    (:sdl-key-right (gl:translate -5 0 0))
    (:sdl-key-up (gl:translate 0 5 0))
    (:sdl-key-down (gl:translate 0 -5 0))
    (:sdl-key-w (vector-modify (dtv *e1*) 0 -0.01 0))
    (:sdl-key-s (vector-modify (dtv *e1*) 0 0.01 0))
    (:sdl-key-a (vector-modify (dtv *e1*) -0.01 0 0))
    (:sdl-key-d (vector-modify (dtv *e1*) 0.01 0 0))))

(defun key-up (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (format t "Key ~a released~%" key))

(defun mouse-down (button state x y)
  (declare (ignore state))
  (when (eq button 3)
    (format t "Right clicked at ~a,~a~%" x y)))

(defun mouse-up (button state x y)
  (declare (ignore state))
  (when (eq button 3)
    (format t "Right unclicked at ~a,~a~%" x y)))
