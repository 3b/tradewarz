(in-package :tradewarz)

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (case key
    (:sdl-key-escape (sdl:push-quit-event))
    (:sdl-key-backquote (toggle-debugging))
    (:sdl-key-left (gl:translate 5 0 0))
    (:sdl-key-right (gl:translate -5 0 0))
    (:sdl-key-up (gl:translate 0 -5 0))
    (:sdl-key-down (gl:translate 0 5 0))
    (:sdl-key-w (vector-modify (dtv *e1*) 0 1 0))
    (:sdl-key-s (vector-modify (dtv *e1*) 0 -1 0))
    (:sdl-key-a (vector-modify (dtv *e1*) -1 0 0))
    (:sdl-key-d (vector-modify (dtv *e1*) 1 0 0))))

(defun key-up (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (format t "Key ~a released~%" key))

(defun mouse-down (button state x y)
  (declare (ignore state))
  (when (eq button 3)
    (when (debugp *game*)
      (make-picking-ray x y))
    (format t "Right clicked at ~a~%" (convert-mouse-coords x y))))

(defun mouse-up (button state x y)
  (declare (ignore state))
  (when (eq button 3)
    (format t "Right unclicked at ~a~%" (convert-mouse-coords x y))))

(defun make-picking-ray (x y)
  (let ((model (get-model :picking-ray))
        (near (unproject-vector x y 0))
        (far (unproject-vector x y 1)))
    (setf (lines model) `((nil ,(vector->list near) (1 1) (1 0 0))
                          (nil ,(vector->list far) (1 1) (1 0 0))))
    (add-node (make-node :picking-ray))))

(defun unproject-vector (x y z)
  (let ((coords (convert-mouse-coords x y)))
    (multiple-value-bind (x y z)
      (glu:un-project (vx coords) (vy coords) z)
      (make-vector x y z))))

(defun convert-mouse-coords (x y)
  "Convert SDL mouse coordinates to OpenGL coordinates with origin at the
   bottom left"
  (let* ((y (- (height (display *game*)) y)))
    (make-vector x y)))
