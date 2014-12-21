(in-package :tradewarz)

(defmethod keyboard-event ((display display) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case state
      (:KEYDOWN (key-down scancode repeat-p))
      (:KEYUP (key-up scancode repeat-p)))))

(defun key-down (scancode repeat-p)
  (unless repeat-p
    (format t "Key pressed: ~s~%" scancode))
  (case scancode
    (:scancode-escape (close-window (display *game*)))
    (:scancode-grave (toggle-debugging))
    (:scancode-up (gl:translate 0 -5 0))
    (:scancode-down (gl:translate 0 5 0))
    (:scancode-left (gl:translate 5 0 0))
    (:scancode-right (gl:translate -5 0 0))))

(defun key-up (scancode repeat-p)
  (unless repeat-p
    (format t "Key released: ~s~%" scancode)))

(defmethod mousebutton-event ((display display) state ts b x y)
  (let ((coords (convert-mouse-coords (float x) (float y))))
    (case state
      (:MOUSEBUTTONDOWN (mouse-down b coords))
      (:MOUSEBUTTONUP (mouse-up b coords)))))

(defun mouse-down (button coords)
  (format t "Mouse button ~a pressed at ~a~%" button coords)
  (when (eq button 3)
    (make-picking-ray (vx coords) (vy coords))))

(defun mouse-up (button coords)
  (format t "Mouse button ~a released at ~a~%" button coords))

(defun make-picking-ray (x y)
  (when (debugp *game*)
    (let ((model (get-model :picking-ray))
          (near (unproject-vector x y 0.0))
          (far (unproject-vector x y 1.0)))
      (setf (geometry model) `(((1.0 1.0 1.0)
                                ,(vector->list near)
                                (1.0 1.0)
                                (1.0 0.0 0.0))
                               ((1.0 1.0 1.0)
                                ,(vector->list far)
                                (1.0 1.0)
                                (1.0 0.0 0.0))))
      (add-node (make-node :picking-ray)))))

(defun unproject-vector (x y z)
  (multiple-value-bind (x y z)
    (glu:un-project x y z)
    (make-vector (coerce x 'single-float)
                 (coerce y 'single-float)
                 (coerce z 'single-float))))

(defun convert-mouse-coords (x y)
  "Convert SDL mouse coordinates to OpenGL coordinates with origin at the
   bottom left"
  (let ((y (- (height (display *game*)) y)))
    (make-vector x y)))
