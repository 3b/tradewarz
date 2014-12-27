(in-package :tradewarz)

(defclass scene (frame)
  ((name :reader name
         :initarg :name)
   (root :reader root
         :initarg :root
         :initform (make-instance 'scene-node))
   (world-map :accessor world-map)
   (models :reader models
           :initform (make-hash-table))))

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets)
        do (load-models object asset)
           (apply #'load-map object world)))

(defun current-scene ()
  (scene *game*))

(defun load-scene (game &key name)
  (sdl2:in-main-thread ()
    (setf (scene game) (make-instance 'scene :name name))
    (generate-map)

    ;; test entities
    (let ((axes (make-node :axes))
          (tank (make-node :tank))
          (jet (make-node :jet)))

      (add-node axes)
      (vector-modify (dv axes) -64 32 0)

      (add-node tank)
      (setf (rotatingp tank) t)
      (vector-modify (drv tank) 0 0 0.01)
      (vector-modify (dv tank) 0 0 8)

      (add-node jet)
      (setf (rotatingp jet) t)
      (vector-modify (drv jet) 0 0 0.01)
      (vector-modify (dv jet) 56 0 32)

      (flet ((heli (p &key (pos '(112 0 40)) (r '(0 0 0)) (s 1.0)
                        (dr 0.0))
               (setf s (float s 1.0))
               (let ((heli (make-node :heli))
                     (heli-rotor-top (make-node :heli-rotor-top))
                     (heli-rotor-tail (make-node :heli-rotor-tail)))
                 (if p
                     (add-node heli :parent p)
                     (add-node heli))
                 (setf (rotatingp heli) t)
                 (vector-modify (drv heli) 0 0 dr)
                 (apply #'vector-modify (dr heli) r)
                 (apply #'vector-modify (dv heli) pos)
                 (setf (local-basis heli)
                       (make-matrix s 0.0 0.0 0.0
                                    0.0 s 0.0 0.0
                                    0.0 0.0 s 0.0
                                    0.0 0.0 0.0 1.0))

                 (add-node heli-rotor-top :parent heli)
                 (setf (rotatingp heli-rotor-top) t)
                 (vector-modify (drv heli-rotor-top) 0 0 0.003)
                 (vector-modify (dv heli-rotor-top) -12 0 0)

                 (add-node heli-rotor-tail :parent heli)
                 (setf (rotatingp heli-rotor-tail) t)
                 (vector-modify (drv heli-rotor-tail) 0 0.1 0.0)
                 (vector-modify (dv heli-rotor-tail) 24 0 -8)
                 heli-rotor-top)))
        (let* ((s1 2)
               (heli (heli nil :pos '(50 -60 20) :s s1 :dr 0.0001)))
          (labels ((h2 (p s x)
                     (when (plusp x)
                       (loop for i below 4
                             for a = (* (+ 0.5 i) (/ pi 2))
                             for h =(heli p :pos (list (+ (* 30 (sin a)))
                                                          (* 30 (cos a)) 5)
                                            :r `(,(/ pi -2) ,a 0)
                                            :s s)
                             do (h2 h s (1- x))))))
            (h2 heli 0.5 4)))))))

(defun loop-scene (func &optional parent)
  (let ((parent (or parent (root (current-scene)))))
    (funcall func parent)
    (loop for child being the hash-values of (children parent)
          do (loop-scene func child))))

(defun update-scene ()
  (loop-scene #'update-node)
  (loop-scene #'render-node)
  (gl:bind-vertex-array 0))


