(in-package :tradewarz)

(defclass world-map ()
  ((tile-shape :reader tile-shape
               :initarg :tile-shape
               :initform :quad)
   (tile-size :reader tile-size
              :initarg :tile-size
              :initform '(64 64 1))
   (tiles :reader tiles
          :initarg :tiles)))

(defun current-map ()
  (world-map (current-scene)))

(defun load-map (scene data)
  (setf (world-map scene) (apply #'make-instance 'world-map data)))

(defun find-tile (tile-id)
  (loop with models = (models (current-scene))
        for tile being the hash-keys of models
        for id = (tile (gethash tile models))
        do (when (and id (= tile-id id))
             (return tile))))

(defmethod draw-tile :around (shape x y)
  (let* ((tile-id (aref (tiles (current-map)) y x))
         (node (make-node (find-tile tile-id)))
         (size (tile-size (current-map)))
         (offset (mapcar #'* size (call-next-method shape x (- y)))))
    (add-node node)
    (apply #'vector-modify (dv node) offset)
    #++(draw-tile-coords y x offset)))

(defmethod draw-tile (shape x y)
  (list x y 0))

(defun coordinates->models (x y)
  (flet ((loop-num (num)
           (let ((models #(:digit-0
                           :digit-1
                           :digit-2
                           :digit-3
                           :digit-4
                           :digit-5
                           :digit-6
                           :digit-7
                           :digit-8
                           :digit-9)))
             (nreverse
               (or (loop until (zerop num)
                         collect (multiple-value-bind (q r) (floor num 10)
                                   (setf num q)
                                   (aref models r)))
                   (list :digit-0))))))
    (flatten (list (loop-num x) #\, (loop-num y)))))

(defun draw-tile-coords (x y offset)
  (when (debugp *game*)
    (loop with digits = (coordinates->models x y)
          with digit-count = 0
          with digit-size = 8.0
          with digit-offset = (make-vector 0.0 digit-size 0.1)
          for digit in digits
          do (if (eq digit #\,)
               (setf digit-count 0
                     (vy digit-offset) (- (vy digit-offset)))
               (let* ((node (make-node digit)))
                 (setf (vx digit-offset) (* digit-count digit-size))
                 (add-node node)
                 (apply #'vector-modify (dv node) offset)
                 (vector-add-* (dv node) digit-offset (dv node))
                 (incf digit-count))))))

(defun generate-map ()
  (let ((shape (tile-shape (current-map))))
    (loop with (h w) = (array-dimensions (tiles (current-map)))
      for x below (or w 0)
      do (loop for y below (or h 0)
               do (draw-tile shape x y)))))
