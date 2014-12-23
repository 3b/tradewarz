(in-package :tradewarz)

(defclass world-map ()
  ((tile-shape :reader tile-shape
               :initarg :tile-shape
               :initform :quad)
   (tile-size :reader tile-size
              :initarg :tile-size
              :initform (make-vector 64 64 1))
   (tiles :reader tiles
          :initarg :tiles)))

(defmethod initialize-instance :after ((map world-map) &key)
  (setf (slot-value map 'tile-size)
        (when (tile-size map) (apply #'make-vector (tile-size map)))))

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
         (offset (map 'list '* size (call-next-method shape x (- y)))))
    (add-node node)
    (apply #'vector-modify (dv node) offset)
    (draw-tile-coords x y offset)))

(defmethod draw-tile (shape x y)
  (list x y 0))

(defun draw-tile-coords (y x offset)
  (when (debugp *game*)
    (loop with digits = (concatenate 'list (format nil "~a,~a" x y))
          with digit-count = 0
          with digit-size = 8
          with digit-offset = (make-vector 0 digit-size 0.1)
          for digit in digits
          for model = (intern (format nil "~:@(digit-~a~)" digit) "KEYWORD")
          do (if (string= digit #\,)
               (setf digit-count 0
                     (vy digit-offset) (- (vy digit-offset)))
               (let ((node (make-node model)))
                 (setf (vx digit-offset) (float (* digit-count digit-size)))
                 (add-node node)
                 (apply #'vector-modify (dv node) offset)
                 (vector-add-to (dv node) digit-offset (dv node))
                 (incf digit-count))))))

(defun generate-map ()
  (let ((shape (tile-shape (current-map))))
    (loop with (h w) = (array-dimensions (tiles (current-map)))
      for x below (or w 0)
      do (loop for y below (or h 0)
               do (draw-tile shape x y)))))
