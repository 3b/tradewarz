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

(defmethod draw-tile :around (shape x y &key layer)
  (let* ((tile-id (aref (tiles (current-map)) y x))
         (node (make-node (find-tile tile-id)))
         (size (tile-size (current-map)))
         (offset (mapcar #'* size (call-next-method shape x y :node node))))
    (add-node node :parent layer)
    (apply #'vector-modify (dv node) offset)
    (draw-tile-coords x y layer offset)))

(defmethod draw-tile (shape x y &key)
  (list x y 0))

(defun draw-tile-coords (x y layer offset)
  ;; the ugliest function ever - must be rewritten
  (when (debugp *game*)
    (let ((digits (concatenate 'list (format nil "~a,~a" x y))))
      (loop with digit-count = 0
            with v-space = 8
            for digit in digits
            do
              (if (string= digit #\,)
                (progn
                  (setf digit-count 0)
                  (if (eq v-space 8)
                    (setf v-space -8)
                    (setf v-space 8)))
                (progn
                  (let* ((model-name (intern (format nil "~:@(digit-~a~)" digit) "KEYWORD"))
                         (node (make-node model-name)))
                    (add-node node :parent layer)
                    (apply #'vector-modify (dv node) offset)
                    (vector-modify (dv node)
                                   (+ (vx (dv node)) digit-count)
                                   (- (vy (dv node)) v-space)
                                   -1)
                    (incf digit-count))))))))

(defun generate-map ()
  (let ((layer (make-instance 'scene-node))
        (shape (tile-shape (current-map))))
    (add-node layer)
    (loop with (h w) = (array-dimensions (tiles (current-map)))
      for x below (or w 0)
      do (loop for y below (or h 0)
               do (draw-tile shape x y :layer layer)))))
