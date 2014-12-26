(in-package :tradewarz)

(defclass model ()
  ((name :reader name
         :initarg :name)
   (image :reader image
          :initarg :image
          :initform nil)
   (tile :reader tile
         :initarg :tile
         :initform nil)
   (primitive :reader primitive
              :initarg :primitive
              :initform :triangle-strip)
   (object :reader object
           :initarg :object
           :initform nil)
   (size :reader size
         :initarg :size
         :initform nil)
   (color :reader color
          :initarg :color
          :initform '(1 1 1))
   (geometry :accessor geometry
          :initarg :geometry
          :initform nil)
   (vao :accessor vao
        :initarg :vao
        :initform nil)
   (vertex-count :accessor vertex-count
                 :initarg :vertex-count
                 :initform nil)
   (radial-extent :accessor radial-extent
                  :initarg :radial-extent
                  :initform (make-vector 0 0 0))
   (texture-id :accessor texture-id
               :initarg :texture-id
               :initform 0)))

(defmethod initialize-instance :after ((object model) &key)
  (when (image object)
    (setf (texture-id object) (load-texture (image object))))
  (when (size object)
    (setf (slot-value object 'size)
          (apply #'make-vector (size object)))
    (loop for i below (length (size object))
          when (zerop (aref (size object) i))
            do (setf (aref (size object) i) 1.0)))
  (setf (slot-value object 'primitive)
        (or (cffi:foreign-enum-value '%gl:enum (primitive object) :errorp nil)
            (primitive object))))

(defun get-model (name)
  (gethash name (models (current-scene))))

(defmethod get-size ((model model))
  (or (size model)
      (tile-size (current-map))))

(defun translate-geometry (geometry)
  (flet ((x (x)
           (etypecase x
             (list
              (make-vector (or (first x) 0) (or (second x) 0) (or (third x) 0)))
             ((simple-array single-float (3))
              x)
             (vector
              (make-vector (aref x 0) (aref x 1) (aref x 2))))))
    (loop for v in geometry
          ;; some entries don't have normals, but also don't disable
          ;; lighting so add a reasonable default normal instead of
          ;; just using last normal from previous thing drawn
          if (first v)
            collect (mapcar #'x v)
          else collect (cons (make-vector 0 1 0)
                             (mapcar #'x (cdr v))))))

(defun create-vao (model)
  (let* ((vao (gl:gen-vertex-array))
         (vbo (car (gl:gen-buffers 1)))
         (number-of-vertices (length (geometry model)))
         ;; 3 floats for normal, 3 for position, 2 for UV, 3 for color
         (floats-per-vertex (+ 3 3 2 3))
         (float-size 4)
         (bytes-per-vertex (* floats-per-vertex float-size)))
    (unwind-protect
         (progn
           ;; fill a buffer with vertex data
           (cffi:with-foreign-object (buffer :float (* floats-per-vertex
                                                       number-of-vertices))
             ;; add a helper function to fill the buffer, I starts from -1 so
             ;; we can directly use the value returned from INCF as the index
             (let ((i -1))
               (labels ((add (a n &key swap)
                          (assert (< i (* floats-per-vertex
                                          number-of-vertices)))
                          (loop for x below n
                                do (setf (cffi:mem-aref buffer :float (incf i))
                                         (if swap
                                             (aref a (- n x 1))
                                             (aref a x))))))
                 (loop for (n v uv c) in (geometry model)
                       do (add v 3)
                          (add n 3)
                          (add uv 2 :swap t)
                          (add c 3))))
             ;; copy it into the VBO
             (gl:bind-vertex-array vao)
             (gl:bind-buffer :array-buffer vbo)
             (%gl:buffer-data :array-buffer (* bytes-per-vertex
                                               number-of-vertices)
                              buffer :static-draw)
             ;; set up the VAO
             (gl:enable-client-state :vertex-array)
             (%gl:vertex-pointer 3 :float bytes-per-vertex
                                 0)
             (gl:enable-client-state :normal-array)
             (%gl:normal-pointer :float bytes-per-vertex
                                 (* 3 float-size))
             (gl:enable-client-state :texture-coord-array)
             (%gl:tex-coord-pointer 2 :float bytes-per-vertex
                                    (* 6 float-size))
             (gl:enable-client-state :color-array)
             (%gl:color-pointer 3 :float bytes-per-vertex
                                (* 8 float-size))
             ;; done modifying the VAO, so turn it off again
             (gl:bind-vertex-array 0)
             ;; we don't need the VBO object anymore, since the VAO keeps
             ;; a reference to it
             (gl:delete-buffers (list vbo))
             (setf vbo nil)
             ;; store the VAO and number of vertices in model
             (setf (vertex-count model) number-of-vertices
                   (vao model) (shiftf vao nil))))
      ;; make sure VAO and VBO get deleted in case we had an error
      (when vbo (gl:delete-buffers (list vbo)))
      (when vao (gl:delete-vertex-arrays (list vao))))))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)
           (if (object model)
               (setf (geometry model) (load-obj (object model)))
               (setf (geometry model) (translate-geometry (geometry model))))
           (find-radial-extent model)
           (create-vao model)))

(defun find-radial-extent (model)
  (loop with farthest = 0
        with origin = (make-vector 0 0 0)
        for (nil vertex nil nil) in (geometry model)
        for distance = (vector-distance origin vertex)
        do (when (> distance farthest)
             (setf (radial-extent model) vertex
                   farthest distance))))
