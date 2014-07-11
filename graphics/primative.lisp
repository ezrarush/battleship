(in-package #:battleship)

(defclass quad ()
  ((vertex-buffer)
   (technique)))

(defgeneric quad-render (quad projection-transform model-view-transform color))

(defmethod initialize-instance :after ((self quad) &key)
  (with-slots (vertex-buffer technique) self
    (let ((quad-pos (make-array 6 :initial-contents (list (sb-cga:vec -1.0 -1.0 0.0)
							  (sb-cga:vec  1.0 -1.0 0.0)
							  (sb-cga:vec  1.0  1.0 0.0)
							  (sb-cga:vec  1.0  1.0 0.0)
							  (sb-cga:vec -1.0  1.0 0.0)
							  (sb-cga:vec -1.0 -1.0 0.0))))
	  (quad-tex-coords (make-array 6 :initial-contents (list (sb-cga:vec 0.0 1.0 0.0)
								 (sb-cga:vec 1.0 1.0 0.0)
								 (sb-cga:vec 1.0 0.0 0.0)
								 (sb-cga:vec 1.0 0.0 0.0)
								 (sb-cga:vec 0.0 0.0 0.0)
								 (sb-cga:vec 0.0 1.0 0.0))))
	  (quad-normals (make-array 6 :initial-contents (list (sb-cga:vec 0.0 1.0 0.0)
							      (sb-cga:vec 0.0 1.0 0.0)
							      (sb-cga:vec 0.0 1.0 0.0)
							      (sb-cga:vec 0.0 1.0 0.0)
							      (sb-cga:vec 0.0 1.0 0.0)
							      (sb-cga:vec 0.0 1.0 0.0))))
	  (arr (gl:alloc-gl-array :float 144))
	  (i 0)) 
      (dotimes (x 6)
        (dotimes (y 3)
          (setf (gl:glaref arr i) (aref (aref quad-pos x) y))
          (incf i))
        (dotimes (y 2)
          (setf (gl:glaref arr i) (aref (aref quad-tex-coords x) y))
          (incf i))
        (dotimes (y 3)
          (setf (gl:glaref arr i) (aref (aref quad-normals x) y))
          (incf i)))
      (setf vertex-buffer (car (gl:gen-buffers 1)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))
    
    (setf technique (make-instance 'color-technique :vs-path "/home/quicklisp/local-projects/battleship/shaders/shader.vertexshader" :fs-path "/home/quicklisp/local-projects/battleship/shaders/shader.fragmentshader"))))

(defmethod quad-render ((self quad) projection-transform model-view-transform color)
  (with-slots (vertex-buffer technique) self

    (technique-enable technique)

    (set-projection-transform technique projection-transform)
    (set-model-view-transform technique model-view-transform)
    (set-color technique color)

    (gl:bind-buffer :array-buffer vertex-buffer)

    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)
    
    (gl:vertex-attrib-pointer 0 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 2 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 3)))
    (gl:vertex-attrib-pointer 2 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 5)))
    
    (gl:draw-arrays :triangles 0 6)
    
    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 2)))

(defclass circle ()
  ((vertex-buffer)
   (technique)
   (vertex-count
    :initarg :vertex-count
    :initform 32)))

(defgeneric circle-render (circle projection-transform model-view-transform color))

(defmethod initialize-instance :after ((self circle) &key)
  (with-slots (vertex-buffer technique vertex-count) self
    (let ((arr (gl:alloc-gl-array :float (* vertex-count 8)))
	  (i 0))
      ;; circle center at (0.0 0.0 0.0) plus the tex and normal coords set to 0.0 for now
      (loop repeat 8 do
	(setf (gl:glaref arr i) 0.0)
	(incf i))
      (dotimes (x (- vertex-count 1))
	(let* ((percent (/ x (- vertex-count 2)))
	       (radian (* percent pi 2)))
	  ;; positios x
	  (setf (gl:glaref arr i) (ensure-float (cos radian)))
	  (incf i)
	  ;; position y
	  (setf (gl:glaref arr i) (ensure-float (sin radian)))
	  (incf i)
	  ;; position z
	  (setf (gl:glaref arr i) 0.0)
	  (incf i)
	  ;; tex and normal coords set to 0.0 for now
	  (loop repeat 5 do
	    (setf (gl:glaref arr i) 0.0)
	    (incf i))))
      (setf vertex-buffer (car (gl:gen-buffers 1)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))
    (setf technique (make-instance 'color-technique :vs-path "/home/quicklisp/local-projects/battleship/shaders/shader.vertexshader" :fs-path "/home/quicklisp/local-projects/battleship/shaders/shader.fragmentshader"))))

(defmethod circle-render ((self circle) projection-transform model-view-transform color)
    (with-slots (vertex-buffer technique vertex-count) self

    (technique-enable technique)

    (set-projection-transform technique projection-transform)
    (set-model-view-transform technique model-view-transform)
    (set-color technique color)

    (gl:bind-buffer :array-buffer vertex-buffer)

    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)
    
    (gl:vertex-attrib-pointer 0 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 2 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 3)))
    (gl:vertex-attrib-pointer 2 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 5)))
    
    (gl:draw-arrays :triangle-fan 0 vertex-count)
      
    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 2))
  )

(defclass sphere ()
  ((vertex-buffer)
   (technique)
   (faces)))

(defgeneric sphere-render (sphere projection-transform model-view-transform color))

(defmethod initialize-instance :after ((self sphere) &key)
  (with-slots (vertex-buffer technique faces) self
    (let* ((radius 1.0)
	   (sub-div-y 12)
	   (sub-div-z 12)
	   (add-angle-y (/ 360.0 (ensure-float sub-div-y)))
	   (add-angle-z (/ 180.0 (ensure-float sub-div-z)))
	   (cur-angle-y 0.0)
	   (tex-u (/ 1.0 (ensure-float sub-div-y)))
	   (tex-v (/ 1.0 (ensure-float sub-div-z)))
	   (faces-added 0)
	   (arr (gl:alloc-gl-array :float (* 870 8)))
	   (i 0))
      (dotimes (steps-y sub-div-y)
	(let* ((next-angle-y (+ cur-angle-y add-angle-y))
	       (sin-y (ensure-float (sin (* (/ cur-angle-y 180.0) pi))))
	       (cos-y (ensure-float (cos (* (/ cur-angle-y 180.0) pi))))
	       (next-sin-y (ensure-float (sin (* (/ next-angle-y 180.0) pi))))
	       (next-cos-y (ensure-float (cos (* (/ next-angle-y 180.0) pi))))
	       (dir-y (sb-cga:vec cos-y 0.0 (- sin-y)))
	       (next-dir-y (sb-cga:vec next-cos-y 0.0 (- next-sin-y)))
	       (cur-angle-z 0.0))
	  (dotimes (steps-z sub-div-z)
	    (let* ((next-angle-z (+ cur-angle-z add-angle-z))
		   (sin-z (ensure-float (sin (* (/ cur-angle-z 180.0) pi))))
		   (cos-z (ensure-float (cos (* (/ cur-angle-z 180.0) pi))))
		   (next-sin-z (ensure-float (sin (* (/ next-angle-z 180.0) pi))))
		   (next-cos-z (ensure-float (cos (* (/ next-angle-z 180.0) pi))))
		   (quad-points (make-array 4 :initial-contents (list (sb-cga:vec (* (aref dir-y 0) sin-z radius) 
										  (* cos-z radius) 
										  (* (aref dir-y 2) sin-z radius))
								      (sb-cga:vec (* (aref dir-y 0) next-sin-z radius) 
										  (* next-cos-z radius) 
										  (* (aref dir-y 2) next-sin-z radius))
								      (sb-cga:vec (* (aref next-dir-y 0) next-sin-z radius) 
										  (* next-cos-z radius) 
										  (* (aref next-dir-y 2) next-sin-z radius))
								      (sb-cga:vec (* (aref next-dir-y 0) sin-z radius) 
										  (* cos-z radius) 
										  (* (aref next-dir-y 2) sin-z radius)))))
		   (normals (make-array 4 :initial-contents (list (sb-cga:normalize (aref quad-points 0))
								  (sb-cga:normalize (aref quad-points 1))
								  (sb-cga:normalize (aref quad-points 2))
								  (sb-cga:normalize (aref quad-points 3)))))
		   (tex-coords (make-array 4 :initial-contents (list (sb-cga:vec (ensure-float (+ (/ (asin (aref (aref normals 0) 0)) pi) 0.5)) 
										 (ensure-float (+ (/ (asin (aref (aref normals 0) 1)) pi) 0.5)) 
										 0.0)
								     (sb-cga:vec (ensure-float (+ (/ (asin (aref (aref normals 1) 0)) pi) 0.5))
										 (ensure-float (+ (/ (asin (aref (aref normals 1) 1)) pi) 0.5))
										 0.0)
								     (sb-cga:vec (ensure-float (+ (/ (asin (aref (aref normals 2) 0)) pi) 0.5))
										 (ensure-float (+ (/ (asin (aref (aref normals 2) 1)) pi) 0.5))
										 0.0)
								     (sb-cga:vec (ensure-float (+ (/ (asin (aref (aref normals 3) 0)) pi) 0.5))
										 (ensure-float (+ (/ (asin (aref (aref normals 3) 1)) pi) 0.5))
										 0.0))))
		   (indices (make-array 6 :initial-contents (list 0 1 2 2 3 0))))
	      (dotimes (z 6)
		(let ((index (aref indices z)))
		  (dotimes (y 3)
		    (setf (gl:glaref arr i) (aref (aref quad-points index) y))
		    (incf i))
		  (dotimes (y 2)
		    (setf (gl:glaref arr i) (aref (aref tex-coords index) y))
		    (incf i))
		  (dotimes (y 3)
		    (setf (gl:glaref arr i) (aref (aref normals index) y))
		    (incf i))))
	      (incf faces-added 2)
	      (incf cur-angle-z add-angle-z))))
	(incf cur-angle-y add-angle-y))
      (setf faces faces-added)
      (setf vertex-buffer (car (gl:gen-buffers 1)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))
        
    (setf technique (make-instance 'color-technique :vs-path "/home/quicklisp/local-projects/battleship/shaders/shader.vertexshader" :fs-path "/home/quicklisp/local-projects/battleship/shaders/shader.fragmentshader"))))

(defmethod sphere-render ((self sphere) projection-transform model-view-transform color)
  (with-slots (vertex-buffer technique faces) self

    (technique-enable technique)

    (set-projection-transform technique projection-transform)
    (set-model-view-transform technique model-view-transform)
    (set-color technique color)
    
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)
    
    (gl:bind-buffer :array-buffer vertex-buffer)
    
    (gl:vertex-attrib-pointer 0 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 2 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 3)))
    (gl:vertex-attrib-pointer 2 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 5)))

    (gl:draw-arrays :triangles 0 (* faces 3))

    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 2)))

