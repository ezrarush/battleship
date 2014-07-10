(in-package #:battleship)

(defclass graphics-engine ()
  ((camera
    :initarg :camera
    :initform (make-instance 'camera 
			     :pos (sb-cga:vec 0.0 0.0 -10.0) 
			     :target (sb-cga:vec 0.0 0.0 0.0))
    :accessor camera)
   (proj-info
    :initarg :proj-info
    :initform (make-projection-info
	       :height (ensure-float *window-height*)
	       :width (ensure-float *window-width*)
	       :z-near -500.0
	       :z-far 500.0)
    :accessor proj-info
    )
   (pipeline)
   (quad)
   (sphere)))

(defgeneric graphics-init (graphics-engine))
(defgeneric render-scene (graphics-engine))

(defmethod graphics-init ((tut graphics-engine))
  (with-slots (pipeline quad sphere ) tut

    (gl:clear-color 0.0 0.0 0.0 1.0)
    
    (setf pipeline (make-instance 'pipeline))
    
    ;; used for fields and shipo
    (setf quad (make-instance 'quad))
    
    ;; used for pings and missiles
    (setf sphere (make-instance 'sphere))))

(defmethod render-scene ((tut graphics-engine))
  (with-slots (pipeline quad sphere) tut
    
    (gl:clear :color-buffer-bit)

    ;; field size
    (setf (scale pipeline) (sb-cga:vec 197.0 196.0 1.0))

    ;; player field
    (setf (world-pos pipeline) (sb-cga:vec -199.0 0.0 0.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))
    
    ;; enemy field
    (setf (world-pos pipeline) (sb-cga:vec 199.0 0.0 0.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))
    
    ;; vertical ship
    (setf (scale pipeline) (sb-cga:vec 6.0 30.0 1.0))
    (setf (world-pos pipeline) (sb-cga:vec -199.0 0.0 0.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0))
    
    ;; horizontal ship
    (setf (scale pipeline) (sb-cga:vec 30.0 6.0 1.0))
    (setf (world-pos pipeline) (sb-cga:vec -299.0 0.0 0.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0))
    
    ;; ping
    (setf (scale pipeline) (sb-cga:vec 60.0 60.0 1.0))
    (setf (world-pos pipeline) (sb-cga:vec 299.0 -99.0 0.0))
    (update-transforms pipeline)
    (sphere-render sphere (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.3 0.0))
    
    ;; ;; missile miss
    ;; (setf (scale pipeline) (sb-cga:vec 6.0 6.0 1.0))
    ;; (setf (world-pos pipeline) (sb-cga:vec 99.0 99.0 0.0))
    ;; (update-transforms pipeline)
    ;; (sphere-render sphere (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.15 0.0 0.0))
    
    ;; ;; missile hit 
    ;; (setf (scale pipeline) (sb-cga:vec 6.0 6.0 1.0))
    ;; (setf (world-pos pipeline) (sb-cga:vec 199.0 0.0 0.0))
    ;; (update-transforms pipeline)
    ;; (sphere-render sphere (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.6 0.0 0.0))

    (loop for missile in *missiles-fired* do
	 (setf (scale pipeline) (sb-cga:vec (radius missile) (radius missile) 1.0))
	 (setf (world-pos pipeline) (pos missile))
	 (update-transforms pipeline)
	 (sphere-render sphere (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.6 0.0 0.0))
	 )
    ))
