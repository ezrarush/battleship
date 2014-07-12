(in-package #:battleship)

(defclass graphics-engine ()
  ((camera
    :initarg :camera
    :initform (make-instance 'camera 
			     :pos (sb-cga:vec 0.0 0.0 -100.0) 
			     :target (sb-cga:vec 0.0 0.0 0.0))
    :accessor camera)
   (proj-info
    :initarg :proj-info
    :initform (make-projection-info
	       :height (ensure-float *window-height*)
	       :width (ensure-float *window-width*)
	       :z-near -500.0
	       :z-far 500.0)
    :accessor proj-info)
   (pipeline)
   (quad)
   (circle)))

(defgeneric graphics-init (graphics-engine))
(defgeneric render-scene (graphics-engine))

(defmethod graphics-init ((tut graphics-engine))
  (with-slots (pipeline quad circle) tut

    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:enable :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:depth-func :less)
       
    (setf pipeline (make-instance 'pipeline))
    
    ;; used for fields and shipo
    (setf quad (make-instance 'quad))
    
    ;; used for pings and missiles
    (setf circle (make-instance 'circle))))

(defmethod render-scene ((tut graphics-engine))
  (with-slots (pipeline quad circle) tut
    
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    
;; GUI depth range (-10, -1)
    
    ;; top board
    (setf (world-pos pipeline) (sb-cga:vec 0.0 250.0 -1.0))
    (setf (scale pipeline) (sb-cga:vec 400.0 54.0 1.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.0 0.0))

    ;; bottom board
    (setf (world-pos pipeline) (sb-cga:vec 0.0 -250.0 -1.0))
    (setf (scale pipeline) (sb-cga:vec 400.0 54.0 1.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.0 0.0))
    
    ;; center bar
    (setf (scale pipeline) (sb-cga:vec 4.0 200.0 1.0))
    (setf (world-pos pipeline) (sb-cga:vec 0.0 0.0 0.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.0 0.0))
    
;; Player field depth range (0, 9)
   
    (setf (world-pos pipeline) (sb-cga:vec -200.0 0.0 9.0))
    (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))

    (loop for ship in *ships-placed* do
	 (setf (scale pipeline) (sb-cga:vec (/ (width ship) 2.0) (/ (height ship) 2.0) 1.0))
	 (setf (world-pos pipeline) (pos ship))
	 (update-transforms pipeline)
	 (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0)))

;; Enemy field (10, 19)
    
    (setf (world-pos pipeline) (sb-cga:vec 200.0 0.0 19.0))
    (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
    (update-transforms pipeline)
    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))

    (when *ping*
	 (setf (scale pipeline) (sb-cga:vec (radius *ping*) (radius *ping*) 1.0))
	 (setf (world-pos pipeline) (pos *ping*))
	 (update-transforms pipeline)	 
	 (circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.3 0.0)))
    
     (loop for missile in *missiles-fired* do
	 (setf (scale pipeline) (sb-cga:vec (radius missile) (radius missile) 1.0))
	 (setf (world-pos pipeline) (pos missile))
	 (update-transforms pipeline)
	 (circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.15 0.0 0.0)))))
