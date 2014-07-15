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
    ;; Player field depth range (0, 9)
    ;; Enemy field (10, 19)
    
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
    
    (ecase (game-state-current-screen *game-state*)
      (:waiting-for-opponent 
       
       ;; waiting message will go on this quad
       (setf (world-pos pipeline) (sb-cga:vec 0.0 0.0 -1.0))
       (setf (scale pipeline) (sb-cga:vec 200.0 50.0 1.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.5 0.0)))
      
      (:place-ships
       
       ;; GUI
       ;; center bar
       (setf (scale pipeline) (sb-cga:vec 4.0 200.0 1.0))
       (setf (world-pos pipeline) (sb-cga:vec 0.0 0.0 0.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.0 0.0))
       
       ;; player field
       (setf (world-pos pipeline) (sb-cga:vec -200.0 0.0 9.0))
       (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))

       (loop 
	  for ship in *ships-placed* 
	  for i from -386.0 by 24.0 do
	    
	  ;; ships on field
	    (setf (scale pipeline) (sb-cga:vec (/ (width ship) 2.0) (/ (height ship) 2.0) 1.0))
	    (setf (world-pos pipeline) (pos ship))
	    (update-transforms pipeline)
	    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0))
	    
	  ;; GUI scoreboard
	    (setf (scale pipeline) (sb-cga:vec 10.0 5.0 1.0))
	    (setf (world-pos pipeline) (sb-cga:vec (ensure-float i) 205.0 -2.0))
	    (update-transforms pipeline)
	    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0))
	    
	    )

       ;; enemy filed
       (setf (world-pos pipeline) (sb-cga:vec 200.0 0.0 19.0))
       (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0)))
      
      (:game-play

       ;; GUI
       ;; center bar
       (setf (scale pipeline) (sb-cga:vec 4.0 200.0 1.0))
       (setf (world-pos pipeline) (sb-cga:vec 0.0 0.0 0.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.0 0.0))
       
       ;; player field
       (setf (world-pos pipeline) (sb-cga:vec -200.0 0.0 9.0))
       (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))

       (loop 
	  for ship in *ships-placed*
	  for i from -386.0 by 24.0 do
	  
	    (setf (scale pipeline) (sb-cga:vec (/ (width ship) 2.0) (/ (height ship) 2.0) 1.0))
	    (setf (world-pos pipeline) (pos ship))
	    (update-transforms pipeline)
	    (if (sunk-p ship)
		(quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.6 0.0 0.0))
		(quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0)))
	  
	  ;; GUI scoreboard
	    (setf (scale pipeline) (sb-cga:vec 10.0 5.0 1.0))
	    (setf (world-pos pipeline) (sb-cga:vec (ensure-float i) 205.0 -2.0))
	    (update-transforms pipeline)
	    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.4 0.0)))
       
       ;; sunk scoreboard overlaps gui scoreboard
       (loop 
       	  for ship in *ships-placed* 
       	  for i from -386.0 by 24.0 when (sunk-p ship) do
	    
       	    (setf (scale pipeline) (sb-cga:vec 10.0 5.0 1.0))
       	    (setf (world-pos pipeline) (sb-cga:vec (ensure-float i) 205.0 -2.0))
       	    (update-transforms pipeline)
       	    (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.6 0.0 0.0)))

       ;; enemy filed
       (setf (world-pos pipeline) (sb-cga:vec 200.0 0.0 19.0))
       (setf (scale pipeline) (sb-cga:vec 196.0 196.0 1.0))
       (update-transforms pipeline)
       (quad-render quad (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.2 0.0))
       
       (when *ping*
	 (setf (scale pipeline) (sb-cga:vec (radius *ping*) (radius *ping*) 1.0))
	 (setf (world-pos pipeline) (pos *ping*))
	 (update-transforms pipeline)	 
	 (circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.0 0.3 0.0)))

       (setf (contour circle) t)
       (loop for hit in *ping-hits* do
	    (setf (scale pipeline) (sb-cga:vec (second hit) (second hit) 1.0))
	    (setf (world-pos pipeline) (first hit))
	    (update-transforms pipeline)	 
	    (circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.3 0.0 0.0)))
       (setf (contour circle) nil)
       
       (loop for missile in *missiles-fired* do
	    (setf (scale pipeline) (sb-cga:vec (radius missile) (radius missile) 1.0))
	    (setf (world-pos pipeline) (pos missile))
	    (update-transforms pipeline)
	    (if (hit-p missile)
		(circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 1.0 0.0 0.0))
		(circle-render circle (projection-transform pipeline) (model-view-transform pipeline) (sb-cga:vec 0.2 0.0 0.0)))))
      
      (:end-score))))

