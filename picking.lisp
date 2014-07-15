(in-package #:battleship)

;; from megabytesoftworks tut
(defun get-3d-ray-under-mouse (x y)
  (let ((viewport (list 0.0 0.0 *window-width* *window-height*)))
    (values
     (unproject (sb-cga:vec x y 0.0)
		(look-at (camera-pos (camera *graphics-engine*))
			 (camera-target (camera *graphics-engine*))
			 (camera-up (camera *graphics-engine*)))
		(ortho (/ (projection-info-width (proj-info *graphics-engine*)) 2.0)
		       (- (/ (projection-info-width (proj-info *graphics-engine*)) 2.0))
		       (- (/ (projection-info-height (proj-info *graphics-engine*)) 2.0))
		       (/ (projection-info-height (proj-info *graphics-engine*)) 2.0)
		       (projection-info-z-near (proj-info *graphics-engine*))
		       (projection-info-z-far (proj-info *graphics-engine*)))
		viewport)
     
     (unproject (sb-cga:vec x y 1.0)
		(look-at (camera-pos (camera *graphics-engine*))
			 (camera-target (camera *graphics-engine*))
			 (camera-up (camera *graphics-engine*)))
		(ortho (/ (projection-info-width (proj-info *graphics-engine*)) 2.0)
		       (- (/ (projection-info-width (proj-info *graphics-engine*)) 2.0))
		       (- (/ (projection-info-height (proj-info *graphics-engine*)) 2.0))
		       (/ (projection-info-height (proj-info *graphics-engine*)) 2.0)
		       (projection-info-z-near (proj-info *graphics-engine*))
		       (projection-info-z-far (proj-info *graphics-engine*)))
		viewport))))

;; from megabytesoftworks tut
(defun ray-sphere-collision (sphere-center sphere-radius va vb)
  
  (let* ((dir-to-sphere (sb-cga:vec- sphere-center va))
	 (line-dir (sb-cga:normalize (sb-cga:vec- vb va)))
	 (line-length (sb-cga:vec-length (sb-cga:vec- vb va)))
	 (p (sb-cga:dot-product dir-to-sphere line-dir))
	 (closest-point (if (<= p 0.0)
			    va
			    (if (>= p line-length)
				vb
				(sb-cga:vec+ va (sb-cga:vec* line-dir p))))))
    (if (<= (sb-cga:vec-length (sb-cga:vec- closest-point sphere-center)) sphere-radius)
	t
	nil)))

;; real-time rendering 3
(defun ray-triangle-collision (o d p0 p1 p2)
  (let* ((e1 (sb-cga:vec- p1 p0))
	 (e2 (sb-cga:vec- p2 p0))
	 (q (sb-cga:cross-product d e2))
	 (a (sb-cga:dot-product e1 q))
	 (f (/ 1 a))
	 (s (sb-cga:vec- o p0))
	 (u (* f (sb-cga:dot-product s q))))
    (if (< u 0.0)
	nil
	(let* ((r (sb-cga:cross-product s e1))
	       (v (* f (sb-cga:dot-product d r))))
	  (if (or (< v 0.0) (> (+ u v) 1.0))
	      nil
	      ;; distance to hit
	      (* f (sb-cga:dot-product e2 r)))))))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)) ))
