(in-package #:battleship)

(defun file->string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun make-vector4 (x y z w)
  "This takes 4 floats and give back a vector4, this is just an
array but it specifies the array type and populates it.
For speed reasons it will not accept integers so make sure
you hand it floats."
  (declare (single-float x y z w))
  (let (( vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) x
	  (aref vec 1) y
	  (aref vec 2) z
	  (aref vec 3) w)
    vec))

(defun mcol*vec4 (mat-a vec)
  (make-vector4
   (+ (* (aref vec 0) (sb-cga:mref mat-a 0 0)) (* (aref vec 1) (sb-cga:mref mat-a 0 1))
      (* (aref vec 2) (sb-cga:mref mat-a 0 2)) (* (aref vec 3) (sb-cga:mref mat-a 0 3)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 1 0)) (* (aref vec 1) (sb-cga:mref mat-a 1 1))
      (* (aref vec 2) (sb-cga:mref mat-a 1 2)) (* (aref vec 3) (sb-cga:mref mat-a 1 3)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 2 0)) (* (aref vec 1) (sb-cga:mref mat-a 2 1))
      (* (aref vec 2) (sb-cga:mref mat-a 2 2)) (* (aref vec 3) (sb-cga:mref mat-a 2 3)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 3 0)) (* (aref vec 1) (sb-cga:mref mat-a 3 1))
      (* (aref vec 2) (sb-cga:mref mat-a 3 2)) (* (aref vec 3) (sb-cga:mref mat-a 3 3)))))

(defun mrow*vec4 (vec mat-a)
  (make-vector4
   (+ (* (aref vec 0) (sb-cga:mref mat-a 0 0)) (* (aref vec 1) (sb-cga:mref mat-a 1 0))
      (* (aref vec 2) (sb-cga:mref mat-a 2 0)) (* (aref vec 3) (sb-cga:mref mat-a 3 0)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 0 1)) (* (aref vec 1) (sb-cga:mref mat-a 1 1))
      (* (aref vec 2) (sb-cga:mref mat-a 2 1)) (* (aref vec 3) (sb-cga:mref mat-a 3 1)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 0 2)) (* (aref vec 1) (sb-cga:mref mat-a 1 2))
      (* (aref vec 2) (sb-cga:mref mat-a 2 2)) (* (aref vec 3) (sb-cga:mref mat-a 3 2)))

   (+ (* (aref vec 0) (sb-cga:mref mat-a 0 3)) (* (aref vec 1) (sb-cga:mref mat-a 1 3))
      (* (aref vec 2) (sb-cga:mref mat-a 2 3)) (* (aref vec 3) (sb-cga:mref mat-a 3 3)))))

;; from clinch
(defun ensure-float (x)
  (coerce x 'single-float))

;; from glm
(defun look-at (eye center up)
  (let* ((f (sb-cga:normalize (sb-cga:vec- center eye)))
	(s (sb-cga:normalize (sb-cga:cross-product f (sb-cga:normalize up))))
	(u (sb-cga:cross-product s f)))
    (sb-cga:matrix (aref s 0)                     (aref s 1)                     (aref s 2)             (- (sb-cga:dot-product s eye))
		   (aref u 0)                     (aref u 1)                     (aref u 2)             (- (sb-cga:dot-product u eye))
		   (- (aref f 0))                 (- (aref f 1))                 (- (aref f 2))         (sb-cga:dot-product f eye)             
		   0.0                            0.0                            0.0                    1.0)))

(defun ortho (left right bottom top z-near z-far)
  (sb-cga:matrix (/ 2.0 (- right left)) 0.0                     0.0                          (- (/ (+ right left) (- right left)))
		 0.0                    (/ 2.0 (- top bottom))  0.0                          (- (/ (+ top bottom) (- top bottom)))
		 0.0                    0.0                     (- (/ 2.0 (- z-far z-near))) (- (/ (+ z-far z-near) (- z-far z-near)))
		 0.0                    0.0                     0.0                          1.0))

(defun unproject (win model proj viewport)
  (let* ((inverse (sb-cga:inverse-matrix (sb-cga:matrix* proj model)))
	 (tmp (sb-cga:vec- (sb-cga:vec* (sb-cga:vec (/ (- (aref win 0) (car viewport)) (third viewport))
						    (/ (- (aref win 1) (second viewport)) (fourth viewport))
						    (aref win 2))
					2.0)
			   (sb-cga:vec 1.0 1.0 1.0)))
	 (vec (make-vector4 (aref tmp 0) (aref tmp 1) (aref tmp 2) 1.0))
	 (obj (mcol*vec4 inverse vec)))
    (sb-cga:vec (/ (aref obj 0) (aref obj 3)) (/ (aref obj 1) (aref obj 3)) (/ (aref obj 2) (aref obj 3)))))
