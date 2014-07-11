(in-package #:battleship)

(defvar *placed-ships* '())

(defclass ship ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   ;; orientation may be either orientation :horizontal or :vertical 
   (orientation
    :initarg :orientation
    :initform :vertical
    :accessor orientation)
   (width
    :initarg :width
    :initform 12.0
    :accessor width)
   (height
    :initarg :height
    :initform 48.0
    :accessor height)))

(defgeneric click-location (ship v1 v2))

(defmethod (setf orientation) :after (symb (self ship))
  (with-slots (width height) self
    (rotatef width height)))

(defmethod ray-intersect ((self ship) v1 v2)
  (with-slots (pos width height) self
    (let* ((half-width (/ width 2.0))
	   (half-height (/ height 2.0))
	   (distance (or (ray-triangle-collision 
			 v1 
			 (sb-cga:vec- v1 v2) 
			 (sb-cga:vec (- (aref pos 0) half-width) (- (aref pos 1) half-height) 0.0)
			 (sb-cga:vec (+ (aref pos 0) half-width) (- (aref pos 1) half-height) 0.0)
			 (sb-cga:vec (+ (aref pos 0) half-width) (+ (aref pos 1) half-height) 0.0))
			(ray-triangle-collision 
			 v1 
			 (sb-cga:vec- v1 v2)
			 (sb-cga:vec (+ (aref pos 0) half-width) (+ (aref pos 1) half-height) 0.0)
			 (sb-cga:vec (- (aref pos 0) half-width) (+ (aref pos 1) half-height) 0.0)
			 (sb-cga:vec (- (aref pos 0) half-width) (- (aref pos 1) half-height) 0.0)))))
      (when distance
	;; calculate click location on ship
	(sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance))))))

;; bounding box twice the size of size so that news ships are not placed over this ship
(defmethod ray-intersect-around ((self ship) v1 v2)
  (with-slots (pos width height) self
    (let ((distance (or (ray-triangle-collision 
			 v1 
			 (sb-cga:vec- v1 v2) 
			 (sb-cga:vec (- (aref pos 0) width) (- (aref pos 1) height) 0.0)
			 (sb-cga:vec (+ (aref pos 0) width) (- (aref pos 1) height) 0.0)
			 (sb-cga:vec (+ (aref pos 0) width) (+ (aref pos 1) height) 0.0))
			(ray-triangle-collision 
			 v1 
			 (sb-cga:vec- v1 v2)
			 (sb-cga:vec (+ (aref pos 0) width) (+ (aref pos 1) height) 0.0)
			 (sb-cga:vec (- (aref pos 0) width) (+ (aref pos 1) height) 0.0)
			 (sb-cga:vec (- (aref pos 0) width) (- (aref pos 1) height) 0.0)))))
      (when distance
	;; calculate click location on or around ship
	(sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance))))))

(defun place-ship (location)
  (push (make-instance 'ship :pos location) *placed-ships*))

(defun remove-ship (ship)
  (setf *placed-ships* (remove ship *placed-ships*)))

