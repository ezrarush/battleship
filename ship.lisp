(in-package #:battleship)

(defvar *ships-placed* '())

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
    :accessor height)
   (sunk-p
    :initform nil
    :accessor sunk-p)))

(defgeneric click-location (ship v1 v2))

(defmethod initialize-instance :after ((self ship) &key)
  (with-slots (orientation width height) self
    (if (eql orientation :horizontal)
	(rotatef width height))))

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

(defun place-ship (v1 v2 location orientation)
  (let* (;; place ship one unit closer in depth so that it is displayed above player field
	 (new-ship (make-instance 'ship :pos (sb-cga:vec+ location (sb-cga:vec 0.0 0.0 -1.0)) :orientation orientation))
	 ;; place new-ship only if it is entirely inside player field
	 (flag (or (ray-triangle-collision 
		    v1 
		    (sb-cga:vec- v1 v2) 
		    (sb-cga:vec (- -4.0 (/ (width new-ship) 2.0))  (- 196.0 (/ (height new-ship) 2.0)) 0.0)
		    (sb-cga:vec (+ -396.0 (/ (width new-ship) 2.0))  (- 196.0 (/ (height new-ship) 2.0)) 0.0)
		    (sb-cga:vec (+ -396.0 (/ (width new-ship) 2.0)) (+ -196.0 (/ (height new-ship) 2.0)) 0.0))
		   (ray-triangle-collision 
		    v1 
		    (sb-cga:vec- v1 v2)
		    (sb-cga:vec (- -4.0 (/ (width new-ship) 2.0))  (- 196.0 (/ (height new-ship) 2.0)) 0.0)
		    (sb-cga:vec (+ -396.0 (/ (width new-ship) 2.0)) (+ -196.0 (/ (height new-ship) 2.0)) 0.0)
		    (sb-cga:vec (- -4.0 (/ (width new-ship) 2.0)) (+ -196.0 (/ (height new-ship) 2.0)) 0.0)))))
    
    (loop for placed-ship in *ships-placed* do
       ;; clicking on an existing ship removes it
	 (when (ray-intersect placed-ship v1 v2) 
	   (setf flag nil)
	   (remove-ship placed-ship))
       ;; stop placement if the new ship is placed over an existing ship 
	 (when (collision-p new-ship placed-ship) (setf flag nil)))
    (when (and flag (< (length *ships-placed*) (game-state-ships *game-state*))) 
      (push new-ship *ships-placed*)
      (when (eql (length *ships-placed*) (game-state-ships *game-state*))
	(loop for ship in *ships-placed* do
	   (send-message *server-connection* (make-place-ship-message (aref (pos ship) 0) (aref (pos ship) 1) (orientation ship))))
	(setf (game-state-current-screen *game-state*) :game-play)))))

(defun remove-ship (ship)
  (setf *ships-placed* (remove ship *ships-placed*)))

;; check if ships collide
(defun collision-p (first-ship second-ship)
  (let ((x-clearance (+ (/ (width first-ship) 2.0) (/ (width second-ship) 2.0)))
	(y-clearance (+ (/ (height first-ship) 2.0) (/ (height second-ship) 2.0)))
	(x-distance (abs (- (aref (pos first-ship) 0) (aref (pos second-ship) 0))))
	(y-distance (abs (- (aref (pos first-ship) 1) (aref (pos second-ship) 1)))))
    (when (and (< x-distance x-clearance)
	       (< y-distance y-clearance))
      t)))
