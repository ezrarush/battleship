 (in-package #:battleship)

(defstruct projection-info
  width
  height
  z-near
  z-far)

(defclass pipeline ()
  ((scale 
    :initarg :scale
    :initform (sb-cga:vec 1.0 1.0 1.0)
    :accessor scale)
   (world-pos
    :initarg :world-pos
    :initform (sb-cga:vec 0.0 0.0 0.0)
    :accessor world-pos)
   (rotate-info
    :initarg :rotate-info
    :initform (sb-cga:vec 0.0 0.0 0.0)
    :accessor rotate-info)
   (view-transform
    :reader view-transform)
   (projection-transform
    :reader projection-transform)
   (world-transform
    :reader world-transform)
   (model-view-transform
    :reader model-view-transform)
   (proj-info
    :initarg :proj-info
    :initform (make-projection-info
	       :height (ensure-float *window-height*)
	       :width (ensure-float *window-width*)
	       :z-near 500.0
	       :z-far -500.0)
    :accessor proj-info)
   (camera
    :initarg :camera
    :initform (make-instance 'camera 
			     :pos (sb-cga:vec 0.0 0.0 100.0) 
			     :target (sb-cga:vec 0.0 0.0 0.0))
    :accessor camera)))

(defgeneric update-transforms (pipeline))

(defmethod initialize-instance :after ((self pipeline) &key)
  (update-transforms self))

(defmethod update-transforms ((self pipeline))
  (with-slots (scale world-pos rotate-info proj-info camera world-transform view-transform model-view-transform projection-transform) self
    (let ((scale-transform (sb-cga:scale scale))
	  (rotate-transform (sb-cga:rotate rotate-info))
	  (translation-transform (sb-cga:translate world-pos)))
      (setf world-transform (sb-cga:matrix* translation-transform rotate-transform scale-transform)))
    
    (setf view-transform (look-at (camera-pos camera)
				  (camera-target camera)
				  (camera-up camera)))
    
    (setf model-view-transform (sb-cga:matrix* view-transform world-transform))

    (setf projection-transform (ortho (- (/ (projection-info-width proj-info) 2.0)) 
    				      (/ (projection-info-width proj-info) 2.0) 
    				      (- (/ (projection-info-height proj-info) 2.0)) 
    				      (/ (projection-info-height proj-info) 2.0) 
    				      (projection-info-z-near proj-info)
    				      (projection-info-z-far proj-info)))
    ))

