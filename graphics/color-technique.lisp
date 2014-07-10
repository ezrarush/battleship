(in-package #:battleship)

(defclass color-technique (technique)
  ((vs-path
    :initarg :vs-path)
   (fs-path
    :initarg :fs-path)
   (model-view-transform-location)
   (projection-transform-location)
   (color-location)))

(defgeneric set-model-view-transform (color-technique model-view-transform))
(defgeneric set-projection-transform (color-technique projection-transform))
(defgeneric set-color (color-technique color))

(defmethod initialize-instance :after ((tec color-technique) &key)
  (with-slots (vs-path
	       fs-path
	       model-view-transform-location
	       projection-transform-location
	       color-location) tec
    (technique-init tec)
    (add-shader tec :vertex-shader vs-path)
    (add-shader tec :fragment-shader fs-path)
    (technique-finalize tec)
    (setf model-view-transform-location (get-uniform-location tec "modelViewMatrix"))
    (setf projection-transform-location (get-uniform-location tec "projectionMatrix"))
    (setf color-location (get-uniform-location tec "gColor"))))

(defmethod set-model-view-transform ((tec color-technique) model-view-transform)
  (with-slots (model-view-transform-location) tec
    (gl:uniform-matrix model-view-transform-location 4 (vector model-view-transform) nil)))

(defmethod set-projection-transform ((tec color-technique) projection-transform)
  (with-slots (projection-transform-location) tec
    (gl:uniform-matrix projection-transform-location 4 (vector projection-transform) nil)))

(defmethod set-color ((tec color-technique) color)
  (with-slots (color-location) tec
    (gl:uniformf color-location (aref color 0) (aref color 1) (aref color 2))))
