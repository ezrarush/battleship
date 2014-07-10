(in-package #:battleship)

(defclass technique ()
  ((shader-prog
    :initform 0)
   (shader-obj-list
    :initform (make-array 0 :fill-pointer 0 :adjustable t))))

(defgeneric set-color-texture-unit (technique texture-unit))
(defgeneric set-camera-position (technique pos))
(defgeneric set-vp (technique vp))
(defgeneric set-wvp (technique wvp))
(defgeneric add-shader (technique shader-type file-path))
(defgeneric get-uniform-location (technique uniform-name))
(defgeneric get-attrib-location (technique attrib-name))
(defgeneric get-program-param (technique param))
(defgeneric technique-init (technique))
(defgeneric technique-enable (technique))
(defgeneric technique-finalize (technique))

(defmethod technique-init ((tec technique))
  (with-slots (shader-prog) tec
    (setf shader-prog (gl:create-program))))

(defmethod add-shader ((tec technique) shader-type file-path)
  (with-slots (shader-prog shader-obj-list) tec
    (let ((shader-obj (gl:create-shader shader-type)))
      (vector-push-extend shader-obj shader-obj-list)
      (gl:shader-source shader-obj (file->string file-path))
      (gl:compile-shader shader-obj)
      
;      (print (gl:get-shader-info-log shader-obj))
      
      (gl:attach-shader shader-prog shader-obj))))

(defmethod technique-finalize ((tec technique))
  (with-slots (shader-prog shader-obj-list) tec
    (gl:link-program shader-prog)
    (setf shader-obj-list nil)
    (gl:validate-program shader-prog)
    (when (> (gl:get-program shader-prog :info-log-length) 0)
      (print (gl:get-program-info-log shader-prog)))))

(defmethod technique-enable ((tec technique))
  (with-slots (shader-prog) tec
    (gl:use-program shader-prog)))

(defmethod get-uniform-location ((tec technique) uniform-name)
  (with-slots (shader-prog) tec
    (gl:get-uniform-location shader-prog uniform-name)))

(defmethod get-attrib-location ((tec technique) attrib-name)
  (with-slots (shader-prog) tec
    (gl:get-attrib-location shader-prog attrib-name)))

(defmethod get-program-param ((tec technique) param)
  (with-slots (shader-prog) tec))
