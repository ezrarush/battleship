(in-package #:battleship)

(defclass texture (buffer)
  ((texture-id
    :reader texture-id)
   (width
    :reader width
    :initarg :width)
   (height
    :reader height
    :initarg :height)
   (stride
    :reader stride
    :initform 4
    :initarg :stride)
   (pixel-count
    :initarg :pixel-count)))

(defmethod initialize-instance :after ((self texture) &key)
  (with-slots (buffer-id texture-id stride pixel-count) self
    (setf buffer-id (car (gl:gen-buffers 1)))
    
    (gl:bind-buffer :pixel-unpack-buffer buffer-id)
    
    (%gl:buffer-data :pixel-unpack-buffer 
		     (* (* stride
			   pixel-count)
			(cffi:foreign-type-size :unsigned-char))
		     (cffi:null-pointer)
		     :static-draw)
    
    (setf texture-id (car (gl:gen-textures 1)))
    
    (gl:bind-texture :texture-2d texture-id)
    
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)))
