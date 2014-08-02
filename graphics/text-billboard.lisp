(in-package #:battleship)

(defclass text-billboard ()
  ((vertex-buffer)
   (texture)
   (technique)
   (text
    :initform "Your text here"
    :initarg :text)))

(defgeneric text-billboard-render (text-billboard projection-transform model-view-transform))

(defmethod initialize-instance :after ((self text-billboard) &key)
  (with-slots (vertex-buffer texture technique text) self
    (let ((billboard-pos-coords (make-array 6 :initial-contents (list (sb-cga:vec -1.0 -1.0 0.0)
								      (sb-cga:vec  1.0 -1.0 0.0)
								      (sb-cga:vec  1.0  1.0 0.0)
								      (sb-cga:vec  1.0  1.0 0.0)
								      (sb-cga:vec -1.0  1.0 0.0)
								      (sb-cga:vec -1.0 -1.0 0.0))))
	  (billboard-tex-coords (make-array 6 :initial-contents (list (sb-cga:vec 0.0 1.0 0.0)
								      (sb-cga:vec 1.0 1.0 0.0)
								      (sb-cga:vec 1.0 0.0 0.0)
								      (sb-cga:vec 1.0 0.0 0.0)
								      (sb-cga:vec 0.0 0.0 0.0)
								      (sb-cga:vec 0.0 1.0 0.0))))
	  (billboard-normals (make-array 6 :initial-contents (list (sb-cga:vec 0.0 1.0 0.0)
								   (sb-cga:vec 0.0 1.0 0.0)
								   (sb-cga:vec 0.0 1.0 0.0)
								   (sb-cga:vec 0.0 1.0 0.0)
								   (sb-cga:vec 0.0 1.0 0.0)
								   (sb-cga:vec 0.0 1.0 0.0))))
	  (arr (gl:alloc-gl-array :float 144))
	  (i 0))
      (dotimes (x 6)
        (dotimes (y 3)
          (setf (gl:glaref arr i) (aref (aref billboard-pos-coords x) y))
          (incf i))
        (dotimes (y 2)
          (setf (gl:glaref arr i) (aref (aref billboard-tex-coords x) y))
          (incf i))
        (dotimes (y 3)
          (setf (gl:glaref arr i) (aref (aref billboard-normals x) y))
          (incf i)))
      (setf vertex-buffer (car (gl:gen-buffers 1)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))
    
    (setf texture (make-instance 'texture
				 :width 800
				 :height 800
				 :pixel-count (* 800 800)))
    
    ;; make image data with text   
    (gl:bind-buffer :pixel-unpack-buffer (buffer-id texture))
	(gl:bind-texture :texture-2d (texture-id texture))
	(let ((bits-var (gl:map-buffer :pixel-unpack-buffer :write-only)))
	  (unwind-protect
	       (let* ((width-var (width texture))
		      (height-var (height texture))
		      (cairo::*surface* (cairo:create-image-surface-for-data bits-var :argb32 width-var height-var (* width-var (stride texture)))))
		 (unwind-protect
		      (let ((cairo::*context* (cairo:create-context cairo::*surface*)))
		      	(unwind-protect
			     (progn
			       (cairo:set-source-rgba 0 1 0 1)
			       (cairo:paint)
			       (cairo:move-to 5 400)
			       (cairo:set-source-rgba 0 0 0 1)
			       (cairo:set-font-size 85)
			       (cairo:show-text text))
		      	  (cairo:destroy cairo::*context*)))
		   (cairo:destroy cairo::*surface*)))
	    (gl:unmap-buffer :pixel-unpack-buffer)
	    (gl:bind-texture :texture-2d (texture-id texture)) ;; this is superflorus
	    (gl:tex-image-2d :texture-2d 0 :rgba (width texture) (height texture) 0 :bgra :unsigned-byte (cffi:null-pointer))
	    (gl:bind-texture :texture-2d 0)
	    (gl:bind-buffer :pixel-unpack-buffer 0)))
    
    (setf technique (make-instance 'color-technique :vs-path "~/quicklisp/local-projects/battleship/shaders/text-billboard.vertexshader" :fs-path "~/quicklisp/local-projects/battleship/shaders/text-billboard.fragmentshader"))))

(defmethod text-billboard-render ((self text-billboard) projection-transform model-view-transform)
  (with-slots (vertex-buffer texture technique) self

    (technique-enable technique)

    
    (set-projection-transform technique projection-transform)
    (set-model-view-transform technique model-view-transform)

    (gl:bind-buffer :array-buffer vertex-buffer)
    
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (texture-id texture))
    
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)
    
    (gl:vertex-attrib-pointer 0 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 2 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 3)))
    (gl:vertex-attrib-pointer 2 3 :float nil (* (cffi:foreign-type-size :float) 8) (cffi:make-pointer (* (cffi:foreign-type-size :float) 5)))
    
    (gl:draw-arrays :triangles 0 6)
    
    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 2)))
