;;;; battleship.lisp

(in-package #:battleship)

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(defparameter *window-width* 800)
(defparameter *window-height* 400)

(defvar *last-time*)
(defvar *current-time*)
(defvar *delta-time*)

(defvar *graphics-engine*)

(defun main (&optional (server-p t) (server-ip usocket:*wildcard-host*) (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (setf *server* server-p)
    (if (server-p)
	(start-server server-ip port)
	(connect-to-server server-ip port))
    
    (sdl2:with-window (win :title (if (server-p) "Battleship Server" "Battleship Client") :w *window-width* :h *window-height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(sdl2:gl-make-current win gl-context)
	
	(setf *graphics-engine* (make-instance 'graphics-engine))
	(graphics-init *graphics-engine*)
	(setf *last-time* (sdl2:get-ticks))
	
	(unwind-protect
	     (sdl2:with-event-loop (:method :poll)
	       (:keydown
		(:keysym keysym)
		(let ((scancode (sdl2:scancode-value keysym))
		      (sym (sdl2:sym-value keysym))
		      (mod-value (sdl2:mod-value keysym)))
		  
		  ))

	       (:mousebuttondown (:x x :y y)
				 (format t "mouse click screen x:~a y:~a~%" x y)
				 (multiple-value-bind (v1 v2)  (get-3d-ray-under-mouse (ensure-float x) (ensure-float (- *window-height* y)))
				   (let ((d (or (ray-triangle-collision v1 
									(sb-cga:vec- v1 v2) 
									(sb-cga:vec 2.0   -196.0 0.0)
									(sb-cga:vec 2.0    196.0 0.0)
									(sb-cga:vec 396.0  196.0 0.0))
						(ray-triangle-collision v1 
									(sb-cga:vec- v1 v2)
									(sb-cga:vec 2.0   -196.0 0.0)
									(sb-cga:vec 396.0 -196.0 0.0)
									(sb-cga:vec 396.0  196.0 0.0)))))
				     (when d
				       (let ((location (sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) d)))
					     (flag nil))
					 (format t "3d location x:~a y:~a~%" (aref location 0) (aref location 1))
					 (loop for missile in *missiles-fired* do
					      (let ((collision (ray-sphere-collision (pos missile) (radius missile) v1 v2)))
						(when collision (setf flag t))))
					 (unless flag (fire-missile location)))))))

	       (:idle ()
		      (setf *current-time* (sdl2:get-ticks))
		      (setf *delta-time* (ensure-float (- *current-time* *last-time*)))
		      
		      (when (>= *delta-time* 10.0)
			
			(incf *last-time* 10))

		      (network)
		      (when (connected-p)

			(render-scene *graphics-engine*)
			(sdl2:gl-swap-window win)))

	       (:quit () t))
	  (if (server-p)
	      (stop-server)
	      (disconnect-from-server)))))))
