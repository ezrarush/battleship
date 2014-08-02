;;;; battleship.lisp

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(in-package #:battleship)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defvar *last-time*)
(defvar *current-time*)
(defvar *delta-time*)

(defvar *graphics-engine*)

;; indicates that mouse button 3 is down to determine ping radius
(defvar *right-click-toggle* nil)

;; a hack to determine which player sent the message received by server
(defvar *current-player* nil)

(defun main (&key (server-p t) (server-ip usocket:*wildcard-host*) (port 2448) (name "Unnamed") opponent)
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (setf *server* server-p)
    (if (server-p)
	
	(progn
	  (start-server server-ip port)
	  (setf *last-time* (sdl2:get-ticks))       
	  (unwind-protect
	       (sdl2:with-event-loop (:method :poll)	     
		 
		 (:idle 
		  ()
		  (setf *current-time* (sdl2:get-ticks))
		  (setf *delta-time* (ensure-float (- *current-time* *last-time*)))
		  (when (>= *delta-time* 10.0)
		    (incf *last-time* 10))

		  (accept-client)

		  (loop for player being the hash-values in *db* do
		       (setf *current-player* player)
		       (read-message (socket-connection player)))
		  (setf *current-player* nil))
		 
		 (:quit () t))
	    (stop-server)))

	(progn
	  (connect-to-server server-ip port)
	  (when (connected-p)
	    (send-message *server-connection* (make-login-message name :opponent opponent))
	    (sdl2:with-window (win :title (if (server-p) "Battleship Server" (concatenate 'string "Battleship Client: " name) ) :w *window-width* :h *window-height* :flags '(:shown :opengl))
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
			      (mod-value (sdl2:mod-value keysym))))
			(ecase (game-state-current-screen *game-state*)
			  (:waiting-for-opponent)
			  (:place-ships)
			  (:game-play)
			  (:end-score))
			)
		       
		       (:mousebuttondown 
			(:x x :y y :button button)
			(print x)
			(print y)
			(finish-output)
			(ecase (game-state-current-screen *game-state*)
			  (:waiting-for-opponent)
			  (:place-ships
			   (multiple-value-bind (v1 v2)  (get-3d-ray-under-mouse (ensure-float x) (ensure-float (- *window-height* y)))
			     (let ((location (player-field-ray-intersect v1 v2)))
			       (if location 
				 (place-ship v1 v2 location (if (eql button 1) :vertical :horizontal) :remove t)
				 ;; if not player field than maybe ready button
				 (when (ready-button-ray-intersect v1 v2)
				   (ready-for-match))))))
			  (:game-play
			   (multiple-value-bind (v1 v2)  (get-3d-ray-under-mouse (ensure-float x) (ensure-float (- *window-height* y)))
			     (let ((location (enemy-field-ray-intersect v1 v2)))
			       (when location 
				 ;; right click for ping any other click fires a missile
				 (if (eql button 3)
				     (progn
				       (make-ping location)
				       (setf *right-click-toggle* t))
				     (fire-missile v1 v2 location))))))
			  (:end-score)))

		       (:mousebuttonup
			()
			(ecase (game-state-current-screen *game-state*)
			  (:waiting-for-opponent)
			  (:place-ships)
			  (:game-play
			   (when *right-click-toggle*
			       (send-message *server-connection* (make-ping-message (aref (pos *ping*) 0) (aref (pos *ping*) 1) (radius *ping*)))
			       (setf *right-click-toggle* nil)))
			  (:end-score)))
		       
		       (:mousemotion 
			(:x x :y y :state state)
			(ecase (game-state-current-screen *game-state*)
			  (:waiting-for-opponent)
			  (:place-ships)
			  (:game-play
			   ;; ping radius is being determined until right click release
			   (when (and *ping* *right-click-toggle*)
			     (multiple-value-bind (v1 v2)(get-3d-ray-under-mouse (ensure-float x) (ensure-float (- *window-height* y)))
			       (let ((location (enemy-field-ray-intersect v1 v2))
				     (pos (pos *ping*)))
				 (when location
				   ;; new radius is distance of mouse from pos
				   (setf (radius *ping*) (distance (aref location 0) (aref location 1) (aref pos 0) (aref pos 1))))))))
			  (:end-score)))
		       
		       (:idle 
			()
			(setf *current-time* (sdl2:get-ticks))
			(setf *delta-time* (ensure-float (- *current-time* *last-time*)))
			(when (>= *delta-time* 10.0)
			  (incf *last-time* 10))
			
			(read-message *server-connection*)
			
			(render-scene *graphics-engine*)
			(sdl2:gl-swap-window win)		     
			)
		       (:quit () t))
		  
		  (disconnect-from-server)))))))))

(defun enemy-field-ray-intersect (v1 v2)
  (let ((distance (or (ray-triangle-collision v1 
					      (sb-cga:vec- v1 v2) 
					      (sb-cga:vec 4.0                            (- (/ (- *window-height* 200.0) 2.0) 4.0) 19.0)
					      (sb-cga:vec 4.0                            (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 19.0)
					      (sb-cga:vec (- (/ *window-width* 2.0) 4.0) (- (/ (- *window-height* 200.0) 2.0) 4.0) 19.0))
		      (ray-triangle-collision v1 
					      (sb-cga:vec- v1 v2)
					      (sb-cga:vec 4.0                            (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 19.0)
					      (sb-cga:vec (- (/ *window-width* 2.0) 4.0) (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 19.0)
					      (sb-cga:vec (- (/ *window-width* 2.0) 4.0) (- (/ (- *window-height* 200.0) 2.0) 4.0) 19.0)))))
    (when distance
      ;; calculate click location on field
      (sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance)))))

(defun player-field-ray-intersect (v1 v2)
  (let ((distance (or (ray-triangle-collision v1 
					      (sb-cga:vec- v1 v2) 
					      (sb-cga:vec -4.0                                (- (/ (- *window-height* 200.0) 2.0) 4.0) 9.0)
					      (sb-cga:vec  (- (- (/ *window-width* 2.0) 4.0)) (- (/ (- *window-height* 200.0) 2.0) 4.0) 9.0)
					      (sb-cga:vec  (- (- (/ *window-width* 2.0) 4.0)) (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 9.0))
		      (ray-triangle-collision v1 
					      (sb-cga:vec- v1 v2)
					      (sb-cga:vec -4.0                                (- (/ (- *window-height* 200.0) 2.0) 4.0) 9.0)
					      (sb-cga:vec  (- (- (/ *window-width* 2.0) 4.0)) (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 9.0)
					      (sb-cga:vec -4.0                                (- (- (/ (- *window-height* 200.0) 2.0) 4.0)) 9.0)))))
    (when distance
      ;; calculate click location on field
      (sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance)))))

(defun ready-button-ray-intersect (v1 v2)
  (or (ray-triangle-collision v1 
			      (sb-cga:vec- v1 v2) 
			      (sb-cga:vec 150.0  25.0 -1.0)
			      (sb-cga:vec 150.0 -25.0 -1.0)
			      (sb-cga:vec 250.0  25.0 -1.0))
      (ray-triangle-collision v1 
			      (sb-cga:vec- v1 v2)
			      (sb-cga:vec 150.0 -25.0 -1.0)
			      (sb-cga:vec 250.0 -25.0 -1.0)
			      (sb-cga:vec 250.0  25.0 -1.0))))
