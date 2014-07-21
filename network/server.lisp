(in-package #:battleship)

(defvar *server* t)

(defvar *server-socket* nil)

(defun server-p () *server*)

(defun start-server (server-ip port)
  (assert (not *server-socket*))
  (setf *server-socket*
	(usocket:socket-listen server-ip
			       port
			       :reuseaddress t
			       :element-type '(unsigned-byte 8))))

(defun stop-server ()
  (assert *server-socket*)
  (usocket:socket-close *server-socket*)
  (setf *server-socket* nil
	*db* (make-hash-table)))

(defun accept-client ()
  (when (usocket:wait-for-input *server-socket*
				:timeout 0
				:ready-only t)
    (make-player (usocket:socket-accept *server-socket*))))

(defun handle-message-from-client (message)
  (userial:with-buffer message
    (ecase (userial:unserialize :client-opcode)
      (:login      (handle-login-message message))
      (:place-ship (handle-place-ship-message message))
      (:ping       (handle-ping-message message))
      (:fire       (handle-fire-message message)))))

(defun handle-login-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:string name
			       :boolean has-opponent)
			      (assert (plusp (length name)))
			      (format t "~a has joined the server~%" name)
			      (finish-output)
			      (cond
				(has-opponent (userial:unserialize-let* (:string opponent)
									(match-or-queue name opponent)))
				(t
				 (match-or-queue name))))))

(defun match-or-queue (name &optional opponent)
  (setf (name *current-player*) name)
  (setf (ships *current-player*) 5)
  (setf (energy *current-player*) 10.0)
  (setf (missiles *current-player*) 20)
  (if opponent
      (progn
	(setf (opponent-name *current-player*) opponent)
	(setf (state *current-player*) :waiting-for-opponent)
	(loop for player being the hash-values in *db* do
	     (unless (eql player *current-player*) 
	       ;; equalp is not case-sensitive
	       (when (equalp (name player) opponent)
		 (if (eql (state player) :waiting-for-anyone)
		     (progn
		       (setf (opponent-id *current-player*) (object-id player))
		       (setf (opponent-id player) (object-id *current-player*))
		       (setf (state *current-player*) :playing-match)
		       (setf (state player) :playing-match)
		       (send-message (socket-connection *current-player*) 
				     (make-welcome-message 40 5 10.0 20 (name player)))
		       (send-message (socket-connection player) 
				     (make-welcome-message 40 5 10.0 20 (name *current-player*)))
		       (return))
		     (progn
		       (when (and (eql (state player) :waiting-for-opponent) (equalp (opponent-name player) (name *current-player*)))
			 (setf (opponent-id *current-player*) (object-id player))
			 (setf (opponent-id player) (object-id *current-player*))
			 (setf (state *current-player*) :playing-match)
			 (setf (state player) :playing-match)
			 (send-message (socket-connection *current-player*) 
				       (make-welcome-message 40 5 10.0 20 (name player)))
			 (send-message (socket-connection player) 
				       (make-welcome-message 40 5 10.0 20 (name *current-player*)))
			 (return))))))))
      (progn
	(setf (state *current-player*) :waiting-for-anyone)
	(unless
	    (loop for player being the hash-values in *db* do
		 (progn (unless (eql player *current-player*)
			  (when (and (eql (state player) :waiting-for-opponent) (equalp (opponent-name player) (name *current-player*)))
			    (setf (opponent-name *current-player*) (name player))
			    (setf (opponent-name player) (name *current-player*))
			    (setf (opponent-id *current-player*) (object-id player))
			    (setf (opponent-id player) (object-id *current-player*))
			    (setf (state *current-player*) :playing-match)
			    (setf (state player) :playing-match)
			    (send-message (socket-connection *current-player*) 
					  (make-welcome-message 40 5 10.0 20 (name player)))
			    (send-message (socket-connection player) 
					  (make-welcome-message 40 5 10.0 20 (name *current-player*)))
			    (return t)))
			nil))
	  (loop for player being the hash-values in *db* do
	       (unless (eql player *current-player*)
		 (when (eql (state player) :waiting-for-anyone)
		   (setf (opponent-name *current-player*) (name player))
		   (setf (opponent-name player) (name *current-player*))
		   (setf (opponent-id *current-player*) (object-id player))
		   (setf (opponent-id player) (object-id *current-player*))
		   (setf (state *current-player*) :playing-match)
		   (setf (state player) :playing-match)
		   (send-message (socket-connection *current-player*) 
				 (make-welcome-message 40 5 10.0 20 (name player)))
		   (send-message (socket-connection player) 
				 (make-welcome-message 40 5 10.0 20 (name *current-player*)))
		   (return))))))))

(defun make-welcome-message (squares ships energy missiles opponent)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :welcome
			:uint8 squares
			:uint8 ships
			:float32 energy
			:uint16 missiles
			:string opponent)))

(defun handle-place-ship-message (message)
   (let (x y orientation)
     (userial:with-buffer message
       (userial:unserialize* :float32        x
			     :float32        y
			     :orientation orientation))
     ;; move x from negative placement field to positive play field
     (add-ship-to-map (+ x 400.0) y :is-vertical (eql orientation :vertical))
     ))

(defun add-ship-to-map (x y &key is-vertical)
  (push (list x y is-vertical) (placed-ships *current-player*))

  (let ((opponent (lookup-object-by-id (opponent-id *current-player*))))
    (when (and (eql (length (placed-ships *current-player*)) 5) (eql (length (placed-ships opponent)) 5))
      (send-message (socket-connection *current-player*) (make-match-begin-message))
      (send-message (socket-connection opponent) (make-match-begin-message)))))

(defun make-match-begin-message ()
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize :server-opcode :match-begin)))

(defun handle-ping-message (message)
  (userial:with-buffer message
    (apply #'calculate-ping-response
	   (userial:unserialize-list* '(:float32 :float32 :float32)))))

(defun calculate-ping-response (radius ping-x ping-y)
  (format t "ping message received from ~a~%" (name *current-player*))
  (finish-output)
  (let ((opponent (lookup-object-by-id (opponent-id *current-player*)))
	(hits '()))
    (loop for ship in (placed-ships opponent) do
	 (let ((d (distance ping-x ping-y (first ship) (second ship))))
	   (when (< d radius)
	     (push d hits))))
    (setf (energy *current-player*) (- (energy *current-player*) 1))
    (send-message (socket-connection *current-player*) (make-ack-message (energy *current-player*) hits))))

(defun make-ack-message (remaining-ping-energy hits)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :ack
			:float32 remaining-ping-energy
			:uint16 (length hits))
    (mapcar #'(lambda (d) (userial:serialize :float32 d)) hits)
    (userial:get-buffer)))

(defun handle-fire-message (message)
  (userial:with-buffer message
    (apply #'calculate-fire-response
	   (userial:unserialize-list* '(:float32 :float32)))))

(defun calculate-fire-response (missile-x missile-y)
  (format t "fire message received from ~a~%" (name *current-player*))
  (finish-output)
  (let ((opponent (lookup-object-by-id (opponent-id *current-player*)))
	(hit nil))
    (loop for ship in (placed-ships opponent) do
	 (let ((ship-x (first ship))
	       (ship-y (second ship))
	       (half-width (if (third ship) 6.0 24.0))
	       (half-height (if (third ship) 24.0 6.0))
	       (missile-radius 2.0))
	   (when (and (<= (- missile-x missile-radius) (+ ship-x half-width))
		      (>= (+ missile-x missile-radius) (- ship-x half-width))
		      (<= (- missile-y missile-radius) (+ ship-y half-height))
		      (>= (+ missile-y missile-radius) (- ship-y half-height)))
	     (setf hit t))))
    (send-message (socket-connection opponent) (make-sunk-message missile-x missile-y))
    (send-message (socket-connection *current-player*) (make-shot-results-message hit))))

(defun make-shot-results-message (hit)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :shot-results
			:shot-result    (if hit :hit :miss))))

(defun make-sunk-message (x y)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :sunk
			:float32           x
			:float32           y)))

