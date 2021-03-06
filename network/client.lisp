(in-package #:battleship)

(defvar *server-connection* nil)

(defun connect-to-server (server-ip port)
  (setf *server-connection*
	(usocket:socket-connect server-ip
				port
				:element-type '(unsigned-byte 8))))


(defun disconnect-from-server ()
  (assert *server-connection*)
  (send-message *server-connection* (userial:with-buffer (userial:make-buffer)
				      (userial:serialize :client-opcode :logout)))
  (usocket:socket-close *server-connection*)
  (setf *server-connection* nil))

(userial:make-accessor-serializer (:game-state-from-welcome welcome-state (make-game-state))
  :uint8   game-state-board-size
  :uint8   game-state-ships
  :float32 game-state-energy
  :uint16  game-state-missiles
  :string  game-state-opponent)

(defun handle-message-from-server (message)
  (userial:with-buffer message
    (ecase (userial:unserialize :server-opcode)
      (:welcome      (handle-welcome-message message))
      (:match-begin  (handle-match-begin))
      (:ack          (handle-ack-message message))
      (:sunk         (handle-sunk-message message))
      (:shot-results (handle-shot-results-message message)))))

(defun handle-welcome-message (message)
  (format t "server started match with opponent: ")
  (finish-output)
  (userial:with-buffer message
    (userial:unserialize :game-state-from-welcome :welcome-state *game-state*)
    (setf (game-state-current-screen *game-state*) :place-ships)
    (format t "~a~%" (game-state-opponent *game-state*))
    (finish-output)))

(defun make-login-message (name &key opponent)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string        name
			:boolean       (if opponent t nil))
    (when opponent
      (userial:serialize :string opponent))
    (userial:get-buffer)))

(defun make-place-ship-message (x y orientation)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :place-ship
			:float32 x
			:float32 y
			:orientation orientation)))

(defun ready-for-match ()
  (loop repeat (- (game-state-ships *game-state*) (length *ships-placed*)) do
       (place-random-ship))
  (loop for ship in *ships-placed* do
	 (send-message *server-connection* (make-place-ship-message (aref (pos ship) 0) (aref (pos ship) 1) (orientation ship))))
  (setf (game-state-current-screen *game-state*) :waiting-for-opponent))

(defun handle-match-begin ()
  (setf (game-state-current-screen *game-state*) :game-play))

(defun make-ping-message (x y radius)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :ping
			:float32       radius
			:float32       x
			:float32       y)))

(defun handle-ack-message (message)
  (userial:with-buffer message
    (let (remaining-energy hit-number)
      (userial:unserialize* :float32 remaining-energy
			    :uint16 hit-number)
      (format t "Received ping ack message from server with ~a hits~%" hit-number)
      (finish-output)
      (loop repeat hit-number do
	   ;; hit pos has less depth so that it is on top of ping
	   (push (list (sb-cga:vec+ (pos *ping*) (sb-cga:vec 0.0 0.0 -1.0)) (userial:unserialize :float32)) *ping-hits*)))))

(defun make-fire-message (x y)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :fire
			:float32 x
			:float32 y)))

(defun handle-shot-results-message (message)
  (userial:with-buffer message
    (setf (hit-p *current-missile*) (if (eql (userial:unserialize :shot-result) :hit) t nil))
    (push *current-missile* *missiles-fired*)
    (setf *current-missile* nil)))

(defun handle-sunk-message (message)
  (userial:with-buffer message
    (apply #'find-sunk-ship
	   (userial:unserialize-list* '(:float32 :float32)))))

(defun find-sunk-ship (missile-x missile-y)
  (format t "sunk message received from server~%")
  (finish-output)
  ;; move the missile-x to player field
  (setf missile-x (- missile-x 400.0))
  (let ((hit nil))
    (loop for ship in *ships-placed* do
	 (let ((ship-x (aref (pos ship) 0))
	       (ship-y (aref (pos ship) 1))
	       (half-width (/ (width ship) 2.0))
	       (half-height (/ (height ship) 2.0))
	       (missile-radius 2.0))
	   (when (and (<= (- missile-x missile-radius) (+ ship-x half-width))
		      (>= (+ missile-x missile-radius) (- ship-x half-width))
		      (<= (- missile-y missile-radius) (+ ship-y half-height))
		      (>= (+ missile-y missile-radius) (- ship-y half-height)))
	     (setf hit t)
	     (setf (sunk-p ship) t))))
    (push (make-instance 'missile :pos (sb-cga:vec missile-x missile-y 7.0) :hit-p hit) *enemy-fire*)))
