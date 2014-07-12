(in-package #:battleship)

(defvar *server* t)

(defvar *server-socket* nil)

(defun server-p () *server*)

(defvar *clients* nil)

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
	*clients* nil))

(defun accept-client ()
  (when (usocket:wait-for-input *server-socket*
				:timeout 0
				:ready-only t)
    (push (usocket:socket-accept *server-socket*) *clients*)
    (format t "a client was accepted")
    ))

(defun batch-update ()
  (when *clients*
    (let ((buffer (userial:make-buffer)))
      (userial:with-buffer buffer
	(userial:serialize :opcodes :batch-update)
	(userial:serialize :int32 0) ; number of delta update in the batch

	;; (userial:serialize :keyword :paddle)
	;; (serialize buffer *paddle-one*)

	;; (userial:serialize :keyword :paddle)
	;; (serialize buffer *paddle-two*)

	;; (userial:serialize :keyword :ball)
	;; (serialize buffer *ball*)
	)
      (loop for client in *clients* do
	   (send-message client buffer)))))
