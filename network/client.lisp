(in-package #:battleship)

(defvar *server-connection* nil)

(defun connect-to-server (server-ip port)
  (setf *server-connection*
	(usocket:socket-connect server-ip
				port
				:element-type '(unsigned-byte 8))))
(defun disconnect-from-server ()
  (assert *server-connection*)
  (usocket:socket-close *server-connection*)
  (setf *server-connection* nil))

(defun delta-update (thing)
  (let ((buffer (userial:make-buffer)))
    (userial:with-buffer buffer
      (userial:serialize* :opcodes :delta-update
			  :keyword (typecase thing
				     (ball   :ball)
				     (paddle :paddle))))
    (serialize buffer thing)
    (send-message *server-connection* buffer)))
