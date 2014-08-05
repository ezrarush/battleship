(in-package #:battleship)

(defvar *db* (make-hash-table))

(let ((next-id 0))
  (defun make-id ()
    (incf next-id)))

(defun id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *db*)
    (declare (ignore id))
    exists))

(defun lookup-object-by-id (id)
  (multiple-value-bind (object exists) (gethash id *db*)
    (unless exists (error "No object for id ~a." id))
    object))

(defun add-object-to-db (object)
  (let ((id (object-id object)))
    (assert (not (id-in-use-p id)))
    (setf (gethash id *db*) object)))

(defun remove-object-from-db (object)
  (let ((id (object-id object)))
    (assert (eql (lookup-object-by-id id) object))
    (remhash id *db*)))

;; the objects that will be put into the *db*

(defclass db-object ()
  ((id :type integer
       :initform (make-id)
       :accessor object-id)))

(defmethod initialize-instance :after ((self db-object) &key)
  (add-object-to-db self))

