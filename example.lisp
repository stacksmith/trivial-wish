(in-package :trivial-wish)

(defparameter *star* (make-instance 'wishes))

(defclass my-wish (wish:wish)
  ((value1 :initarg :v1 :accessor value1 :initform nil)
   (value2 :initarg :v2 :accessor value2 :initform nil)))

(defmethod wish:fulfil ((wish my-wish)&key)
  (with-slots (value1 value2) wish
    (format t "~A + ~A = ~A~&" value1 value2 (+ value1 value2))))

(defun example ()
  (make-instance 'my-wish :upon *star* :v1 2 :v2 2)
  
  (format t "Example of using trivial wishes.~&")

  (let ((partial
	 (make-instance 'my-wish :upon *star* :v1 3 )))

    (format t "~&Wishes are being made, but nothing is happening yet.~&")

    (make-instance 'my-wish :upon *star* :v1 99 :v2 1)

    (format t "~&...~&")

    (setf (value2 partial) 4)); wishes can be modifed or finished at any time...
  ;;
  ;; Finally,
  ;;
  (wish:fulfil *star* :lifo nil))
