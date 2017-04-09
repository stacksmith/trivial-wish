;; 
(in-package :cl-wish)

;; Structured buffer.  Uses gapbuf and range together
;; 
;;

(defclass wish ()
  ())

(defclass wishes ()
  ((data :accessor data :initform nil)))

(defun make (wish &key wishes)
  (push wish (data wishes)))

(defun reverse (wishes)
  (setf (data wishes) (reverse (data wishes))))

(defmethod fulfil ((wishes wishes) &rest rest)
  (loop for wish in (data wishes) do
       (fulfil wish rest))
  (setf (data wishes) nil))

