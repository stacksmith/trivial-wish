(in-package :trivial-wish)

(defclass wish ()
  ()
  (:documentation "Subclass your wish class and specialize (fulfil...) method"))

(defgeneric fulfil (wish &key &allow-other-keys)
  (:documentation "Perform the requested computation on a wish-derived object"))

(defmethod initialize-instance :after ((wish wish) &key upon)
  (push wish (data upon)))

(defclass wishes ()
  ((data :accessor data :initform nil))
  (:documentation "A container for wish-derived objects"))

(defmethod fulfil ((wishes wishes) &key (lifo t) (fun #'fulfil))
  "Fulfil all wishes.  Optionally, 
:lifo nil to feverse the native last-in-first-out order;
:fun to supply a different function to process each wish"
  (mapc fun (if lifo
		(data wishes)
		(reverse (data wishes))))
  (setf (data wishes) nil))

