(in-package :trivial-wish)

(defclass wish ()
  ()
  (:documentation "Subclass your wish class and specialize (fulfil...) method"))

(defgeneric fulfil (wish &key &allow-other-keys)
  (:documentation "Perform the requested computation on a wish-derived object"))

(defmethod initialize-instance :after ((wish wish) &key upon)
  (push wish upon))


(defmethod fulfil ((wishes cons) &key (lifo t) )
  "Fulfil all wishes.  Optionally, 
:lifo nil to feverse the native last-in-first-out order;
:fun to supply a different function to process each wish"
  (mapc #'fulfil (if lifo wishes (reverse wishes)))
  (setf wishes nil))

;;
;; A convenience for defining wish classes
;;
(defmacro define (name &rest slots)
  (let ((expanded-slots
	 (mapcar (lambda (slot)
		 (list slot :initarg
		       (intern (string-upcase slot) "KEYWORD")
		       :accessor slot :initform nil))
		 slots)))
    `(defclass ,name (wish:wish)
       ,expanded-slots)))
