;;;; package.lisp

(defpackage #:cl-wish
  (:nicknames :wish)
  (:use #:cl)
  (:shadow cl:reverse)
  (:export
   #:wish #:wishes #:make #:fulfil #:reverse   ))

 
