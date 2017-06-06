# CL-WISH

A wish is a request for a later computation.

Note that this is not threaded, or related to promises and other multiprocessing libraries.

# Why?

There are situations that require delaying computation until later, while more data is gathered.  Rather than keeping track of the data yourself, you can create a wish class containing the data and 'fulfilling' the wish later.

This pattern repeats often enough to justify this miniscule library.

# Usage

Use `(wish:define name slotnames..)` to quickly create a wish class with slots to hold your data, or subclass from `wish:wish` the long way.

Create a `wish:fulfil` method for your wish.

Embed a `wishes` instance somewhere, to keep all the wishes.

Make wishes with  by `(make-instance...)`.

Continue with your code, possibly setting data in your wish or making other wishes.

Call `(wish:fulfil wishes)` to finally invoke `fulfil` method on all the wishes.

# Example, literally...

```lisp
;; -----------------------------------------------------------------------------
;; We need to keep wishes somewhere.
;;
(defparameter *star* (make-instance 'wishes))
;;
;; In real life, we would probably place wishes into a slot of some class.
;;
;; -----------------------------------------------------------------------------
;; Now we subclass 'wish' to keep our data...
;;
(wish:define my-wish value1 value2)
;;
;; which is a quick way of doing
;;
;; (defclass my-wish (wish:wish)
;;   ((value1 :initarg :value1 :accessor value1 :initform nil)
;;   (value2 :initarg :value2 :accessor value2 :initform nil)))
;;
;;------------------------------------------------------------------------------
;; Every wish class must have a 'fulfil' method that will be called later.
;;
(defmethod wish:fulfil ((wish my-wish)&key)
  (with-slots (value1 value2) wish
    (format t "~A + ~A = ~A~&" value1 value2 (+ value1 value2))))
;;==============================================================================
;;
;; And here is how to use wishes:
;;
(defun example ()
  ;;----------------------------------------------------------------------------
  ;; We can make a wish like this:
  ;;
  (make-instance 'my-wish :upon *star* :value1 2 :value2 2)
  ;;
  (format t "Example of using trivial wishes.~&")
  ;;----------------------------------------------------------------------------
  ;; Or, if we don't know all the data, just put in what we do know:
  ;;
  (let ((partial (make-instance 'my-wish :upon *star* :value1 3 )))
    ;;
    (format t "~&Wishes are being made, but nothing is happening yet.~&")
    ;;--------------------------------------------------------------------------
    ;; and another one:
    ;;
    (make-instance 'my-wish :upon *star* :value1 99 :value2 1)
    ;;
    (format t "~&...~&")
    ;;--------------------------------------------------------------------------
    ;; Ah, let's not forget to fill in the missing data
    ;;
    (setf (value2 partial) 4)); wishes can be modifed or finished at any time...
  ;;----------------------------------------------------------------------------
  ;; Finally, make wishes come true, in original order
  ;;
  (wish:fulfil *star* :lifo nil)
  ;;----------------------------------------------------------------------------
  ;; *stars* is now cleared and ready to accept more wishes.
  )
```

```
WISH> (example)
Example of using trivial wishes.
Wishes are being made, but nothing is happening yet.
...
2 + 2 = 4
3 + 4 = 7
99 + 1 = 100
NIL
```
