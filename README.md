# TRIVIAL-WISH

This library allows you to create _wishes_ which are requests for a later computation, and fulfil them at a later time.

Note that this is not threaded, or related to promises and other multiprocessing libraries.

Classes derived from `wish:wish` contain data, and the computation is performed via the `fulfil` method you specialize for each class.  Different kinds of wishes may be freely mixed.

# Why?

There are situations that require delaying computation until later, while more data is gathered.  Other times, you may want to batch up a number of computations and perform them together.  Rather than keeping track of the data yourself, you can create wish classes containing the data and 'fulfilling' the wishes later.

This pattern occurs often enough (in my opinion) to justify this miniscule library.

# Usage

Use `(wish:define name slotnames..)` to quickly create a wish class with slots to hold your data, or subclass from `wish:wish` the long way.

Create a `wish:fulfil` method for your wish.

Wishes must be kept a `wish:wishes` object -- create a slot or a variable with a reasonable scope, and initialize with `(make-instance 'wish:wishes)` 

Make wishes with `(make-instance 'wishname :upon wishlist)`; they are automatically pushed onto the wishlist.

Continue with your code, possibly setting data in your wish or making other wishes.

Call `(wish:fulfil wishes)` to finally invoke `fulfil` method on all the wishes in LIFO order, or in natural order with the optional `:lifo nil` parameter

# Example, literally...

```lisp
(in-package :trivial-wish)
;; -----------------------------------------------------------------------------
;; We need to keep wishes somewhere.  Wishes are a list, so any reasonable
;; location will do.
;;

(defparameter *star* (make-instance 'wish:wishes))
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
  (format t "~&Example of using trivial wishes.~&")
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
# NOTES
example.lisp file does not load with the package; if you want to run it, compile it yourself.

The `fulfil` method when invoked on a wish fulfils it.  When invoked on an instance of `wishes`, it calls `(fulfil wish) invoke on every wish in the container.

If your fulfilment logic requires more than just the wish itself you can try to defun the `fulfil` method in a lexical scope that has access to the data you need.  

fulfil allows other keyboards, but I could not think of a way to generalize passing arbitrary data to it in a loop...  If more is required, consider that all fulfil does is:
```
(with-slots (data) wishes
    (mapc #'fulfil (if lifo data (reverse data)))
    (setf data nil))
```
Just write your own!
