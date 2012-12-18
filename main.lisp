(defpackage :commac
  (:use :cl))

(in-package :commac)

(deftype vector-2d ()
  '(simple-vector 2))

(declaim (inline v2 v2-x v2-y v2+ v2- v2-dot))

(defun v2 (x y)
  (declare (type fixnum x y))
  (let ((v2 (make-array 2)))
    (setf (svref v2 0) x
	  (svref v2 1) y)
    v2))

(defun v2-x (v2)
  (declare (type vector-2d v2))
  (the fixnum (svref v2 0)))

(defun v2-y (v2)
  (declare (type vector-2d v2))
  (the fixnum (svref v2 1)))

(defmacro define-v2-operator2 (name &body body)
  `(defun ,name (v u)
     (declare (type vector-2d v u))
     (let ((x1 (v2-x v))
	   (y1 (v2-y v))
	   (x2 (v2-x u))
	   (y2 (v2-y u)))
       ,@body)))

(define-v2-operator2 v2+
  (v2 (+ x1 x2) (+ y1 y2)))

(define-v2-operator2 v2-
  (v2 (- x1 x2) (- y1 y2)))

(define-v2-operator2 v2-dot
  (the fixnum
    (+ (the fixnum (* x1 x2))
       (the fixnum (* y1 y2)))))

(defun right-angled-p (a b c d)
  (declare (type vector-2d a b c d))
  (zerop (v2-dot (v2- a b) (v2- c d))))

