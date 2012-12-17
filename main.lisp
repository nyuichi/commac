(defpackage :commac
  (:use :cl))

(in-package :commac)

(defstruct (vector-2d
	     (:constructor make-vector-2d)
	     (:constructor v2 (x y))
	     (:conc-name v2-))
  x y)

(defmacro define-v2-operator2 (name &body body)
  `(defun ,name (v u)
     (with-slots ((x1 x) (y1 y)) v
       (with-slots ((x2 x) (y2 y)) u
	 ,@body))))

(define-v2-operator2 v2+
  (v2 (+ x1 x2) (+ y1 y2)))

(define-v2-operator2 v2-
  (v2 (- x1 x2) (- y1 y2)))


