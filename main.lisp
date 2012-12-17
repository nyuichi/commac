(defpackage :commac
  (:use :cl))

(in-package :commac)

(defstruct (vector-2d
	     (:constructor make-vector-2d)
	     (:constructor v2 (x y))
	     (:conc-name v2-))
  x y)

(defun v2+ (v u)
  (with-slots ((x1 x) (y1 y)) v
    (with-slots ((x2 x) (y2 y)) u
      (v2 (+ x1 x2) (+ y1 y2)))))

(defun v2- (v u)
  (with-slots ((x1 x) (y1 y)) v
    (with-slots ((x2 x) (y2 y)) u
      (v2 (- x1 x2) (- y1 y2)))))
