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

(defmacro with-gensyms (syms body)
  `(let ,(loop :for sym :in syms
	    :collect `(,sym (gensym ,(symbol-name sym))))
     ,body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-vector-2d (v)
    (cond ((and (atom (first v)) (eq (first v) 'v2+))
	   (with-gensyms (x1 y1 x2 y2)
	     `(multiple-value-bind (,x1 ,y1) ,(expand-vector-2d (second v))
		(multiple-value-bind (,x2 ,y2) ,(expand-vector-2d (third v))
		  (values (+ ,x1 ,x2) (+ ,y1 ,y2))))))
	  ((and (atom (first v)) (eq (first v) 'v2-))
	   (with-gensyms (x1 y1 x2 y2)
	     `(multiple-value-bind (,x1 ,y1) ,(expand-vector-2d (second v))
		(multiple-value-bind (,x2 ,y2) ,(expand-vector-2d (third v))
		  (values (- ,x1 ,x2) (- ,y1 ,y2))))))
	  (t
	   (with-gensyms (val)
	     `(let ((,val ,v))
		(values (v2-x ,val) (v2-y ,val))))))))

(define-compiler-macro v2+ (&whole form)
  (with-gensyms (x y)
    `(multiple-value-bind (,x ,y) ,(expand-vector-2d form)
       (v2 ,x ,y))))

(define-compiler-macro v2- (&whole form)
  (with-gensyms (x y)
    `(multiple-value-bind (,x ,y) ,(expand-vector-2d form)
       (v2 ,x ,y))))

(define-compiler-macro v2-dot (v u)
  (with-gensyms (x1 y1 x2 y2)
    `(multiple-value-bind (,x1 ,y1) ,(expand-vector-2d v)
       (multiple-value-bind (,x2 ,y2) ,(expand-vector-2d u)
	 (the fixnum
	   (+ (the fixnum (* ,x1 ,x2))
	      (the fixnum (* ,y1 ,y2))))))))

(defun right-angled-p (a b c d)
  (declare (type vector-2d a b c d))
  (zerop (v2-dot (v2- a b) (v2- c d))))

(defun right-angled-p* (a b c d)
  (declare (type vector-2d a b c d))
  (zerop (+ (the fixnum
	      (* (- (v2-x a) (v2-x b))
		 (- (v2-x c) (v2-x d))))
	    (the fixnum
	      (* (- (v2-y a) (v2-y b))
		 (- (v2-y c) (v2-y d)))))))
