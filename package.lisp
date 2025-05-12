
(defpackage #:lispylambda
  (:use #:cl #:split-sequence #:adpgh)
  (:shadow gensym))

(in-package #:lispylambda)

;; Definimos nuestro propia funcion gensym para tener controlados los valores.
(defparameter *lispylambda-gensym-counter* 0)

(defun gensym (&optional (x "G"))
  (let ((name (etypecase x
                (string (format nil "~a~a" x *lispylambda-gensym-counter*))
                (integer (format nil "G~a" x)))))
    (incf *lispylambda-gensym-counter*)
    (make-symbol name)))
