
(in-package #:lispylambda)


(defmacro example-with-gensym-counter (num &rest exprs)
  (example (format nil "(let ((*gensym-counter* ~s)) ~{~a~})" num exprs)))
