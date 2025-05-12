
(in-package #:lispylambda)

(defclass repl-example ()
  ((example :initarg :example)
   (package :initarg :package)))

(adp:defmacro repl (&rest expressions)
  (let ((example-obj (apply #'adpgh:example expressions))
        (package *package*))
    (make-instance 'repl-example :example example-obj :package package)))

(defun shortest-string (strings)
  (declare (type list strings))
  (loop for str in strings
	for shortest = str then (if (< (length str) (length shortest))
				    str
				    shortest)
	finally (return shortest)))

(defun get-shortest-package-name (package)
  (shortest-string
   (cons (package-name package)
         (package-nicknames package))))

(defun indent-text-code (text-code package)
  (let* ((package-name (get-shortest-package-name package))
         (package-length (length package-name))
         (lines (split-sequence #\Newline text-code))
         (first-line (format nil "~a> ~a" package-name (car lines)))
         (rest-lines (mapcar (lambda (line)
                               (format nil "~a  ~a"
                                       (make-string package-length :initial-element #\Space) line))
                             (cdr lines))))
    (format nil "~{~a~^~%~}" (cons first-line rest-lines))))

(defmethod adpgh::print-element (stream (element repl-example))
  (with-slots (example package) element
    (with-slots ((code adpgh::code) (output adpgh::output) (results adpgh::results)) example
      (format stream "`````common-lisp~%~a~%`````~%" (indent-text-code code package))
      (when (> (length output) 0)
        (format stream "`````text~%;; Output~%~a~%`````~%" output))
      (format stream "`````common-lisp~%;; Returns~%~a~%`````" (if (zerop (length results))
                                                                   "; No values"
                                                                   results)))))
