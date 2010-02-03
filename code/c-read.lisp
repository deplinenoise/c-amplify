(in-package #:se.defmacro.c-amplify)

(defparameter *c-readtable* (copy-readtable))

(setf (readtable-case *c-readtable*) :preserve)

(defparameter *read-end* (gensym "end"))

(defun read-ca-file (path)
  (with-open-file (stream path)
    (let ((*readtable* *c-readtable*))
      (let ((*package* (find-package :se.defmacro.c-amplify.csym)))
	(loop for x = (read stream nil *read-end*)
	   while (not (eq x *read-end*)) collect x)))))
