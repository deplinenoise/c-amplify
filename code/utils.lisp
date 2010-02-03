(in-package #:se.defmacro.c-amplify)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Make a symbol by concatenating all ARGS as strings"
  (values (intern (string-upcase (apply #'mkstr args)))))

(defmacro defun/compile-time (&rest form)
  "Exactly like DEFUN, but wrapped in an EVAL-WHEN that makes it
available at compile-time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,@form)))

(defun logor (x y)
  "Implements the missing logical or function of two integers X and Y."
  (declare (type integer x y))
  (logorc1 (lognot x) y))

(defun dump-hash (hash)
  (maphash #'(lambda (k v) (format t "~S = ~S~%" k v)) hash))
