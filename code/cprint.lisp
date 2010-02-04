;;;; C code pretty printing

(in-package #:se.defmacro.c-amplify)

(defvar *cg-newline* (gensym "newline-symbol")
  "Symbol used to represent newlines")

(defvar *cg-optional-separator* (gensym "optional-separator")
  "Symbol used to represent optional spaces before opening braces (those we don't want printed at the start of a fresh line)")

(defvar *cg-freshline* (gensym "freshline-symbol")
  "Symbol used to represent newlines on non-blank lines")

(defparameter *outer-precedence* 1000
  "The precedence of the current closes expression on the stack. This is used to avoid parenthesis around every subexpression. We default it to something very high as the toplevel expression never needs parens.")

(defparameter *cg-print-depth* 0)
(defparameter *cg-indent-str* "    ")
(defparameter *pending-indent* t)
(defparameter *stmt-depth* 0)
(defparameter *expr-depth* 0)
(defparameter *compound-depth* 0)

(defparameter *cg-toplevel-spacing* (list *cg-newline* *cg-newline*))
(defparameter *cg-begin-block* (list *cg-freshline* *cg-optional-separator* "{" *cg-newline*))
(defparameter *cg-end-block* (list *cg-freshline* "}"))
(defparameter *cg-defun-return-type-separator* (list *cg-newline*))

(defun %cg-print (&rest items)
  "Pretty-print one or more ITEMS, taking indentation into account."
  (labels ((indent-level ()
	     (+ (* 0 *stmt-depth*) *compound-depth*))
	   (finish-line ()
	     (princ #\newline)
	     (setf *pending-indent* t))
	   (fresh-line ()
	    (unless *pending-indent* (finish-line)))
	   (really-print (datum)
	     (when *pending-indent*
	       ;; (format t "/*~a/~a*/ " *stmt-depth* *compound-depth*)
	       (dotimes (x (indent-level)) (princ *cg-indent-str*))
	       (setf *pending-indent* nil))
	     (princ datum)))
    (dolist (item items)
      (cond ((eq *cg-freshline* item) (fresh-line))
	    ((eq *cg-newline* item) (finish-line))
	    ((eq *cg-optional-separator* item) (unless *pending-indent* (princ " ")))
	    ((listp item) (dolist (i item) (%cg-print i)))
	    ((typep item 'c-stmt) (let ((*stmt-depth* (1+ *stmt-depth*)))
				    (fresh-line)
				    (generate-code item)))
	    ((typep item 'ast-node) (generate-code item))
	    ((typep item 'c-type) (emit-c-type item #'%cg-print))
	    ((stringp item) (really-print item))
	    ((or (integerp item) (floatp item)) (really-print (format nil "~a" item)))
	    ((symbolp item) (really-print (format nil "~a" item)))
	    (t (error "unsupported item: ~a" item)))))
  (values))

