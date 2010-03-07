;;;; C code pretty printing

(in-package #:se.defmacro.c-amplify)

(defvar *cg-newline* (gensym "newline-symbol")
  "Symbol used to represent newlines")

(defvar *cg-optional-separator* (gensym "optional-separator")
  "Symbol used to represent optional spaces before opening
  braces (those we don't want printed at the start of a fresh line)")

(defvar *cg-freshline* (gensym "freshline-symbol")
  "Symbol used to represent newlines on non-blank lines")

(defparameter *outer-precedence* 1000
  "The precedence of the current closes expression on the stack. This
  is used to avoid parenthesis around every subexpression. We default
  it to something very high as the toplevel expression never needs
  parens.")

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

(defun %flush-indent ()
  (when *pending-indent*
    (dotimes (x *compound-depth*) (princ *cg-indent-str*))
    (setf *pending-indent* nil)))

(defmacro generate-code* (&rest items)
  `(progn
    ,@(loop for i in items collecting `(generate-code ,i))))

(defmethod generate-code ((symbol symbol))
  (%flush-indent)
  (labels ((translate-symbol-char (ch)
	     (cond
	       ((eql #\- ch) #\_)
	       (t ch))))
    (princ (map 'string #'translate-symbol-char (symbol-name symbol)))))

(defmethod generate-code ((list list))
  (dolist (i list)
    (generate-code i)))

(defmethod generate-code ((type-obj c-type))
  (%flush-indent)
  (emit-c-type type-obj))

(defmethod generate-code ((sym (eql *cg-newline*)))
  (princ #\newline)
  (setf *pending-indent* t))

(defmethod generate-code ((sym (eql *cg-freshline*)))
  (unless *pending-indent*
    (generate-code *cg-newline*)))

(defmethod generate-code ((sym (eql *cg-optional-separator*)))
  (unless *pending-indent*
    (princ " ")))

(defmethod generate-code (datum)
  (%flush-indent)
  (princ datum))

(defun generate-source-file (nodes)
  (let ((used-decls (make-hash-table)))
    (dolist (ast nodes)
      (gather-decls ast used-decls))
    ;; TODO: Sort declarations in dependency order.
    (loop for gv being the hash-keys in used-decls
       do (generate-decl gv))
    (generate-code nodes)))


(defun %generate-defun-decl (gv)
  (let ((sym (gval-sym gv))
	(ftype (gval-type gv)))
    (generate-code* (gval-linkage gv) " " (return-type ftype) " " sym "(")
    (let (comma-needed)
      (loop
	 for arg-type in (argument-types ftype)
	 do (progn
	      (when comma-needed
		(generate-code ", "))
	      (setf comma-needed t)
	      (generate-code arg-type)))
      (when (variadic-p ftype)
	(when comma-needed
	  (generate-code ", "))
	(generate-code "..."))
      (generate-code ");")
      (generate-code *cg-newline*))))

(defun %generate-type-decl (gv)
  (let ((sym (gval-sym gv))
	(ftype (gval-type gv)))
    (emit-c-type ftype sym)
    (princ ";")))

(defun generate-decl (gv)
  (ecase (gval-kind gv)
    (:defun (%generate-defun-decl gv))
    (:type (%generate-type-decl gv))))
