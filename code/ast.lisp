;;;; The basic definitions for all AST types.

(in-package #:se.defmacro.c-amplify)

(defgeneric generate-code (ast)
  (:documentation "Print code for the datum passed which may be a c-type or ast-node or a list of such objects for convenience."))

(defgeneric simplify (ast)
  (:documentation "Visit the ast-node AST and try to simplify it"))

(defgeneric ast-children (ast)
  (:documentation "Return a list of child nodes for the specified AST."))

;;; Lexical scoping works by associating each ast-node instance with a
;;; list that is the start of the search tree for that node. The macro
;;; WITH-LEXICAL-SCOPE establishes a new binding for the *lexical-env*
;;; variable. The lexical scope shared structure all the way back to
;;; the top.

(defparameter *lexical-scope-active* nil
  "Sanity check for when %PUSH-LEXICAL-BINDING can be used")

(defparameter *lexical-env* nil
  "An alist (id . type) of variables currently in lexical
scope (including formal arguments) in innermost-first order.")

(defun %push-lexical-binding (id type)
  "Push a binding of the symbol ID to the type TYPE on the current
lexical scope chain."
  (assert *lexical-scope-active*)
  (push (cons id type) *lexical-env*))

(defmacro with-lexical-scope (&rest body)
  "Limit the lexical environment manipulations done by BODY to the
BODY forms. For instance, the parser for a C compound statement should
be wrapped in an WITH-LEXICAL-SCOPE macro."
  `(let ((*lexical-scope-active* t)
	 (*lexical-env* *lexical-env*))
     ,@body))

(defun %env-lookup (symbol env-alist)
  (cdr (assoc symbol env-alist)))

(defun %type-of-lexical-symbol (symbol env-alist)
  "Look up the type of SYMBOL lexically (and then globally, if a
lexical binding cannot be found). Calls ERROR if the symbol isn't bound."
  (let ((result (%env-lookup symbol env-alist)))
    (if result
	result
	(let ((gv (lookup-gval symbol)))
	  (if gv
	      (gval-type gv)
	      (error "symbol is not bound lexically or globally: ~a" symbol))))))


(defclass ast-node ()
  ((env
    :type list
    :accessor ast-env
    :initform *lexical-env*
    :documentation "Set to the state of the lexical environment alist
    when the node is created, reflecting the lexical bindings
    available at that point in the program."))
  (:documentation "The base class for AST nodes."))

(defmethod simplify ((self ast-node))
  "The default simplification just recurses to children and returns the same object."
  (dolist (child (ast-children self))
    (simplify child)
    self))

(defun dump-ast (ast)
  "Utility function to pretty-print an AST structure when debugging."
  (labels ((indent (level)
	     (dotimes (x level) (princ "    ")))
	   (walk (node level)
	     (indent level)
	     (format t "~a~%" (class-name (class-of node)))
	     (let ((children (ast-children node)))
	       (if (null children)
		   (values)
		   (progn
		     (loop for binding on (ast-env (car children))
			until (equal binding (ast-env node))
			do (progn
			     (indent level)
			     (format t "  Lexical: ~a ~a~%" (caar binding) (cdar binding))))
		     (loop for child in (ast-children node)
			do (walk child (1+ level))))))))
    (walk ast 0))
  (values))

