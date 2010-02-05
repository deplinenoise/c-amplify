;;;; Statement AST types and support code

(in-package #:se.defmacro.c-amplify)

(defmacro %cg-stmt-around (&rest body)
  `(let ((*stmt-depth* (1+ *stmt-depth*)))
     (generate-code *cg-freshline*)
     ,@body))

(defun c-type-p (datum)
  (typep datum 'c-type))

(defun c-stmt-p (datum)
  (typep datum 'c-stmt))

(defun c-stmt-list-p (datum)
  (and (listp datum)
       (every #'c-stmt-p datum)))

(defmacro %cg-block-around (&rest body)
  `(progn
     (generate-code *cg-begin-block*)
     (let ((*compound-depth* (1+ *compound-depth*)))
       ,@body)
     (generate-code *cg-end-block*)))

(deftype c-stmt-list ()
'(satisfies c-stmt-list-p))

(defclass c-stmt (ast-node) ())

(defclass c-null-stmt (c-stmt) ())

(defmethod ast-children ((self c-null-stmt))
  nil)

(defmethod generate-code :around ((self c-stmt))
  (let ((*stmt-depth* (1+ *stmt-depth*)))
    (generate-code *cg-freshline*)
    (call-next-method)))

(defmethod generate-code ((self c-null-stmt))
  (when (> *stmt-depth* 0)
    (princ ";")))

(defclass c-compound-stmt-base (c-stmt)
  ((body :type c-stmt-list :initarg :body :accessor c-compound-body))
  (:documentation "Superclass for statements that contain other statements."))

(defun %emit-compound-body (compound-stmt)
  (%cg-block-around
   (generate-code (c-compound-body compound-stmt))))

(defclass c-compound-stmt (c-compound-stmt-base) ())

(defmethod generate-code ((self c-compound-stmt))
  (%emit-compound-body self))

(defun inline-compound-bodies (stmt-list outer-env)
  (let ((only-declarations-so-far t))
    (labels ((compound-declarations (compound) ; => list of c-declarator instances for compound
	       (loop for stmt in (c-compound-body compound)
		  while (typep stmt 'c-declare-stmt)
		  appending (declarators stmt)))
	     (will-shadow-p (compound) ; => t if any declaration in the compound will shadow outer-env
	       (some #'(lambda (decl) (%env-lookup (name decl) outer-env))
		     (compound-declarations compound)))
	     (mergable-p (compound) ; => t if the sub-compound can be merged
	       (and (typep compound 'c-compound-stmt)
		    only-declarations-so-far
		    (not (will-shadow-p compound))))
	     ;; Try to merge the contents of stmt if it is
	     (simplify-stmt (stmt)
	       (simplify stmt)
	       (cond ((mergable-p stmt) (copy-list (c-compound-body stmt)))
		     ((typep stmt 'c-declare-stmt) (setf outer-env (ast-env stmt)) (list stmt))
		     (t (setf only-declarations-so-far nil)
			(list stmt)))))
      (if (some #'(lambda (x) (typep x 'c-compound-stmt)) stmt-list)
	  (loop for stmt in stmt-list appending (simplify-stmt stmt))
	  stmt-list))))

(defmethod simplify ((self c-compound-stmt))
  (with-slots (body) self
    (setf body (inline-compound-bodies body (ast-env self))))
  self)

(defclass c-break-stmt (c-stmt) ())

(defmethod generate-code ((self c-break-stmt))
  (generate-code "break;"))

(defclass c-continue-stmt (c-stmt) ())

(defmethod generate-code ((self c-continue-stmt))
  (generate-code "continue;"))

(defclass c-do-while-stmt (c-stmt)
  ((test-expr :initarg :test-expr :accessor test-expr :type c-expr)
   (stmt :initarg :stmt :accessor stmt :type c-stmt)))

(defmethod generate-code ((self c-do-while-stmt))
  (with-slots (test-expr stmt) self
    (generate-code* "do" stmt "while (" test-expr ");")))

(defclass c-expr-stmt (c-stmt)
  ((expr :type c-expr :initarg :expr :accessor expr)))

(defmethod generate-code ((self c-expr-stmt))
  (with-slots (expr) self
    (%cg-stmt-around
     (generate-code* expr ";"))))

(defclass c-for-stmt (c-compound-stmt-base)
  ((init-expr :type c-expr :initarg :init-expr :accessor init-expr)
   (test-expr :type c-expr :initarg :test-expr :accessor test-expr)
   (iter-expr :type c-expr :initarg :iter-expr :accessor iter-expr)))

(defmethod generate-code ((self c-for-stmt))
  (with-slots (init-expr test-expr iter-expr) self
    (%cg-stmt-around
     (generate-code* "for (" init-expr "; " test-expr "; " iter-expr ")")
     (%emit-compound-body self))))

(defclass c-goto-stmt (c-stmt)
  ((label :type symbol :initarg :label :accessor label)))

(defmethod generate-code ((self c-goto-stmt))
  (with-slots (label) self
    (%cg-stmt-around
     (generate-code* "goto " label ";"))))

(defmethod ast-children ((self c-goto-stmt))
  nil)

(defclass c-label-stmt (c-stmt)
  ((label :type symbol :initarg :label :accessor label)))

(defmethod generate-code ((self c-label-stmt))
  (with-slots (label) self
    (%cg-stmt-around
     (generate-code* *cg-freshline* label ":"))))

(defmethod ast-children ((self c-label-stmt))
  nil)

(defclass c-if-stmt (c-stmt)
  ((test-expr :type c-expr :initarg :test-expr :accessor test-expr)
   (true-stmt :type c-stmt :initarg :true-stmt :accessor true-stmt)
   (false-stmt :type (or c-stmt null) :initform nil :initarg :false-stmt :accessor false-stmt)))

(defmethod generate-code ((self c-if-stmt))
  (with-slots (test-expr true-stmt false-stmt) self
    (%cg-stmt-around
     (generate-code* "if (" test-expr ")" true-stmt)
     (when false-stmt
       (generate-code* "else" false-stmt)))))

(defclass c-return-stmt (c-stmt)
  ((expr :type (or c-expr null) :initarg :expr :initform nil :accessor expr)))

(defmethod generate-code ((self c-return-stmt))
  (with-slots (expr) self
    (%cg-stmt-around
     (generate-code "return")
     (when expr
       (generate-code* " " expr))
     (generate-code ";"))))

(defclass c-declarator (ast-node)
  ((name :type symbol :initarg :name :accessor name)
   (decl-type :type c-type :initarg :decl-type :accessor decl-type)
   (init-expr :type (or c-expr null) :initarg :init-expr :initform nil :accessor init-expr)))

(defmethod generate-code ((self c-declarator))
  (with-slots (name decl-type init-expr) self
    ;; FIXME: need a (emit-declaration ...) to make function types
    ;; work, because there the type surrounds the identifier
    (%cg-stmt-around
     (generate-code (c-type->string decl-type name))
     (when init-expr
       (generate-code* " = " init-expr)))))

(defclass c-declare-stmt (c-stmt)
  ((declarators :type list :initarg :declarators :accessor declarators)))

(defmethod generate-code ((self c-declare-stmt))
  (with-slots (declarators) self
    (dolist (d declarators)
      (generate-code* d ";"))))

(defclass c-defstruct-node (ast-node)
  ((struct-type :type c-structured-type :initarg :struct-type :accessor struct-type)))

(defmethod generate-code ((self c-defstruct-node))
  (with-slots (struct-type) self
    (with-slots (fields kind name) struct-type
      (generate-code* *cg-toplevel-spacing* (format nil "~(~a~)" kind) " " name)
      (%cg-block-around
       (dolist (f fields)
	 (generate-code* *cg-freshline* (c-type->string (cdr f) (car f)) ";")))
      (generate-code ";"))))

(defclass c-defun-node (c-compound-stmt-base)
  ((gv :type gval :initarg :gval)
   (formals :type list :initarg :formals)))

(defmethod generate-code ((self c-defun-node))
  (with-slots (gv body formals) self
    (assert (eq (gval-kind gv) :defun))
    (let* ((symbol (gval-sym gv))
	   (ftype (gval-type gv))
	   (return-type (return-type ftype))
	   (argument-types (argument-types ftype))
	   (variadic-p (variadic-p ftype)))
      (generate-code *cg-toplevel-spacing*)
      (generate-code return-type)
      (generate-code *cg-defun-return-type-separator*)
      (generate-code* symbol "(")
      (loop
	 for formal in formals
	 for argtype in argument-types
	 for comma-required-p = nil then t
	 do (progn
	      (when comma-required-p
		(generate-code ", "))
	      (generate-code* argtype " " formal)))
      (when variadic-p
	(when (not (null formals))
	  (generate-code ", "))
	(generate-code "..."))
      (generate-code ")")
      (%cg-block-around
       (generate-code body)))))

(defmethod simplify ((self c-defun-node))
  (with-slots (body) self
    (setf body (inline-compound-bodies body (ast-env self))))
  self)


(defclass c-switch-case (c-compound-stmt-base)
  ((test-expr :type c-expr :initarg :test-expr :accessor test-expr)))

(defmethod generate-code ((self c-switch-case))
  (generate-code* *cg-freshline* "case " (test-expr self) ":")
  (%emit-compound-body self))

(defmethod simplify ((self c-switch-case)) self)

(defmethod ast-children ((self c-switch-case))
  (cons (test-expr self) (c-compound-body self)))


(defclass c-switch-default (c-compound-stmt-base) ())

(defmethod generate-code ((self c-switch-default))
  (generate-code* *cg-freshline* "default:")
  (%emit-compound-body self))

(defmethod simplify ((self c-switch-default)) self)

(defmethod ast-children ((self c-switch-default))
  (c-compound-body self))


(defclass c-switch-stmt (c-compound-stmt-base)
  ((test-expr :type c-expr :initarg :test-expr :accessor test-expr)
   (cases :initarg :cases :accessor cases)))

(defmethod generate-code ((self c-switch-stmt))
  (generate-code* "switch (" (test-expr self) ")")
  (%cg-block-around
   (generate-code (cases self))))

(defmethod simplify ((self c-switch-stmt)) self)

(defmethod ast-children ((self c-switch-stmt))
  (with-slots (test-expr) self
    (cons test-expr (c-compound-body self))))


(defmethod ast-children ((ast c-compound-stmt-base))
  (with-slots (body) ast
    body))

(defmethod ast-children ((ast c-defstruct-node)) nil)

(defmethod ast-children ((ast c-if-stmt))
  (with-slots (test-expr true-stmt false-stmt) ast
    (if false-stmt
	(list test-expr true-stmt false-stmt)
	(list test-expr true-stmt))))

(defmethod ast-children ((ast c-expr))
  nil)

(defmethod ast-children ((ast c-declare-stmt))
  (loop for decl in (declarators ast)
       when (init-expr decl)
       collect it))

(defmethod ast-children ((ast c-return-stmt))
  (list (expr ast)))

(defmethod ast-children ((ast c-expr-stmt))
  (list (expr ast)))

