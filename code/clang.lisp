
;;;; C language parser

(in-package #:se.defmacro.c-amplify)

(defun flatten-field-list (clauses)
  "Flatten CLAUSES into a list of id . type conses. Each clause can
either be a single symbol or have the format (id type) or ((id1 ...)
type)."
  (loop for clause in clauses
     nconcing
       (multiple-value-bind  (ids type-spec)
	   (match clause
	     ((list* (and (list* sym _) syms) type-spec) (values (copy-list syms) type-spec))
	     ((list* sym type-spec) (values (list sym) type-spec))
	     (sym (values (list sym) nil)))
	 (mapcar #'(lambda (x) (cons x type-spec)) ids))))

(defun %parse-field-list (context clauses)
  "Parse the list CLAUSES as a field/parameter specification, using
CONTEXT as an error reporting string.  Struct fields and defun
parameters lists share the same format. Each item is a sublist of the
format (id type) or ((id1 id2 ...) type). The function flattens the
nested id structure, returning a list of pairs (id . resolved-type)
for each id declared."
  (loop for clause in clauses
     nconcing
       (multiple-value-bind  (ids type-spec)
	   (match clause
	     ((list* sym type-spec)
	      (values (if (listp sym) (copy-list sym) (list sym)) type-spec))
	     (_ (error "illegal ~a clause: ~a" context clause)))
	 (let ((type-node (eval-c-type type-spec)))
	   (mapcar #'(lambda (x) (cons x type-node)) ids)))))

(defun %parse-defstruct-fields (clauses)
  (%parse-field-list "defstruct" clauses))

(defparameter *c-keyword-symbols*
  '(if for return aref and or xor
    case default switch
    progn break continue goto label declare defstruct defunion defun defun/extern
    deftype cast sizeof sizeof-type struct union ptr const volatile
    restrict -> |...|))

(dolist (sym *c-keyword-symbols*) (export-symbol-to-c sym))

(defun %phase2-defun (id prototype-clauses body)
  (multiple-value-bind (return-type formals variadic-p)
      (analyze-defun-prototype prototype-clauses)
    (declare (ignore variadic-p return-type))
    (let ((my-gv (lookup-gval id)))
      (unless my-gv
	(error "~a is not bound" id))
      (let ((*current-return-type* (return-type (gval-type my-gv))))
	(setf (gval-ast my-gv)
	      (make-instance 'c-defun-node
			     :gval my-gv
			     :formals (loop for cons in formals collecting (car cons))
			     :body (with-lexical-scope
				       (loop for cons in formals
					  do (%push-lexical-binding (car cons) (cdr cons)))
				     (mapcar #'parse-c-stmt body))))))))

(defun current-defun-void-p ()
  (unless *current-return-type*
    (error "current-defun-void-p can only be used in a defun context"))
  (eq *void-type-instance* *current-return-type*))

(defun %parse-structured-c-stmt (decl)
  (let ((candidate (%parse-structured-c-stmt-opt decl)))
    (if candidate
	candidate
	(make-instance 'c-null-stmt))))

(defun %parse-structured-c-stmt-opt (decl)
  "Parse a list DECL starting with a symbol into a proper AST object
representing the statement"
  ;; TODO: This function is going to be called very often so it could
  ;; be optimized. The match macro generates a bunch of redundant
  ;; consp tests and tries all the clauses in order. It would perhaps
  ;; be more efficient to dispatch on the first symbol of the list
  ;; using e.g. a hash table to find the alternatives.
  (declare (type list decl))
  (match decl
    ((list* 'for init-expr test-expr iter-expr body)
     (make-instance 'c-for-stmt
		    :init-expr (parse-c-expr init-expr)
		    :test-expr (parse-c-expr test-expr)
		    :iter-expr (parse-c-expr iter-expr)
		    :body (with-lexical-scope (mapcar #'parse-c-stmt body))))
    ((list* 'progn body)
     (make-instance 'c-compound-stmt :body (with-lexical-scope (mapcar #'parse-c-stmt body))))
    ((list 'break) (make-instance 'c-break-stmt))
    ((list 'continue) (make-instance 'c-continue-stmt))
    ((list 'do stmt 'while expr)
     (make-instance 'c-do-while-stmt :test-expr (parse-c-expr expr) :stmt (parse-c-stmt stmt)))
    ((list 'goto label) (make-instance 'c-goto-stmt :label label))
    ((list 'label label) (make-instance 'c-label-stmt :label label))
    ((list 'if expr stmt) (make-instance 'c-if-stmt
					 :test-expr (parse-c-expr expr)
					 :true-stmt (parse-c-stmt stmt)))
    ((list 'if expr t-stmt f-stmt) (make-instance 'c-if-stmt
						  :test-expr (parse-c-expr expr)
						  :true-stmt (parse-c-stmt t-stmt)
						  :false-stmt (parse-c-stmt f-stmt)))
    ((list 'return) (make-instance 'c-return-stmt))
    ((list 'return expr) (make-instance 'c-return-stmt :expr (parse-c-expr expr)))
    ((list* 'declare forms)
     (make-instance 'c-declare-stmt :declarators
		    (loop for form in forms collect
			 (match form
			   ((list var-name type-name)
			    (let ((type-obj (eval-c-type type-name)))
			      (%push-lexical-binding var-name type-obj)
			      (make-instance 'c-declarator :name var-name :decl-type type-obj)))
			   ((list var-name '= init-expr)
			    (let* ((expr-obj (parse-c-expr init-expr))
				   (expr-type (expr-typeof expr-obj)))
			      (%push-lexical-binding var-name expr-type)
			      (make-instance 'c-declarator
					     :name var-name
					     :init-expr (parse-c-expr init-expr)
					     :decl-type expr-type)))
			   (_ (error "illegal declarator: ~a" form))))))
    ((list* 'defun/extern _) nil)
    ((list* 'defun id prototype body)
     (%phase2-defun id prototype body))
    ((list* (and (or 'defstruct 'defunion) k/w) id _)
     (make-instance 'c-defstruct-node
		    :struct-type (%lookup-tagged-type (case k/w
							(defstruct 'struct)
							(defunion 'union)) id)))
    ((list* 'switch expr cases)
     (make-instance 'c-switch-stmt
		    :test-expr (parse-c-expr expr)
		    :cases (loop for c in cases collecting
				(match c
				  ((list* 'case expr body)
				   (make-instance 'c-switch-case
						  :test-expr (parse-c-expr expr)
						  :body (mapcar #'parse-c-stmt body)))
				  ((list* 'default body)
				   (make-instance 'c-switch-default
						  :body (mapcar #'parse-c-stmt body)))
				  (_ (error "unsupported case/default form: ~a" c))))))

    ((list 'ast-stmt-if expr data) (if (eval expr)
				       (%parse-structured-c-stmt (c-macroexpand data))
				       nil))
    ((list 'ast-stmt expr) (%parse-structured-c-stmt (c-macroexpand (eval expr))))
    ((list* 'deftype _) nil)
    (_ (make-instance 'c-expr-stmt :expr (parse-c-expr decl)))))

(defun analyze-defun-prototype (proto)
  (let ((proto-pairs (flatten-field-list proto))
	return-type
	formals
	variadic-p)
    (dolist (pair proto-pairs)
      (cond ((eq '|...| (car pair))
	     (setf variadic-p t))
	    ((eq 'return (car pair))
	     (setf return-type (eval-c-type (cdr pair))))
	    (t
	     (push (cons (car pair) (eval-c-type (cdr pair))) formals))))
    (values (or return-type *void-type-instance*)
	    (nreverse formals)
	    variadic-p)))

(defun %phase1-defun (id prototype &key external attributes)
  (multiple-value-bind (return-type formals variadic-p)
      (analyze-defun-prototype prototype)
    (let* ((arg-types (loop for x in formals collect (cdr x)))
	   (fn-type (get-function-type arg-types return-type variadic-p)))
      (set-global-gv
       (cond
	 (external (make-defun/extern-gval id fn-type attributes))
	 (t (make-defun-gval id fn-type)))))))

(defun %phase1-tagged-type (k/w id clauses)
  (let ((kind (ecase k/w (defstruct :struct) (defunion :union))))
    (add-tagged-type kind id (%parse-defstruct-fields clauses))))

(defun %phase1-deftype (id type-decl)
  (let ((gval (make-type-gval id (eval-c-type type-decl))))
    (set-global-gv gval)))

(defun parse-c-stmt-phase1 (decl)
  "Populate the global namespace with type information about functions and types from DECL."
  (match decl
    ;; FIXME: Variadic support
    ((list* 'defun id (and (list* _) proto) _) (%phase1-defun id proto))
    ((list* (and (or 'defstruct 'defunion) k/w) id clauses) (%phase1-tagged-type k/w id clauses))
    ((list* 'deftype id type-decl) (%phase1-deftype id type-decl))
    ((list* 'defun/extern id (and (list* _) proto) attrs) (%phase1-defun id proto :external t :attributes attrs)))
  (values))

(defun parse-c-stmt (decl)
  "Parse the statment DECL into a proper c-stmt AST node. Alias
symbols will be resolved."
  (if (not (listp decl))
      (make-instance 'c-expr-stmt :expr (parse-c-expr decl))
      (%parse-structured-c-stmt decl)))
