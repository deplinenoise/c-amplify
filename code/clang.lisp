
;;;; C language parser

(in-package #:se.defmacro.c-amplify)

(defgeneric generate-code (ast)
  (:documentation
"Print code for the datum passed which may be a c-type or ast-node or
a list of such objects for convenience."))

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

;;; AST nodes

(defgeneric simplify (ast)
  (:documentation "Visit the ast-node AST and try to simplify it"))

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
  (dolist (child (ast-children self))
    (simplify child)
    self))

(defun c-expr-p (datum)
  (typep datum 'c-expr))

(defun c-expr-list-p (datum)
  (and (listp datum)
       (every #'c-expr-p datum)))

(defun c-type-p (datum)
  (typep datum 'c-type))

(defun c-stmt-p (datum)
  (typep datum 'c-stmt))

(defun c-stmt-list-p (datum)
  (and (listp datum)
       (every #'c-stmt-p datum)))

(deftype c-expr-list () '(satisfies c-expr-list-p))
(deftype c-stmt-list () '(satisfies c-stmt-list-p))

(defclass c-expr (ast-node) ()
  (:documentation "The base class for AST nodes representing expressions."))

(defgeneric expr-typeof (expr)
  (:documentation "Returns the type (a C-TYPE instance) of EXPR,
  taking into account rules such as arithmetic type promotion."))

(defclass c-unary-expr (c-expr)
  ((operand :type c-expr :initarg :operand :accessor operand))
  (:documentation "Base class for unary operators"))

(defclass c-binary-expr (c-expr)
  ((lhs :type c-expr :initarg :lhs :accessor lhs)
   (rhs :type c-expr :initarg :rhs :accessor rhs))
  (:documentation "Base class for binary operators"))

(defvar *cg-newline* (gensym "newline-symbol")
  "Symbol used to represent newlines")

(defvar *cg-optional-separator* (gensym "optional-separator")
  "Symbol used to represent optional spaces before opening
  braces (those we don't want printed at the start of a fresh line)")

(defvar *cg-freshline* (gensym "freshline-symbol")
  "Symbol used to represent newlines on non-blank lines")

(defparameter *outer-precedence* 1000)
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

(defmacro %cg-block-around (&rest body)
  `(progn
     (%cg-print *cg-begin-block*)
     (let ((*compound-depth* (1+ *compound-depth*)))
       ,@body)
     (%cg-print *cg-end-block*)))

(defmacro %cg-stmt-around (&rest body)
  `(let ((*stmt-depth* (1+ *stmt-depth*)))
     (%cg-print *cg-freshline*)
     ,@body))

(defmacro %cg-expr-scope (prec &rest body)
  (with-gensyms (need-parens prec-once)
    `(let* ((,prec-once ,prec)
	    (*expr-depth* (1+ *expr-depth*))
	    (,need-parens (> ,prec-once *outer-precedence*))
	    (*outer-precedence* ,prec-once))
       (when ,need-parens
	 (%cg-print "("))
       (progn
	 ,@body)
       (when ,need-parens
	 (%cg-print ")")))))

(defun %emit-comma-sequence (args)
  (loop
     for emit-comma = nil then t
     for operand in args
     do (progn
	  (when emit-comma
	    (%cg-print ", "))
	  (%cg-print operand)))
  (values))

(defun %quote-c-string (str)
  (with-output-to-string (quoted-value)
    (princ #\")
    (loop for ch across str
       do (cond ((eql #\\ ch) (princ "\\\\"))
		((eql #\" ch) (princ "\\\""))
		(t (princ ch))))
    (princ #\")
    quoted-value))

(defun %parse-type (e)
  (match e
    ((list 'type-of expr) (expr-typeof (parse-c-expr expr)))
    (_ (eval-c-type e))))

(defun %select-field (struct-type field)
  (let ((base-type (base-type-of struct-type)))
    (when (typep base-type 'c-pointer-type)
      (setf base-type (base-type-of (remove-ptr base-type))))
    (unless (typep base-type 'c-structured-type)
      (error "cannot select field ~a from non-structured type ~a" field base-type))
    (get-struct-field-type base-type field)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *expression-types* nil))

(defun/compile-time %gen-printer (self-sym class-name clauses slot-names)
  (unless (getf clauses :precedence)
    (error "no precedence specified for ~a" class-name))
  `(defmethod generate-code ((,self-sym ,class-name))
     (let ((*outer-precedence* ,(getf clauses :precedence)))
       ,(let ((emit-expr (getf clauses :emit)))
	     (cond
	       ((null emit-expr) (error "no :emit clause specified"))
	       ((eq 'function (car emit-expr)) ; if :emit specifies a function, use that
		`(funcall ,emit-expr ,self-sym))
	       (t `(with-slots ,slot-names ,self-sym
		     (%cg-print ,@emit-expr))))))))

(defun/compile-time %gen-expr-type (base-type self-sym class-name clauses slot-names)
  (let ((expr (getf clauses :expr-type)))
    `(defmethod expr-typeof ((,self-sym ,class-name))
       (with-slots ,slot-names ,self-sym
	 ,(cond
	   ((eq :promote expr)
	    (ecase base-type
	      (c-unary-expr '(arit-promote-unary (expr-typeof operand)))
	      (c-binary-expr '(arit-promote-binary (expr-typeof lhs) (expr-typeof rhs)))))
	   ((and (listp expr) (eq 'function (car expr)))
	    `(funcall ,expr ,self-sym))
	   ((not (null expr)) expr)
	   (t (error "no suitable expr-type expression defined for ~a" class-name)))))))

(defun/compile-time %record-expr-type (base-type class-name clauses)
  (pushnew class-name *expression-types*)
  (setf (get class-name 'c-amplify-parse-order) (eval (getf clauses :parse-order 0)))
  (with-gensyms (lhs rhs operand)
    (let ((parse-clause (getf clauses :parse))
	  (symbol-clause (getf clauses :symbol)))
      (setf (get class-name 'c-amplify-parser)
	    (cond (parse-clause parse-clause)
		  ((and (eq 'c-unary-expr base-type) symbol-clause)
		   `((list ',symbol-clause ,operand) (make-instance ',class-name
								    :operand (parse-c-expr ,operand))))
		  ((and (eq 'c-binary-expr base-type) symbol-clause)
		   `((list ',symbol-clause ,lhs ,rhs) (make-instance ',class-name
								     :lhs (parse-c-expr ,lhs)
								     :rhs (parse-c-expr ,rhs))))
		  (t (error "unsupported kind ~a" base-type)))))))

(defmacro %def-n-ary-expr (base-type class-name clauses)
  (with-gensyms (self)
    (let ((slot-names (if (eq base-type 'c-unary-expr) '(operand) '(lhs rhs))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%record-expr-type ',base-type ',class-name ',clauses)
	   (export-symbol-to-c ',(getf clauses :symbol)))
	 (defclass ,class-name (,base-type) ())
	 ,(%gen-printer self class-name clauses slot-names)
	 ,(%gen-expr-type base-type self class-name clauses slot-names)))))

(defmacro def-unary-expr (class-name &rest clauses)
  `(%def-n-ary-expr c-unary-expr ,class-name ,clauses))

(defmacro def-binary-expr (class-name &rest clauses)
  `(%def-n-ary-expr c-binary-expr ,class-name ,clauses))

(defmacro def-custom-expr (class-name &rest clauses)
  (with-gensyms (self)
    (let ((slot-names (loop for slot-spec in (getf clauses :slots) collecting (car slot-spec))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (%record-expr-type 'c-expr ',class-name ',clauses))
	 (defclass ,class-name (c-expr) ,(getf clauses :slots))
	 ,(%gen-printer self class-name clauses slot-names)
	 ,(%gen-expr-type 'c-expr self class-name clauses slot-names)))))

(defun/compile-time %get-parse-order-vec ()
  (sort (coerce *expression-types* 'vector)
	#'(lambda (a b)
	    (<
	     (get a 'c-amplify-parse-order)
	     (get b 'c-amplify-parse-order)))))

(defmacro emit-expr-parser (function-name)
  (with-gensyms (expr)
    `(defun ,function-name (,expr)
       (match ,expr ,@(loop for type-name across (%get-parse-order-vec)
			 collecting (get type-name 'c-amplify-parser))))))

(def-custom-expr c-string-literal-expr
    :precedence 1
    :slots ((value :type string :initarg :value :accessor literal-value))
    :parse ((type string a) (make-instance 'c-string-literal-expr :value a))
    :expr-type (eval-c-type '(ptr const char))
    :emit #'(lambda (self) (%cg-print (%quote-c-string (slot-value self 'value)))))

(def-custom-expr c-integer-literal-expr
    :precedence 1
    :slots ((value :type integer :initarg :value :accessor literal-value))
    :parse ((type integer a) (make-instance 'c-integer-literal-expr :value a))
    :expr-type (eval-c-type 'int)
    :emit (value))

(def-custom-expr c-float-literal-expr
    :precedence 1
    :slots ((value :type float :initarg :value :accessor literal-value))
    :parse ((type float a) (make-instance 'c-float-literal-expr :value a))
    :expr-type (eval-c-type 'float)
    :emit (value))

(def-custom-expr c-variable-expr
    :precedence 1
    :slots ((var-name :type symbol :initarg :var-name :accessor var-name))
    :parse ((type symbol a) (make-instance 'c-variable-expr :var-name a))
    :expr-type #'(lambda (self)
		   (%type-of-lexical-symbol (slot-value self 'var-name) (ast-env self)))
    :emit (var-name))

(def-custom-expr c-member-expr
    :precedence 1
    :slots ((struct-expr :type c-expr :initarg :struct-expr :accessor struct-expr)
	    (member-names :type list :initarg :member-names :accessor member-names))
    :parse ((list* '-> struct-expr symbols)
	    (make-instance 'c-member-expr
			   :struct-expr (parse-c-expr struct-expr)
			   :member-names symbols))
    :expr-type #'(lambda (self)
		   (with-slots (struct-expr member-names) self
		     (let* ((btype (base-type-of (expr-typeof struct-expr)))
			    (struct-type btype))
		       (loop for symbol in member-names
			  do (setf struct-type (%select-field struct-type symbol)))
		       struct-type)))

    :emit #'(lambda (self)
	      (with-slots (struct-expr member-names) self
		(%cg-expr-scope
		 1
		 (%cg-print struct-expr)
		 (let* ((et (expr-typeof struct-expr))
			(ct (base-type-of et)))
		   (loop for symbol in member-names
		      do (progn
			   (if (typep (base-type-of ct) 'c-pointer-type)
			       (%cg-print "->" symbol)
			       (%cg-print "." symbol))
			   (setf ct (%select-field ct symbol)))))))))

(def-unary-expr c-post-increment-expr :precedence 1 :symbol ++post :emit (operand "++") :expr-type (expr-typeof operand))
(def-unary-expr c-post-decrement-expr :precedence 1 :symbol --post :emit (operand "--") :expr-type (expr-typeof operand))

(def-binary-expr c-subscript-expr
    :precedence 1
    :symbol aref
    :emit (lhs "[" rhs "]")
    :expr-type #'(lambda (self)
		   (with-slots (lhs) self
		     (let ((lt (expr-typeof lhs)))
		       (remove-ptr lt))))) ; fixme: support arrays

(def-unary-expr c-pre-increment-expr :precedence 2 :symbol ++ :assoc right :emit ("++" operand) :expr-type (expr-typeof operand))
(def-unary-expr c-pre-decrement-expr :precedence 2 :symbol -- :assoc right :emit ("--" operand) :expr-type (expr-typeof operand))
(def-unary-expr c-plus-expr :precedence 2 :symbol + :assoc right :emit ("+" operand) :expr-type :promote)
(def-unary-expr c-minus-expr :precedence 2 :symbol - :assoc right :emit ("-" operand) :expr-type :promote)
(def-unary-expr c-not-expr :precedence 2 :symbol ! :assoc right :emit ("!" operand) :expr-type (eval-c-type 'int))
(def-unary-expr c-complement-expr :precedence 2 :symbol ~ :assoc right :emit ("~" operand) :expr-type :promote)
(def-unary-expr c-dereference-expr :precedence 2 :symbol * :assoc right :emit ("*" operand) :expr-type (remove-ptr (expr-typeof operand)))
(def-unary-expr c-address-of-expr :precedence 2 :symbol & :assoc right :emit ("&" operand) :expr-type (add-ptr (expr-typeof operand)))

(def-custom-expr c-sizeof-type-expr
    :precedence 2
    :slots ((source-type :accessor source-type :type c-type :initarg :source-type))
    :parse ((list* 'sizeof-type type-spec)
	    (make-instance 'c-sizeof-type-expr
			   :source-type (eval-c-type type-spec)))
    :expr-type (eval-c-type 'size-t)
    :emit ("sizeof(" source-type ")"))

(def-custom-expr c-cast-expr
    :precedence 2
    :slots ((target-type :accessor target-type :type c-type :initarg :target-type)
	    (operand :accessor operand :type c-expr :initarg :operand))
    :parse ((list 'cast type expr)
	    (make-instance 'c-cast-expr
			   :target-type (eval-c-type type)
			   :operand (parse-c-expr expr)))
    :expr-type target-type
    :emit ("(" target-type ") " operand))

(def-binary-expr c-multiply-expr :precedence 3 :symbol * :emit (lhs " * " rhs) :expr-type :promote)
(def-binary-expr c-divide-expr :precedence 3 :symbol / :emit (lhs " / " rhs) :expr-type :promote)
(def-binary-expr c-modulo-expr :precedence 3 :symbol % :emit (lhs " % " rhs) :expr-type :promote)

(def-binary-expr c-add-expr :precedence 4 :symbol + :emit (lhs " + " rhs) :expr-type :promote)
(def-binary-expr c-subtract-expr :precedence 4 :symbol - :emit (lhs " - " rhs) :expr-type :promote)

(def-binary-expr c-shift-left-expr :precedence 5 :symbol << :emit (lhs " << " rhs) :expr-type :promote)
(def-binary-expr c-shift-right-expr :precedence 5 :symbol >> :emit (lhs " >> " rhs) :expr-type :promote)

(def-binary-expr c-less-expr :precedence 5 :symbol < :emit (lhs " < " rhs) :expr-type (eval-c-type 'int))
(def-binary-expr c-less-equal-expr :precedence 5 :symbol <= :emit (lhs " <= " rhs) :expr-type (eval-c-type 'int))
(def-binary-expr c-greater-expr :precedence 5 :symbol > :emit (lhs " > " rhs) :expr-type (eval-c-type 'int))
(def-binary-expr c-greater-equal-expr :precedence 5 :symbol >= :emit (lhs " >= " rhs) :expr-type (eval-c-type 'int))

(def-binary-expr c-equals-expr :precedence 7 :symbol == :emit (lhs " == " rhs) :expr-type (eval-c-type 'int))
(def-binary-expr c-not-equals-expr :precedence 7 :symbol != :emit (lhs " != " rhs) :expr-type (eval-c-type 'int))

(def-binary-expr c-bitwise-and-expr :precedence 8 :symbol bit-and :emit (lhs " & " rhs) :expr-type :promote)
(def-binary-expr c-bitwise-xor-expr :precedence 9 :symbol bit-xor :emit (lhs " ^ " rhs) :expr-type :promote)
(def-binary-expr c-bitwise-or-expr :precedence 10 :symbol bit-or :emit (lhs " | " rhs) :expr-type :promote)

(def-binary-expr c-logical-and-expr :precedence 11 :symbol and :emit (lhs " && " rhs) :expr-type (eval-c-type 'int))
(def-binary-expr c-logical-or-expr :precedence 12 :symbol or :emit (lhs " || " rhs) :expr-type (eval-c-type 'int))


(def-binary-expr c-assign-expr :precedence 13 :symbol = :assoc right :emit (lhs " = " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-add-assign-expr :precedence 13 :symbol += :assoc right :emit (lhs " += " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-subtract-assign-expr :precedence 13 :symbol -= :assoc right :emit (lhs " -= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-multiply-assign-expr :precedence 13 :symbol *= :assoc right :emit (lhs " *= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-divide-assign-expr :precedence 13 :symbol /= :assoc right :emit (lhs " /= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-modulus-assign-expr :precedence 13 :symbol %= :assoc right :emit (lhs " %= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-bitwise-and-assign-expr :precedence 13 :symbol bit-and-= :assoc right :emit (lhs " &= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-bitwise-xor-assign-expr :precedence 13 :symbol bit-xor-= :assoc right :emit (lhs " ^= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-bitwise-or-assign-expr :precedence 13 :symbol bit-or-= :assoc right :emit (lhs " -= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-shift-left-assign-expr :precedence 13 :symbol <<= :assoc right :emit (lhs " <<= " rhs) :expr-type (expr-typeof lhs))
(def-binary-expr c-shift-right-assign-expr :precedence 13 :symbol >>= :assoc right :emit (lhs " >>= " rhs) :expr-type (expr-typeof lhs))

(def-custom-expr c-comma-expr
    :precedence 13
    :slots ((operands :type c-expr-list :accessor operands :initarg :operands))
    :parse ((list* 'comma operands)
	    (make-instance 'c-comma-expr
			   :operands (mapcar #'parse-c-expr operands)))
    :expr-type (expr-typeof (last operands))
    :emit #'(lambda (self)
	      (%cg-expr-scope
	       13
	       (%emit-comma-sequence (slot-value self 'operands)))))

(def-custom-expr c-ternary-expr
    :precedence 13
    :slots ((test-expr :type c-expr :accessor test-expr :initarg :test-expr)
	    (true-expr :type c-expr :accessor true-expr :initarg :true-expr)
	    (false-expr :type c-expr :accessor false-expr :initarg :false-expr))
    :expr-type (expr-typeof true-expr)
    :parse ((list '? test-expr true-expr false-expr)
	    (make-instance 'c-ternary-expr
			   :test-expr (parse-c-expr test-expr)
			   :true-expr (parse-c-expr true-expr)
			   :false-expr (parse-c-expr false-expr)))
    :emit (test-expr "? " true-expr " : " false-expr))

(def-custom-expr c-funcall-expr
    :precedence 1
    :slots ((fun-expr :type c-expr :accessor fun-expr :initarg :fun-expr :accessor fun-expr)
	    (args :type c-expr-list :accessor args :initarg :args :accessor fun-args))
    :parse ((list* fun-expr args)
	    (make-instance 'c-funcall-expr
			   :fun-expr (parse-c-expr fun-expr)
			   :args (mapcar #'parse-c-expr args)))
    :parse-order 100 ; keep this last to avoid clobbering builtins
    :expr-type #'(lambda (self)
		   (with-slots (fun-expr) self
		     (let ((tt (expr-typeof fun-expr)))
		       (unless (typep tt 'c-function-type)
			 (error "can only call function type value: ~a" tt))
		       (return-type tt))))
    :emit #'(lambda (self)
	      (%cg-expr-scope
	       1
	       (with-slots (fun-expr args) self
		 (%cg-print fun-expr "(")
		 (let ((*outer-precedence* 13))
		   (%emit-comma-sequence args))
		 (princ ")")))))

(emit-expr-parser parse-c-expr)

(defclass c-stmt (ast-node) ())

(defclass c-null-stmt (c-stmt) ())

(defmethod ast-children ((self c-null-stmt))
  nil)

(defmethod generate-code ((self c-null-stmt))
  (when (> *stmt-depth* 0)
    (princ ";")))

(defclass c-compound-stmt-base (c-stmt)
  ((body :type c-stmt-list :initarg :body :accessor c-compound-body))
  (:documentation "Superclass for statements that contain other statements."))

(defun %emit-compound-body (compound-stmt)
  (%cg-block-around
   (%cg-print (c-compound-body compound-stmt))))

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
  (%cg-print "break;"))

(defclass c-continue-stmt (c-stmt) ())

(defmethod generate-code ((self c-continue-stmt))
  (%cg-print "continue;"))

(defclass c-do-while-stmt (c-stmt)
  ((test-expr :initarg :test-expr :accessor test-expr :type c-expr)
   (stmt :initarg :stmt :accessor stmt :type c-stmt)))

(defmethod generate-code ((self c-do-while-stmt))
  (with-slots (test-expr stmt) self
    (%cg-print "do" stmt "while (" test-expr ");")))

(defclass c-expr-stmt (c-stmt)
  ((expr :type c-expr :initarg :expr :accessor expr)))

(defmethod generate-code ((self c-expr-stmt))
  (with-slots (expr) self
    (%cg-stmt-around
     (%cg-print expr ";"))))

(defclass c-for-stmt (c-compound-stmt-base)
  ((init-expr :type c-expr :initarg :init-expr :accessor init-expr)
   (test-expr :type c-expr :initarg :test-expr :accessor test-expr)
   (iter-expr :type c-expr :initarg :iter-expr :accessor iter-expr)))

(defmethod generate-code ((self c-for-stmt))
  (with-slots (init-expr test-expr iter-expr) self
    (%cg-stmt-around
     (%cg-print "for (" init-expr "; " test-expr "; " iter-expr ")")
     (%emit-compound-body self))))

(defclass c-goto-stmt (c-stmt)
  ((label :type symbol :initarg :label :accessor label)))

(defmethod generate-code ((self c-goto-stmt))
  (with-slots (label) self
    (%cg-stmt-around
     (%cg-print "goto " label ";"))))

(defmethod ast-children ((self c-goto-stmt))
  nil)

(defclass c-label-stmt (c-stmt)
  ((label :type symbol :initarg :label :accessor label)))

(defmethod generate-code ((self c-label-stmt))
  (with-slots (label) self
    (%cg-stmt-around
     (%cg-print *cg-freshline* label ":"))))

(defmethod ast-children ((self c-label-stmt))
  nil)

(defclass c-if-stmt (c-stmt)
  ((test-expr :type c-expr :initarg :test-expr :accessor test-expr)
   (true-stmt :type c-stmt :initarg :true-stmt :accessor true-stmt)
   (false-stmt :type (or c-stmt null) :initform nil :initarg :false-stmt :accessor false-stmt)))

(defmethod generate-code ((self c-if-stmt))
  (with-slots (test-expr true-stmt false-stmt) self
    (%cg-stmt-around
     (%cg-print "if (" test-expr ")" true-stmt)
     (when false-stmt
       (%cg-print "else" false-stmt)))))

(defclass c-return-stmt (c-stmt)
  ((expr :type (or c-expr null) :initarg :expr :initform nil :accessor expr)))

(defmethod generate-code ((self c-return-stmt))
  (with-slots (expr) self
    (%cg-stmt-around
     (%cg-print "return")
     (when expr
       (%cg-print " " expr))
     (%cg-print ";"))))

(defclass c-declarator (ast-node)
  ((name :type symbol :initarg :name :accessor name)
   (decl-type :type c-type :initarg :decl-type :accessor decl-type)
   (init-expr :type (or c-expr null) :initarg :init-expr :initform nil :accessor init-expr)))

(defmethod generate-code ((self c-declarator))
  (with-slots (name decl-type init-expr) self
    ;; FIXME: need a (emit-declaration ...) to make function types
    ;; work, because there the type surrounds the identifier
    (%cg-stmt-around
     (%cg-print decl-type " " name)
     (when init-expr
       (%cg-print " = " init-expr)))))

(defclass c-declare-stmt (c-stmt)
  ((declarators :type list :initarg :declarators :accessor declarators)))

(defmethod generate-code ((self c-declare-stmt))
  (with-slots (declarators) self
    (dolist (d declarators)
      (%cg-print d ";"))))

(defclass c-defstruct-node (ast-node)
  ((struct-type :type c-structured-type :initarg :struct-type :accessor struct-type)))

(defmethod generate-code ((self c-defstruct-node))
  (with-slots (struct-type) self
    (with-slots (fields kind name) struct-type
      (%cg-print *cg-toplevel-spacing* (format nil "~(~a~)" kind) " " name)
      (%cg-block-around
       (dolist (f fields)
	 (%cg-print *cg-freshline* (cdr f) " " (car f) ";")))
      (%cg-print ";"))))

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
      (%cg-print *cg-toplevel-spacing*)
      (%cg-print return-type)
      (%cg-print *cg-defun-return-type-separator*)
      (%cg-print symbol "(")
      (loop
	 for formal in formals
	 for argtype in argument-types
	 for comma-required-p = nil then t
	 do (progn
	      (when comma-required-p
		(%cg-print ", "))
	      (%cg-print argtype " " formal)))
      (when variadic-p
	(when (not (null formals))
	  (%cg-print ", "))
	(%cg-print "..."))
      (%cg-print ")")
      (%cg-block-around
       (%cg-print body)))))

(defmethod simplify ((self c-defun-node))
  (with-slots (body) self
    (setf body (inline-compound-bodies body (ast-env self))))
  self)

(defmethod generate-code ((list list))
  (%cg-print list)
  (values))

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
    progn break continue goto label declare defstruct defunion defun
    deftype cast sizeof sizeof-type struct union ptr const volatile
    restrict ->))

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
    ((list* 'defun id prototype body)
     (%phase2-defun id prototype body))
    ((list* (and (or 'defstruct 'defunion) k/w) id _)
     (make-instance 'c-defstruct-node
		    :struct-type (%lookup-tagged-type (case k/w
							(defstruct 'struct)
							(defunion 'union)) id)))
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

(defun %phase1-defun (id prototype)
  (multiple-value-bind (return-type formals variadic-p)
      (analyze-defun-prototype prototype)
    (let* ((arg-types (loop for x in formals collect (cdr x)))
	   (fn-type (get-function-type arg-types return-type variadic-p))
	   (gval (make-instance 'gval
				:symbol id
				:kind :defun
				:type fn-type)))
      (set-global-gv gval))))

(defun %phase1-tagged-type (k/w id clauses)
  (let ((kind (ecase k/w (defstruct :struct) (defunion :union))))
    (add-tagged-type kind id (%parse-defstruct-fields clauses))))

(defun %phase1-deftype (id type-decl)
  (let ((gv (make-instance 'gval
			   :kind :type
			   :type (eval-c-type type-decl)
			   :symbol id)))
    (set-global-gv gv)))

(defun parse-c-stmt-phase1 (decl)
  "Populate the global namespace with type information about functions and types from DECL."
  (match decl
    ;; FIXME: Variadic support
    ((list* 'defun id (and (list* _) proto) _) (%phase1-defun id proto))
    ((list* (and (or 'defstruct 'defunion) k/w) id clauses) (%phase1-tagged-type k/w id clauses))
    ((list* 'deftype id type-decl) (%phase1-deftype id type-decl)))
  (values))

(defparameter *dump-ast-level* 0)

(defgeneric ast-children (ast)
  (:documentation "Return a list of child nodes for the specified AST."))

(defmethod ast-children ((ast c-unary-expr))
  (with-slots (operand) ast
    (list operand)))

(defmethod ast-children ((ast c-funcall-expr))
  (cons (fun-expr ast) (fun-args ast)))

(defmethod ast-children ((ast c-binary-expr))
  (with-slots (lhs rhs) ast
    (list lhs rhs)))

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

(defun dump-ast (ast)
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

(defun parse-c-stmt (decl)
  "Parse the statment DECL into a proper c-stmt AST node. Alias
symbols will be resolved."
  (if (not (listp decl))
      (make-instance 'c-expr-stmt :expr (parse-c-expr decl))
      (%parse-structured-c-stmt decl)))
