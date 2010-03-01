(in-package #:se.defmacro.c-amplify)

;;;; Support for expression types

(defclass c-expr (ast-node) ()
  (:documentation "The base class for AST nodes representing expressions."))

(defun c-expr-p (datum)
  (typep datum 'c-expr))

(defun c-expr-list-p (datum)
  (and (listp datum)
       (every #'c-expr-p datum)))

(deftype c-expr-list ()
  '(satisfies c-expr-list-p))

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

;; This variable is a set of class names set up at compile time. It
;; lists classes that have a parse expression associated with their
;; symbol-plist that should be in the expression parser.
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
		     (generate-code* ,@emit-expr))))))))

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

(defun %emit-comma-sequence (args)
  (loop
     for emit-comma = nil then t
     for operand in args
     do (progn
	  (when emit-comma
	    (generate-code ", "))
	  (generate-code operand)))
  (values))

(defmacro %cg-expr-scope (prec &rest body)
  (with-gensyms (need-parens prec-once)
    `(let* ((,prec-once ,prec)
	    (*expr-depth* (1+ *expr-depth*))
	    (,need-parens (> ,prec-once *outer-precedence*))
	    (*outer-precedence* ,prec-once))
       (when ,need-parens
	 (generate-code "("))
       (progn
	 ,@body)
       (when ,need-parens
	 (generate-code ")")))))

(defun %quote-c-string (str)
  (with-output-to-string (quoted-value)
    (princ #\")
    (loop for ch across str
       do (cond ((eql #\\ ch) (princ "\\\\"))
		((eql #\" ch) (princ "\\\""))
		(t (princ ch))))
    (princ #\")
    quoted-value))


(def-custom-expr c-string-literal-expr
    :precedence 1
    :slots ((value :type string :initarg :value :accessor literal-value))
    :parse ((type string a) (make-instance 'c-string-literal-expr :value a))
    :expr-type (eval-c-type '(ptr const char))
    :emit #'(lambda (self) (generate-code (%quote-c-string (slot-value self 'value)))))

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

(defmethod gather-decls ((var-exp c-variable-expr) state)
  (let* ((sym (var-name var-exp))
	 (gv (lookup-gval sym)))
    (when gv
      (setf (gethash gv state) t))))

(defun %select-field (struct-type field)
  (let ((base-type (base-type-of struct-type)))
    (when (typep base-type 'c-pointer-type)
      (setf base-type (base-type-of (remove-ptr base-type))))
    (unless (typep base-type 'c-structured-type)
      (error "cannot select field ~a from non-structured type ~a" field base-type))
    (get-struct-field-type base-type field)))

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
		 (generate-code struct-expr)
		 (let* ((et (expr-typeof struct-expr))
			(ct (base-type-of et)))
		   (loop for symbol in member-names
		      do (progn
			   (if (typep (base-type-of ct) 'c-pointer-type)
			       (generate-code* "->" symbol)
			       (generate-code* "." symbol))
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
		 (generate-code* fun-expr "(")
		 (let ((*outer-precedence* 13))
		   (%emit-comma-sequence args))
		 (princ ")")))))

(emit-expr-parser parse-c-expr)

(defmethod ast-children ((ast c-unary-expr))
  (with-slots (operand) ast
    (list operand)))

(defmethod ast-children ((ast c-funcall-expr))
  (cons (fun-expr ast) (fun-args ast)))

(defmethod ast-children ((ast c-binary-expr))
  (with-slots (lhs rhs) ast
    (list lhs rhs)))

