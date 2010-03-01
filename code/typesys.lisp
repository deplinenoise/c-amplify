(in-package :se.defmacro.c-amplify)

(defconstant +const-bit+ 1)
(defconstant +volatile-bit+ 2)
(defconstant +restrict-bit+ 4)

(defparameter *current-struct-type* nil
  "Set to the type of the current defstruct in 2nd-phase defstruct, defunion parsing")

(defparameter *current-return-type* nil
  "Set to the type of the current function in 2nd-phase defun parsing")

(defparameter *c-struct-namespace* (make-hash-table) "Namespace for structures and unions")

(defparameter *c-int-rank* 3)

(defparameter *c-basic-types* `((char 1 :unspecified "char")
				(s-char 1 :signed "signed char")
				(u-char 1 :unsigned "unsigned char")
				(short 2 :signed "short")
				(u-short 2 :signed "short")
				(int ,*c-int-rank* :signed "int")
				(u-int 4 :unsigned "unsigned int")
				(long 5 :signed "long")
				(u-long 6 :unsigned "unsigned long")
				(float 10 :signed "float")
				(double 11 :signed "double")
				(long-double 11 :signed "long double")))

;;; C type classes

(defparameter *type-index* 0)

(defun %next-type-index ()
  (incf *type-index*))

(defclass c-type ()
  ((derivations
    :initform nil
   :documentation "Optional hash table mapping variation keys (integer bit fields) to derived and pointer types.")
   (pointer-type
    :initform nil
    :documentation "The type of a pointer to this type, created on demand.")
  (%type-index
   :type integer
   :initform (%next-type-index)))
  (:documentation "Abstract base class for type expressions."))

(defclass c-void-type (c-type)
  ())

(defclass c-basic-type (c-type)
  ((name
    :type symbol
    :initarg :name
    :reader name
    :documentation "The symbol representing this basic type, e.g. char")
   (rank
    :type fixnum
    :initarg :rank
    :reader rank
    :documentation "The rank of this type in artihmetic conversions.")
   (sign-type
    :type symbol
    :initarg :sign-type
    :reader sign-type
    :documentation "One of :signed, :unsigned or :unspecified")
   (appearance
    :type string
    :initarg :appearance
    :reader appearance
    :documentation "The printed appearance of the type.")))

(defclass c-derived-type (c-type)
  ((qualifier-mask
    :type integer
    :initarg :qualifier-mask)
   (base-type
    :type c-type
    :initarg :base-type
    :documentation "The basic or structured type this derivation stems from")))

(defclass c-pointer-type (c-type)
  ((pointed-to-type
    :type c-type
    :initarg :pointed-to-type
    :documentation "The pointer's underlying type"
    :reader pointed-to-type)))

(defclass c-function-type (c-type)
  ((return-type
    :type c-type
    :reader return-type
    :initarg :return-type)
   (argument-types
    :type list
    :initarg :argument-types
    :reader argument-types
    :documentation "List of argument types")
   (is-variadic
    :type boolean
    :reader variadic-p
    :initarg :is-variadic
    :documentation "True if the function type accepts vararg-style arguments."))
  (:documentation "Describes the type of a function."))

(defclass c-structured-type (c-type)
  ((fields
    :type list
    :initarg :fields
    :accessor struct-fields
    :documentation "A list of fields of the type; each field is represented by a cons (name . type)")
   (kind
    :type symbol
    :initarg :kind
    :accessor struct-kind
    :documentation "The kind of structured type, either STRUCT or UNION")
   (name
    :type symbol
    :initarg :name))
  (:documentation "Describes a structured type (union or struct)")) 

(defun get-struct-field-type (struct-type symbol)
  (declare (type c-structured-type struct-type) (type symbol symbol))
  (let ((field (assoc symbol (struct-fields struct-type))))
    (unless field 
      (error "->: ~a has no field ~a" struct-type symbol))
    (cdr field)))

;;; Printing objects in the REPL

(defmethod print-object ((self c-type) stream)
  (let ((*standard-output* stream))
    (format t "#<~a \"" (class-name (class-of self)))
    (emit-c-type self)
    (format t "\">")))

(defun lisp-id->string (symbol)
  (map 'string
       (lambda (ch) (if (eql ch #\-) #\_ ch))
       (string-downcase (symbol-name symbol))))

;;; Formatted type emission

(defun c-type->string (type-obj &optional declared-name)
  (with-output-to-string (*standard-output*)
    (emit-c-type type-obj declared-name)))

(defgeneric emit-c-type (object &optional declared-name)
  (:documentation "Emit a c-type subtype OBJECT. If DECLARED-NAME is non-nil, a declaration of that name is emitted rather than just the type."))

(defmethod emit-c-type ((self c-void-type) &optional declared-name)
  (princ "void")
  (when declared-name
    (error "cannot declare void variabled")))

(defun %maybe-emit-name (declared-name)
  (when declared-name
    (format t " ~a" declared-name)))

(defmethod emit-c-type ((self c-basic-type) &optional declared-name)
  (with-slots (appearance) self
    (princ appearance))
  (%maybe-emit-name declared-name))

(defmethod emit-c-type ((self c-derived-type) &optional declared-name)
  (with-slots (qualifier-mask base-type) self
    (emit-c-type base-type)
    (macrolet ((print-qualifier (bit string)
		 `(when (/= 0 (logand ,bit qualifier-mask))
		    (format t " ~a" ,string))))
      (print-qualifier +const-bit+ "const")
      (print-qualifier +volatile-bit+ "volatile")
      (print-qualifier +restrict-bit+ "restrict")))
  (%maybe-emit-name declared-name))

(defmethod emit-c-type ((self c-pointer-type) &optional declared-name)
  (with-slots (pointed-to-type) self
    (emit-c-type pointed-to-type)
    (princ " *"))
  (%maybe-emit-name declared-name))

(defmethod emit-c-type ((self c-function-type) &optional declared-name)
  (with-slots (return-type argument-types is-variadic) self
    (emit-c-type return-type)
    (princ " (*")
    (%maybe-emit-name declared-name)
    (princ ")(")
    (let ((first-arg t))
      (dolist (arg-type argument-types)
	(if first-arg
	    (setf first-arg nil)
	    (princ ", "))
	(emit-c-type arg-type))
      (when is-variadic
	(unless first-arg
	  (princ ", "))
	(princ "..."))
      (princ ")"))))

(defmethod emit-c-type ((self c-structured-type) &optional declared-name)
  (with-slots (kind name) self
    (format t "~a ~a" (lisp-id->string kind) (lisp-id->string name)))
  (%maybe-emit-name declared-name))

(defgeneric base-type-of (type))

(defmethod base-type-of ((self c-derived-type))
  (with-slots (base-type) self
    base-type))

(defmethod base-type-of ((self c-basic-type)) self)
(defmethod base-type-of ((self c-pointer-type)) self)
(defmethod base-type-of ((self c-void-type)) self)
(defmethod base-type-of ((self c-function-type)) self)
(defmethod base-type-of ((self c-structured-type)) self)

(defparameter *void-type-instance* (make-instance 'c-void-type))

(defun add-ptr (type-obj)
  "Given a c-type instance TYPE-OBJ, return the type representing a pointer to that type."
  (declare (type c-type type-obj))
  (with-slots (pointer-type) type-obj
    (unless pointer-type
      (setf pointer-type (make-instance 'c-pointer-type :pointed-to-type type-obj)))
    pointer-type))

(defun remove-ptr (ptr-type)
  "Given a c-pointer-type instance PTR-TYPE, return the type
dereferencing that pointer would yield."
  (declare (type c-type ptr-type))
  (let ((base-ptr-type (base-type-of ptr-type)))
    (unless (typep base-ptr-type 'c-pointer-type)
      (error "~a cannot be dereferenced as a pointer type" ptr-type))
    (pointed-to-type base-ptr-type)))
  
(defun add-cv (type-obj qualifier-mask)
  "Given a c-type instance TYPE, create a new (or find an existing)
variation of it using the specified QUALIFIERS."
  (declare (type c-type type-obj)
	   (type integer qualifier-mask))
  (if (= 0 qualifier-mask)
      type-obj			    ; no qualifiers added -- same type
      (let ((base-type (base-type-of type-obj)))
	(declare (type c-type base-type))
	(with-slots (derivations) base-type
	  (unless derivations
	    (setf derivations (make-hash-table)))
	  (multiple-value-bind (derived-type exists) (gethash qualifier-mask derivations)
	    (if exists
		derived-type
		(setf (gethash qualifier-mask derivations)
		      (make-instance 'c-derived-type
				     :base-type base-type
				     :qualifier-mask qualifier-mask))))))))

(defun lookup-type (sym)
  (assert sym)
  (macrolet ((test-special-var (sym special-var)
	       `(when (eq ,sym ',special-var)
		  (unless ,special-var
		    (error "~a is not currently bound" ',special-var))
		  (return-from lookup-type ,special-var))))
    (test-special-var sym *current-return-type*)
    (test-special-var sym *current-struct-type*))
  (let ((gv (lookup-gval sym)))
    (unless gv
      (error "~a: undefined symbol" sym))
    (unless (eq (gval-kind gv) :type)
      (error "~a: not a type" gv))
    (gval-type gv)))

(defun %scan-qualifiers (decl)
  "Parse leading const, volatile, restrict keywords from DECL, evaluating to two values. The first is a qualifier mask for the parsed keywords, and the second is the rest of DECL following the keywords."
  (let* ((qualifiers 0)
	 (rest 0))
    (loop for cons on decl for kw in decl
       do (progn
	    (setf rest cons)
	    (cond
	      ((eq 'const kw) (setf qualifiers (logor qualifiers +const-bit+)))
	      ((eq 'volatile kw) (setf qualifiers (logor qualifiers +volatile-bit+)))
	      ((eq 'restrict kw) (setf qualifiers (logor qualifiers +restrict-bit+)))
	      (t (loop-finish)))))
    (values qualifiers rest)))

(defun %ftype-key (arg-types return-type is-variadic)
  (append arg-types (list return-type is-variadic)))

(defun %hash-cfun-key (key)
  (let ((result 0))
    (declare (type fixnum result))
    (loop for item in key
       do (let ((iter (+ result (* 33
			 (if (typep item 'c-type)
			     (sxhash (slot-value item '%type-index))
			     (sxhash item))))))
	    (setf result (coerce (mod iter most-positive-fixnum) 'fixnum))))
    result))

(defparameter *c-function-types*
  (make-hash-table :test 'equal
		   :hash-function
		   #+sbcl #'%hash-cfun-key
		   #+clozure '%hash-cfun-key )
  "Maintains a mapping of signature lists to function types to enforce the c-type eq guarantee")

(defun get-function-type (arg-types return-type is-variadic)
  (let* ((key (%ftype-key arg-types return-type is-variadic))
	 (existing (gethash key *c-function-types*)))
    ;; (format t "key: ~a (hash ~a)~%" key (%hash-cfun-key key))
    (if existing
	existing
	(setf (gethash key *c-function-types*)
	      (make-instance 'c-function-type
			     :argument-types arg-types
			     :return-type return-type
			     :is-variadic is-variadic)))))

; (fn int (ptr const char) [...] => int)

(defun %parse-fn-type (decl)
  "Parse a list of the form ([<args=type-decl> ...] ['...] [=>
<return=type-decl>]), looking up (or creating a new) instance of
c-function-type."
  (let ((args nil)
	(return-type nil)
	(is-variadic nil))
    (loop for cons on decl for item in decl
       do (cond
	    ((eq '=> item) (progn
			     (let ((tail (cdr cons)))
			     (unless tail
			       (error "=> designator not followed by a type in ~a" decl))
			     (setf return-type (eval-c-type tail))
			     (loop-finish))))
	    ((eq '|...| item) (setf is-variadic t))
	    (t (push (eval-c-type item) args))))
    (get-function-type (nreverse args) (or return-type *void-type-instance*) is-variadic)))

(defun %add-pointer (type-obj)
  (with-slots (pointer-type) type-obj
    (if pointer-type
	pointer-type
	(setf pointer-type
	      (make-instance 'c-pointer-type
			     :pointed-to-type type-obj)))))

(defun %lookup-tagged-type (kind name)
  (let ((kind (ecase kind
		(struct :struct)
		(union :union))))
    (let ((type (gethash name *c-struct-namespace*)))
      (unless type
	(error "no such tagged type: ~a" name))
      (unless (eq (slot-value type 'kind) kind)
	(error "tagged type kind mismatch: ~a; expected ~a but found ~a" name (slot-value type 'kind) kind))
      type)))

(defun %eval-c-type-list (decl)
  (multiple-value-bind (qualifiers type-data) (%scan-qualifiers decl)
    (let* ((head (car type-data))
	   (result (cond
		     ((listp head) (%eval-c-type-list head))
		     ((not (symbolp head)) (error "illegal type declaration: ~a (~a)" decl head))
		     ((eq head 'ptr) (%add-pointer (eval-c-type (cdr type-data))))
		     ((member head '(struct union)) (%lookup-tagged-type (car type-data) (cadr type-data)))
		     ((eq head 'fn) (%parse-fn-type (cdr type-data)))
		     (t  (lookup-type head)))))
      (unless result
	(error "couldn't resolve type declaration: ~a" decl))
      (add-cv result qualifiers))))

(defun eval-c-type (decl)
  "Parse a type declaration DECL which may be either 1) a single
symbol indicating a typedef or builtin type 2) a list of possibly
nested lists combining the keywords const, volatile, restrict, ptr,
struct and union or 3) a c-type instance."
  (cond
    ((c-type-p decl) decl)
    ((symbolp decl) (lookup-type decl))
    ((listp decl) (%eval-c-type-list decl))
    (t (error "illegal type declaration: ~a" decl))))

(defun add-tagged-type (kind name fields)
  (assert (or (eq kind :struct) (eq kind :union)))
  (let ((obj (make-instance 'c-structured-type :name name :kind kind :fields fields)))
    (setf (gethash name *c-struct-namespace*) obj)))

(defun arit-promote-unary (ct)
  "Given a basic type CT, compute its promoted integer type."
  (declare (type c-type ct))
  (let ((real-type (base-type-of ct)))
    (unless (typep real-type 'c-basic-type)
      (error "~a is not a type suitable for arithmetic promotion" ct))
    (let ((rank (rank real-type)))
      (if (< rank *c-int-rank*)
	  (eval-c-type 'int)
	  real-type))))

(defun arit-promote-binary (ltype-in rtype-in)
  "Given two basic types LTYPE and TYPE, compute the promoted
arithmetic type of a binary expression involving them."
  (declare (type c-type ltype-in rtype-in))
  (let ((ltype (base-type-of ltype-in))
	(rtype (base-type-of rtype-in)))
    (flet ((verify-type (type-obj)
	     (unless (typep ltype 'c-basic-type)
	       (error "~a is not a type suitable for arithmetic promotion" type-obj))))
      (verify-type ltype)
      (verify-type rtype))
    (let ((lrank (rank ltype))
	  (rrank (rank rtype)))
      (cond
	;; If both types rank lower than int, the result is of type int.
	((and (< lrank *c-int-rank*)
	      (< rrank *c-int-rank*))
	 (eval-c-type 'int))
	;; Otherwise, just select the highest rank.
	((> lrank rrank) ltype)
	(t rtype)))))

(defun add-global-type (id type)
  (set-global-gv (make-instance 'gval
				:symbol id
				:kind :type
				:type type))
  (export-symbol-to-c id))
  

(defun init-typesys ()
  "Installs the basic types in the type system"
  (progn
    (loop for data in *c-basic-types*
       do (destructuring-bind (name rank sign-type appearance) data
	    (add-global-type name
			     (make-instance 'c-basic-type
					    :name name
					    :rank rank
					    :sign-type sign-type
					    :appearance appearance))))
    (add-global-type 'void *void-type-instance*))
  (values))
