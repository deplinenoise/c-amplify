(in-package :se.defmacro.c-amplify)

(defvar *macro-result-symbol* (gensym "macro-result"))

(defparameter *c-macros* (make-hash-table :test 'equalp)
  "Maps strings case insensitively to global C macros")

(defparameter *quoting-c-forms* '(ast-expr ast-expr-if ast-stmt ast-stmt-if)
  "Keywords for forms that are not macroexpanded but passed straight to the ast evaluator.")

(defvar *c-gensym-counter* 1)

(defun c-gensym (&optional label)
  (let ((hint-text (if label
		       (substitute #\_ #\- (format nil "~(~a~)" label))
		       "gensym_")))
    (intern (format nil "~a_~d_" hint-text (incf *c-gensym-counter*)))))

(defmacro with-c-gensyms (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (c-gensym ',s)))
     ,@body))

(defun %c-eval-macrolet-form (form)
  (destructuring-bind (name lambda-list &body body) form
    (with-gensyms (args env)
      (let* ((expander-code `#'(lambda (,args ,env)
				 (destructuring-bind ,lambda-list ,args
				  (values ,@body ,env))))
	     (expander-proc (eval expander-code)))
	(values name expander-proc)))))

(defun %c-install-expander (symbol proc)
  (setf (gethash (symbol-name symbol) *c-macros*) proc))

(defmacro def-c-macro (macro-name lambda-list &body body)
  (with-gensyms (g-form g-env g-lambda g-datum)
    `(%c-install-expander ',macro-name
			  #'(lambda (,g-form ,g-env)
			      (let ((,g-lambda
				     #'(lambda (,g-datum)
					 (destructuring-bind ,lambda-list ,g-datum
					   ,@body))))
				(values (funcall ,g-lambda ,g-form) ,g-env))))))

(defun %c-expand-macrolet (l env)
  (destructuring-bind (macrolet-form &body body-forms) l
    (let ((new-env env))
      (multiple-value-bind (symbol expander-proc) (%c-eval-macrolet-form macrolet-form)
	(push (cons (string-downcase (symbol-name symbol)) expander-proc) new-env))
      (values `(,*macro-result-symbol* ,@body-forms) new-env))))

(%c-install-expander 'macrolet #'%c-expand-macrolet)

(defun %c-env-lookup (env symbol)
  (let ((key (symbol-name symbol)))
    (or (cdr (assoc key env :test #'string-equal))
	(gethash key *c-macros*))))

(defun %c-expand-list (l env)
  (let ((head (car l)))
    (when (consp head)
      (return-from %c-expand-list (mapcar #'(lambda (x) (%c-macroexpand x env)) l)))
    (when (member head *quoting-c-forms*)
      (return-from %c-expand-list l))
    (let* ((tail (cdr l)))
      (when (symbolp head)
	(let ((expander (%c-env-lookup env head)))
	  (when expander
	    (multiple-value-bind (inner-form inner-env) (funcall expander tail env)
	      (return-from %c-expand-list (%c-macroexpand inner-form inner-env))))))
      (cons head (mapcar #'(lambda (x) (%c-macroexpand x env)) tail)))))

(defun %c-macroexpand (l env)
  (cond
    ((listp l) (%c-expand-list l env))
    ((symbolp l) (resolve-symbol-aliases l))
    ; TODO: add test for symbol macros (symbol-macrolet.. and friends)
    (t l)))

(defun %drop-expansion-artefacts (form)
  "Recursively splice nested (<macro-result> ...) forms into their outer lists."
  (labels ((simple-result (x)
	     (let ((cell (cons x nil)))
	       (values cell cell)))
	   (freshen (x)
	     (let ((cells (loop for item in x collect item)))
	       (values cells (last cells))))
	   (map-value (x)
	     (cond
	       ((atom x) (simple-result x))
	       ((eq (car x) *macro-result-symbol*) (freshen (%drop-expansion-artefacts (cdr x))))
	       (t (simple-result (freshen (%drop-expansion-artefacts x)))))))
    (if (listp form)
	(let ((result nil)
	      (next-cell nil))
	  (dolist (x form)
	    (multiple-value-bind (head tail) (map-value x)
	      (when (null result)
		(setf result head))
	      (when next-cell
		(setf (cdr next-cell) head))
	      (setf next-cell tail)))
	  result)
	form)))

(defun c-macroexpand (datum)
  "Macro-expand DATUM (which can be any lisp object) using the global macro environment"
  (%drop-expansion-artefacts
   (%c-macroexpand datum nil)))

(def-c-macro unwind-protect (form &body cleanup-forms)
  (with-c-gensyms (cleanup result)
    `(progn
       (ast-stmt-if (not (current-defun-void-p))
		    (declare (,result *current-return-type*)))
       (macrolet (return (&optional expr)
			 `(progn
			    (ast-stmt
			     (if (not (current-defun-void-p))
				 `(= ,',',result ,,expr)
				 `(cast void ,,expr)))
			    (goto ,',cleanup)))
	 ,form)
       (label ,cleanup)
       ,@cleanup-forms
       (return ,result))))

(def-c-macro with-open-file ((var file-name mode) &body body)
  `(progn
     (declare (,var = (cast (ptr #$FILE) 0)))
     (unwind-protect
	  (progn
	    (= ,var (#$fopen ,file-name ,mode))
	    ,@body)
       (when ,var
	 (#$fclose ,var)))))

(def-c-macro when (expr &body body)
  `(if ,expr
       (progn
	 ,@body)))

(def-c-macro with-lock-held (lock-expr &body body)
  `(unwind-protect
	(progn
	  (#$lock-mutex ,lock-expr)
	  ,@body)
     (#$unlock-mutex ,lock-expr)))

(def-c-macro let ((&rest bindings) &body body)
  `(progn
     (declare ,@(loop for (var expr) in bindings collect `(,var = ,expr)))
     ,@body))

