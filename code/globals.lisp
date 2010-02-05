(in-package :se.defmacro.c-amplify)

(defparameter *c-namespace* (make-hash-table)
  "The global namespace of functions and types. Maps symbols to gval instances.")

(defparameter *current-source-file* nil
  "Bound to the current source file; automatically stuck onto gvals to track their home files.")

(defparameter *csym-package* (find-package :se.defmacro.c-amplify.csym))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym ,(symbol-name n))))
     ,@body))

(defclass source-file ()
  ((path :reader source-file-path :initarg :path)
   (gvals :reader defined-gvals :initform (make-array 8 :adjustable t :fill-pointer t)))
  (:documentation "Represents a source file."))

(defclass gval ()
  ((sym :type symbol :reader gval-sym :initarg :symbol)
   (kind :type symbol :reader gval-kind :initarg :kind)
   (linkage :accessor linkage)
   (type-obj :accessor gval-type :initarg :type)
   (ast :accessor gval-ast :initarg :ast)
   (source :type (or source-file null) :accessor gval-source :initarg :source)))

;; Linkage:
;; - static, extern
;; - _declspec(dllexport)

(defun make-defun-gval (sym function-type)
  (make-instance 'gval
		 :sym sym
		 :type-obj function-type
		 :linkage "extern"))

(defun set-global-gv (gv)
  (declare (type gval gv))
  (setf (gval-source gv) *current-source-file*)
  (setf (gethash (gval-sym gv) *c-namespace*) gv))

(defun lookup-gval (sym)
  (declare (type symbol sym))
  (gethash sym *c-namespace*))

(defvar *c-aliases* (make-hash-table)
  "Maps symbols from the CSYM package back to their (usually
  uppercased) versions in C-AMPLIFY.")

(defun/compile-time make-c-sym (&rest datums)
  (intern
   (with-output-to-string (stream)
     (dolist (a datums) (princ a stream)))
   *csym-package*))

(defparameter *eof* (gensym "eof"))

(defun/compile-time read-c-sym (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((name (make-array 32 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for ch = (peek-char nil stream nil *eof* nil)
	 until (or (eq ch *eof*)
		   (eql #\) ch)
		   (eql #\space ch)
		   (eql #\tab ch)
		   (eql #\newline ch))
	 do (progn
	      (read-char stream nil nil nil)
	      (vector-push-extend ch name)))
    (when (= (fill-pointer name) 0)
      (error "null C-symbols are not allowed"))
    (make-c-sym name)))

;; Install #$ as a reader macro for reading case-sensitive symbols in
;; the C-SYM package. This is very useful in macros.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$ #'read-c-sym))

(defun symbol-name-downcase (symbol)
  (string-downcase (symbol-name symbol)))

(defun export-symbol-to-c (symbol)
  "Make SYMBOL available in in C code by means of shadowing-import
into the CSYM package. If the symbol isn't lowercase, also install a
lower-case alias that maps back to the original symbol via the
*c-aliases* hash table."
  ;; (format t "Exporting ~a~%" symbol)
  (shadowing-import symbol *csym-package*)
  (let* ((symbol-name (symbol-name symbol))
	 (symbol-lower (string-downcase symbol-name)))
    (unless (string= symbol-name symbol-lower)
      (export-symbol-alias symbol-lower symbol))))

(defun export-symbol-alias (sym-string aliased-symbol)
  "Create a symbol in the CSYM package with the name SYM-STRING,
mapping back to the symbol ALIASED-SYMBOL."
  ;; (format t "Aliasing ~a = ~a~%" sym-string aliased-symbol)
  (setf (gethash (make-c-sym sym-string) *c-aliases*)
	aliased-symbol))

(defun resolve-symbol-aliases (decl)
  "Replace any aliased symbols in DECL with their Lisp variants. In
effect this converts keywords from lowercase form to Lisp uppercase
form."
  (cond ((symbolp decl) (or (gethash decl *c-aliases*) decl))
	((listp decl) (mapcar #'resolve-symbol-aliases decl))
	(t decl)))
