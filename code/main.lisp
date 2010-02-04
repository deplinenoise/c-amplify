(in-package #:se.defmacro.c-amplify)

(defvar *c-systems* (make-hash-table))

(defparameter *output-extension* "c"
  "File extension of files produced.")

(defparameter *input-extension* "ca"
  "File extension of files read.")

(defun %eval-ca-phase1 (expanded-forms)
  (dolist (form expanded-forms)
    (parse-c-stmt-phase1 form)))

(defun %eval-ca-phase2 (expanded-forms)
  (mapcar #'(lambda (form)
	      (let ((result (parse-c-stmt form)))
		(simplify result)
		result))
	  expanded-forms))

(defun eval-ca-forms (forms)
  (let ((expansions (loop for form in forms collecting (resolve-symbol-aliases (c-macroexpand form)))))
    (%eval-ca-phase1 expansions)
    (%eval-ca-phase2 expansions)))

(defun read-csys-file (fn)
  "Read a bunch of forms from FN to be parsed as a defsystem group."
  (with-open-file (sysdef-stream fn)
    (let ((sys-forms (loop
			for datum = (read sysdef-stream nil nil)
			unless datum do (loop-finish)
			collecting datum)))
      sys-forms)))

(defstruct c-system
  (id nil :type symbol)
  (output-file nil :type pathname)
  (files nil :type list)
  (deps nil :type list))

(defun systems-connected-p (source-sys dest-sys)
  "Return t if DEST-SYS can be reached from SOURCE-SYS. Useful to
detect cyclic dependencies."
  (let ((dep-list (c-system-deps source-sys)))
    (cond ((null dep-list) nil)
	  ((member dest-sys dep-list) t)
	  (t (some #'(lambda (d) (systems-connected-p d dest-sys)) dep-list)))))

(defun eval-defsystem (system-fn id clauses)
  "Evaluate a defsystem form for the system ID, read from SYSTEM-FN
with the specified CLAUSES. If the function succeeds, ID will be
associated with the resulting c-system object in *c-systems* for later
retreival. Returns the resulting c-system object."
  (let* ((default-output-file (make-pathname :defaults system-fn
					     :name (string-downcase (symbol-name id))
					     :type *output-extension*))
	 (system (make-c-system :id id :output-file default-output-file)))
    (labels ((resolve-system-file (name)
	       (make-pathname :name name :type *input-extension* :defaults system-fn))
	     (resolve-system-files (names)
	       (mapcar #'(lambda (name) (resolve-system-file name)) names))
	     (resolve-system-deps (dep-systems)
	       (loop for sym in dep-systems
		  collect (multiple-value-bind (target exists) (gethash sym *c-systems*)
			    (unless exists
			      (error "~a: ~a depends on undefined system ~a" system-fn id sym))
			    (when (systems-connected-p target system)
			      (error "~a: cyclic dependency: ~a <-> ~a" system-fn id (c-system-id target)))
			    target))))
      (dolist (clause clauses)
	(unless (listp clause)
	  (error "defsystem clauses must be lists: ~a" clause))
	(ecase (car clause)
	  (:files (setf (c-system-files system) (resolve-system-files (cdr clause))))
	  (:depends (setf (c-system-deps system) (resolve-system-deps (cdr clause))))))
      (setf (gethash id *c-systems*) system)
      system)))

(defun update-system (system)
  "Given the c-system SYSTEM; load, macro-expand and evaluate all its
source files and then generate a single output file."
  (let* ((filenames (c-system-files system))
	 (input (loop for fn in filenames appending (read-ca-file fn)))
	 (expansion (resolve-symbol-aliases (c-macroexpand input))))
    (%eval-ca-phase1 expansion)
    (let ((ast-nodes (%eval-ca-phase2 expansion)))
      (with-open-file (*standard-output* (c-system-output-file system) :direction :output :if-exists :supersede)
	(generate-code ast-nodes)))))

(defun find-system (id)
  (multiple-value-bind (system exists-p) (gethash id *c-systems*)
    (unless exists-p
      (error "~a: no such system" id))
    system))

(defun load-csys-file (fn)
  "Load the c system definition file FN, parsing and binding the
systems defined therein in *C-SYSTEMS* for future use. Returns a list
of systems which are to be updated by default."
  (let* ((systems-to-update nil)
	 (sys-forms (read-csys-file fn)))
    (dolist (sys-def sys-forms)
      (match sys-def
	((list* 'defsystem id clauses) (eval-defsystem fn id clauses))
	((list 'update id) (push (find-system id) systems-to-update))
	(_ (error "expected defsystem clause: ~a" sys-def))))
    systems-to-update))

(defun parse-ca-file (fn)
  (eval-ca-forms (read-ca-file fn)))

(init-typesys)

