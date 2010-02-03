
(in-package #:common-lisp-user)

(defpackage #:se.defmacro.c-amplify
  (:use :common-lisp)
  (:import-from :cl-match match))

(defpackage #:se.defmacro.c-amplify.csym
  (:use)
  (:documentation 
  "The package where C symbols are interned as ca files are
  read. Macros and special operators will also be inserted into this
  package."))

