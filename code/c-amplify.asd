
(asdf:defsystem
    :c-amplify

    ;; System meta-data
    :name "S-expression C development system"
    :version "0.1"
    :maintainer "dep"
    :author "dep"
    :license "GPL"

    :depends-on (:cl-match)

    ;; Components
    :serial t
    :components ((:file "package")
		 (:file "utils")
		 (:file "globals")
		 (:file "typesys")
		 (:file "macroexpand")
		 (:file "c-read")
		 (:file "ast")
		 (:file "cprint")
		 (:file "expr")
		 (:file "stmt")
		 (:file "clang")
		 (:file "main")))

