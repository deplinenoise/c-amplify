
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
    :components ((:file "code/package")
		 (:file "code/utils")
		 (:file "code/globals")
		 (:file "code/typesys")
		 (:file "code/macroexpand")
		 (:file "code/c-read")
		 (:file "code/ast")
		 (:file "code/cprint")
		 (:file "code/expr")
		 (:file "code/stmt")
		 (:file "code/clang")
		 (:file "code/main")))

