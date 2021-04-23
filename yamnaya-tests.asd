;;;; yamnaya-tests.asd

(asdf:defsystem #:yamnaya-tests
  :description "Tests for yamnaya"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (#:yamnaya
	       #:0am)
  :components ((:file "packages")
               (:file "yamnaya-tests")))
