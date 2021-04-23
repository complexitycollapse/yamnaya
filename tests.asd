;;;; %ApplicationName%-tests.asd

(asdf:defsystem #:%ApplicationName%-tests
  :description "Tests for %ApplicationName%"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (#:%ApplicationName%
	       #:0am)
  :components ((:file "packages")
               (:file "%ApplicationName%-tests")))
