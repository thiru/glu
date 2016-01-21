;;;; Utils build configuration

(in-package :cl-user)

(asdf:defsystem :glu
  :version "0.5"
  :description "Global Lisp utilties that should be useful for most of my apps."
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :depends-on (:local-time)
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "glu")
               (:file "on-lisp")
               (:file "let-over-lambda")))
