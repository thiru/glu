;;;; Utils build configuration

(asdf:defsystem :glu
  :version "0.1"
  :description "Global Lisp utilties that should be useful for most of my apps."
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "glu")))

