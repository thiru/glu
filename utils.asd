;;;; Utils build configuration

(asdf:defsystem :utils
  :version "0.1"
  :description "Utilities based on On Lisp, Let Over Lambda, etc."
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "utils")))

