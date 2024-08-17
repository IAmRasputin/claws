;;;; claws.asd

(asdf:defsystem #:claws
  :description "Yet another AWS SDK for Common Lisp"
  :author "IAmRasputin <ryanmgannon.dev@gmail.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-fad" "com.inuoe.jzon" "aws-sign4")
  :components ((:module "utils")
               (:file "spec")
               (:file "claws")
               (:static-file "LICENSE")))
