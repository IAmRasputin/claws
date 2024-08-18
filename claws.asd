;;;; claws.asd

(asdf:defsystem #:claws
  :description "Yet another AWS SDK for Common Lisp"
  :author "IAmRasputin <ryanmgannon.dev@gmail.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (:com.inuoe.jzon
               :cl-fad)
  :components ((:module "util"
                  :components ((:file "spec")))
               (:file "claws")
               (:static-file "LICENSE")))
