;;;; claws.asd

(asdf:defsystem #:claws
  :class :package-inferred-system
  :description "Yet another AWS SDK for Common Lisp"
  :author "IAmRasputin <ryanmgannon.dev@gmail.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "spec")
               (:file "claws")))
