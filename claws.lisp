;;;; claws.lisp
(defpackage #:claws
  (:use #:cl)
  (:nicknames #:spec)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:import-from :fad
                :list-directory))

(in-package #:claws)
