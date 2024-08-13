(defpackage #:claws.spec
  (:use #:cl)
  (:nicknames #:spec)
  (:import-from :cl-fad
                :list-directory)
  (:import-from :com.inuoe.jzon
                :parse))
(in-package :claws.spec)

(defun load-service-spec-file (service &optional (file "service-2") api-version)
  (let* ((service-namestring (ctypecase service
                               (string service)
                               (symbol (string-downcase (symbol-name service)))))
         (service-root-dir (if api-version
                               (asdf:system-relative-pathname
                                 :claws
                                 (format nil
                                         "data/~a/~a/"
                                         service-namestring
                                         api-version))
                               (first (sort (mapcar #'namestring
                                                    (list-directory
                                                     (asdf:system-relative-pathname
                                                      :claws
                                                      (format nil
                                                              "data/~a/"
                                                              service-namestring))))
                                            #'string>))))
         (service-filepath (truename (format nil "~a~a.json" service-root-dir file))))
    (parse service-filepath)))
