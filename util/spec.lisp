(defpackage :claws.util.spec
  (:use :cl)
  (:import-from :com.inuoe.jzon)
  (:import-from :cl-fad :list-directory)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :load-service-spec
           :service-api-versions))
(in-package :claws.util.spec)

(defun service-namestring (service)
  "String-ifies SERVICE so it fits in a pathname"
  (ctypecase service
    (string service)
    (symbol (string-downcase (symbol-name service)))))

(defun service-api-versions (service)
  "Returns a list of all available API versions for SERVICE, newest first"
  (sort (mapcar #'namestring
                (list-directory
                 (asdf:system-relative-pathname
                  :claws
                  (format nil
                          "data/~a/"
                          (service-namestring service)))))
        #'string>))

(defun service-spec-files (service &optional api-version)
  "Returns a list of every file in the service specification"
  (let* ((service-root-dir (if api-version
                               (asdf:system-relative-pathname
                                 :claws
                                 (format nil
                                         "data/~a/~a/"
                                         (service-namestring service)
                                         api-version))
                               (first (service-api-versions service)))))
    (list-directory service-root-dir)))


(defun load-service-spec (service &optional (file "service-2") api-version)
  "Loads a service spec into a hashmap"
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
                               (first (service-api-versions service))))
         (service-filepath (truename (format nil "~a~a.json" service-root-dir file))))
    (jzon:parse service-filepath)))

(defun all-service-names ()
  (labels ((last-part (path)
             (let ((pathstring (ctypecase path
                                 (string path)
                                 (pathname (directory-namestring path)))))
               (car (last (remove "" (uiop:split-string pathstring :separator "/") :test #'string=))))))
    (mapcar #'last-part
            (list-directory
             (asdf:system-relative-pathname :claws "data/")))))
