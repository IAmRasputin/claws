(defpackage :claws.util.spec
  (:use :cl)
  (:import-from :com.inuoe.jzon)
  (:import-from :cl-fad)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :load-service-spec
           :all-service-names
           :service-api-versions
           :service-spec-files
           :service-root-directory))
(in-package :claws.util.spec)

(defclass aws-service-specification ()
  ((name :initarg :name
         :initform (error "You can't have a service without a name.")
         :type string
         :reader service-name)
   (endpoint-rule-set :initarg :endpoint-rule-set
                      :initform (error "Missing endpoint rule set")
                      :type hash-table
                      :reader service-endpoint-rule-set)
   (examples :initarg :examples
             :type (or hash-table nil)
             :reader service-examples)
   (paginators :initarg :paginators
               :type (or hash-table nil)
               :reader service-paginators)
   (paginators-sdk-extras :initarg :paginators-sdk-extras
                          :type (or hash-table nil)
                          :reader service-paginators-sdk-extras)
   (service-definition :initarg :service-definition
                       :initform (error "Missing service definition")
                       :type hash-table
                       :reader service-definition)
   (service-sdk-extras :initarg :service-sdk-extras
                       :type (or hash-table nil)
                       :reader service-sdk-extras)
   (waiters :initarg :waiters
            :type (or hash-table nil)
            :reader service-waiters)))

(define-condition service-not-found (error)
  ((missing-service :initarg :service
                    :reader service
                    :initform (error "No service name was provided")))
  (:report (lambda (condition stream)
             (format stream
                     "No AWS service found with name ~s in ~a"
                     (service condition)
                     (asdf:system-relative-pathname :claws "data/")))))

(define-condition service-api-version-not-found (error)
  ((missing-version :initarg :version
                    :reader version
                    :initform (error "No API version provided"))
   (service :initarg :service
            :reader service
            :initform (error "No service name provided")))
  (:report (lambda (condition stream)
             (format stream
                     "No API version ~s found for service ~s"
                     (version condition)
                     (service condition)))))

(defun subdirectory-names (pathspec)
  (mapcar (lambda (p)
            (let ((parts (uiop:split-string (namestring p) :separator "/")))
              (car (last (remove "" parts :test #'equal)))))
          (uiop:subdirectories pathspec)))

(defun new-value-input-prompt (prompt)
  (format *query-io* prompt)
  (force-output *query-io*)
  (list (ensure-string (read *query-io*))))

(defun ensure-string (in)
  "String-ifies IN so it fits in a pathname"
  (ctypecase in
    (string in)
    (symbol (string-downcase (symbol-name in)))))

(defun service-root-directory (service)
  "Return the root spec directory for SERVICE"
  (restart-case
      (let* ((service-name (ensure-string service))
             (service-root
               (asdf:system-relative-pathname :claws
                                              (format nil "data/~a/" service-name))))
        (if (fad:directory-exists-p service-root)
            service-root
            (error (make-condition 'service-not-found :service service))))
    (pick-new-service (s)
      :report "Enter another service name"
      :interactive (lambda () (new-value-input-prompt "Enter a service to locate specs: "))
      (service-root-directory s))))

(defun service-api-directory (service &optional (version (first (service-api-versions service))))
  "Given a SERVICE and VERSION, return a pathname to its specification root"
  (let* ((root-dir (service-root-directory (ensure-string service)))
         (version-dir (merge-pathnames version root-dir)))
    (restart-case
        (if (fad:directory-exists-p version-dir)
            (fad:pathname-as-directory version-dir)
            (error (make-condition 'service-api-version-not-found :service service :version version)))
      (pick-new-version (v)
        :report "Enter a valid API version"
        :interactive (lambda ()
                       (new-value-input-prompt (format nil
                                                       "Enter a valid API version [available versions: ~{~s~}]: "
                                                       (service-api-versions service))))
        (service-api-directory service v))
      (pick-latest-version ()
        :report (lambda ()
                  (format nil "Assume latest API version (~s)" (first (service-api-versions service))))
        (service-api-directory service (first (service-api-versions service)))))))

(defun service-api-versions (service)
  "Returns a list of all available API versions for SERVICE, newest first"
  (sort (subdirectory-names (service-root-directory service))
        #'string>))

(defun service-spec-files (service &optional (api-version (first (service-api-versions service))))
  "Returns a list of every file in the service specification"
  (let* ((service-api-dir (service-api-directory service api-version)))
    (fad:list-directory service-api-dir)))


(defun load-service-spec (service &optional (api-version (first (service-api-versions service))))
  "Loads a service spec into a hashmap"
  (let* ((root (service-api-directory service api-version))
         (definition-file (merge-pathnames "service-2.json" root))
         (endpoint-rule-file (merge-pathnames "endpoint-rule-set-1.json" root))
         (examples-file (merge-pathnames "examples-1.json" root))
         (paginators-file (merge-pathnames "paginators-1.json" root))
         (paginator-extras-file (merge-pathnames "paginators-1.sdk-extras.json" root))
         (service-sdk-extras-file (merge-pathnames "service-2.sdk-extras.json" root))
         (waiters-file (merge-pathnames "waiters-2.json" root)))
    (make-instance 'aws-service-specification
                   :name (ensure-string service)
                   :service-definition (jzon:parse definition-file)
                   :endpoint-rule-set (jzon:parse endpoint-rule-file)
                   :examples (when (uiop:file-exists-p examples-file)
                               (jzon:parse examples-file))
                   :waiters (when (uiop:file-exists-p waiters-file)
                              (jzon:parse waiters-file))
                   :service-sdk-extras (when (uiop:file-exists-p service-sdk-extras-file)
                                         (jzon:parse service-sdk-extras-file))
                   :paginators (when (uiop:file-exists-p paginators-file)
                                 (jzon:parse paginators-file))
                   :paginators-sdk-extras (when (uiop:file-exists-p paginator-extras-file)
                                            (jzon:parse paginator-extras-file)))))

(defun all-service-names ()
  (subdirectory-names (asdf:system-relative-pathname :claws "data/")))
