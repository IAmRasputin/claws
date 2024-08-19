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

(defun service-api-directory (service version)
  "Given a SERVICE and VERSION, return a pathname to its specification root"
  (let* ((root-dir (service-root-directory (ensure-string service)))
         (version-dir (merge-pathnames version root-dir)))
    (restart-case
        (if (fad:directory-exists-p version-dir)
            version-dir
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
  (let ((files (service-spec-files service api-version))
        (spec (make-hash-table :test #'equal)))
    (dolist (f files)
      (setf (gethash (file-namestring f) spec) (jzon:parse f)))
    spec))

(defun all-service-names ()
  (subdirectory-names (asdf:system-relative-pathname :claws "data/")))
