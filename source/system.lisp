;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

;;;;;;
;;; Readtable support

(defclass readtable-function-mixin ()
  ((setup-readtable-function-name
    :initform nil
    :initarg :setup-readtable-function-name
    :accessor setup-readtable-function-name)))

(defclass cl-source-file-with-readtable (cl-source-file readtable-function-mixin)
  ())

(defclass system-with-readtable (system readtable-function-mixin)
  ())

#+asdf-system-connections
(defclass system-connection-with-readtable (system-connection readtable-function-mixin)
  ())

(defmethod asdf::module-default-component-class ((class system-with-readtable))
  'cl-source-file-with-readtable)

#+asdf-system-connections
(defmethod asdf::module-default-component-class ((class system-connection-with-readtable))
  'cl-source-file-with-readtable)

(defun maybe-funcall-setup-readtable-function (system &optional component)
  (setf system (find-system system)) ; safe to call both with symbols and system instances
  (let ((setup-readtable-function-name (or (when component
                                             (setup-readtable-function-name component))
                                           (setup-readtable-function-name system))))
    (when setup-readtable-function-name
      (unless (position #\: (string setup-readtable-function-name) :test #'char=)
        (error "You probably want to fully qualify your SETUP-READTABLE-FUNCTION-NAME ~S"
               setup-readtable-function-name))
      ;; we need to ignore-errors because the setup-readtable-function is only defined
      ;; after some of the early files are already loaded.
      ;; FIXME this will intern into random packages because SETUP-READTABLE-FUNCTION-NAME has a default value...
      (let ((setup-readtable-function (ignore-errors
                                        (fdefinition (read-from-string setup-readtable-function-name)))))
        (when setup-readtable-function
          (funcall setup-readtable-function))))))

(defmethod perform :around ((op operation) (component cl-source-file-with-readtable))
  (let* ((*readtable* *readtable*)
         (system (loop :for parent = (component-parent component) :then (component-parent parent)
                       :while parent
                       :when (typep parent 'system)
                       :return parent)))
    (maybe-funcall-setup-readtable-function system component)
    (call-next-method)))

;;;;;;
;;; Package support

(defclass system-with-package (system)
  ((package-name
    :initform nil
    :initarg :package-name
    :accessor system-package-name)))

(defmethod reinitialize-instance :after ((system system-with-package) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (system-package-name system)
    (setf (system-package-name system)
          (string-upcase (component-name system)))))

;;;;;;
;;; Readtable and package support

(defclass system-with-readtable+package (system-with-readtable system-with-package)
  ())

(defmethod reinitialize-instance :after ((system system-with-readtable+package) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (setup-readtable-function-name system)
    (setf (setup-readtable-function-name system)
          (concatenate 'string (string-upcase (system-package-name system)) "::SETUP-READTABLE"))))

;;;;;;
;;; DWIM system

(defvar *muffle-optimization-warnings* t)

(defclass hu.dwim.source-file (cl-source-file-with-readtable)
  ())

(defclass hu.dwim.system (system-with-readtable+package)
  ((test-system-name
    :initform nil
    :initarg :test-system-name
    :accessor system-test-system-name)
   (documentation-system-name
    :initform nil
    :initarg :test-system-name
    :accessor system-documentation-system-name)))

(defclass hu.dwim.test-system (system-with-readtable+package)
  ((test-result
    :initform nil
    :initarg :test-result
    :accessor system-test-result)))

(defclass hu.dwim.documentation-system (system-with-readtable+package)
  ())

#+asdf-system-connections
(defclass hu.dwim.system-connection (system-connection-with-readtable)
  ())

(defmethod reinitialize-instance :after ((system hu.dwim.system) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (system-test-system-name system)
    (setf (system-test-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".test")))
  (unless (system-documentation-system-name system)
    (setf (system-documentation-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".documentation"))))

(defmethod asdf::module-default-component-class ((class hu.dwim.system))
  'hu.dwim.source-file)

#+asdf-system-connections
(defmethod asdf::module-default-component-class ((class hu.dwim.system-connection))
  'hu.dwim.source-file)

(defmethod perform :around ((op operation) (component hu.dwim.source-file))
  (let ((*features* *features*))
    (unless hu.dwim.asdf:*load-as-production?*
      (pushnew :debug *features*))
    (call-in-system-environment op (component-system component) #'call-next-method)))

(defun call-with-muffled-boring-compiler-warnings (thunk)
  (handler-bind (#+sbcl(sb-ext:compiler-note #'muffle-warning)
                 ;; NOTE: muffle these warnings to reduce compilation noise, tests already cover interesting cases
                 #+sbcl(sb-kernel:undefined-alien-style-warning #'muffle-warning))
    (funcall thunk)))

(defmacro with-muffled-boring-compiler-warnings (&body body)
  `(call-with-muffled-boring-compiler-warnings (lambda () ,@body)))

(defgeneric call-in-system-environment (operation system function)
  (:method ((op operation) (system system) function)
    (if *muffle-optimization-warnings*
        (call-with-muffled-boring-compiler-warnings function)
        (funcall function))))

(defmethod perform ((op test-op) (system hu.dwim.system))
  (let ((test-system (find-system (system-test-system-name system) nil)))
    (if test-system
        (progn
          (load-system test-system)
          (setf (system-test-result test-system)
                (eval (funcall (find-symbol "FUNCALL-TEST-WITH-FEEDBACK-MESSAGE" "HU.DWIM.STEFIL")
                               (find-symbol "TEST" (system-package-name test-system))))))
        (warn "There is no test system for ~A, no tests were run" system))))

;;;;;;
;;; Develop

(defvar *development-package*)

(defclass develop-op (operation)
  ())

(defmethod operation-done-p ((operation develop-op) (component component))
  nil)

(defmethod perform ((operation develop-op) (component component))
  nil)

(defmethod perform ((operation develop-op) (system system))
  (load-system system))

(defmethod perform ((operation develop-op) (system hu.dwim.system))
  (let ((test-system (find-system (system-test-system-name system) nil)))
    (if test-system
        (load-system test-system)
        (load-system system))
    (pushnew :debug *features*)
    (declaim (optimize (debug 3)))
    (let ((package (find-package (system-package-name (or test-system system)))))
      (when package
        (setf *development-package* package)))
    (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")))

(defun develop-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:develop-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (assert (not (boundp '*development-package*)))
  (let ((*development-package* nil))
    (multiple-value-prog1
        (apply #'operate 'develop-op system args)
      (when *development-package*
        (setf *package* *development-package*)))))

;;;;;;
;;; Util

(defun system-pathname (name)
  (component-pathname (find-system name)))

(change-class (find-system :hu.dwim.asdf) 'hu.dwim.system)
