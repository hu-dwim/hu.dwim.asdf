;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

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
;;; DWIM system

(defvar *muffle-optimization-warnings* t)

(defclass hu.dwim.cl-source-file (cl-source-file)
  ())

(defclass hu.dwim.system (system-with-package)
  ((test-system-name
    :initform nil
    :initarg :test-system-name
    :accessor system-test-system-name)
   (documentation-system-name
    :initform nil
    :initarg :test-system-name
    :accessor system-documentation-system-name))
  (:default-initargs
   :author '("Tamás Borbély <tomi.borbely@gmail.com>"
             "Attila Lendvai <attila.lendvai@gmail.com>"
             "Levente Mészáros <levente.meszaros@gmail.com>")
   :licence "BSD / Public domain"))

(defclass hu.dwim.test-system (system-with-package)
  ((test-result
    :initform nil
    :initarg :test-result
    :accessor system-test-result)))

(defclass hu.dwim.documentation-system (system-with-package)
  ())

(defmethod reinitialize-instance :after ((system hu.dwim.system) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (system-test-system-name system)
    (setf (system-test-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".test")))
  (unless (system-documentation-system-name system)
    (setf (system-documentation-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".documentation"))))

(defmethod asdf::module-default-component-class ((class hu.dwim.test-system))
  'hu.dwim.cl-source-file)

(defmethod asdf::module-default-component-class ((class hu.dwim.system))
  'hu.dwim.cl-source-file)

(defmethod perform :around ((op operation) (component hu.dwim.cl-source-file))
  (let ((*features* *features*)
        (*readtable* (copy-readtable *readtable*))
        (*package* *package*)
        (hu.dwim.common-package (find-package :hu.dwim.common)))
    (when hu.dwim.common-package
      ;; when the hu.dwim.common package is available, then we read lisp files into that, so that hu.dwim.common:in-package can shadow cl:in-package.
      ;; see hu.dwim.def/source/extended-package.lisp for more info.
      (setf *package* hu.dwim.common-package))
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

(defmacro with-ignored-boring-compiler-warnings (&body body)
  `(locally (declare #+sbcl(sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
     ,@body))

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
