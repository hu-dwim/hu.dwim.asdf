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
    :initarg :package-name
    :accessor system-package-name)))

(defmethod reinitialize-instance :after ((system system-with-package) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'package-name)
    (setf (system-package-name system)
          (string-upcase (component-name system)))))

(defclass system-with-target ()
  ((target-system-name
    :initarg :target-system-name
    :accessor system-target-system-name)))

(defmethod reinitialize-instance :after ((system system-with-target) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let* ((system-name (string-downcase (component-name system)))
         (last-dot-position (position #\. system-name :from-end t)))
    (unless (slot-boundp system 'target-system-name)
      (setf (system-target-system-name system)
            (subseq system-name 0 last-dot-position)))
    (let ((target-system (find-system (system-target-system-name system) nil)))
      (when target-system
        (when (and (slot-boundp target-system 'asdf::author)
                   (not (slot-boundp system 'asdf::author)))
          (setf (system-author system)
                (system-author target-system)))
        (when (and (slot-boundp target-system 'asdf::licence)
                   (not (slot-boundp system 'asdf::licence)))
          (setf (system-licence system)
                (system-licence target-system)))
        (unless (slot-boundp system 'asdf::description)
          (setf (system-description system)
                (concatenate 'string
                             (string-capitalize (subseq system-name (1+ last-dot-position)))
                             " for "
                             (system-target-system-name system))))))))

;;;;;;
;;; DWIM system

(defvar *muffle-optimization-warnings* t)

(defclass hu.dwim.cl-source-file (cl-source-file)
  ())

(defclass hu.dwim.system (system-with-package)
  ((test-system-name
    :initarg :test-system-name
    :accessor system-test-system-name)
   (documentation-system-name
    :initarg :test-system-name
    :accessor system-documentation-system-name))
  (:default-initargs
   :author '("Tamás Borbély <tomi.borbely@gmail.com>"
             "Attila Lendvai <attila.lendvai@gmail.com>"
             "Levente Mészáros <levente.meszaros@gmail.com>")
   :licence "BSD / Public domain"))

(defclass hu.dwim.test-system (system-with-target system-with-package)
  ((test-result
    :initarg :test-result
    :accessor system-test-result)
   (test-output
    :initarg :test-output
    :accessor system-test-output)))

(defclass hu.dwim.documentation-system (system-with-target system-with-package)
  ())

(defmethod reinitialize-instance :after ((system hu.dwim.system) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'test-system-name)
    (setf (system-test-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".test")))
  (unless (slot-boundp system 'documentation-system-name)
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
        (let ((package-name (system-package-name test-system)))
          (load-system test-system)
          (when package-name
            (let* ((output (make-string-output-stream))
                   (*standard-output* (make-broadcast-stream *standard-output* output))
                   (*error-output* (make-broadcast-stream *error-output* output))
                   (*debug-io* (make-broadcast-stream *debug-io* output)))
              (setf (system-test-result test-system)
                    (eval (funcall (find-symbol "FUNCALL-TEST-WITH-FEEDBACK-MESSAGE" "HU.DWIM.STEFIL")
                                   (find-symbol "TEST" package-name))))
              (setf (system-test-output test-system)
                    (get-output-stream-string output)))))
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
    (load-system :hu.dwim.debug)
    (use-package :hu.dwim.debug :hu.dwim.common)
    (do-external-symbols (symbol :hu.dwim.debug)
      (export symbol :hu.dwim.common))
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

(defun collect-system-dependencies (name)
  (let ((systems nil))
    (labels ((recurse (system)
               (dolist (specification (component-depends-on 'load-op system))
                 (when (eq 'load-op (first specification))
                   (dolist (name-specification (cdr specification))
                     (let ((name (string-downcase (if (consp name-specification)
                                                      (second name-specification)
                                                      name-specification))))
                       (let ((system (find-system name)))
                         (unless (member system systems)
                           (push system systems)
                           (recurse system)))))))))
      (recurse (find-system name)))
    systems))

(reinitialize-instance (change-class (find-system :hu.dwim.asdf) 'hu.dwim.system))

(in-package :sb-impl)

;; KLUDGE: TODO: this is an ugly hack to work around the bug https://bugs.launchpad.net/sbcl/+bug/501075
(sb-ext::without-package-locks
  (defun line-length (&optional (stream *standard-output*))
    (declare (ignore stream))
    160))
