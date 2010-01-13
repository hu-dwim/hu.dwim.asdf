;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

;; kept here in a comment to make it trivial to build with safety 2
;; (sb-ext:restrict-compiler-policy 'safety 2)

;;;;;;
;;; Production support

(defvar *load-as-production?* nil
  "When T, load the lisp files so that it will be used in a production system. This means that debug-only blocks are dropped, some hot functions are optimized, compile time and runtime log levels and various other variables are initialized accordingly.")

(defmacro debug-only (&body body)
  (if *load-as-production?*
      (values)
      `(progn
         ,@body)))

(defmacro debug-only* (&body body)
  `(if *load-as-production?*
       (values)
       (progn
         ,@body)))

(defmacro production-only (&body body)
  (if *load-as-production?*
      `(progn
         ,@body)
      (values)))

(defmacro production-only* (&body body)
  `(if *load-as-production?*
       (progn
         ,@body)
       (values)))

(defun optimize-declaration ()
  (if *load-as-production?*
      '(optimize (speed 3) (debug 0) (safety 0))
      '(optimize (debug 3) (safety 3))))
