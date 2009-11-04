;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.asdf
  (:use :asdf
        :common-lisp)

  (:export #:hu.dwim.system
           #:hu.dwim.test-system
           #:hu.dwim.documentation-system
           #:system-pathname
           #:system-relative-pathname
           #:system-package-name
           #:system-test-system-name
           #:system-documentation-system-name
           #:system-test-result
           #:develop-op
           #:develop-system
           #:*load-as-production?*
           #:debug-only
           #:debug-only*
           #:production-only
           #:production-only*
           #:optimize-declaration
           #:with-muffled-boring-compiler-warnings
           #:*workspace-directory*
           #:reread-asdf-registry))
