;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.asdf
  :description "Various ASDF extensions such as attached test and documentation system, explicit development support, etc."
  :components ((:module "source"
                :components ((:file "package")
                             (:file "production" :depends-on ("workspace"))
                             (:file "system" :depends-on ("production"))
                             (:file "workspace" :depends-on ("package"))))))
