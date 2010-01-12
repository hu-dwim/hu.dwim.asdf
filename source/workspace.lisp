;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defvar *workspace-directory* (truename (system-relative-pathname :hu.dwim.asdf "..")))

(defparameter *original-central-registry* (copy-list *central-registry*))

(defun %initialize-asdf-registry/push-all (systems-dir &key (process-outside-links t))
  (setf systems-dir (ignore-errors (truename systems-dir)))
  (unless systems-dir
    (return-from %initialize-asdf-registry/push-all))
  (dolist (dir-candidate (directory (concatenate 'string (namestring systems-dir) "*/")))
    ;; skip dirs starting with a _ and .
    (let ((first-char (elt (car (last (pathname-directory dir-candidate))) 0)))
      (when (and (not (member first-char (list #\_ #\.)))
                 (or process-outside-links
                     (loop for a in (pathname-directory dir-candidate)
                           for b in (pathname-directory systems-dir)
                           while b do
                           (unless (equal a b)
                             (return nil))
                           finally (return t)))
                 (directory (merge-pathnames "*.asd" dir-candidate)))
        (format *debug-io* "; Pushing in *central-registry* ~A~%" dir-candidate)
        (pushnew dir-candidate *central-registry* :test 'equal)))))

(defmacro initialize-asdf-registry (&rest fallback-path-list)
  `(progn
     (setf *central-registry* (copy-list *original-central-registry*))
     ,@(mapcar (lambda (path)
                 `(%initialize-asdf-registry/push-all ,path))
               fallback-path-list)
     (%initialize-asdf-registry/push-all *workspace-directory*)
     ;; iolib has its *.asd's inside its src directory
     (%initialize-asdf-registry/push-all (merge-pathnames "iolib/" *workspace-directory*) :process-outside-links nil)))
