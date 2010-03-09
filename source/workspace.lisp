;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defvar *workspace-directory* (truename (system-relative-pathname :hu.dwim.asdf "..")))

(defparameter *original-central-registry* (copy-list *central-registry*))

(defun %register-directories-into-asdf-registry (systems-dir &key (process-outside-links t) (insert-at :head))
  (format *debug-io* "; Extending *central-registry* recursively with ~S~%" systems-dir)
  (setf systems-dir (ignore-errors (truename systems-dir)))
  (unless systems-dir
    (return-from %register-directories-into-asdf-registry))
  (dolist (dir-candidate (directory (concatenate 'string (namestring systems-dir) "*/")
                                    #+ccl :directories #+ccl t))
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
        (unless (find dir-candidate *central-registry* :test 'equal)
          (ecase insert-at
            (:head (push dir-candidate *central-registry*)
                   (format *debug-io* "; Pushing into *central-registry* ~A~%" dir-candidate))
            (:tail (setf *central-registry* (append *central-registry* (list dir-candidate)))
                   (format *debug-io* "; Appending to *central-registry* ~A~%" dir-candidate))))))))

(defun initialize-asdf-registry (&rest fallback-path-list)
  (setf *central-registry* (copy-list *original-central-registry*))
  (dolist (path fallback-path-list)
    (%register-directories-into-asdf-registry path))
  (%register-directories-into-asdf-registry *workspace-directory*)
  ;; iolib has its *.asd's inside its src directory
  (%register-directories-into-asdf-registry (merge-pathnames "iolib/" *workspace-directory*) :process-outside-links nil))

(defun extend-asdf-registry (&rest path-list)
  (dolist (path path-list)
    (%register-directories-into-asdf-registry path :insert-at :tail)))
