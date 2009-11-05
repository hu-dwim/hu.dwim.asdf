;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defvar *workspace-directory* (truename (system-relative-pathname :hu.dwim.asdf "..")))

(defparameter *original-central-registry* (copy-list *central-registry*))

(defun reread-asdf-registry ()
  (flet ((push-all (systems-dir &key (process-outside-links t))
           (setf systems-dir (ignore-errors (truename systems-dir)))
           (unless systems-dir
             (return-from push-all))
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
                 (pushnew dir-candidate *central-registry* :test 'equal))))))
    (setf *central-registry* (copy-list *original-central-registry*))
    ;; on the dev environments we have a symlink called "global" to /usr/share/common-lisp/source/
    ;; (push-all (merge-pathnames "global/" *workspace-directory*) :process-outside-links nil)
    (push-all "/usr/share/common-lisp/source/")
    (push-all "/usr/local/share/common-lisp/source/")
    ;; (push-all (merge-pathnames ".sbcl/site/" (user-homedir-pathname)))
    (push-all *workspace-directory*)
    ;; iolib has its *.asd's inside its src directory
    (push-all (merge-pathnames "iolib/" *workspace-directory*) :process-outside-links nil)))
