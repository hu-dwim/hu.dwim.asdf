;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defvar *workspace-directory*
  (truename (or #+sbcl(sb-ext:posix-getenv "DWIM_WORKSPACE")
                (let ((dir (system-relative-pathname :hu.dwim.asdf "..")))
                  (warn "Initialized ~S using the path of the ASDF system called :hu.dwim.asdf to ~A (beware of possibly symlinking it somewhere else)"
                        '*workspace-directory* dir)
                  dir))))

(defun initialize-asdf-source-registry (directories &key (excluded-directories '()) (inherit-configuration? nil) (insert-at :tail))
  (check-type inherit-configuration? boolean)
  (unless (consp directories)
    (setf directories (list directories)))
  (let ((entries `(:source-registry
                   (:also-exclude ,@excluded-directories))))
    (labels ((collect-directories (root-directory)
               (mapcar (lambda (el)
                         (list :directory el))
                       (collect-directories-for-source-registry root-directory)))
             (extend-with (path)
               (ecase insert-at
                 (:head (setf entries (append (collect-directories path) entries)))
                 (:tail (setf entries (append entries (collect-directories path)))))))
      (map nil #'extend-with directories)
      ;; iolib has its *.asd's inside its src directory
      (extend-with (merge-pathnames "iolib/" *workspace-directory*))
      (initialize-source-registry (append entries (list (if inherit-configuration?
                                                            :inherit-configuration
                                                            :ignore-inherited-configuration)))))))

(defun collect-directories-for-source-registry (root-directory &key (process-outside-links t))
  (format *debug-io* "; Collecting directories for the source registry under ~S~%" root-directory)
  (setf root-directory (ignore-errors (truename root-directory)))
  (unless root-directory
    (return-from collect-directories-for-source-registry))
  (let ((result ()))
    (dolist (candidate-directory (directory (concatenate 'string (namestring root-directory) "*/")
                                            #+ccl :directories #+ccl t))
      ;; skip dirs starting with a _ and .
      (let ((first-char (elt (car (last (pathname-directory candidate-directory))) 0)))
        (when (and (not (member first-char (list #\_ #\.)))
                   (or process-outside-links
                       (loop for a in (pathname-directory candidate-directory)
                             for b in (pathname-directory root-directory)
                             while b do
                             (unless (equal a b)
                               (return nil))
                             finally (return t)))
                   (directory (merge-pathnames "*.asd" candidate-directory)))
          (unless (find candidate-directory result :test 'equal)
            (push candidate-directory result)
            (format *debug-io* "; Collecting ~A~%" candidate-directory)))))
    result))
