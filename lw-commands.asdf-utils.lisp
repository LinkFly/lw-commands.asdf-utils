(defpackage :lw-commands.asdf-utils
  (:use :cl :editor)
  (:import-from :lispworks #:when-let)
  (:import-from :editor #:parse-for-something #:funcall-background-job-with-typeout #:choose-lispeval-pane #:complete-string))

(in-package :lw-commands.asdf-utils)

(defparameter *cur-dir* nil)

(defun make-asdf-path (dir &optional name)
  (unless name (setf name 
                     (first (last (pathname-directory dir)))))
  (make-pathname :defaults dir
                 :name name
                 :type "asd"))

(defun systems-in-dir (dir)
  (mapcar #'pathname-name 
          (directory (make-pathname :defaults dir :name "*" :type "asd"))))

(defun complete-system-in-dir (string parse-inf)
  "Completion function used by PROMPT-FOR-ASDF-SYSTEM."
  (declare (ignore parse-inf))
  (editor::complete-string string (systems-in-dir *cur-dir*)
                           :ignore-case t))
(defun prompt-for-asdf-name ()
  "Prompts for an ASDF system name with STRING being the default."
  (editor::parse-for-something
   :prompt (format nil "Select ASDF system: ")
   :must-exist t
   :help "Type a directory with ASDF system."
   :default ""
   :default-string ""
   :verify-func (lambda (string parse-inf)
                  (declare (ignore parse-inf))
                  string)
   :type :string
   :default-in-prompt nil
   :complete-func 'complete-system-in-dir))
  
(defun prompt-for-asdf-dir ()
  "Prompts for an ASDF system name with STRING being the default."
  (editor::parse-for-something
   :prompt "Load ASDF into directory: "
   :must-exist t
   :help "Type a directory with ASDF system."
   :default ""
   :default-string ""
   :verify-func (lambda (string parse-inf)
                  (declare (ignore parse-inf))
                  (probe-file string))
   :type :string
   :default-in-prompt nil
   ))

(defun asdf-add-dir-and-load (dir name)
  (pushnew dir asdf:*central-registry*)
  (asdf:load-system name))

(defcommand "ASDF Add Load" (p)
     "Add directory in asdf:*central-registry* and load ASDF system \(and compiles it if necessary)."
     "Add ASDF directory and load system."
  (declare (ignore p))
  (when-let (dir (prompt-for-asdf-dir))
    (let ((*cur-dir* dir)
          (asdf-path (make-asdf-path dir))
          name)
      (if (probe-file asdf-path)
          (setf name (pathname-name asdf-path))
        (setf name (prompt-for-asdf-name)))
      (when (probe-file (make-asdf-path dir name))
        (editor::funcall-background-job-with-typeout
         (editor::choose-lispeval-pane (current-buffer) (current-window))
         'asdf-add-dir-and-load dir name)))))
          

