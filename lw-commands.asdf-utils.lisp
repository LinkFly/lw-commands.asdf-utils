(defpackage :lw-commands.asdf-utils
  (:use :cl :editor)
  (:import-from :lispworks #:when-let)
  (:import-from :editor #:parse-for-something #:funcall-background-job-with-typeout #:choose-lispeval-pane))

(in-package :lw-commands.asdf-utils)

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
   :default-in-prompt nil))

(defun asdf-add-dir-and-load (dir)  
  (pushnew dir asdf:*central-registry*)
  (asdf:load-system (first (last (pathname-directory dir)))))

(defcommand "ASDF Add Dir And Load" (p)
     "Add directory in asdf:*central-registry* and load ASDF system \(and compiles it if necessary)."
     "Add ASDF directory and load system."
  (declare (ignore p))
  (when-let (dir (prompt-for-asdf-dir))
    (editor::funcall-background-job-with-typeout
     (editor::choose-lispeval-pane (current-buffer) (current-window))
     'asdf-add-dir-and-load dir)))
