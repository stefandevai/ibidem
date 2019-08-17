
;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(defun write-to-file (str file-path)
  "Receive a string and a filepath and writes the string to the file"
  (with-open-file (out
                   file-path
                   :direction :output
                   :if-exists :supersede
                   :external-format :utf-8)
    (format out str)))

(defun create (input-path output-path &key (layout nil))
  "Receive a Markdown (.md) file as input and outputs a Latex (.tex) to a file."
  (let ((object (parse-markdown (uiop:read-file-string input-path))))
    (write-to-file (make-latex-article object layout) output-path)))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (setf *debugger-hook*
        (lambda (condition old-hook)
          (declare (ignore old-hook))
          (format *error-output*
                  "Caught error: ~a~%"
                  condition)
          (finish-output *error-output*)
          (sb-ext:quit)))

  (opts:define-opts
    (:name :help
           :description "show this help text."
           :short #\h
           :long "help")
    (:name :layout
           :description "add a LaTeX layout to your output file."
           :short #\l
           :long "layout"
           :arg-parser #'identity
           :meta-var "FILE")
    (:name :output
           :description "the output path for the LaTeX file."
           :short #\o
           :long "output"
           :arg-parser #'identity
           :meta-var "FILE"))

  (let ((output-path "./article.tex")
        (layout-path nil))
    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "Fatal: option ~s needs an argument.~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "Fatal: cannot parse ~s as argument of ~s.~%"
                    (opts:raw-arg condition)
                    (opts:option condition)))
          (opts:missing-required-option (con)
            (format t "Fatal: ~a is required.~%" con)
            (opts:exit 1)))


      (cond ((getf options :help)
             (opts:describe
              :prefix "latex-builder input_file.md -o output_file.tex"
              :usage-of "latex-builder"
              :args "[INPUT_FILE]"))
            ((null (car free-args))
             (format t "Fatal: an input markdown file is required."))
            (t (progn
                 (when (getf options :output)
                   (setf output-path (getf options :output)))
                 (when (getf options :layout)
                   (setf layout-path (getf options :layout)))
                 (create (first free-args) output-path :layout layout-path)))))))
