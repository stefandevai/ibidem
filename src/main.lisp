
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

(defun create (input-path output-path)
  "Receive a Markdown (.md) file as input and outputs a Latex (.tex) to a file."
  (let ((object (parse-markdown (uiop:read-file-string input-path))))
    (write-to-file (make-latex-article object) output-path)))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (opts:define-opts
      (:name :help
             :description "show this help text"
             :short #\h
             :long "help")
      (:name :out
             :description "the output latex file name"
             :short #\o
             :long "output"
             :arg-parser #'identity
             :meta-var "FILE"))

  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (con)
          (format t "fatal: ~a~%" con)
          (opts:exit 1)))

    (when-option (options :help)
                 (opts:describe
                  :prefix "example of how it works"
                  :suffix "so thats it"
                  :usage-of "usage.sh"
                  :args "[FREE-ARGS]"))

    (if (first free-args)
        (and (let ((output-file nil))
             (when-option (options :out) (setf output-file (getf options :out)))
             (if (not output-file) (setf output-file "./article.tex"))
             (create (first free-args) output-file))))))
