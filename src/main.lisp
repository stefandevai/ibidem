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
    (write-to-file (make-latex-article object layout) output-path))
  (format t "> SUCCESS: Created \"~A\"~%" output-path))

(defun print-intro ()
  "Print intro for cli tool."
  (format t "

\`7XXF\'            XXP\"\"XX\"\"YXX    \`YXX\'   \`XP\'
  XX              P'   XX   `7      VXb.  ,P
  XX         ,6\"Yb.    XX  .gP\"Ya    \`XX.X\'
  XX        8)   XX    XX ,X\'   Yb     XXb
  XX      ,  ,pm9XX    XX 8X\"\"\"\"\"\"   ,X\'\`Xb.
  XX     ,X 8X   XX    XX YX.    ,  ,P   \`XX.
.JXXmmmmXXX \`Xoo9^Yo..JXXL.\`Xbmmd\'.XX:.  .:XXa.


                         ,,    ,,        ,,
`7XX\"\"\"Yp,               db  \`7XX      \`7XX
  XX    Yb                     XX        XX
  XX    dP \`7XX  \`7XX  \`7XX    XX   ,X\"\"bXX  .gP\"Ya \`7Xb,od8
  XX\"\"\"bg.   XX    XX    XX    XX ,AP    XX ,X\'   Yb  XX\' \"\'
  XX    \`Y   XX    XX    XX    XX 8XI    XX 8X\"\"\"\"\"\"  XX
  XX    ,9   XX    XX    XX    XX \`Xb    XX YX.    ,  XX
.JXXmmmd9    \`Xbod\"YXL..JXXL..JXXL.\`Wbmd\"XXL.\`Xbmmd\'.JXXL.


 Latex formatting automation.

================================================================
 AUTHOR: Stefan Devai
 WEBSITE: https://stefandevai.me/
 SOURCE: https://github.com/stefandevai/latex-builder
================================================================~%~%~%~%"))

(defun define-options ()
  "Define cli options."
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
           :meta-var "FILE")
    (:name :no-intro
           :description "hide program logo when running application."
           :long "no-intro")))

(defun unknown-option (condition)
  "Inform unknown cli option."
  (format t "> WARNING: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  "When an option `opt' exists in the cli command."
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (setf *debugger-hook*
        (lambda (condition old-hook)
          (declare (ignore old-hook))
          (format *error-output*
                  "> CAUGHT ERROR: ~a~%"
                  condition)
          (finish-output *error-output*)
          (sb-ext:quit)))

  (define-options)

  (let ((output-path *default-output-path*)
        (layout-path nil))
    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "> FATAL: option ~s needs an argument.~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "> FATAL: cannot parse ~s as argument of ~s.~%"
                    (opts:raw-arg condition)
                    (opts:option condition)))
          (opts:missing-required-option (con)
            (format t "> FATAL: ~a is required.~%" con)
            (opts:exit 1)))


      (cond ((getf options :help)
             (opts:describe
              :prefix "latex-builder input_file.md -o output_file.tex"
              :usage-of "latex-builder"
              :args "[INPUT_FILE]"))
            ((null (car free-args))
             (format t "> FATAL: an input markdown file is required.~%"))
            (t (progn
                 (when-option (options :no-intro)
                   (setf *show-ascii-intro* nil))
                 (when-option (options :output)
                   (setf output-path it))
                 (when-option (options :layout)
                   (setf layout-path it))

                 (when *show-ascii-intro* (print-intro))
                 (create (first free-args) output-path :layout layout-path)))))))
