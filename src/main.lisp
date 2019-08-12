;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:md-to-latex)

(defun make-latex-article (object)
  "Creates a Latex string from *article* instance"
  (let ((begin-str "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[T1]{fontenc}~%\\usepackage[french]{babel}~%\\usepackage{geometry}~%\\geometry{a4paper, margin=1in}~%~%\\renewcommand{\\tiny}{\\normalsize}~%\\renewcommand{\\footnotesize}{\\normalsize}~%\\renewcommand{\\small}{\\normalsize}~%\\renewcommand{\\large}{\\normalsize}~%\\renewcommand{\\Large}{\\normalsize}~%\\renewcommand{\\LARGE}{\\normalsize}~%\\renewcommand{\\huge}{\\normalsize}~%\\renewcommand{\\Huge}{\\normalsize}~%~%\\begin{document}~%~%")
        (end-str "\\end{document}~%"))
    (str:concat begin-str
                (str:concat (make-latex-header object)
                            (make-latex-body object))
                end-str)))

(defun make-latex-header (object)
  (begin-end "flushright"
    (if (author object) (make-latex-header-item (author object)))
    (if (location object) (make-latex-header-item (location object)))
    (if (date object) (make-latex-header-item (date object)))))

(defun make-latex-header-item (item)
  (str:concat (bold item) "~%"  *linebreak*))

(defun make-latex-body (object)
  (reduce #'str:concat (mapcar 'make-latex-body-line (body object))))

(defun make-latex-body-line (line)
  (ecase (getf line :line-type) (:paragraph (make-latex-paragraph (getf line :content)))
                                (:section (make-latex-section (getf line :content)))
                                (:heading (make-latex-heading (getf line :content)))))

(defun make-latex-paragraph (string)
  (str:concat (make-latex-emphasis (make-latex-bold string))
              "~%~%"))

(defun make-latex-section (string)
  (section (str:trim (str:substring 2 nil string))))

(defun make-latex-heading (string)
  (str:concat (noindent (bold (uppercase (str:trim (str:substring 1 nil string)))))
              "~%~%"))

(defun make-latex-bold (text)
  (ppcre:regex-replace-all "\\*\\*([^\\*]\\S(.*?\\S)?)\\*\\*" text (bold "\\1")))

(defun make-latex-emphasis (text)
  (ppcre:regex-replace-all "\\*([^\\*]\\S(.*?\\S)?)\\*" text (emph "\\1")))

(defun write-to-file (str file-path)
  "Receives a string and a filepath and writes the string to the file"
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
