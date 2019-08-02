;;;; latex-article.lisp

(in-package #:latex-article)

;; Holds article's parsed content
(defclass article ()
  ((author    :initform nil :accessor author)
   (location  :initform nil :accessor location)
   (date      :initform nil :accessor date)
   (body      :initform nil :accessor body)))

;; Article's main instance
(defparameter *article* (make-instance 'article))

(defun parse-header (header-str)
  "Parse content within markdown's header"
  (setf (author     *article*) (parse-header-param "author" header-str))
  (setf (date       *article*) (parse-header-param "date" header-str))
  (setf (location   *article*) (parse-header-param "location" header-str)))

(defun parse-header-param (param header-str)
  "Parse a single parameter in markdown's header with a format: param: \"content\""
  (let* ((param-start (1+ (search "\"" header-str :start2 (search (str:concat param ":") header-str))))
         (param-end   (search "\"" header-str :start2 param-start)))
    (str:substring param-start param-end header-str)))

(defun parse-body (body-str)
  "Parse content in markdown's body"
  (setf (body *article*) (remove nil (mapcar 'parse-body-line (str:lines body-str)))))

(defun parse-body-line (str)
  "Parse a single line in markdown's body"
  (if (not (str:empty? (str:trim str))) 
    (list :line-type (body-line-type str) :content str) (body *article*)))

(defun body-line-type (str)
  "Defines a type for a markdown line. If it starts by #, it is a heading; if it starts by ## it is a section; otherwise it is a paragraph."
  (if (>= (length str) 2)
    (let ((begin-str (str:substring 0 2 str)))
      (cond ((equal begin-str "##")   ':section)        ; if (starts by "##")
            ((equal begin-str "# ")   ':heading)        ; else if (starts by "# ")
            (t                        ':paragraph)))))  ; else

(defun parse-markdown (md-str)
  "Receives a markdown string and return a list with parsed contents"
  (let* ((header-start (1+ (search '(#\Newline) md-str)))
         (header-end   (search "---" md-str :start2 header-start)))
    (parse-header (str:substring header-start header-end md-str))
    (parse-body   (str:substring (+ header-end 4) nil md-str))))

(defun create-latex-article ()
  "Creates a Latex string from *article* instance"
  (let ((begin-str    "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[T1]{fontenc}~%\\usepackage[french]{babel}~%\\usepackage{geometry}~%\\geometry{a4paper, margin=1in}~%~%\\renewcommand{\\tiny}{\\normalsize}~%\\renewcommand{\\footnotesize}{\\normalsize}~%\\renewcommand{\\small}{\\normalsize}~%\\renewcommand{\\large}{\\normalsize}~%\\renewcommand{\\Large}{\\normalsize}~%\\renewcommand{\\LARGE}{\\normalsize}~%\\renewcommand{\\huge}{\\normalsize}~%\\renewcommand{\\Huge}{\\normalsize}~%~%\\begin{document}~%~%")
        (end-str      "\\end{document}~%"))
    (str:concat begin-str (str:concat (create-latex-header) (create-latex-body)) end-str)))

(defun create-latex-header ()
  (str:concat 
    "\\begin{flushright}~%"
    (if (author *article*)     (create-latex-header-item (author *article*)))
    (if (location *article*)   (create-latex-header-item (location *article*)))
    (if (date *article*)       (create-latex-header-item (date *article*)))
    "\\end{flushright}~%~%"))

(defun create-latex-header-item (item)
  (str:concat "  \\textbf{" item "}~%  \\linebreak~%"))

(defun create-latex-body ()
  (reduce #'str:concat (mapcar 'create-latex-body-line (body *article*))))

(defun create-latex-body-line (line)
  (ecase (getf line :line-type) (:paragraph (create-latex-paragraph (getf line :content)))
                                (:section   (create-latex-section (getf line :content)))
                                (:heading   (create-latex-heading (getf line :content)))))

(defun create-latex-paragraph (paragraph)
  (str:concat
    paragraph
    "~%~%"))

(defun create-latex-section (section)
  (str:concat
    "\\section*{"
    (str:trim (str:substring 2 nil section))
    "}~%"))

(defun create-latex-heading (heading)
  (str:concat
    "\\noindent{\\textbf{\\uppercase{"
    (str:trim (str:substring 1 nil heading))
    "}}}~%~%"))

(defun write-to-file (str file-path)
  "Receives a string and a filepath and writes the string to the file"
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    (format out str)))

(defun create (input-path &optional (output-path "./article.tex"))
  "Receives a Markdown (.md) file as input and outputs a Latex (.tex) file."
  (parse-markdown (uiop:read-file-string input-path))
  (setf latex-article (create-latex-article))
  (write-to-file latex-article output-path))

