;;;; ========================================================================================== ;;;;
;;;; write-latex.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(defun make-latex-article (object &optional layout)
  "Creates a Latex string from a `markdown-object' instance."
  (destructuring-bind (begin-str end-str) (latex-layout layout)

;;  (let ((begin-str "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[utf8]{inputenc}~%\\usepackage[T1]{fontenc}~%\\usepackage[french]{babel}~%\\usepackage{hyperref}~%\\usepackage{geometry}~%\\geometry{a4paper, margin=1in}~%~%\\renewcommand{\\tiny}{\\normalsize}~%\\renewcommand{\\footnotesize}{\\normalsize}~%\\renewcommand{\\small}{\\normalsize}~%\\renewcommand{\\large}{\\normalsize}~%\\renewcommand{\\Large}{\\normalsize}~%\\renewcommand{\\LARGE}{\\normalsize}~%\\renewcommand{\\huge}{\\normalsize}~%\\renewcommand{\\Huge}{\\normalsize}~%~%\\begin{document}~%~%")
;;        (end-str "\\end{document}~%"))
    (print begin-str)
    (str:concat begin-str
                (str:concat (make-latex-header object)
                            (make-latex-body object)
                            (make-latex-bibliography object))
                end-str)))

(defun latex-layout (layout)
  (if (null layout)
      (list "\\documentclass{article}~%\\usepackage{hyperref}~%~%\\begin{document}~%~%" "\\end{document}")
      (latex-layout-from-file layout)))

(defun latex-layout-from-file (file-path)
  (let* ((layout-string (uiop:read-file-string file-path))

         (end-delimiter "\\end{document}")
         (end-index (search end-delimiter layout-string))

         (start-delimiter "\\begin{document}")
         (start-index (search '(#\Newline)
                              layout-string
                              :start2 (search start-delimiter
                                              layout-string))))

    (list (str:substring 0 start-index layout-string)
          (str:substring end-index nil layout-string))))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Header writing                                                                               ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun make-latex-header (object)
  "Receive a `markdown-object' and return a formatted latex header string."
  (begin-end "flushright"
             (if (author object) (make-latex-header-item (author object)))
             (if (location object) (make-latex-header-item (location object)))
             (if (date object) (make-latex-header-item (date object)))))

(defun make-latex-header-item (item)
  "Receive a single header item and return a formatted latex string."
  (str:concat (bold item) "~%"  *linebreak*))


;;; -------------------------------------------------------------------------------------------- ;;;
;;; Body writing                                                                                 ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun make-latex-body (object)
  "Receive a `markdown-object' and return a formated latex body string."
  (reduce #'str:concat (mapcar 'make-latex-body-line (body object))))

(defun make-latex-body-line (line)
  "Receive a single body line and return a formatted latex string
  according with the body `line-type'."
  (ecase (getf line :line-type)
    (:paragraph (make-latex-paragraph (getf line :content)))
    (:section (make-latex-section (getf line :content)))
    (:heading (make-latex-heading (getf line :content)))))

(defun make-latex-paragraph (string)
  "Return a formatted string as latex paragraph."
  (str:concat (make-latex-emphasis (make-latex-bold string))
              "~%~%"))

(defun make-latex-section (string)
  "Return a formatted string as a latex section."
  (section (str:trim (str:substring 2 nil string))))

(defun make-latex-heading (string)
  "Return a formatted string as a latex paragraph."
  (str:concat (noindent (bold (uppercase (str:trim (str:substring 1 nil string)))))
              "~%~%"))

(defun make-latex-bold (text)
  "Return `text' with all instances of markdown bold as latex bold.

  Example:
  (make-latex-bold \"aaa **aaa**  aaa\")
  ;; => \"aaa \\textbf{aaa} aaa\""
  (ppcre:regex-replace-all "\\*\\*([^\\*]\\S(.*?\\S)?)\\*\\*" text (bold "\\1")))

(defun make-latex-emphasis (text)
  "Return `text' with all instances of markdown bold as latex emphasis.

  Example:
  (make-latex-emphasis \"aaa **aaa**  aaa\")
  ;; => \"aaa \\emph{aaa} aaa\""
  (ppcre:regex-replace-all "\\*([^\\*]\\S(.*?\\S)?)\\*" text (emph "\\1")))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Bibliography writing                                                                         ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defmacro build-citation (source)
  "Builds a single citation according to a format provided in `source'."
  (let ((style-params (getf +citation-format+
                            +citation-style+))
        (params (gensym)))
    `(let ((,params (getf ,style-params
                          (read-from-string
                          (str:concat ":"
                                      (slot-value ,source
                                      'ctype))))))
       (reduce #'str:concat
               (loop :for param :in ,params
                  :collect (cond ((listp param)
                                  (str:concat (eval (list (car param)
                                                          (slot-value ,source
                                                                      (cadr param))))
                                              (reduce #'str:concat (cddr param))))
                                 (t (str:concat (slot-value ,source param) ". "))))))))

(defun author-surname-initials (string)
  "Return author' surname followed by the initials of the other names.

  Example:
  (author-surname-initials \"Fernand Paul Achille Braudel\")
  ;; => \"Braudel, F. P. A.\""
  (let ((names (str:split #\Space string)))
    (str:concat (car (reverse names))
                ", "
                (reduce #'str:concat
                        (mapcar (lambda (name) (str:concat (str:s-first name) ". "))
                                (reverse (cdr (reverse names))))))))



(defun get-citation-source (citation-id sources)
  "Return a `citation-source' from `sources' list where its id is equal to `citation-id'."
  (let* ((citation-id (subseq citation-id 0 (search "-" citation-id))))
        (find-if (lambda (x) (string= citation-id (id x))) sources)))

(defun make-citation (citation sources)
  "Return a formatted latex citation.
  `string' cointains information about the citation as \"citation-id:pages\";
  `sources' contains a list of `citation-source'."
  (str:concat
   (build-citation
    (get-citation-source (car citation) sources))))

(defun make-citation-pages (pages)
  (print pages)
  (if (or (position #\, pages) (position #\- pages))
      (str:concat "pp. " pages ". ")
      (str:concat "p. " pages ". " )))

(defun make-latex-bibliography-item (citation index object)
  "Return a formatted latex bibliography item."
    (str:concat (bibitem (car citation))
                (make-citation citation (bibliography object))
                (when (> (length citation) 1) (make-citation-pages (cadr citation)))
                "~%~%"))

(defun make-latex-bibliography (object)
  "Return a formatted latex bibliography.
  `object' is a `markdown-object' that contains information about the citations and sources."
  (thebibliography
    (reduce #'str:concat
            (let ((citations (citations object)))
              (loop :for citation :in citations
                 :for index :from 1 :to (length citations)
                 :collect (make-latex-bibliography-item citation
                                                        index
                                                        object))))))
