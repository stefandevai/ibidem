;;;; ========================================================================================== ;;;;
;;;; latex.lisp                                                                                 ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(defvar *linebreak* "\\linebreak~%"
  "Default linebreak sintax in Latex")

(defmacro surround-string (str1 str2 &body body)
  "Surround strings from `body' with `str1' and `str2'."
  `(str:concat ,str1 ,@body ,str2))

(defmacro latex-element (element-string newline &body body)
  "Facilitate the creation of a Latex element string."
  `(surround-string (str:concat "\\" ,element-string "{")
                    (if ,newline "}~%" "}")
                    ,@body))

(defmacro begin-end (arg &body body)
  "Surround strings with `\begin{ arg }' and `\end{ arg }'."
  `(surround-string (latex-element "begin" t ,arg)
                    (latex-element "end" t ,arg)
     ,@body))

(defmacro parenthesis (&body body)
  "Surround strings with parenthesis."
  `(surround-string "(" ")" ,@body))

(defmacro bold (&body body)
  "Surround strings with `\textbf{}' directive in Latex."
  `(latex-element "textbf" nil ,@body))

(defmacro emph (&body body)
  "Emphasizes strings with `\emph{}' directive in Latex."
  `(latex-element "emph" nil ,@body))

(defmacro url (&body body)
  "Emphasizes strings with `\url{}' directive in Latex."
  `(latex-element "url" nil ,@body))

(defmacro noindent (&body body)
  "Surround strings with `\noindent{}' directive in Latex."
  `(latex-element "noindent" nil ,@body))

(defmacro uppercase (&body body)
  "Surround strings with `\uppercase{}' directive in Latex."
  `(latex-element "uppercase" nil ,@body))

(defmacro section (&body body)
  "Surround strings with `\section{}' directive in Latex."
  `(latex-element "section*" t ,@body))

(defmacro thebibliography (&body body)
  "Surround strings with `\begin{thebibliography}{1}' and `\end{thebibliography}'."
  `(surround-string (latex-element "begin" t "thebibliography}{1")
                    (latex-element "end" t "thebibliography")
     ,@body))

(defmacro bibitem (&body body)
  "Surround strings with `\section{}' directive in Latex."
  `(latex-element "bibitem" t ,@body))
