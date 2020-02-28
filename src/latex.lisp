;;;; ========================================================================================== ;;;;
;;;; latex.lisp                                                                                 ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(defvar *linebreak* "\\linebreak~%"
  "Default linebreak sintax in Latex")

(defvar *centering* "\\centering~%"
  "Default centering sintax in Latex")

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
                    (latex-element "end" nil ,arg)
     ,@body))

(defmacro parenthesis (&body body)
  "Surround strings with parenthesis."
  `(surround-string "(" ")" ,@body))

(defmacro bold (&body body)
  "Surround strings with `\textbf{}' directive in Latex."
  `(latex-element "textbf" nil ,@body))

(defmacro textit (&body body)
  "Surround strings with `\textit{}' directive in Latex."
  `(latex-element "textit" nil ,@body))

(defmacro emph (&body body)
  "Emphasizes strings with `\emph{}' directive in Latex."
  `(latex-element "emph" nil ,@body))

(defmacro href (url &body body)
  "Creates a link to `url' with `body' as text using `\href{}{}'"
  `(latex-element "href" nil (str:concat ,url "}{" ,@body)))

(defmacro url (&body body)
  "Creates a url string with `\url{}' directive in Latex."
  `(latex-element "url" nil ,@body))

(defmacro includegraphics (&body body)
  "Creates a graphics to the url in body with `\includegraphics{}' directive in Latex."
  `(latex-element "includegraphics" t ,@body))

(defmacro caption (&body body)
  "Creates a caption string with `\caption{}' directive in Latex."
  `(latex-element "caption" t ,@body))

(defmacro image (aurl &body body)
  "Creates a figure with `\begin{figure}[h]' and `\end{figure}'."
  `(surround-string (latex-element "begin" nil "figure")
                    (latex-element "end" t "figure")
		    (str:concat "[h]~%"
				*centering*
				(includegraphics ,aurl)
				(if (not (str:blankp (str:concat ,@body)))
				    (caption ,@body)
				  nil))))

(macroexpand '(loop for i upto 10 collect i))

(defmacro noindent (&body body)
  "Surround strings with `\noindent{}' directive in Latex."
  `(latex-element "noindent" nil ,@body))

(defmacro uppercase (&body body)
  "Surround strings with `\uppercase{}' directive in Latex."
  `(latex-element "uppercase" nil ,@body))

(defmacro section (&body body)
  "Surround strings with `\section{}' directive in Latex."
  `(latex-element "section*" nil ,@body))

(defmacro thebibliography (&body body)
  "Surround strings with `\begin{thebibliography}{1}' and `\end{thebibliography}'."
  `(surround-string (latex-element "begin" t "thebibliography}{1")
                    (latex-element "end" t "thebibliography")
     ,@body))

(defmacro bibitem (&body body)
  "Surround strings with `\section{}' directive in Latex."
  `(latex-element "bibitem" t ,@body))
