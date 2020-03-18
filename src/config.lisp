;;;; ========================================================================================== ;;;;
;;;; config.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:ibidem)

(defparameter *bibliography-in-newpage* t
  "If set to true, bibliography will always be rendered in a new page after the body,")

(defparameter *show-ascii-intro* t
  "If set to true, show program logo when running on cli.")

(defvar *default-output-path* "./article.tex"
  "Default latex file output path.")

(defvar *default-citation-style* :apa
  "Global style for citations in biliography.")

(defparameter *citation-styles*
		(list :apa "[surname, initials. ][(year). ][article. ][*source*. ][other. ][location. ][edition. ][volume][(issue). ][page. ][www]"
								:other "[SURNAME name, ][article.]")
  "Order and style rules to generate citations.")

(defvar *default-citation-format* :apa
  "Global style for citations in biliography.")

(defvar *citation-formats*
  (list :apa (list
			  :article '((author-surname-initials author)
						 (parenthesis year ". ")
						 title
						 (emph journal ", ")
						 volume-issue)
			  :web '((author-surname-initials author)
					 (parenthesis year ". ")
					 (emph title ". ")
					 (url web-link)))

		:other (list :article '(author)
					 :web '(author)))
  "Order and style rules to generate citation formats.")

(defvar *latex-default-layout*
  (list "\\documentclass{article}~%\\usepackage{hyperref}~%\\usepackage[T1]{fontenc}~%~%\\begin{document}~%~%"
        "\\end{document}")
  "Basic default layout in case the user hasn't provided one.")

(defvar *latex-chars-to-escape* '(#\%)
  "List of characters to escape when writing LaTeX file.")

(defvar *group-markdown-types* '(:list :quote)
  "Markdown types that should be grouped together.")

(defvar *delimiter-bibliography-start* "~~~bibliography"
  "Custom markdown delimiter for the start of bibliography's section.")

(defvar *delimiter-bibliography-end* "~~~"
  "Custom markdown delimiter for the end of bibliography's section.")

(defvar *delimiter-default* "---"
  "Default markdown delimiter.")

(setq cl-ppcre:*allow-quoting* t)
