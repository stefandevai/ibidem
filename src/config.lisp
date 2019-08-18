;;;; ========================================================================================== ;;;;
;;;; config.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(defvar *citation-style* :apa
  "Global style for citations in biliography.")

(defvar *citation-format*
  '(:apa (list
          :article '((author-surname-initials author)
                     (parenthesis year ". ")
                     title
                     (emph journal ", ")
                     volume-issue)
          :web '((author-surname-initials author)
                 (parenthesis year ". ")
                 (emph title ". ")
                 (url web-link)))

    :other '(:article '(author)
             :web '(author)))
  "Order and style rules to generate citation formats.")

(defvar *latex-default-layout*
  (list "\\documentclass{article}~%\\usepackage{hyperref}~%~%\\begin{document}~%~%"
        "\\end{document}")
  "Basic default layout in case the user hasn't provided one.")

(defvar *delimiter-bibliography-start* "~~~bibliography"
  "Custom markdown delimiter for the start of bibliography's section.")

(defvar *delimiter-bibliography-end* "~~~"
  "Custom markdown delimiter for the end of bibliography's section.")

(defvar *delimiter-default* "---"
  "Default markdown delimiter.")
