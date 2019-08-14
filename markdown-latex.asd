;;;; ========================================================================================== ;;;;
;;;; markdown-latex.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:markdown-latex
  :description "Convert a markdown file to a latex file."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:file "package")
               (:file "src/definitions")
               (:file "src/latex")
               (:file "src/parse-markdown")
               (:file "src/write-latex")
               (:file "src/main")))
