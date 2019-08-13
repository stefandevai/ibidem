;;;; md-to-latex.asd

(asdf:defsystem #:md-to-latex
  :description "Convert a markdown file to a latex file."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:file "package")
               (:file "src/definitions")
               (:file "src/latex")
               (:file "src/markdown")
               (:file "src/bibliography")
               (:file "src/main")))
