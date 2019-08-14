;;;; ========================================================================================== ;;;;
;;;; markdown-latex.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:latex-builder
  :description "Converts a custom markdown syntax file to latex."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:file "src/package")
               (:file "src/definitions")
               (:file "src/latex")
               (:file "src/parse-markdown")
               (:file "src/write-latex")
               (:file "src/main")))
