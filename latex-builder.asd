;;;; ========================================================================================== ;;;;
;;;; latex-builder.asd                                                                          ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:latex-builder
  :description "Converts a custom markdown syntax file to latex."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :build-operation "program-op"
  :build-pathname "./build/latex-builder"
  :entry-point "latex-builder:main"
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:module "src"
                :components
                 ((:file "package")
                 (:file "config")
                 (:file "latex" :depends-on ("config"))
                 (:file "parse-markdown" :depends-on ("config"))
                 (:file "write-latex" :depends-on ("latex"))
                 (:file "main" :depends-on ("write-latex" "parse-markdown"))))))
