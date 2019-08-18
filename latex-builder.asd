;;;; ========================================================================================== ;;;;
;;;; latex-builder.asd                                                                          ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:latex-builder
  :description "Converts a custom markdown syntax file to latex."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config")
                 (:file "latex")
                 (:file "parse-markdown")
                 (:file "write-latex")
                 (:file "main")))))
