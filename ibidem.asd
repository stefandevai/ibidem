;;;; ========================================================================================== ;;;;
;;;; ibidem.asd                                                                                 ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem "ibidem"
  :description "CLI tool that makes it easier to write academic papers with citations."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.6.2"
  :build-operation "program-op"
  :build-pathname "./build/ibidem"
  :entry-point "ibidem:main"
  :depends-on ("str" "cl-ppcre" "unix-opts")
  :components ((:module "src"
                :components
                 ((:file "package")
                 (:file "config")
                 (:file "latex" :depends-on ("config"))
                 (:file "parse-markdown" :depends-on ("config"))
                 (:file "write-latex" :depends-on ("latex" "parse-markdown"))
                 (:file "main" :depends-on ("write-latex" "parse-markdown"))))))
