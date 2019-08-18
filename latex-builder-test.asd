;;;; ========================================================================================== ;;;;
;;;; latex-builder-test.asd                                                                     ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:latex-builder-test
  :description "Converts a custom markdown syntax file to latex."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on (#:rove #:str #:cl-ppcre #:unix-opts)
  :components ((:file "test/package-test")

               (:file "src/config")
               (:file "src/latex")
               (:file "src/parse-markdown")
               (:file "src/write-latex")
               (:file "src/main")

               (:file "test/latex-test")
               (:file "test/parse-markdown-test")
               (:file "test/write-latex-test")
               (:file "test/main-test")))
