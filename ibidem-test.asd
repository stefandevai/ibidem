;;;; ========================================================================================== ;;;;
;;;; ibidem-test.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem "ibidem-test"
  :description "CLI tool that makes it easier to write academic papers with citations."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.5.1"
  :serial t
  :depends-on ("ibidem" "rove")
  :components ((:file "test/package-test")
               (:file "test/latex-test")
               (:file "test/parse-markdown-test")
               (:file "test/write-latex-test")
               (:file "test/main-test")))
