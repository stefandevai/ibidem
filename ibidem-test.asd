;;;; ========================================================================================== ;;;;
;;;; ibidem-test.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem "ibidem-test"
  :description "Converts a custom markdown syntax file to latex."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :depends-on ("ibidem" "rove")
  :components ((:file "test/package-test")
               (:file "test/latex-test")
               (:file "test/parse-markdown-test")
               (:file "test/write-latex-test")
               (:file "test/main-test")))
