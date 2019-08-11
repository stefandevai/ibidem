;;;; md-to-latex.asd

(asdf:defsystem #:md-to-latex
  :description "Describe md-to-latex here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:cl-ppcre #:unix-opts)
  :components ((:file "package")
               (:file "md-to-latex")))
