;;;; latex-article.asd

(asdf:defsystem #:latex-article
  :description "Describe latex-article here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str)
  :components ((:file "package")
               (:file "latex-article")))
