;;;; ========================================================================================== ;;;;
;;;; write-latex-test.lisp                                                                      ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(testing "write latex header"
  (ok (equal (make-latex-header-item "name") "\\textbf{name}~%\\linebreak~%"))
  (ok (equal 3 2)))

(rove:run-suite :latex-builder)
