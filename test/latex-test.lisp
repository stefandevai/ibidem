;;;; ========================================================================================== ;;;;
;;;; latex-test.lisp                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:ibidem)

(deftest latex-creation-test
  (testing "general macros"
    (ok (equal (surround-string "aa" "cc" "bb") "aabbcc"))
    (ok (equal (surround-string "aa" "cc" "bb" "dd") "aabbddcc"))
    (ok (equal (surround-string "aa" "cc" (if (> 0 1) "ff" "gg") "ss") "aaggsscc"))

    (ok (equal (latex-element "testel" nil "testc") "\\testel{testc}"))
    (ok (equal (latex-element "testel" t "testc") "\\testel{testc}~%")))

  (testing "latex macros"
    (ok (equal (parenthesis "test") "(test)"))
    (ok (equal (bold "test") "\\textbf{test}"))
    (ok (equal (emph "test") "\\emph{test}"))
    (ok (equal (url "test") "\\url{test}"))
    (ok (equal (noindent "test") "\\noindent{test}"))
    (ok (equal (uppercase "test") "\\uppercase{test}"))
    (ok (equal (section "test") "\\section*{test}"))
    (ok (equal (begin-end "arg" "test") "\\begin{arg}~%test\\end{arg}"))
    (ok (equal (begin-end "arg" (section (bold (emph (url (noindent (uppercase (parenthesis "test"))))))))
               "\\begin{arg}~%\\section*{\\textbf{\\emph{\\url{\\noindent{\\uppercase{(test)}}}}}}\\end{arg}"))))
