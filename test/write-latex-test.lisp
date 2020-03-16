;;;; ========================================================================================== ;;;;
;;;; write-latex-test.lisp                                                                      ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:ibidem)

(deftest write-latex-header
  (testing "write single latex header item"
    (ok (equal (make-latex-header-item "name") "\\textbf{name}~%\\linebreak~%"))
    (ok (equal (make-latex-header-item nil) (make-latex-header-item nil))))

  (testing "write latex quote"
    (ok (equal "\\begin{quote}~%\\textit{aaaaaaaa~%}\\end{quote}"
               (make-latex-quote '("> aaaaaaaa"))))
    (ok (equal "\\begin{quote}~%\\textit{aaa~%aaa~%}\\end{quote}"
               (make-latex-quote '("> aaa" "> aaa"))))))

(deftest write-latex-body
  (testing "write latex lists"
    (ok (equal "\\begin{itemize}~%\\item aaaaaaaa~%\\end{itemize}"
               (make-latex-list '("- aaaaaaaa"))))
    (ok (equal "\\begin{itemize}~%\\item aaa~%\\item aaa~%\\item aaa~%\\end{itemize}"
               (make-latex-list '("- aaa" "- aaa" "- aaa")))))

  (testing "write latex headings"
    (ok (equal "\\section*{aaa}"
               (make-latex-heading "# aaa")))
    (ok (equal "\\subsection*{aaa}"
               (make-latex-heading "## aaa")))
    (ok (equal "\\subsubsection*{aaa}"
               (make-latex-heading "### aaa")))
    (ok (equal "\\subsubsection*{aaa}"
               (make-latex-heading "############### aaa"))))

  (testing "escape special characters in latex"
    (ok (equal "aaa 25\\% aaa"
               (latex-escape "aaa 25% aaa")))
    (ok (equal "\\%\\%\\%"
               (latex-escape "%%%")))
    (ok (equal "~%~%~%"
               (latex-escape "~%~%~%")))
    (ok (equal "aaa 25\\% aaa~%"
               (latex-escape "aaa 25% aaa~%"))))

  (testing "write latex bold"
    (ng (equal "\\textbf{aaa}"
               (make-latex-bold "**aaa*")))
    (ok (equal "bbb\\textbf{aaa}bbb"
               (make-latex-bold "bbb**aaa**bbb")))
    (ok (equal "\\textbf{aaa}*"
               (make-latex-bold "**aaa***")))
    (ok (equal "\\textbf{aaa}"
               (make-latex-bold "**aaa**")))
    (ok (equal "aaa \\textbf{aaa} aaa"
               (make-latex-bold "aaa **aaa** aaa"))))

  (testing "write latex emphasis"
    (ng (equal "\\emph{aaa}"
               (make-latex-emphasis "*aaa")))
    (ok (equal "bbb\\emph{aaa}bbb"
               (make-latex-emphasis "bbb*aaa*bbb")))
    (ok (equal "\\emph{aaa}*"
               (make-latex-emphasis "*aaa**")))
    (ok (equal "\\emph{aaa}"
               (make-latex-emphasis "*aaa*")))
    (ok (equal "aaa \\emph{aaa} aaa"
               (make-latex-emphasis "aaa *aaa* aaa")))))
