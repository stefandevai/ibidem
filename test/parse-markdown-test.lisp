;;;; ========================================================================================== ;;;;
;;;; parse-markdown-test.lisp                                                                   ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

(deftest string-parsing
  (testing "occurences between delimiters"
    (ok (equal '("a" "b" "c")
               (let ((a-list nil))
                 (do-occurrences-between var "-1-" "-2-" "-1-a-2--1-b-2--1-c-2-"
                   (pushnew var a-list))
                 (reverse a-list))))

    (ok (equal '("a" "b")
               (let ((a-list nil))
                 (do-occurrences-between var "-1-" "-2-" "-1-a-2--1-b-2--1-c-1-"
                   (pushnew var a-list))
                 (reverse a-list))))

    (ok (equal '("a" "b" "c")
               (let ((a-list nil))
                 (do-occurrences-between var "--" "--" "--a----b----c--"
                   (pushnew var a-list))
                 (reverse a-list))))

    (ok (equal '("a" "c")
               (let ((a-list nil))
                 (do-occurrences-between var "--" "--" "--a--b--c--"
                   (pushnew var a-list))
                 (reverse a-list)))))

  (testing "string between delimiters"
    (ok (equal "test" (string-between "*" "*" "aa*test*aa")))
    (ok (equal nil (string-between "*" "*-" "aaa*test*aaa")))
    (ok (equal "testing" (string-between "*" nil "aa*testing")))
    (ok (equal "test" (string-between "*1" "*2" "aa*1test*2aa")))
    (ok (equal "test2" (string-between "*" "*" "a*test1*a*test2*a" :start 8))))

  (testing "parse quoted params"
    (ok (equal "value" (parse-quoted-param "param" "aa param:    \"value\"")))
    (ok (equal nil (parse-quoted-param nil "aa param:    \"value\"")))
    (ok (equal nil (parse-quoted-param "aa" "aa param:    \"value\"")))))

(deftest markdown-header-parsing
  (testing "parse markdown header params"
    (let ((object (make-instance 'markdown-object)))
      (parse-header "---\
                    author: \"Stefan Devai\"\
                    date: \"01/02/1900\" \
                    location: \"Wales\"\
                    ---"
                    object)
      (ok (equal "Stefan Devai" (author object)))
      (ok (equal "01/02/1900" (date object)))
      (ok (equal "Wales" (location object))))

    (let ((object (make-instance 'markdown-object)))
      (parse-header "---\
                    author: \"Ninel\"\
                    location: \"Finland\"\
                    ---"
                    object)
      (ok (equal "Ninel" (author object)))
      (ok (equal nil (date object)))
      (ok (equal "Finland" (location object))))))

(deftest markdown-body-parsing
  (testing "markdown line type"
    (ok (null (parse-body-line "  ")))
    (ok (null (parse-body-line nil)))
    (ok (null (parse-body-line "")))
    (ok (equal ':paragraph (body-line-type "-abcdef")))
    (ok (equal ':paragraph (body-line-type "##abcdef")))
    (ok (equal ':paragraph (body-line-type "#abcdef")))
    (ok (equal ':paragraph (body-line-type "  abcdef")))
    (ok (equal ':paragraph (body-line-type "abcdef")))
    (ok (equal ':subsubsection (body-line-type "############# aaa")))
    (ng (equal ':subsubsection (body-line-type "#############aaa")))
    (ok (equal ':subsubsection (body-line-type "### aaa")))
    (ng (equal ':subsubsection (body-line-type "###aaa")))
    (ok (equal ':subsection (body-line-type "## aaa")))
    (ng (equal ':subsection (body-line-type "##aaa")))
    (ok (equal ':section (body-line-type "# aaa")))
    (ng (equal ':section (body-line-type "#aaa")))
    (ok (equal ':maths (body-line-type "$e^2$")))
    (ng (equal ':list (body-line-type "-aaa")))
    (ok (equal ':list (body-line-type "- aaa")))
    (ok (equal ':quote (body-line-type ">aaa")))
    (ok (equal ':quote (body-line-type "   >aaa")))
    (ok (equal ':quote (body-line-type "   > aaa")))
    (ok (equal ':quote (body-line-type "> aaa"))))

  (testing "parse lists"
    (ok (equal (list :line-type :list :content (list "- li" "- li" "- li"))
               (parse-body-group :list'("- li" "- li" "- li")
                                 '(:list :list :list))))

    (ok (equal (list :line-type :list :content (list "- li"))
               (parse-body-group :list'("- li" "nli" "- li")
                                 '(:list :paragraph :list))))

    (ok (equal (list :line-type :list :content (list "- li"))
               (parse-body-group :list'("- li" "nli" "nli")
                                 '(:list :paragraph :paragraph))))

    (ok (equal (list :line-type :list :content (list "- li" "- li"))
               (parse-body-group :list '("- li" "- li" "-nli")
                                 '(:list :list :paragraph))))))

(deftest markdown-bibliography-parsing
  (testing "citation source parsing"
    (let* ((cit-src (parse-citation-source "id: \"aid\"
                                           type: \"atype\"
                                           author: \"aauthor\"
                                           title:     \"atitle\"
                                           journal: \"ajournal\"
                                           year:\"2019\"
                                           volume-issue:  \"1(2)\"
                                           url: \"aurl\"")))
    (ok (equal "aid" (id cit-src)))
    (ok (equal "atype" (ctype cit-src)))
    (ok (equal "aauthor" (author cit-src)))
    (ok (equal "atitle" (title cit-src)))
    (ok (equal "ajournal" (journal cit-src)))
    (ok (equal "2019" (year cit-src)))
    (ok (equal "1(2)" (volume-issue cit-src)))
    (ok (equal "aurl" (web-link cit-src)))))

  (testing "markdown citation parsing"
    (ok (null (parse-citation nil)))
    (ok (equal '("aa") (parse-citation "aa")))
    (ok (equal '("aa-3") (parse-citation "aa" 3)))
    (ok (equal '("aa-3" "12") (parse-citation "aa:12" 3)))
    (ok (equal '("aa" "12") (parse-citation "aa:12"))))

  (testing "markdown citation substitution"
    (ng (equal "aa\\cite{a1} aa" (substitute-citation "aac[a] aa" "a" '("a1" "12"))))
    (ok (equal "aa \\cite{a1}aa" (substitute-citation "aa c[a]aa" "a" '("a1" "12"))))
    (ng (equal "aa\\cite{a1}aa" (substitute-citation "aac[a]aa" "a" '("a1" "12"))))
    (ok (equal "aa \\cite{a1} aa" (substitute-citation "aa c[a] aa" "a" '("a1" "12"))))
    (ok (equal " \\cite{a1}" (substitute-citation " c[a]" "a" '("a1" "12")))))

  (testing "parsing markdown multiple citations into object"
    (let* ((md-obj (make-instance 'markdown-object)))
      (ok (equal "aa \\cite{aa-1} aa \\cite{bb-2}. 12 \\cite{cc-3}, \\cite{dd-4}"
                 (parse-citations "aa c[aa:12] aa c[bb:14-17]. 12 c[cc:1,2], c[dd]"
                                  md-obj)))

      (ok (equal '(("aa-1" "12") ("bb-2" "14-17") ("cc-3" "1,2") ("dd-4"))
                 (citations md-obj))))))
