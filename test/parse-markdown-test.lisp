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
    (ok (equal nil (parse-quoted-param "aa" "aa param:    \"value\""))))

  (testing "parse markdown header"
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
