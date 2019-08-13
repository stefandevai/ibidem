;;;; ========================================================================================== ;;;;
;;;; bibliography.lisp                                                                          ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:md-to-latex)

(defmacro with-occurrences-between (var delimiter-1 delimiter-2 string &body body)
  "Loop through `string' and store substrings between `delimiter-1' and `delimiter-2'
  in `var'."
  (let ((index-1 (gensym))
        (index-2 (gensym)))
    `(loop :for ,index-1 := (search ,delimiter-1 ,string)
        :then (if ,index-2 (search ,delimiter-1 ,string :start2 (+ ,index-2 ,(length delimiter-2))))
        :for ,index-2 := (if ,index-1
                            (search ,delimiter-2 ,string :start2 (+ ,index-1,(length delimiter-1))))
        :then (if ,index-1 (search ,delimiter-2 ,string :start2 (+ ,index-1 ,(length delimiter-1))))
        :while (and ,index-1 ,index-2)
        :do (let ((,var (str:substring (+ ,index-1 (length ,delimiter-1))
                                       ,index-2
                                       ,string)))
              ,@body))))

(defun parse-bibliography (string object)
  "Parse content within markdown's bibliography."
  (let ((biblio-string (string-between +d-bibliography+ +d-bibliography+ string)))
    (when biblio-string
      (with-occurrences-between source-string
          "--"
          "--"
          biblio-string
        (pushnew (parse-citation-source source-string) (bibliography object))))))

(defun parse-citation-source (string)
  (let ((source (make-instance 'citation-source)))
    (setf (id source) (parse-quoted-param "id" string))
    (setf (ctype source) (parse-quoted-param "type" string))
    (setf (author source) (parse-quoted-param "author" string))
    (setf (title source) (parse-quoted-param "title" string))
    (setf (journal source) (parse-quoted-param "journal" string))
    (setf (year source) (parse-quoted-param "year" string))
    (setf (volume-issue source) (parse-quoted-param "volume-issue" string))
    (setf (web-link source) (parse-quoted-param "url" string))
    (return-from parse-citation-source source)))

(defun parse-citations (string object)
  (with-occurrences-between citation-string
      "c["
      "]"
      string
    (pushnew citation-string
             (citations object)))
  (return-from parse-citations string))
