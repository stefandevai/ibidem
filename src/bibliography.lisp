;;;; ========================================================================================== ;;;;
;;;; bibliography.lisp                                                                          ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:md-to-latex)

(defvar *d-bibliography* "~--"
  "Custom markdown delimiter for the bibliography section.")

(defvar *d-citation-source* "--"
  "Custom markdown delimiter for a single citation source.")

(defclass citation-source ()
  ((id :initform nil
       :accessor id
       :documentation "Unique id for this citation source.")

   (ctype :initform nil
          :accessor ctype
          :documentation "Type of source: article or web.")

   (author :initform nil
           :accessor author
           :documentation "Author or authors of source.")

   (title :initform nil
          :accessor title
          :documentation "Source's title.")

   (journal :initform nil
            :accessor journal
            :documentation "Journal of publication if source is of article type.")

   (year :initform nil
         :accessor year
         :documentation "Year of source's publication.")

   (volume-issue :initform nil
                 :accessor volume-issue
                 :documentation "Issue and volume if source is of article type.")

   (url :initform nil
        :accessor url
        :documentation "Resource's url if source is of web type."))

  (:documentation "Hold information about a single citation source."))

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
  (let ((biblio-string (string-between *d-bibliography* *d-bibliography* string)))
    (when biblio-string
      (with-occurrences-between citation-string
          "--"
          "--"
          biblio-string
        (pushnew (parse-citation-source citation-string) (bibliography object))))))

(defun parse-citation-source (string)
  (let ((source (make-instance 'citation-source)))
    (print (parse-quoted-param "id" string))
    (setf (id source) (parse-quoted-param "id" string))
    (setf (ctype source) (parse-quoted-param "type" string))
    (setf (author source) (parse-quoted-param "author" string))
    (setf (title source) (parse-quoted-param "title" string))
    (setf (journal source) (parse-quoted-param "journal" string))
    (setf (year source) (parse-quoted-param "year" string))
    (setf (volume-issue source) (parse-quoted-param "volume-issue" string))
    (setf (url source) (parse-quoted-param "url" string))
    (return-from parse-citation-source source)))

(defun parse-citations (string)
  )
