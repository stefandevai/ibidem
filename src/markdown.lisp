;;;; ========================================================================================== ;;;;
;;;; markdown.lisp                                                                              ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:md-to-latex)

(defclass markdown-object ()
  ((author :initform nil
           :accessor author
           :documentation "Markdown contents' author specified in the header.")

   (location :initform nil
             :accessor location
             :documentation "Markdown contents' location specified in the header.")

   (date :initform nil
         :accessor date
         :documentation "Markdown contents' date specified in the header.")

   (body :initform nil
         :accessor body
         :documentation "Markdown contents between header and bibliography of EOF."))

  (:documentation "Hold information for a markdown file."))

(defun string-between (delimiter-1 delimiter-2 string &key (start 0))
  "Return the substring between `delimiter-1' and `delimiter-2' in `string'"
  (let* ((substring-start (+ (search delimiter-1 string :start2 start)
                             (length delimiter-1)))
         (substring-end (search delimiter-2 string :start2 substring-start)))
    (str:substring substring-start substring-end string)))

(defun parse-markdown (md-str)
  "Receives a markdown string and return a list with parsed contents"
  (let ((object (make-instance 'markdown-object)))
    (parse-header md-str object)
    (parse-body md-str object)
    (format t "~A~%" (date object))
    (return-from parse-markdown object)))

(defun parse-header (string object)
  "Parse content within markdown's header"
  (let ((header-string (string-between "---" "---" string)))
    (setf (author object) (parse-quoted-param "author" header-string))
    (setf (date object) (parse-quoted-param "date" header-string))
    (setf (location object) (parse-quoted-param "location" header-string))))

(defun parse-quoted-param (param header-string)
  "Parse a single parameter in markdown's header with a format: `param: \"content\"'"
  (string-between "\"" "\"" header-string :start (search (str:concat param ":")
                                                         header-string)))

(defun parse-body (string object)
  "Parse content in markdown's body"
  (let ((body-string (string-between "---" "~--" string :start 3)))
    (setf (body object)
          (remove nil
                  (mapcar 'parse-body-line
                          (str:lines body-string))))))

(defun parse-body-line (str)
  "Parse a single line in markdown's body"
  (if (not (str:empty? (str:trim str)))
      (list :line-type (body-line-type str) :content str)
      nil))

(defun body-line-type (str)
  "Defines a type for a markdown line. If it starts by #, it is a heading; if it starts by ## it is
  a section; otherwise it is a paragraph."
  (if (>= (length str) 2)
    (let ((begin-str (str:substring 0 2 str)))
      (cond ((equal begin-str "##") ':section) ; if (starts by "##")
            ((equal begin-str "# ") ':heading) ; else if (starts by "# ")
            (t ':paragraph)))))                ; else
