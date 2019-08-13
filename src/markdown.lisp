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
         :documentation "Markdown contents between header and bibliography of EOF.")

   (citations :initform nil
              :accessor citations
              :documentation "Hold citations found in text.")

   (bibliography :initform nil
                 :accessor bibliography
                 :documentation "Keep all citation source objects."))

  (:documentation "Hold information about a markdown file structure."))

(defun string-between (delimiter-1 delimiter-2 string &key (start 0))
  "Return the substring between `delimiter-1' and `delimiter-2' in `string'.
  If delimiter-2 is nil, return the string from delimiter-1 until the end-of-file."
  (let* ((delimiter-found-p (search delimiter-1 string :start2 start))
         (substring-start (if delimiter-found-p
                              (+ delimiter-found-p (length delimiter-1))))
         (substring-end (if (and delimiter-found-p delimiter-2)
                            (search delimiter-2 string :start2 substring-start))))

    ;; cond (1) If we found the first delimiter and delimiter-2 is explicitly nil;
    ;; cond (2) If we found both delimiters.
    (cond ((and delimiter-found-p (null delimiter-2))
           (str:substring substring-start nil string))
          ((and substring-start substring-end)
           (str:substring substring-start substring-end string)))))

(defun parse-markdown (md-str)
  "Receives a markdown string and return a list with parsed contents."
  (let ((object (make-instance 'markdown-object)))
    (parse-header md-str object)
    (parse-body md-str object)
    (parse-bibliography md-str object)
    (return-from parse-markdown object)))

(defun parse-header (string object)
  "Parse content within markdown's header."
  (let ((header-string (string-between "---" "---" string)))
    (setf (author object) (parse-quoted-param "author" header-string))
    (setf (date object) (parse-quoted-param "date" header-string))
    (setf (location object) (parse-quoted-param "location" header-string))))

(defun parse-quoted-param (param header-string)
  "Parse a single parameter in markdown's header with a format: `param: \"content\"'"
  (let ((param-index (search (str:concat param ":") header-string)))
    (if param-index
        (string-between "\"" "\"" header-string :start (search (str:concat param ":")
                                                         header-string)))))

(defun parse-body (string object)
  "Parse content in markdown's body."
  ;; Checks if there is a bibliography section;
  ;; if there is, body string is the section in between;
  ;; otherwise it's the string after the header until the end-of-file.
  (let* ((has-bibliography? (string-between "~--" "~--" string))
         (body-string (if has-bibliography?
                          (string-between "---" "~--" string :start 3)
                          (string-between "---" nil string :start 3))))
    (setf (body object)
          (remove nil
                  (mapcar 'parse-body-line
                          (str:lines body-string))))))

(defun parse-body-line (str)
  "Parse a single line in markdown's body."
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
