;;;; ========================================================================================== ;;;;
;;;; write-markdown.lisp                                                                        ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:ml)

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Helper functions and macros                                                                  ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defmacro with-occurrences-between (var delimiter-1 delimiter-2 string &body body)
  "Loop through `string' and store substrings between `delimiter-1' and 2`delimiter-2'
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

(defun parse-quoted-param (param header-string)
  "Parse a single parameter in markdown's header with a format: `param: \"content\"'"
  (let ((param-index (search (str:concat param ":") header-string)))
    (if param-index
        (string-between "\"" "\"" header-string :start (search (str:concat param ":")
                                                         header-string)))))

(defun parse-markdown (md-str)
  "Receives a markdown string and return a list with parsed contents."
  (let ((object (make-instance 'markdown-object)))
    (parse-header md-str object)
    (parse-body md-str object)
    (parse-bibliography md-str object)
    (return-from parse-markdown object)))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Header parsing                                                                               ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun parse-header (string object)
  "Parse content within markdown's header."
  (let ((header-string (string-between "---" "---" string)))
    (setf (author object) (parse-quoted-param "author" header-string))
    (setf (date object) (parse-quoted-param "date" header-string))
    (setf (location object) (parse-quoted-param "location" header-string))))


;;; -------------------------------------------------------------------------------------------- ;;;
;;; Body parsing                                                                                 ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun parse-body (string object)
  "Parse content in markdown's body."
  ;; Checks if there is a bibliography section;
  ;; if there is, body string is the section in between;
  ;; otherwise it's the string after the header until the end-of-file.
  (let* ((has-bibliography? (string-between "~--" "~--" string))
         (body-string (if has-bibliography?
                          (parse-citations (string-between "---" "~--" string :start 3)
                                           object)
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

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Bibliography parsing                                                                         ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

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
  "Parse citation source fields contained in `string' and return a instance of `citation-source'."
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
  "Parses each citation contained in `string' and push them to `citations' in `object'."
  (with-occurrences-between citation-string
      "c["
      "]"
      string
    (pushnew citation-string (citations object)))
  (return-from parse-citations string))
