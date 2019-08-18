;;;; ========================================================================================== ;;;;
;;;; parse-markdown.lisp                                                                        ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:latex-builder)

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Helper functions and macros                                                                  ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defmacro do-occurrences-between (var delimiter-1 delimiter-2 string &body body)
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
  (when param
    (let ((param-index (search (str:concat param ":") header-string)))
      (if param-index
          (string-between "\"" "\"" header-string :start (search (str:concat param ":")
                                                                 header-string))))))

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
  (let ((header-string (string-between *delimiter-default* *delimiter-default* string)))
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
  (let* ((has-bibliography? (string-between *delimiter-bibliography-start* *delimiter-bibliography-end* string))
         (body-string (if has-bibliography?
                          (parse-citations (string-between *delimiter-default*
                                                           *delimiter-bibliography-start*
                                                           string
                                                           :start 3)
                                           object)
                          (string-between *delimiter-default* nil string :start 3))))
    (setf (body object) (parse-body-lines body-string))))

(defun parse-body-lines (string)
  "Loop through lines in `string' and return a tree of markdown elements."
  (remove nil
          (let* ((lines (str:lines string))
                 (types (mapcar #'body-line-type lines))
                 (llength (length lines)))

            (loop :with content := nil
               :for index :from 0 :below llength

               :if (equal :list-item (elt types index))
               :do (let ((md-list (list :line-type :list
                                        :content (loop :for i :from index :below llength
                                                    :until (not (equal ':list-item (elt types i)))
                                                    :collecting (elt lines i)))))
                     (setq content md-list)
                     (setq index (+ index (length md-list))))

               :else :do (if (str:empty? (elt lines index))
                             (setq content nil)
                             (setq content (list :line-type (elt types index)
                                                 :content (elt lines index))))
               :collect content))))

(defun parse-body-line (str)
  "Parse a single line in markdown's body."
  (if (not (str:empty? (str:trim str)))
      (list :line-type (body-line-type str) :content str)
      nil))

(defun body-line-type (str)
  "Defines a type for a markdown line. If it starts by:
  ####, it is a subsubsection;
  ###, it is a subsection;
  ##, it is a section;
  # , it is a heading;
  $, it is a math formulation;
  - , it a list item;
  otherwise it is a paragraph."
  (if (>= (length (str:trim str)) 2)
      (let* ((trimmed-str (str:trim str))
             (begin-str (str:substring 0 2 trimmed-str)))
        (cond ((and (>= (length trimmed-str) 4)
                   (equal (str:substring 0 4 trimmed-str) "####"))
               ':subsubsection)
              ((and (>= (length trimmed-str) 3)
                   (equal (str:substring 0 3 trimmed-str) "###"))
               ':subsection)
              ((equal begin-str "##")
               ':section)
              ((equal begin-str "# ")
               ':heading)
              ((char= (char begin-str 0) #\$)
               ':maths)
              ((and (char= (char begin-str 0) #\-)
                  (char= (char begin-str 1) #\Space))
               ':list-item)
              ((or (char= (char trimmed-str 0) #\")
                 (char= (char trimmed-str 0) #\“))
               ':quote)
              (t ':paragraph)))
      ':paragraph))


;;; -------------------------------------------------------------------------------------------- ;;;
;;; Bibliography parsing                                                                         ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun parse-bibliography (string object)
  "Parse content within markdown's bibliography."
  (let ((biblio-string (string-between *delimiter-bibliography-start* *delimiter-bibliography-end* string)))
    (when biblio-string
      (do-occurrences-between source-string
          "---"
          "---"
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
  (let ((cl-ppcre:*allow-quoting* t)
        (counter 1))
    (do-occurrences-between citation-string
      "c["
      "]"
      string
      (let ((citation (parse-citation citation-string counter)))
        (pushnew citation (citations object))

        ;; TODO: Create a function that modifies the value of string
        ;; instead of making a new copy each funcall.
        ;; Use nsubstitute http://clhs.lisp.se/Body/f_sbs_s.htm
        (setf string (substitute-citation string citation-string citation)))
      (incf counter))

    (setf (citations object) (reverse (citations object)))
    (return-from parse-citations string)))

(defun parse-citation (citation-string &optional counter)
  "Parse an `id'and `pages' in citation-string separated by an ':' and
  add a counter (if provided) to the id."
  (let* ((citation-raw (str:split ":" citation-string :omit-nulls t))
         (first-item (car citation-raw)))
    (if counter
        (substitute (str:concat first-item "-" (write-to-string counter))
                first-item
                citation-raw
                :test #'equal)
        (return-from parse-citation citation-raw))))

(defun substitute-citation (string citation-string citation)
  "Return a string with the original citation-string substituted with a
  LaTeX citation."
  (ppcre:regex-replace (concatenate 'string "\\Qc[" citation-string "]")
                       string
                       (latex-element "cite" nil (car citation))))
