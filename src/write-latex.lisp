;;;; ========================================================================================== ;;;;
;;;; write-latex.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:ibidem)

(defun make-latex-article (object &optional layout)
  "Creates a Latex string from a `markdown-object' instance."
  (destructuring-bind (begin-str end-str) (latex-layout layout)
    (str:concat begin-str
                "~%~%"
                (str:concat (make-latex-header object)
                            (make-latex-body object)
                            (make-latex-bibliography object))
                "~%"
                end-str)))

(defun latex-layout (layout)
  "Return a LaTeX layout list with two strings to surround the article."
  (if (null layout)
      *latex-default-layout*
      (latex-layout-from-file layout)))

(defun latex-layout-from-file (file-path)
  "Return a custom user layout read from `file-path'."
  (let* ((layout-string (uiop:read-file-string file-path))

         (end-delimiter "\\end{document}")
         (end-index (search end-delimiter layout-string))

         (start-delimiter "\\begin{document}")
         (start-index (when start-delimiter
                        (search '(#\Newline)
                                layout-string
                                :start2 (search start-delimiter
                                                layout-string)))))
    (if (and start-index end-index)
        (list (str:substring 0 start-index layout-string)
              (str:substring end-index nil layout-string))
        (error "> FATAL: Layout file is badly formatted.~%"))))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Header writing                                                                               ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun make-latex-header (object)
  "Receive a `markdown-object' and return a formatted latex header string."
  (str:concat (begin-end "flushright"
                (if (author object) (make-latex-header-item (author object)))
                (if (location object) (make-latex-header-item (location object)))
                (if (date object) (make-latex-header-item (date object))))
              "~%~%"))

(defun make-latex-header-item (item)
  "Receive a single header item and return a formatted latex string."
  (str:concat (bold item) "~%"  *linebreak*))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Body writing                                                                                 ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defun make-latex-body (object)
  "Receive a `markdown-object' and return a formated latex body string."
  (reduce #'str:concat (mapcar 'make-latex-body-line (body object))))

(defun make-latex-body-line (line)
  "Receive a single body line and return a formatted latex string
  according with the body `line-type'."
  (str::concat
   (make-latex-emphasis
    (make-latex-bold
     (make-latex-url
      (make-latex-href
       (make-latex-image
		(latex-escape
		 (ecase (getf line :line-type)
		   (:paragraph (getf line :content))
		   (:list (make-latex-list (getf line :content)))
		   ((or :section :subsection :subsubsection)
			(make-latex-heading (getf line :content)))
		   (:quote (make-latex-quote (getf line :content))))))))))
   "~%~%"))

(defun make-latex-quote (lines)
  "Return a formatted string as latex quote."
  (format t "here~%")
  (begin-end "quote"
	(textit
	  (reduce
	   #'str:concat
	   (mapcar
		(lambda (line)
		  (str:concat
		   (str:substring
		    (position-if-not
		     #'(lambda (c) (char= c #\Space))
		     (str:trim-left line)
		     :start 1)
		    nil
		    line) "~%")) lines)))))

(defun make-latex-list (items)
  "Return a formatted string as a latex list of `items'."
  (begin-end "itemize"
	(reduce
	 #'str:concat
     (mapcar
      #'(lambda (line)
          (str:concat "\\item"
                      (str:substring 1 nil (str:trim-left line))
                      "~%"))
      items))))

(defun make-latex-heading (string)
  "Return a formatted string as a latex section, subsection or subsubsection."
  (let* ((trimmed-heading (str:trim-left string))
         (space-index (position #\Space trimmed-heading))
         (section-str (case space-index
						(1 "section*")
						(2 "subsection*")
						(otherwise "subsubsection*"))))

    (latex-element section-str nil
      (str:trim-left (str:substring space-index nil trimmed-heading)))))

(defun latex-escape (text)
  "Return `text' with escaped LaTeX special characters."
  (ppcre:regex-replace-all (str:concat "(?<!~)(["
                                       (coerce *latex-chars-to-escape* 'string)
                                       "])")
                           text
                           "\\\\\\1"))

(defun make-latex-bold (text)
  "Return `text' with all instances of markdown bold as latex bold.

  Example:
  (make-latex-bold \"aaa **aaa**  aaa\")
  ;; => \"aaa \\textbf{aaa} aaa\""
  (ppcre:regex-replace-all "\\*\\*([^\\*]\\S(.*?\\S)?)\\*\\*" text (bold "\\1")))

(defun make-latex-emphasis (text)
  "Return `text' with all instances of markdown bold as latex emphasis.

  Example:
  (make-latex-emphasis \"aaa **aaa**  aaa\")
  ;; => \"aaa \\emph{aaa} aaa\""
  (ppcre:regex-replace-all "\\*([^\\*]\\S(.*?\\S)?)\\*" text (emph "\\1")))

(defun make-latex-href (text)
  "Return `text' with all instances of markdown link as a latex link.
  Example:
  (make-latex-link \"Click [here](https://test.com/).\")
  ;; => \"Click \\href{https://test.com/}{here}.\""
  (ppcre:regex-replace-all "[\\s]\\[(.*?\\S)\\]\\((.*?\\S)?\\)" text (href "\\2" "\\1")))

(defun make-latex-url (text)
  "Return `text' with all instances of markdown link as a latex url.
  Example:
  (make-latex-url \"A url: https://test.com/. Another one: <http://as.com/>.\")
  ;; => \"A url: \\url{https://test.com/}. Another one: \\url{http://as.com/}.\""
  (ppcre:regex-replace-all "<((?:http).\\S+\\..\\S+)>" text (url "\\1")))

(defun make-latex-image (text)
  "Return `text' with all instances of markdown link as a latex url.
  Example:
  (make-latex-url \"A url: https://test.com/. Another one: <http://as.com/>.\")
  ;; => \"A url: \\url{https://test.com/}. Another one: \\url{http://as.com/}.\""
  (let ((result text))
    (ppcre:do-register-groups
		(cap link)
		("!\\[([^\\]]+|.*)\\]\\((\\S+)\\)" text)
	  (setq result
			(str:replace-all (str:concat "![" cap "](" link ")")
							 (image link cap)
							 result)))
    result))

;;; -------------------------------------------------------------------------------------------- ;;;
;;; Bibliography writing                                                                         ;;;
;;; -------------------------------------------------------------------------------------------- ;;;

(defmacro build-citation (source citation-style)
  "Build a single citation according to a format provided in `source'."
  (let ((style-params (gensym))
        (params (gensym)))
    `(let* ((,style-params (get-citation-style ,citation-style))
			(,params (getf ,style-params
						   (read-from-string
							(str:concat ":"
										(slot-value ,source
													'ctype))))))
       (reduce #'str:concat
               (loop :for param :in ,params
					 :collect (cond ((listp param)
									 (let ((param-value (slot-value ,source (cadr param))))
									   (when param-value
										 (str:concat (eval (list (car param) param-value))
													 (reduce #'str:concat (cddr param))))))
									(t (when (slot-value ,source param)
										 (str:concat (slot-value ,source param) ". ")))))))))

(defun get-citation-style (string)
  "Return a built-in citation style or a customized one.

  Examples:
  (citation-format \"APA\")
  (citation-format \"[SURNAME name, « title », source, volume, publisher, year, pages.]\")"
  (if (and (str:starts-with? "[" string) (str:ends-with? "]" string))
						(build-citation-style string)
						(let ((style (getf *citation-formats*
																									(intern (string-upcase string) :keyword))))
								(if style
												style
												(getf *citation-formats* *default-citation-format*)))))

(defun build-citation-style (string)
		"TODO: return a default citation style from a string"
		(getf *citation-formats* *default-citation-format*))

(defun author-surname-initials (string)
  "Return author' surname followed by the initials of the other names.

  Example:
  (author-surname-initials \"Fernand Paul Achille Braudel\")
  ;; => \"Braudel, F. P. A.\""
  (let ((names (str:split #\Space string)))
    (str:concat (car (reverse names))
                ", "
                (reduce #'str:concat
                        (mapcar (lambda (name) (str:concat (str:s-first name) ". "))
                                (reverse (cdr (reverse names))))))))



(defun get-citation-source (citation-id sources)
  "Return a `citation-source' from `sources' list where its id is equal to `citation-id'."
  (let* ((citation-id (subseq citation-id 0 (search "-" citation-id))))
	(find-if (lambda (x) (string= citation-id (id x))) sources)))

(defun make-citation (citation sources citation-style)
  "Return a formatted latex citation.
  `string' cointains information about the citation as \"citation-id:pages\";
  `sources' contains a list of `citation-source'."
  (str:concat
   (build-citation
    (get-citation-source (car citation) sources) citation-style)))

(defun make-citation-pages (pages)
  (if (or (position #\, pages) (position #\- pages))
      (str:concat "pp. " pages ". ")
      (str:concat "p. " pages ". " )))

(defun make-latex-bibliography-item (citation object)
  "Return a formatted latex bibliography item."
  (str:concat (bibitem (car citation))
			  (make-citation citation (bibliography object) (citation-style object))
			  (when (> (length citation) 1) (make-citation-pages (cadr citation)))
			  "~%~%"))

(defun make-latex-bibliography (object)
  "Return a formatted latex bibliography.
  `object' is a `markdown-object' that contains information about the citations and sources."
  (let ((citations (citations object)))
	(when citations
	  (str:concat
	   (when *bibliography-in-newpage* "\\newpage~%~%")
	   (thebibliography
		 (str:concat
		  "\\raggedright~%"
		  (reduce #'str:concat
				  (loop :for citation :in citations
						:collect (make-latex-bibliography-item citation
															   object)))))))))
