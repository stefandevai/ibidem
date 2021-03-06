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

(defun build-citation (source citation-style)
  "Build a single citation according to the style provided in `citation-style'."
  (let ((style (get-citation-style citation-style)))
	(mapcar #'(lambda (element)
				(setq style (add-citation-element element source style)))
			(mapcar #'sb-mop:slot-definition-name
					(sb-mop:class-slots (class-of source))))
	style))

(defun add-citation-element (element source style)
  "Return `element' value contained in `source' replaced in `style'."
  (case element
	(id style)
	(www (replace-element-value element source style :extra-style (lambda (x) (url x))))
	(author (replace-author-value source style))
	(otherwise (replace-element-value element source style))))

(defun replace-element-value (element source style &key (extra-style (lambda (string) string)))
  "Return `style' string replaced with a `element' value contained in `source'.
   There's also the possibility of adding custom style with `extra-style'."
  (let* ((element-string (string-downcase (symbol-name element)))
		 (element-style (find-element-style element-string style))
		 (element-value (slot-value source element)))	
	(if element-style
		(if element-value
			(ppcre:regex-replace
			 (str:concat "\\Q" element-style)
			 style
			 (funcall extra-style (str:substring 1 -1 (ppcre:regex-replace element-string
													  element-style
													  element-value))))
			(ppcre:regex-replace (str:concat "\\Q" element-style) style ""))
		style)))

(defun find-element-style (element style)
  "Return single element style in a style string.

  Example:
  (find-element-style \"article\" \"[author, ] [article.]\")
  ;; \"[article.]\""
  (multiple-value-bind (element-start element-end) (cl-ppcre:scan element style)
	(when (and element-start element-end)	  
	  (let* ((element-style-start (search "[" style :from-end t :end2 element-start))
			 (element-style-end (search "]" style :start2 element-end)))
		(when (and element-style-start element-style-end)
		  (str:substring element-style-start (1+ element-style-end) style))))))

(defun replace-author-value (source style)
  "Return `style' string with author(s) name(s) properly formatted."
  (if (search "author" style)
	  (replace-element-value 'author source style)
	  (multiple-value-bind (author-style last-index)(find-author-style style)
		(if author-style
			(str:replace-all ; Remove double punctuation
			 ".." "."
			 (ppcre:regex-replace (str:concat "\\Q" author-style)
								  style
								  (str:concat
								   (build-author-name (tokenize-names (slot-value source 'author))
													  (str:substring 1 last-index author-style))
								   (str:substring last-index -1 author-style))))
			style))))

(defun build-author-name (names style)
  "Return a string with names separated by a comma and formatted according to style string."
  (str:join ", "
			(mapcar (lambda (name)
					  (let ((name-style style))
						(when (search "SURNAME" name-style) ; Replace with uppercase surname
						  (setq name-style
								(ppcre:regex-replace
								 "SURNAME"
								 name-style
								 (string-upcase (car (reverse name))))))
						
						(when (search "surname" name-style) ; Replace with capitalized surname
						  (setq name-style
								(ppcre:regex-replace
								 "surname"
								 name-style
								 (string-capitalize (car (reverse name))))))
						
						(when (ppcre:scan "[^A-Za-z]NAME" name-style) ; Replace with uppercase name
						  (setq name-style
								(ppcre:regex-replace
								 "NAME"
								 name-style
								 (str:join " "
										   (reverse
											(cdr
											 (reverse
											  (mapcar #'string-upcase name))))))))

						(when (ppcre:scan "[^A-Za-z]name" name-style) ; Replace with capitalized name
						  (setq name-style
								(ppcre:regex-replace
								 "name"
								 name-style
								 (str:join " "
										   (reverse
											(cdr
											 (reverse
											  (mapcar #'string-capitalize name))))))))
						
						(when (search "initials" name-style)
						  (setq name-style
								(ppcre:regex-replace
								 "initials"
								 name-style
								 (str:join " "
										   (reverse
											(cdr
											 (reverse
											  (get-name-initials name))))))))
						name-style))
					names)))

(defun find-author-style (style)
  "Return:
  1.  author style if any style keyword is found;
  2.  index of the last keyword found."
  (let ((author-keywords '("name" "NAME" "surname" "SURNAME" "initials"))
		(author-style nil)
		(last-index 0))
	(loop :for keyword :in author-keywords
		  :when (search keyword style) :do (progn
		  									 (setq last-index
		  										   (max last-index
		  												(nth-value 1 (ppcre:scan keyword
		  																		 style))))
											 
		  									 (when (null author-style)
											   (setq author-style
													 (find-element-style keyword style)))))
	(values author-style last-index)))

(defun tokenize-names (names)
  "Return a list of tokens for each name in `names' separated by a comma.

  Example:
  (tokenize-names \"Marc Bloch, Fernand Braudel\")
  ;; => ('(\"Marc\" \"Bloch\") '(\"Fernand\" \"Braudel\"))"
  (mapcar (lambda (name) (str:split " " name))
		  (mapcar #'str:trim (str:split "," names))))

(defun get-name-initials (names)
  "Receive a list of names and return a list of its initials."
  (mapcar (lambda (name) (str:concat
						  (str:substring 0 1 (string-capitalize name))
						  "."))
		  names))

(defun get-citation-style (string)
  "Return a built-in citation style or a customized one."
  (preprocess-style
   (if (and (str:starts-with? "[" string) (str:ends-with? "]" string))
	   string
	   (let ((style (getf *citation-styles*
						  (intern (string-upcase string) :keyword))))
		 (if style
			 style
			 (getf *citation-styles*
				   *default-citation-style*))))))

(defun preprocess-style (string)
  "Add bold, italic and trim style string."
  (make-latex-emphasis
   (make-latex-bold string)))

(defun get-citation-source (citation-id sources)
  "Return a `citation-source' from `sources' list where its id is equal to `citation-id'."
  (let* ((citation-id (subseq citation-id 0 (search "-" citation-id))))
	(find-if (lambda (x) (string= citation-id (id x))) sources)))

(defun make-citation-pages (pages)
  (if (or (position #\, pages) (position #\- pages))
      (str:concat "pp. " pages)
      (str:concat "p. " pages)))

(defun make-latex-bibliography-item (citation object)
  "Return a formatted latex bibliography item."
  (let* ((citation-id (car citation))
		 (citation-source (get-citation-source citation-id (bibliography object))))
	
	(when (> (length citation) 1)
	  (setf (slot-value citation-source 'page) (make-citation-pages (cadr citation))))
	(str:concat (bibitem citation-id)
				(build-citation citation-source (citation-style object))
				"~%~%")))

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
