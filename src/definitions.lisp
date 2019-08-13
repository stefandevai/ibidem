;;;; ========================================================================================== ;;;;
;;;; definitions.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:md-to-latex)

(defconstant +d-bibliography+ "~--"
  "Custom markdown delimiter for the bibliography section.")

(defconstant +d-citation-source+ "--"
  "Custom markdown delimiter for a single citation source.")

(defconstant +citation-style+ :apa
  "Global style for citations in biliography.")

(defconstant +citation-format+
  '(:apa (list :article '((author-surname-initials author)
                          (parenthesis year ". ")
                          title
                          (emph journal ", ")
                          volume-issue)
               :web '((author-surname-initials author)
                      (parenthesis year ". ")
                      (emph title ". ")
                      (url web-link)))

    :other '(:article '(author)
             :web '(author)))
  "Order and style rules to generate citation formats.")

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

   (web-link :initform nil
             :accessor web-link
             :documentation "Resource's url if source is of web type."))

  (:documentation "Hold information about a single citation source."))
