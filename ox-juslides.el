;; Implement a Jupyter Slides backend for Org mode
;; Specically geared towards the React.JS based
;; presentation mode of Jupyter Notebooks.
;; It is a derived backend from Markdown export 


(require 'ox-md)

(defgroup org-export-juslides nil
  "Options for Jupyer Slides export backend."
  :tag "Org Jupyter Slides Notebook"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;; so far, nothing to customize yet :-/ 



;;; Define Back-End

(org-export-define-derived-backend 'juslides 'md
  ; :export-block '("JUSLIDES" "JUSLIDES NOTEBOOK")
  :filters-alist '((:filter-final-output . org-juslides-final-function))
  :menu-entry
  '(?s "Export to Jupyter Notebook as Slides"
       ((?b "As temporary MD buffer" 
            (lambda (a s v b) (org-juslides-export-to-buffer a s v)))
        (?f "To file" (lambda (a s v b) (org-juslides-export-to-file a s v)))
       ))
  :translate-alist '((headline . org-juslides-headline)
		     (bold . org-juslides-bold)
		     (italic . org-juslides-italic)
		     ; (code . org-juslides-code)
		     (src-block . org-juslides-src-block)
		     (inner-template . org-juslides-inner-template)
		     (latex-fragment . org-juslides-latex-fragment)
		     (latex-environment . org-juslides-latex-environment)
		     ; TODO: Make sure to set org-html-with-latex to verbatim for this to work! 
		     )
  )


;;; Translators

(defun org-juslides-protect-backslash (text)
  "Replace any single backslash with two backslahes."
  ;; Note: the following looks odd, but it really does double a single backslad.
  ;; Watch out for the second optioal argument, LITERAL = true, to see why. 
  (replace-regexp-in-string "\\\\" "\\\\" text t t)
  )

(defun org-juslides-latex-fragment (latex-fragment _contents info)
  (let ( (tmp (org-html-latex-fragment latex-fragment _contents info) )
	 )
    (org-juslides-protect-backslash tmp) 
    )
  )

(defun org-juslides-latex-environment (latex-fragment _contents info)
  (let ( (tmp (org-html-latex-environment latex-fragment _contents info)))
    (org-juslides-protect-backslash tmp) 
    )
  )

(defun org-juslides-src-block (src-block contents info)
  (let ( (code (org-export-format-code-default src-block info))
	 (animate (org-export-read-attribute :attr_juslides src-block :animate))
	     )
    (org-juslides-cell "code"
		       (if animate
			   "fragment"
			 "-")
		       code)
    )
  )

(defun org-juslides-headline (headline contents info)
  "Don't show headlines level 1; turn them into a separation slide. 
This is based on markdown exporter's headline handling"

  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (animate (org-export-data (org-element-property :animate headline) info))
	   (tag-list (and (plist-get info :with-tags)
			  (org-export-get-tags headline info))
		     )
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   (anchor
	    (and (plist-get info :with-toc)
		 (format "<a id=\"%s\"></a>"
			 (or (org-element-property :CUSTOM_ID headline)
			     (org-export-get-reference headline info)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" "    " contents)))))
       ;; Headline level 1?
       ((eq level 1)
	(concat (org-juslides-cell "markdown"
			   "slide"
			   ;; (concat "Some fancy markup for a section start slide \n\n"
			   ;; 	   heading tags anchor "\n\n")
			   (org-juslides-tocslide info heading)
			   )
		contents
		)
	)
       ;; HEadline level 2, i.e., a normal slide? 
       ((eq level 2)
	(concat (org-juslides-cell "markdown"
				   "slide"
				   (concat "# " heading tags anchor "\n\n" )
				   )
		contents
		)
	)
       ;; Ordinary processing
       ;; Use "Setext" style.
       ((eq style 'setext)
	(concat heading tags anchor "\n"
		(make-string (length heading) (if (= level 1) ?= ?-))
		"\n\n"
		contents))
       ;; Use "atx" style.
       (t (let ((source (concat (make-string (- level 1) ?#) " " heading tags anchor "\n\n" contents))
		)
	    (if (member "animate" tag-list)
		(org-juslides-cell "markdown" "fragment" source)
	      (org-juslides-cell "markdown" "-" source)
	      )
	    ))
       ;; No further alternatives 
       ))
    )
  
  )  


(defun org-juslides-bold (_bold contents _info)
  "Transcode BOLD object into Markdown format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))

(defun org-juslides-italic (_italics contents _info)
  "Transcode italics/emphasized object into Markdown format."
  (format "*%s*" contents))

; not clear, need to look up concrete data structure of "code" objects
;; (defun org-juslides-code (_object contents _info)
;;   "Transcode code/fixed-font object into Markdown format."
;;   (message (gethash "value" (cdr _object)))
;;   (format "--%s--" contents))



;;; Output functions to produce JSON

(defun org-juslides-cell (celltype slidetype source &optional x)
  "Produce a JSON-formated cell of celltype, with slidetype 
set accordingly. Source is the content of the cell. 
The optinal parameter x tells us whether to prepend this block with the closing 
brackets for the previous block; x is nil for ordinary blocks and only set to true 
for the very first block we crete (i.e., true suppresses prepending of closing brackets."
  (format
   "%s{
     \"cell_type\": \"%s\",
     \"metadata\": {
        \"slideshow\": {
           \"slide_type\": \"%s\"
         } 
     }, %s
     \"source\": [[[ 
%s"
   (if (not x)
       "\n     ]]]
    },"
     "")
   celltype
   slidetype
   (if (equal celltype "code")
       "\n\"outputs\": [],
\"execution_count\": null,"
     "")
   source
   )
  )

;;; Structure functions: Title slide, Overview slides

(defun org-juslides-titleslide (info)
  (let ( (titlestr   (org-export-data (plist-get info :title) info))
	 (author   (org-export-data (plist-get info :author) info))
	)
    (concat "<h1>" titlestr "</h1>\n<p><p><h2>" author "</h2>")
    )
  )

(defun org-juslides-tocslide (info &optional current-title scope)

  (let* ((toc
	  (mapconcat (lambda (entry)
		       (let* ((number (mapconcat (lambda (x) (format "%d" x))
						(org-export-get-headline-number
						 entry
						 info)
						"."
						))
			      (tmptitle (org-export-get-alt-title entry info))
			      (strtitle (format "%s" tmptitle))
			      (title (substring strtitle 1 -1))
			      (ref (org-export-get-reference entry info))
			      )
			 (if (equal title current-title)
			     (format "- **%s**"  title)
			   (format "- %s"  title)
			   )
			 ; TODO: Get HREFs working, but that seems rather nontrivial: 
			 ; (format "- <a href=\"%s\">%s</a>"  ref  title)
			 ))
		     (org-export-collect-headlines info 1 scope)
		     "\n"))
	)
    (format 
     "<h1>Overview</h1> 

%s"
     toc)
    )
  )

;;; High-level funtions

(defun org-juslides-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((titleslide (org-juslides-titleslide info))
	(overviewslide (org-juslides-tocslide info))
	)
      (format "{
\"cells\": [
 %s
 %s 
 %s
  ]]]
 }
],
 \"metadata\": {
  \"celltoolbar\": \"Slideshow\",
  \"kernelspec\": {
   \"display_name\": \"Python 3\",
   \"language\": \"python\",
   \"name\": \"python3\"
  },
  \"language_info\": {
   \"codemirror_mode\": {
    \"name\": \"ipython\",
    \"version\": 3
   },
   \"file_extension\": \".py\",
   \"mimetype\": \"text/x-python\",
   \"name\": \"python\",
   \"nbconvert_exporter\": \"python\",
   \"pygments_lexer\": \"ipython3\",
   \"version\": \"3.5.1\"
  }
 },
 \"nbformat\": 4,
 \"nbformat_minor\": 0
}"
	      (org-juslides-cell "markdown" "slide" titleslide t)
	      (org-juslides-cell "markdown" "slide" overviewslide nil)
	      contents)))


;;; final filter function
(defun org-juslides-final-function (contents _backend info)
  ;; work on the buffer to replace the sources strings

  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    ;; process the source instructions: 
    (while (re-search-forward "\"source\": \\[\\[\\[\\(\\(.\\|\n\\)*?\\)\\]\\]\\]" nil t)
    					; (replace-match decorated)
      (let* (
    	     (source (replace-regexp-in-string
    	     	      "\""
    	     	      "\\\\\""
    	     	      (match-string 1)))
    	     (protectedSource (replace-regexp-in-string
    	     		       "\n"
    	     		       "\\\\n\",\n\""
    	     		       source
			       ; (match-string 1)
    	     		       ))
    	     (completeSource (concat "\"" protectedSource "\"\n"))
    	     (sourcedSource (concat "\"source\": [" completeSource "]"))
    	    )
					; (replace-match sourcedSource)
    	(replace-match sourcedSource t t) 
    	)
      )

    ;; nice indentation:
    (json-mode)
    (mark-whole-buffer)
    (indent-for-tab-command)

    
    ;; return buffer content as resulting string:
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )


;;; Interactive functions
;;;###autoload

(defun org-juslides-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ( (file (org-export-output-file-name ".ipynb" subtreep))
	  )
    (org-export-to-file 'juslides file
      async subtreep visible-only body-only ext-plist)
    )
  )

(defun org-juslides-export-to-buffer (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org JUSLIDES Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)

  (org-export-to-buffer 'juslides "*Org JUSLIDES Export*"
    async subtreep visible-only nil nil (lambda () (json-mode)))  
  )

(provide 'ox-juslides)
  
