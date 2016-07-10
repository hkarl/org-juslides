;; Implement a Jupyter Slides backend for Org mode
;; Specically geared towards the React.JS based
;; presentation mode of Jupyter Notebooks.
;; It is a derived backend from Markdown export 


(require 'ox-md)
(require 'dash)

(defgroup org-export-juslides nil
  "Options for Jupyer Slides export backend."
  :tag "Org Jupyter Slides Notebook"
  :group 'org-export
  :version "0.1"
  :package-version '(Org . "8.0"))


;; TODO: figure out how to put a map
;; of (string, string, string) -> string
;; into a customization variable 
;; (defcustom org-export-juslides-beamer-divs
;;   '(("theorem" "heading" "pre" "AAA")
;;     ("theorem" "heading" "post" "BBB ")
;;     ("theorem" "content" "pre" "CCC")
;;     ("theorem" "content" "post" "DDD")
;;     )
;;   "For beamer blocks, which divs should be generated around the header and the content of the corresponding block?"
;;   :version "0.1"
;;   :group 'org-export-juslides
;;   :type '(alist :key-type (string string string)
;; 		:value-type string)
;;   )

(defcustom org-export-juslides-divs-heading-pre
  '(("theorem" "<center> <div class=\"theorem-head\" style=\"border:2px; border-style:solid; border-color:#00FF00; padding: 1em; background-color:lightgreen; text-align:center; border-radius: 15px 15px 0px 0px; margin-bottom: 0; width: 80%;\">")
    ("proof" "<div class=\"proof-head\">")
    ("example" "<div class=\"example-head\">")
    ("note" "<div class=\"note-head\">")
    ("definition" "<div class=\"definition-head\">")
    ("quote" "<div class=\"quote-head\">")
    ("alertblock" "<div class=\"alertblock-head\">")
    )
  "Divs to go before the heading"
  :type '(alist :key-type string
		:value-type (group string))
  )

(defcustom org-export-juslides-divs-heading-post
  '(("theorem" "</div></center>")
    ("proof" "</div>")
    ("example" "</div>")
    ("note" "</div>")
    ("definition" "</div>")
    ("quote" "</div>")
    ("alertblock" "</div>")
    )
  "Divs to go after the heading"
  :type '(alist :key-type string
		:value-type (group string))
  )

(defcustom org-export-juslides-divs-content-pre
  '(("theorem" "<center><div class=\"theorem-content\" style=\"border:2px; border-style:solid; border-color:#00FF00; padding: 1em; margin-top: 0; border-radius: 0px 0px 15px 15px; width: 80%;\">")
    ("proof" "<div class=\"proof-content\">")
    ("example" "<div class=\"example-content\">")
    ("note" "<div class=\"note-content\">")
    ("definition" "<div class=\"definition-content\">")
    ("quote" "<div class=\"quote-content\">")
    ("alertblock" "<div class=\"-content\">")
    )
  "Divs to go before the content"
  :type '(alist :key-type string
		:value-type (group string))
  )


(defcustom org-export-juslides-divs-content-post
  '(("theorem" "</div></center>")
    ("proof" "</div>")
    ("example" "</div>")
    ("note" "</div>")
    ("definition" "</div>")
    ("quote" "</div>")
    ("alertblock" "</div>")
    )
  "Divs to go after the content"
  :type '(alist :key-type string
		:value-type (group string))
  )


;; so far, nothing to customize yet :-/ 
;; TODO: add proper customization variables 

;;; Suppress LaTeX output only

(defun org-juslides-export-change-options (plist backend)
  (let ( (oldlist (plist-get plist :exclude-tags))
	 )
    (cond
     ((equal backend 'latex)
      (plist-put plist :exclude-tags
		 (add-to-list 'oldlist '"nolatex")))
     ((equal backend 'juslides)
      (plist-put plist :exclude-tags
		 (add-to-list 'oldlist '"dropslide" )))
     )
    )
  (print (plist-get plist :exclude-tags))
  plist 
  )

(add-to-list 'org-export-filter-options-functions 'org-juslides-export-change-options)


;;; Define Back-End

(org-export-define-derived-backend 'juslides 'md
  ; :export-block '("JUSLIDES" "JUSLIDES NOTEBOOK")
  :filters-alist '((:filter-final-output . org-juslides-final-function)
		   ; (:filter-link . org-juslides-filter-link)
		   )
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


;;; Filters

;; none needed so far


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
  (let* ( (code (org-export-format-code-default src-block info))
	 (animate (org-export-read-attribute :attr_juslides src-block :animate))
	 (tags (org-export-get-tags (org-export-get-parent-headline src-block) info '("invalid" "tags") t))
	 (skipslide (or (member "skipslide" tags)
			(org-export-read-attribute :attr_juslides src-block :skip)
			))
	 )
    (message "juslides-src")
    (print code)
    (org-juslides-cell "code"
		       (cond
			(skipslide "skip")
			(animate "fragment")
			(t "-"))
		       code)
    )
  )


(defun org-juslides-get-div (beamertag p1 p2)
  "if beamtag is non-nil, grab the divs for the various places. 
p1 should be header or content, p2 should be pre or post"
  (if beamertag
      (let* ((customized-divs (cond
			       ((string= p1 "heading")
				(cond
				 ((string= p2 "pre") org-export-juslides-divs-heading-pre)
				 ((string= p2 "post") org-export-juslides-divs-heading-post)
				 (t nil))
				)
			       ((string= p1 "content")
				(cond
				 ((string= p2 "pre") org-export-juslides-divs-content-pre)
				 ((string= p2 "post") org-export-juslides-divs-content-post)
				 (t nil))
				)
			       (t nil))
			   )
	     (divstring-tmp (-filter
			     (lambda (x) (string= beamertag (car x)))
			     customized-divs))
	     (divstring-div (car ( cdr (car divstring-tmp))))
	     (divstring (if divstring-div
			    divstring-div
			  ""))
	     )
	;; (print "divstring")
	;; (print beamertag)
	;; (print customized-divs)
	;; (print divstring-div)
	;; (print divstring)
	divstring)
    "")
  )


(defun org-juslides-format-content (headline contents anchor level info)
  "We have a block with a headline. Produce the actual source for this part. 
Check for possible BEAMER tags!"
  (let* ((tags-list (org-export-get-tags headline info))
					; get only those tags that start with B_ for Beamer tag
	 (beamer-tags (-filter (lambda (x) (string-prefix-p "B_" x))
			       tags-list))
					; let's for now assume there is just a single beamer tag per block heading
	 (tmp-beamer-tag (car beamer-tags))
	 (beamer-tag (if tmp-beamer-tag
			 (substring tmp-beamer-tag 2)
		       nil))
	 (heading_pre_div (org-juslides-get-div beamer-tag "heading" "pre"))
	 (heading_post_div (org-juslides-get-div beamer-tag "heading" "post"))
	 (content_pre_div (org-juslides-get-div beamer-tag "content" "pre"))
	 (content_post_div (org-juslides-get-div beamer-tag "content" "post"))
	 )
    (message "juslides-format-content")
    (print beamer-tag)
    (concat
     (if heading_pre_div
	 (concat 
	  heading_pre_div
	  heading
	  anchor
	  heading_post_div
	  )
       (concat 
	(make-string (- level 1) ?#)
	" "
	heading
	anchor
	))
     "\n\n"
     content_pre_div
     contents
     content_post_div
     )
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
	   ;; (skipslide (and tags
	   ;; 		   (or 
	   ;; 		    (string-match "skipslide" tags)
	   ;; 		    ; add alternative keywords to skip slides? 
	   ;; 		    )
	   ;; 		   ))
	   (skipslide (member "skipslide" (org-export-get-tags headline info) ))
	   (animateslide (member "animate" (org-export-get-tags headline info) ))
	   (notesslide (member "notes" (org-export-get-tags headline info) ))
	   (subslide (member "subslide" (org-export-get-tags headline info) ))
	   ;; (subslide (and tags
	   ;; 		  (string-match "subslide" tags)))
	   ;; (notesslide (and tags
	   ;; 		  (string-match "notes" tags)))
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
				   (cond
				    (notesslide "notes")
				    (skipslide "skip")
				    (subslide "subslide")
				    (t "slide"))
				   (concat "# " heading  "\n\n" )
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
       (t (let ((source (org-juslides-format-content headline contents anchor level info))
		)
	    ;; (if animateslide
	    ;; 	(org-juslides-cell "markdown" "fragment" source)
	    ;;   (org-juslides-cell "markdown" "-" source)
	    ;;   )
	    (org-juslides-cell "markdown"
			       (cond
				(animateslide "fragment")
				(notesslide "notes")
				(t "-")
				)
			       source
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
	 (subtitle 
	  (let ((tmp (plist-get info :subtitle))
		)
	    (if tmp
	 	(org-export-data tmp info)
	      "")
	  ))
	)
    (concat "<h1>" titlestr "</h1>\n<p>" subtitle "<p><h2>" author "</h2>" )
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
			     (format "1. **<font color=\"red\">%s</font>**"  title)
			   (format "1. %s"  title)
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
  \"livereveal\": {
        \"theme\": \"simple\",
        \"transition\": \"none\"
    },
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
    ;; replace the links to figures TODO: build the pngs out of it
    (while (re-search-forward "\\(<figures/\\(.*?\\)\\.pdf>\\)" nil t)
      (let* ( (filebase (match-string 2))
	      (pdffile (format "figures/%s.pdf" filebase))
	      (pngfile (format "figures/%s.png" filebase))
	      )
    	(replace-match (format "<center>![imgimg](figures/%s.png)</center>"
    			       (match-string 2)
    			       t t))
	(message pdffile)
	(message pngfile)
    	(when (file-newer-than-file-p  pdffile pngfile)
	  (message "run convert on pdf")
	  (let ( (convertcommand (format "convert -density 300 %s %s"
					 pdffile
					 pngfile)))
	    (message "command is ")
	    (message convertcommand)
	    (shell-command convertcommand)
	    )
    	  )
    	)
      )
    (goto-char (point-min))
    ;; center figures
    (while (re-search-forward "\\!\\[img\\]\\(\\(.*\\)\\)" nil t)
      (replace-match (concat "<center>![img]"
			     (match-string 1)
			     "</center>"))
      )
    
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




;;;----------------
(provide 'ox-juslides)
  
