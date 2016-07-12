;;; ox-julatex.el

;; A derived backend to export to latex,
;; with minimal incoroporation of beamer
;; markup

(require 'ox)


(org-export-define-derived-backend 'julatex 'latex
 :menu-entry
 '(?s 99
      ((?L "As temporary latex buffer"
	   (lambda (a s v b) (org-julatex-export-to-buffer a s v)))
       (?l "As LaTeX file with beamer support"
	   (lambda (a s v b) (org-julatex-export-to-file a s v)))
       ))
 :translate-alist '((headline . org-julatex-headline))
 )


;; call the output functions with the correct backend: 
(defun org-julatex-export-to-buffer (&optional async subtreep visible-only)
  (interactive)

  (org-export-to-buffer 'julatex "*Org JULATEX Export*"
    async subtreep visible-only nil nil (lambda () (tex-mode)))  
  )

(defun org-julatex-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ( (file (org-export-output-file-name ".tex" subtreep))
	  )
    (org-export-to-file 'julatex file
      async subtreep visible-only body-only ext-plist)
    )
  )


;;; Headline
;;
;; The only thing that needs changing, compared to ordinary LaTeX export,
;; is how to handle headlines when there is a Beamer tag present.
;; Simple approach here: we test for these tags, and then
;; just wrap an environment around them

(defun org-julatex-headline (headline contents info)
  (let* ((tags-list (org-export-get-tags headline info))
					; get only those tags that start with B_ for Beamer tag
	 (no-latex (-filter (lambda (x) (string-prefix-p "nolatex" x))
			       tags-list))
	 (beamer-tags (-filter (lambda (x) (string-prefix-p "B_" x))
			       tags-list))
					; let's for now assume there is just a single beamer tag per block heading
	 (tmp-beamer-tag (car beamer-tags))
	 (beamer-tag (if tmp-beamer-tag
			 (substring tmp-beamer-tag 2)
		       nil))
	 )

    (cond
     (no-latex "")
     (beamer-tag
	(let* ((plainlabel (org-latex--label headline info))
	       (labelstr (if plainlabel
			     (format "\\label{%s}\n" plainlabel)
			   ""
			   ))
	       )
	  (format "\\begin{%s}[%s]\n%s\n%s\n\\end{%s}"
		  beamer-tag
		  (org-export-data (org-element-property :title headline) info)
		  labelstr
		  contents
		  beamer-tag
		  )
	  ))
     (t (org-latex-headline headline contents info))
    )
  ))





;; some simple test code: 				  

;; (with-temp-buffer
;;   (require 'ox-org)
;;   (insert "
;; * Beamer markup tests 

;; ** A slide with a beamer block 
;;    :PROPERTIES:
;;    :CUSTOM_ID: sec:sec
;;    :END:

;; This is text before the block. 


;; *** A theorem title 						  :B_theorem:
;;     :PROPERTIES:
;;     :BEAMER_env: theorem
;;     :CUSTOM_ID: the:test
;;     :END:

;; and this is the famous theorem  described in [[#the:test]] in Section
;; [[#sec:sec]]. 


;; *** A proof title 						  :B_proof:
;;     :PROPERTIES:
;;     :BEAMER_env: proof
;;     :END:
;; and here we proof this. 
;; ")

;;   (org-export-to-buffer 'julatex "*julatex backend export*" nil nil nil t))

(provide 'ox-julatex)
