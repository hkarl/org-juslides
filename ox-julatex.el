;;; ox-julatex.el

;; A derived backend to export to latex,
;; with minimal incoroporation of beamer
;; markup

(require 'ox)
; (require 's)

;; the next two secxp are from:
;; http://emacs.stackexchange.com/questions/2952/display-errors-and-warnings-in-an-org-mode-code-block
;; still needs to be tied into the processing loop? 
(defvar org-babel-eval-verbose t
  "A non-nil value makes `org-babel-eval' display")

(defun org-babel-eval (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
            (org-babel--shell-command-on-region
             (point-min) (point-max) cmd err-buff))
      (if (or (not (numberp exit-code)) (> exit-code 0)
              (and org-babel-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
          (progn
            (with-current-buffer err-buff
              (org-babel-eval-error-notify exit-code (buffer-string)))
            nil)
        (buffer-string)))))
;;------------


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


;; a filter to get rid of %%tutor magic markup

(defun julatex-filter-notutor (backend)
  "Ensure that tutor magic markup is removed for Latex export."
  (when (org-export-derived-backend-p backend 'latex)
    ; (replace-regexp "%%tutor.*\n *" "")
    (while (re-search-forward "%%tutor\\(.*\\)\n *\\([\0-\377[:nonascii:]]*?\\)#\\+END_SRC" nil t)
      (replace-match (concat ; (match-string 1)
			     (match-string 2)
			     "\n#+END_SRC\n\n"
			     "[[http://www.pythontutor.com/visualize.html#code="
			     (url-hexify-string (match-string 2))
			     (format "&py=%s][(PT link)]]\n\n"
				     (if (cl-search "java" (match-string 1))
					 "java"
				       "3"
					 )
				     )
			     )
		     t t 
		     )
      )
    )
  )

(add-to-list 'org-export-before-processing-hook
	     'julatex-filter-notutor)


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
