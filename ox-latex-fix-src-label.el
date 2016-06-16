;;; fix latex export: labels for source code blocks are not set correctly 

(defun org-latex-fix-src-code-label (oldlabelfct &rest args)
  "Fixing the incorrect generation of labels for source blocks. 
Oringal function taks datum and info as compulsory, 
and force and full as optional labels."
  (message "before fix-minted")
  (let* ((datum (nth 0 args))
	 (info (nth 1 args))
	 (type (org-element-type datum))
	 )
    (case type
      (src-block
       (let ( (labelname (org-element-property :name datum))
	     )
	 (format "\\label{%s}" labelname)
	   )
       )
      (t (apply oldlabelfct args))
      )
    )
  )

(advice-add 'org-latex--label :around #'org-latex-fix-src-code-label)


;;; fix export for dot environments

(defun org-latex-fix-src-block (oldsrcblockfct &rest args)
  "Latex export mode has issues dealing with org files. This is an attempt to fix this."
  (let* ((src-block (nth 0 args))
	 (contents (nth 1 args))
	 (info (nth 2 args))
	 (lang (org-element-property :language src-block))
	 )
    (message "org-latex-fix-src-block")
    (print lang)
    (case lang
      (dot
       (message "dot block detected"))
      (t
       (format "\\begin{listing}[H]\n%s\n\\end{listing}"
	       (apply oldsrcblockfct args)))
      ) 
    )
  )

(advice-add 'org-latex-src-block :around #'org-latex-fix-src-block)


(provide 'ox-latex-fix-src-label)

