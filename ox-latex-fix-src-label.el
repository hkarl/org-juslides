;;; fix minted output
;; (defun org-juslides-before-minted-label (datum info &optional force full)
;;   (print "before minted")
;;   (print datum)
;;   )

(defun org-latex-fix-src-code-label (oldlabelfct &rest args)
  "Fixing the incorrect generation of labels for source blocks. 
Oringal function taks datum and info as compulsory, 
and force and full as optional labels."
  (message "before fix-minted")
  (let* ((datum (nth 0 args))
	 (info (nth 1 args))
	 (type (org-element-type datum))
	 )
    (print type)
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

; (advice-add 'org-latex--label :before #'org-juslides-before-minted-label)
(advice-add 'org-latex--label :around #'org-latex-fix-src-code-label)

(provide 'ox-latex-fix-src-label)

