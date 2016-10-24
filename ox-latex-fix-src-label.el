;;; fix latex export: labels for source code blocks are not set correctly 

(defun org-latex-fix-label (oldlabelfct &rest args)
  "Fixing the incorrect generation of labels for source blocks. 
Oringal function taks datum and info as compulsory, 
and force and full as optional labels."
  (let* ((datum (nth 0 args))
	 (info (nth 1 args))
	 (type (org-element-type datum))
	 )
    (print "fix label2")
    (print (format "type: %s type-of type: %s\n"
		   type
		   (type-of type)))
    (print (org-element-property :name datum))
    (case type
      ((headline)
       (print "headline!")
       (print (org-element-property :name datum))
       (format "\\label{%s}" "hallo")
       )
      ((src-block paragraph)
       (let ( (labelname (org-element-property :name datum))
	     )
	 (format "\\label{%s}" labelname)
	   )
       )
      (t (apply oldlabelfct args))
      )
    )
  )

(advice-add 'org-latex--label :around #'org-latex-fix-label)


;;; fix export for dot environments

;; (defun org-latex-fix-src-block (oldsrcblockfct &rest args)
;;   "Latex export mode has issues dealing with dot files. This is an attempt to fix this."
;;   (let* ((src-block (nth 0 args))
;; 	 (contents (nth 1 args))
;; 	 (info (nth 2 args))
;; 	 (lang (org-element-property :language src-block))
;; 	 )
;;     (message "new message org-latex-fix-src-block")
;;     (print lang)
;;     (print (type-of lang))
;;     (if (equal lang "python")
;; 	(format "\\begin{listing}[H]\n%s\n\\end{listing}"
;; 		 (apply oldsrcblockfct args))
;;       (apply oldsrcblockfct args))

;;     ;; (case lang
;;     ;;   (("\"latex\"")
;;     ;;    (print "latex detected")
;;     ;;    )
;;     ;;   (dot 
;;     ;;    (format "\\begin{figure}\n%s\n\\end{figure}"
;;     ;; 	       (apply oldsrcblockfct args))
;;     ;;    )
;;     ;;   (t
;;     ;;    (format "\\begin{listing}[H]\n%s\n\\end{listing}"
;;     ;; 	       (apply oldsrcblockfct args)))
;;     ;;   ) 
;;     )
;;   )

;; (advice-add 'org-latex-src-block :around #'org-latex-fix-src-block)


;;; fix the latex export problem for dot files 

;; (defun org-latex-dot-latex (link info)
;;   (print "latex-dot-latex")
;;   (print link)
;;   (print info)
;;   )

;; (advice-add 'org-latex--inline-image :before #'org-latex-dot-latex)


(provide 'ox-latex-fix-src-label)

