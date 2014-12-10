;;----------------------------------------------------------------------------
;; nxml mode configuration
;;----------------------------------------------------------------------------

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook (lambda ()
                            (set (make-local-variable 'ido-use-filename-at-point) nil)))
(setq nxml-slash-auto-complete-flag t)

;; @see: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (begin end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))
      
 ;; prettify the xml in the active region
 (defalias 'pt 'sanityinc/pp-xml-region)

(provide 'init-nxml)
