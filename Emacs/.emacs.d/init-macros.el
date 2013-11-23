;; save kbd-macros definitions

;; restore last workspace
(fset 'restore-workspace
   [?\C-x ?2 ?\C-x ?3 ?\M-x ?s ?h return ?\C-x ?o ?\C-x ?d return ?\C-x ?o ?\C-x ?d return ?\C-x ?o ?\C-x ?r ?w ?w])

;; apply function to region lines inspired by "apply-macro-to-region-lines"
(defun apply-function-to-region-lines (fn)
  (interactive "aFunction to apply to lines in region: ")
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
          next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
          (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (funcall fn)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))   

(provide 'init-macros)
