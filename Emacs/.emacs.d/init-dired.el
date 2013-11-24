;;----------------------------------------------------------------------------
;; dired setting
;;----------------------------------------------------------------------------

;; In a file, how to go to its directory and place cursor on the file name
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; allow dired to be able to delete or copy a whole dir.
;; "always" means no asking. "top" means ask once. 
;; Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; copy from one dired dir to the next dired dir shown in a split window
(setq dired-dwim-target t)

;; make dired use the same buffer for viewing directory, instead of spawning many
;; (add-hook 'dired-mode-hook
;;  (lambda ()
;;   (define-key dired-mode-map (kbd "<return>")
;;     'dired-find-alternate-file) ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda () (interactive) (find-alternate-file "..")))
;;   ; was dired-up-directory
;;  ))
 
(provide 'init-dired)
