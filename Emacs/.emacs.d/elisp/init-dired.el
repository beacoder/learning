;;----------------------------------------------------------------------------
;; dired setting
;;----------------------------------------------------------------------------

;; In a file, how to go to its directory and place cursor on the file name
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; allow dired to be able to delete or copy a whole dir.
;; "always" means no asking. "top" means ask once.
;; Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies (quote always)
      dired-recursive-deletes (quote top))

;; copy from one dired dir to the next dired dir shown in a split window
(setq dired-dwim-target t)

;; @see http://ergoemacs.org/emacs/dired_sort.html
(setq dired-listing-switches "-alc --si --time-style long-iso")

;; restore positions and markers after dired-view-file exits
(defadvice dired-view-file (around advice-dired-view-file activate)
  (interactive)
  (save-excursion ad-do-it))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; make dired use the same buffer for viewing directory, instead of spawning many
;; (add-hook 'dired-mode-hook
;;  (lambda ()
;;   (define-key dired-mode-map (kbd "<return>")
;;     'dired-find-alternate-file) ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda () (interactive) (find-alternate-file "..")))
;;   ; was dired-up-directory
;;  ))

(after-load 'dired (define-key dired-mode-map (kbd "M-b") nil))

(provide 'init-dired)
