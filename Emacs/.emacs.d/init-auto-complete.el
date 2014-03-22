;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/popup-20140207.1702"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140314.802"))

;; start auto-complete with emacs 
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140314.802/dict")
;; do default config for auto-complete
(ac-config-default)

;; enable auto-complete in all buffers
(global-auto-complete-mode t)  

;; auto complete is CPU sensitive
(setq ac-auto-start nil)
(ac-set-trigger-key "<C-return>")

(provide 'init-auto-complete)
