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
;; use default configuration
(ac-config-default)

;; auto complete is CPU sensitive
(setq ac-auto-start nil)
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
;; use "C-?" to show tip in another buffer
(setq ac-use-quick-help t
      ac-quick-help-delay 0.5)

(provide 'init-auto-complete)
