;;----------------------------------------------------------------------------
;; multiple-cursors setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/multiple-cursors-20140418.815"))

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/popup-20140207.1702"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140512.43"))

;; do default config for auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140512.43/dict")
(ac-config-default)

;; auto complete is CPU sensitive
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
(setq ac-auto-start nil
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-quick-help-height 30)

(provide 'init-3rd-party)
