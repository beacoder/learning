;;----------------------------------------------------------------------------
;; multiple-cursors setting
;;----------------------------------------------------------------------------

(require 'multiple-cursors)

;; apply mulitiple cursor to region lines
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; apply mulitiple cursor to lines like this region
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)

;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; auto complete is CPU sensitive
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
(setq ac-auto-start nil
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-quick-help-height 30)

;;----------------------------------------------------------------------------
;; third-party setting
;;----------------------------------------------------------------------------

(require 'magit)
(require 'init-helm)
(require 'init-dictionary)
(require 'init-dired)
(require 'buffer-move)
(require 'flymake-cursor)
(require 'init-highline)
(require 'init-async)
(require 'init-productivity)

(provide 'init-3rd-party)
