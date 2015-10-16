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
;; undo-tree setting
;;----------------------------------------------------------------------------

;; "C-x u" => open the undo-tree-visualizer
(require 'undo-tree)
(global-undo-tree-mode)

;; undo-buffer limit -> 100 MB                                                       |
(setq undo-outer-limit (* 100 (expt 1024 2)))

;;----------------------------------------------------------------------------
;; enabling ace-jump-mode
;;----------------------------------------------------------------------------
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; If you use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;; If you use evil
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

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
