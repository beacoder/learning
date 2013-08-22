;; learning emacs configuration

;; enable icomplete-mode
(icomplete-mode t)

;; enable ido-mode
(ido-mode t)

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;; enable electric-pair-mode
(electric-pair-mode t)

;; save desktop
(desktop-save-mode t)

;; enable ibuffer
(defalias 'list-buffers 'ibuffer)

;; highlight the active region
(transient-mark-mode t)

;; hide toolbar
(tool-bar-mode -1)

;; hide scroll bar
(scroll-bar-mode -1)

;; enable the delete-selection-mode
(delete-selection-mode t)

;; bind the dired-jump to "C-x C-j"
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; bind company-complete command to the key [f12]
(global-set-key (kbd "<f12>") 'company-complete)

;; disable emacs system beep
(setq visible-bell t)
