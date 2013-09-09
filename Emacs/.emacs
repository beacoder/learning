;; learning emacs configuration

;; add "~/" to load-path
(add-to-list 'load-path (expand-file-name "~/"))

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

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; always use ibuffer
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

;; enlarge kill-ring-max value
(setq kill-ring-max 200)

;; enable global high light
(global-font-lock-mode t)

;; set text-mode as default major mode
(setq default-major-mode 'text-mode)

;; show buffer-name in title
(setq frame-title-format "emacs@%b")

;; enable emacs to open image file
(auto-image-file-mode)
