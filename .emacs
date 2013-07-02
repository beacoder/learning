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

;; bind the dired-jump to "C-x C-j"
(global-set-key "\C-x\C-j" 'dired-jump)
