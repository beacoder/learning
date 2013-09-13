;; -*- coding: utf-8 -*-

;; learning emacs configuration

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;;----------------------------------------------------------------------------
;; use alias to shorten commands
;;----------------------------------------------------------------------------

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; always use ibuffer
(defalias 'list-buffers 'ibuffer)

;; sh stands for shell
(defalias 'sh 'shell)

;; eb stands for eval-buffer
(defalias 'eb 'eval-buffer)

;; elm stands for emacs-lisp-mode
(defalias 'elm 'emacs-lisp-mode)

;; ~ stands for make-backup
(defalias '~ 'make-backup)

;;----------------------------------------------------------------------------
;; global key-bindings definition
;;----------------------------------------------------------------------------

;; bind the dired-jump to "C-x C-j"
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; bind company-complete command to the key [f12]
(global-set-key (kbd "<f12>") 'company-complete)

;;----------------------------------------------------------------------------
;; mode setting
;;----------------------------------------------------------------------------

;; enable icomplete-mode
(icomplete-mode t)

;; enable ido-mode
(ido-mode t)

;; enable electric-pair-mode
(electric-pair-mode t)

;; save desktop
(desktop-save-mode t)

;; highlight the active region
(transient-mark-mode t)

;; hide toolbar
(tool-bar-mode -1)

;; hide scroll bar
(scroll-bar-mode -1)

;; enable the delete-selection-mode
(delete-selection-mode t)

;; enable global high light
(global-font-lock-mode t)

;; set text-mode as default major mode
(setq default-major-mode 'text-mode)

;; enable emacs to open image file
(auto-image-file-mode)

;;----------------------------------------------------------------------------
;; some other settings
;;----------------------------------------------------------------------------

;; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; disable emacs system beep
(setq visible-bell t)

;; enlarge kill-ring-max value
(setq kill-ring-max 200)

;; show buffer-name in title
(setq frame-title-format "emacs@%b")

;;----------------------------------------------------------------------------
;; key-bindings for specific mode in emacs
;;----------------------------------------------------------------------------

;; the key definition only happen once        
(eval-after-load "coffee-mode"
    '(define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file)))
    
;; the key definition happens every time coffee-mode is enabled
(add-hook 'coffee-mode-hook
    (lambda ()
        (define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file))))
        

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-hippie-expand)
(require 'init-gnus)
(require 'init-cc-mode)
(require 'init-ctags)
(require 'init-alias)
