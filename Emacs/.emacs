;; -*- coding: utf-8 -*-

;; learning emacs configuration

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;----------------------------------------------------------------------------
;; key-bindings for specific mode in emacs
;;----------------------------------------------------------------------------

;; the key definition only happen once        
;;(eval-after-load "coffee-mode"
;;    '(define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file)))
    
;; the key definition happens every time coffee-mode is enabled
;;(add-hook 'coffee-mode-hook
;;    (lambda ()
;;        (define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file))))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-basics)
(require 'init-hippie-expand)
(require 'init-gnus)
(require 'init-cc-mode)
(require 'init-ctags)
(require 'init-alias)
