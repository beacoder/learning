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

(provide 'init-alias)
