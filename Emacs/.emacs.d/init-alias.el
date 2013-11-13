;;----------------------------------------------------------------------------
;; use alias to shorten commands
;;----------------------------------------------------------------------------

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; always use ibuffer
(defalias 'list-buffers 'ibuffer)

;; sh stands for shell
(defalias 'sh 'shell)

;; esh stands for eshell
(defalias 'esh 'eshell)

;; eb stands for eval-buffer
(defalias 'eb 'eval-buffer)

;; ed stands for ediff
(defalias 'ed 'ediff)

;; elm stands for emacs-lisp-mode
(defalias 'elm 'emacs-lisp-mode)

;; ~ stands for make-backup
;; (defalias '~ 'make-backup)

(defalias 'ka 'kill-some-buffers)

(defalias 'rw 'restore-workspace)
(defalias 'cpl 'compile)
(defalias 'll 'list-matching-lines)
(defalias 'ts 'tags-search)

(provide 'init-alias)
