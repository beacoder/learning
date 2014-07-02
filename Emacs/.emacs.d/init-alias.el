;;----------------------------------------------------------------------------
;; use alias to shorten commands
;;----------------------------------------------------------------------------

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; always use ibuffer
(when (> emacs-major-version 21)
  (defalias 'list-buffers 'ibuffer))

;; sh stands for shell
(defalias 'sh 'shell)

;; esh stands for eshell
(defalias 'esh 'eshell)

;; eb stands for eval-buffer
(defalias 'eb 'eval-buffer)

;; ed stands for ediff
(defalias 'ed 'ediff)
(defalias 'edb 'ediff-buffers)

;; original multiple cursor key-bindings doesn't work in cc-mode
(defalias 'el 'mc/edit-lines)
(defalias 'ep 'mc/mark-all-like-this)

;; shortcut for kill-some-buffers
(defalias 'ka 'kill-some-buffers)

;; goto-line
(defalias 'gl 'goto-line)

(defalias 'ca 'c-beginning-of-defun)
(defalias 'ce 'c-end-of-defun)
(defalias 'rw 'restore-workspace)
(defalias 'll 'list-matching-lines)
(defalias 'ts 'tags-search)
(defalias 'tq 'tags-query-replace)

;; magit-mode
(defalias 'ms 'magit-status)

(defalias 'afa 'apply-function-to-region-lines-with-args)
(defalias 'af 'apply-function-to-region-lines-without-args)

(provide 'init-alias)
