;;----------------------------------------------------------------------------
;;  Helm setting
;;----------------------------------------------------------------------------

;; enable helm mode
(helm-mode 1)

;; replace original switch buffer
(global-set-key (kbd "C-x b") 'helm-mini)

(provide 'init-helm)
