;;----------------------------------------------------------------------------
;; python programming mode setting
;;----------------------------------------------------------------------------

;; "C-c C-p" =>  run-python
;; "C-c C-s" =>  python-shell-send-string
;; "C-c C-r" =>  python-shell-send-region
;; "C-c C-z" =>  python-shell-switch-to-shell
;; "C-c C-j" =>  imenu

;; "C-c <"   =>  python-indent-shift-left
;; "C-c >"   =>  python-indent-shift-right

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

    (define-key python-mode-map (kbd "C-c c") 'python-skeleton-class)
    (define-key python-mode-map (kbd "C-c d") 'python-skeleton-def)
    (define-key python-mode-map (kbd "C-c f") 'python-skeleton-for)
    (define-key python-mode-map (kbd "C-c i") 'python-skeleton-if)
    (define-key python-mode-map (kbd "C-c m") 'python-skeleton-import)
    (define-key python-mode-map (kbd "C-c t") 'python-skeleton-try)
    (define-key python-mode-map (kbd "C-c w") 'python-skeleton-while))

  (after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") 'anaconda-mode-go-back)
    (define-key anaconda-mode-map (kbd "M-]") 'anaconda-mode-find-references)
    (define-key anaconda-mode-map (kbd "M-=") 'anaconda-mode-find-assignments)
    (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-show-doc))

  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))


(provide 'init-python)
