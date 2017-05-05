;;----------------------------------------------------------------------------
;; python programming mode setting
;;----------------------------------------------------------------------------

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
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
