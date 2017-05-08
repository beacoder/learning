;;----------------------------------------------------------------------------
;; python programming mode setting
;;----------------------------------------------------------------------------

;; "C-c C-p"    =>  run-python
;; "C-c C-s"    =>  python-shell-send-string
;; "C-c C-r"    =>  python-shell-send-region
;; "C-c C-z"    =>  python-shell-switch-to-shell
;; "C-c C-j"    =>  imenu

;; "C-c <"      =>  python-indent-shift-left
;; "C-c >"      =>  python-indent-shift-right

;; "C-c C-t c"  =>  python-skeleton-class
;; "C-c C-t d"  =>  python-skeleton-def
;; "C-c C-t f"  =>  python-skeleton-for
;; "C-c C-t i"  =>  python-skeleton-if
;; "C-c C-t m"  =>  python-skeleton-import
;; "C-c C-t t"  =>  python-skeleton-try
;; "C-c C-t w"  =>  python-skeleton-while

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
