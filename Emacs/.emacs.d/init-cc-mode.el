;; c/c++ programming mode setting

;; show function name in mode-line
(add-hook 'c-mode-common-hook
  (lambda()
    (which-function-mode t)))

;; navigation between header and cpp/cc files
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c o") 'ff-find-other-file)))
    
;; jump to the start of the function    
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)))

;; jump to the end of the function    
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-M-e") 'c-end-of-defun)))
    
;; use <f5> to compile
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "<f5>") 'compile)))

(provide 'init-cc-mode)
