;; c/c++ programming mode setting

;; show function name in mode-line
(add-hook 'c-mode-common-hook
  (lambda ()
    (which-function-mode t)))

;; navigation between header and cpp/cc files
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(provide 'init-cc-mode)
