;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/popup-20140207.1702"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140314.802"))

;; use bash command => echo "" | g++ -v -x c++ -E -
;; to find out include file search path of your g++,
;; tell clang what the path is.
(defconst user-include-dirs 
  (list "/usr/include/c++/4.6"
	"/usr/include/c++/4.6/i686-linux-gnu/."
	"/usr/include/c++/4.6/backward"
	"/usr/lib/gcc/i686-linux-gnu/4.6/include"
	"/usr/local/include"
	"/usr/lib/gcc/i686-linux-gnu/4.6/include-fixed"
	"/usr/include/i386-linux-gnu"
	"/usr/include")
  "g++'s include file search path.")

;; start auto-complete with emacs 
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140314.802/dict")
;; do default config for auto-complete
;; (ac-config-default)

;; you should install clang first
(require 'auto-complete-clang)

;; auto complete is CPU sensitive
;; so we manually call it
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
(setq ac-auto-start nil
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-quick-help-height 30)

;; tell clang where is the include path
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
	      user-include-dirs))

;; define my own completion
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

;; use clang for c++ completion
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang  ac-source-yasnippet ac-source-gtags) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

;; use my own completion
(my-ac-config)

(provide 'init-auto-complete)
