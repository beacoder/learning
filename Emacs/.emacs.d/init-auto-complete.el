;;----------------------------------------------------------------------------
;; auto-complete setting
;;
;; download auto-complete from http://cx4a.org/software/auto-complete/
;; install it using "emacs -batch -l etc/install.el", choose path "~/.emacs.d/"
;; add following scripts in .emacs
;;----------------------------------------------------------------------------

(when (> emacs-major-version 21)
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")))
    
;; enable pos-tip
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)

;; show tooltip
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)

(setq ac-dwim t)

;; auto complete using clang is CPU sensitive
(setq ac-auto-start nil)
(ac-set-trigger-key "<C-return>")

;; add function into ac-trigger-commands to trigger complete
(setq ac-trigger-commands
   (cons 'backward-delete-char-untabify ac-trigger-commands))
   
;; trigger complete even if your typing is wrong   
(setq ac-fuzzy-enable t)

(provide 'init-auto-complete)

