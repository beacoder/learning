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
