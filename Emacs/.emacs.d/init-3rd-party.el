;;----------------------------------------------------------------------------
;; multiple-cursors setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/multiple-cursors-20140418.815"))

(require 'multiple-cursors)

;; apply mulitiple cursor to region lines
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; apply mulitiple cursor to lines like this region
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)

;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/popup-20140207.1702"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140512.43"))

;; do default config for auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140512.43/dict")
(ac-config-default)

;; auto complete is CPU sensitive
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
(setq ac-auto-start nil
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-quick-help-height 30)

;;----------------------------------------------------------------------------
;; God-Mode setting
;;----------------------------------------------------------------------------

;; Mapping

;; all commands are assumed to be "C-<something>" unless otherwise indicated.
;; a -> C-a
;; akny -> C-a C-k C-n C-y
;; x_s -> C-x s
;; (note the use of space to produce "C-x s")

;; "g" is a special key to indicate "M-<something>".
;; "G" is a special key to indicate "C-M-<something>".

;; digital arguments.
;; 12f -> M-12 C-f

;; repetition (with "." key-binding)
;; gf... -> M-f M-f M-f

;; add to load-path
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/god-mode-20140413.420"))

;; (require 'god-mode)

;; ;; toggle buffer-local god-mode
;; (global-set-key (kbd "<escape>") 'god-local-mode)
;; ;; toggle global god-mode
;; ;; (global-set-key (kbd "<escape>") 'god-mode-all)

;; ;; change cursor style when god-mode is on
;; (defun my-update-cursor ()
;;   (setq cursor-type (if (and god-local-mode) 'bar 'box)))

;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; ;; key-bindings used in god-mode
;; ;; (define-key god-local-mode-map (kbd ".") 'repeat)

;; ;; disable god-mode in some major modes
;; (add-to-list 'god-exempt-major-modes 'dired-mode)

;;----------------------------------------------------------------------------
;; Magit-Mode setting
;;----------------------------------------------------------------------------

;; add to load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/magit-20140623.1208"))

(require 'magit)

(provide 'init-3rd-party)
