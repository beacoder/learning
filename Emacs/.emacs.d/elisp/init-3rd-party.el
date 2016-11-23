;;----------------------------------------------------------------------------
;; third-party setting
;;----------------------------------------------------------------------------

;;; whitespace setting
(require 'init-whitespace)


;;; dired setting
(require-package 'dired+)
(require-package 'dired-details)
(require-package 'dired-details+)


;;; async setting
(require-package 'async)
;; deal with problems when updating packages
(require 'async-bytecomp)
(async-bytecomp-package-mode 1)

;; provide dired with asynchronous abilities
(after-load "dired-aux" (require 'dired-async))

;; sending emails asynchronously
(require 'smtpmail-async)
(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)


;;; multiple-cursors setting
(require-package 'multiple-cursors)

;; apply mulitiple cursor to region lines
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; apply mulitiple cursor to lines like this region
(global-set-key (kbd "C-c C-s") 'mc/mark-all-like-this)


;;; undo-tree setting
;; "C-x u" => open the undo-tree-visualizer
(require-package 'undo-tree)
(global-undo-tree-mode)
;; undo-buffer limit -> 100 MB                                                       |
(setq undo-outer-limit (* 100 (expt 1024 2)))



;;; slime setting
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program
;;       (replace-regexp-in-string "/lib/sbcl/?$" "/bin/sbcl" (getenv "SBCL_HOME")))
;; (slime-setup '(slime-fancy slime-indentation slime-asdf))
;; (setq lisp-indent-function 'common-lisp-indent-function)


;;; paredit setting
;; (require-package 'paredit)
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (eval-after-load 'paredit
;;   '(progn
;;      ;; Modify kill-sentence, which is easily confused with the kill-sexp
;;      ;; binding, but doesn't preserve sexp structure
;;      (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
;;      (define-key paredit-mode-map [remap backward-kill-sentence] nil)

;;      ;; Allow my global binding of M-? to work when paredit is active
;;      (define-key paredit-mode-map (kbd "M-?") nil)
;;      ))
;; (add-hook 'prog-mode-hook 'enable-paredit-mode)

;; ;; "C-)" might not work as expected in putty, so we create a new prefix-key for paredit.
;; (define-prefix-command 'paredit-map)
;; (define-key global-map "\C-xp" paredit-map)
;; (define-key paredit-map (kbd "s") 'paredit-forward-slurp-sexp)
;; (define-key paredit-map (kbd "b") 'paredit-forward-barf-sexp)


;;; smex setting
(require-package 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)


;;; ttcn3 setting
(autoload 'ttcn-3-mode "ttcn3" "Major mode for ttcn3 files" t)
(add-to-list 'auto-mode-alist '("\\.ttcn$" . ttcn-3-mode))
(add-to-list 'auto-mode-alist '("\\.ttcnpp$" . ttcn-3-mode))


;;; keyfreq setting
;; keyfreq-show could show the key-frequency
(require-package 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        forward-char
        backward-char
        previous-line
        next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;;; regex-tool setting
;; "C-c C-c" => force an update
;; "C-c C-k" => quit regex-tool
(require-package 'regex-tool)
;; use pcre instead of emacs
(setq regex-tool-backend 'perl)
(global-set-key (kbd "C-c C-r") 'regex-tool)


;;; zenburn setting
;; need to setup putty color which goes well with zenburn first
(require-package 'zenburn-theme)
(load-theme 'zenburn t)


;;; The guide-key package pops up keybinding reminders after a short delay.
(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-c" "C-c ;" "C-x r" "C-x t"))
(guide-key-mode 1)


;;; diminish
(require-package 'diminish)


;;; ggtags setting
;; "cd /path/to/project && gtags" => generate gtags
;; "M-." =>  ggtags-find-tag-dwim
;; "C-c M-r" ggtags-find-tag-regexp
;; "M-]" =>  ggtags-find-reference
;; "C-c M-f" ggtags-find-file
;; "C-c M-g" ggtags-grep
(require-package 'ggtags)
(after-load 'ggtags (define-key ggtags-mode-prefix-map "\M-r" 'ggtags-find-tag-regexp))


;;; weather report
;;  "wttrin" => Display weather; "g" => Change city; "q" => Quit
(require-package 'wttrin)
(setq wttrin-default-cities '("Shanghai" "Taizhou"))


;;; other setting
(require 'init-hydra)
(require 'init-git)
;; (require 'init-helm)
(require 'init-company)
(require 'init-rtags)
(require 'init-dictionary)
(require 'init-dired)
(require 'init-org)
(require-package 'buffer-move)
(require-package 'flymake-cursor)

(provide 'init-3rd-party)
