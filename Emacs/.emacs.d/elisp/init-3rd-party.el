;;----------------------------------------------------------------------------
;; multiple-cursors setting
;;----------------------------------------------------------------------------

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

;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; auto complete is CPU sensitive
(ac-set-trigger-key "<C-return>")

;; use tooltip and set delay time
(setq ac-auto-start nil
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-quick-help-height 30)
      
;;----------------------------------------------------------------------------
;; undo-tree setting
;;----------------------------------------------------------------------------

;; "C-x u" => open the undo-tree-visualizer
(require 'undo-tree)
(global-undo-tree-mode)

;; undo-buffer limit -> 100 MB                                                       |
(setq undo-outer-limit (* 100 (expt 1024 2)))

;;----------------------------------------------------------------------------
;; slime setting
;;----------------------------------------------------------------------------

;; (require 'slime-autoloads)
;; (setq inferior-lisp-program
;;       (replace-regexp-in-string "/lib/sbcl/?$" "/bin/sbcl" (getenv "SBCL_HOME")))
;; (slime-setup '(slime-fancy slime-indentation slime-asdf))
;; (setq lisp-indent-function 'common-lisp-indent-function)

;;----------------------------------------------------------------------------
;; paredit setting
;;----------------------------------------------------------------------------

(require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(eval-after-load 'paredit
  '(progn
     ;; Modify kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)

     ;; Allow my global binding of M-? to work when paredit is active
     (define-key paredit-mode-map (kbd "M-?") nil)
     ))
(add-hook 'prog-mode-hook 'enable-paredit-mode)

;; "C-)" might not work as expected in putty, so we create a new prefix-key for paredit.
(define-prefix-command 'paredit-map)
(define-key global-map "\C-xp" paredit-map)
(define-key paredit-map (kbd "fs") 'paredit-forward-slurp-sexp)
(define-key paredit-map (kbd "bs") 'paredit-backward-slurp-sexp)
(define-key paredit-map (kbd "fb") 'paredit-forward-barf-sexp)
(define-key paredit-map (kbd "bb") 'paredit-backward-barf-sexp)

;;----------------------------------------------------------------------------
;; third-party setting
;;----------------------------------------------------------------------------

(require 'magit)
(require 'init-helm)
(require 'init-dictionary)
(require 'init-dired)
(require 'buffer-move)
(require 'flymake-cursor)
(require 'init-highline)
(require 'init-async)
(require 'init-productivity)

(provide 'init-3rd-party)
