;;----------------------------------------------------------------------------
;; global key-bindings
;;----------------------------------------------------------------------------

;; Added after reading http://steve.yegge.googlepages.com/effective-emacs
;; M-x may not be avalible everywhere, for example:
;; In term-char-mode, or when there is no Meta key.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; bind pop-tag-mark
(global-set-key (kbd "M-?") 'pop-tag-mark)

;; bind goto-line command
(global-set-key (kbd "M-g M-g") 'goto-line)

;; dictionary key-bindings
(global-set-key (kbd "\C-cs") 'dictionary-search)
(global-set-key (kbd "\C-cm") 'dictionary-match-words)

;; enable recentf-mode
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 1000
      recentf-exclude '(
                     ;; "/tmp/"
                        "/ssh:"
                        "/sudo:"
                     ;; "/home/[a-z]\+/\\."
                        ))
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; @see http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
(setq recentf-keep '(file-remote-p file-readable-p))

;; bind compile command
(global-set-key (kbd "C-c p") 'compile)

;; move point from window to window using shift and arrow keys
;; instead of 'C-x o'
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; swap buffers without typing C-x b on each window
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;----------------------------------------------------------------------------
;; Make "C-x o" prompt for a target window when there are more than 2
;;----------------------------------------------------------------------------
(require 'switch-window)
(setq switch-window-shortcut-style 'alphabet)
(global-set-key (kbd "C-x o") 'switch-window)

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

;;----------------------------------------------------------------------------
;; mode setting
;;----------------------------------------------------------------------------

;; navigate window layouts with "C-c <left>" and "C-c <right>"
(winner-mode 1)

;; enable icomplete-mode
(icomplete-mode t)

;; incremental picking of buffers
;; (if (fboundp 'iswitchb-mode) (iswitchb-mode t))

;; auto-refresh all buffers when files have changed on disk.
(global-auto-revert-mode t)

;; enable ibuffer-mode
(if (fboundp 'ibuffer-mode) (ibuffer-mode))

;; enable electric-pair-mode
(if (fboundp 'electric-pair-mode) (electric-pair-mode t))

;; highlight matching pairs of parentheses
(if (fboundp 'electric-pair-mode)
    (progn
      (show-paren-mode 1)
      (setq show-paren-delay 0)))

;; highlight the active region
(transient-mark-mode t)

;; hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; hide toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; enable the delete-selection-mode
(delete-selection-mode t)

;; enable global high light
(global-font-lock-mode t)

;; set text-mode as default major mode
(setq default-major-mode 'text-mode)

;; enable emacs to open image file
(auto-image-file-mode)

;; enable line-number-mode
(line-number-mode t)

;; enable column-number-mode
(column-number-mode t)

;; show line-number in left margin
(when (> emacs-major-version 21)
  (global-linum-mode t))

;; enable flyspell in text-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; improve performance
(setq flyspell-issue-message-flag nil)

;; replace tab with spaces, use "C-q [tab]" to get a real tab
(setq-default indent-tabs-mode nil)

;; highlight tabulations
(setq-default highlight-tabs t)

;; show trailing white spaces
(setq-default show-trailing-whitespace t)

;; remove useless whitespaces before saving a file
(add-hook 'before-save-hook
          (lambda()
            (when (member (message "%s" major-mode)
                          '("c-mode" "c++-mode"))
              (whitespace-cleanup)
              (delete-trailing-whitespace))))

;;----------------------------------------------------------------------------
;; setting locales
;;----------------------------------------------------------------------------

;; set locale to utf-8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;----------------------------------------------------------------------------
;; some other settings
;;----------------------------------------------------------------------------

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;; disable emacs system beep
(setq visible-bell t)

;; enlarge kill-ring-max value
(setq kill-ring-max 200)

;; enlarge max length for all history lists,
;; including minibuffer-history, extended-command-history ...
(setq history-length 200)

;; show file's full path in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; "DejaVu Sans Mono" is a nice open source font, good for programming
;;(if (display-graphic-p)
;;    (set-face-attribute 'default nil
;;                        :font "DejaVu Sans Mono-10:weight=normal"
;;                        :height 100))

(when (display-graphic-p)
   ;; "DejaVu Sans Mono" is a nice open source font family
   (set-face-attribute 'default nil
                       :font "-misc-dejavu lgc sans mono-medium-r-normal--0-0-0-0-m-0-iso8859-1"
                       :height 100)
   ;; swap the foreground and background colors of face
   (invert-face 'default))

;; set minibuffer-prompt color
(set-face-foreground 'minibuffer-prompt "red")

;; set cursor color
(add-hook 'window-setup-hook '(lambda () (set-cursor-color "white")))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "white"))))
(setq cursor-type 'box)

;; save place in files between sessions
(require 'saveplace)
(setq-default save-place t)

;; ediff splits window horizontally and sets whitespace insensitivity 
(setq ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")

;; Overwrite flymake-display-warning so that no annoying dialog box is
;; used.

(when (display-graphic-p)
   (require 'flymake))

;; This version uses lwarn instead of message-box in the original version.
;; lwarn will open another window, and display the warning in there.
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (lwarn 'flymake :warning warning))

;; Using lwarn might be kind of annoying on its own, popping up windows and
;; what not. If you prefer to recieve the warnings in the mini-buffer, use:
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))

;; show elisp error backtrace
;; (setq debug-on-error t)

;; make sure shell in emacs can see alias in ~/.bashrc
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-ic")

;; fix Error: No word lists can be found for the language "zh_CN"
;; use apsell as ispell backend
(setq-default ispell-program-name "aspell")  
;; use American English as ispell default dictionary  
(ispell-change-dictionary "american" t)

(provide 'init-basics)
