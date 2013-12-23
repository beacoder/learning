;;----------------------------------------------------------------------------
;; global key-bindings
;;----------------------------------------------------------------------------

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
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."
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

;;----------------------------------------------------------------------------
;; mode setting
;;----------------------------------------------------------------------------

;; enable icomplete-mode
(icomplete-mode t)

;; enable ido-mode
(if (fboundp 'ido-mode) (ido-mode t))

;; enable ibuffer-mode
(if (fboundp 'ibuffer-mode) (ibuffer-mode))

;; enable electric-pair-mode
(if (fboundp 'electric-pair-mode) (electric-pair-mode t))

;; highlight matching pairs of parentheses
(if (fboundp 'electric-pair-mode) 
    (progn
      (show-paren-mode 1)
      (setq show-paren-delay 0)))

;; save desktop
(if (fboundp 'desktop-save-mode) (desktop-save-mode t))

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

;; incremental picking of buffers
(if (fboundp 'iswitchb-mode) (iswitchb-mode t))

;; enable flyspell in text-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; improve performance
(setq flyspell-issue-message-flag nil)

;;----------------------------------------------------------------------------
;; some other settings
;;----------------------------------------------------------------------------

;; enable narrow
(put 'narrow-to-region 'disabled nil)

;; disable emacs system beep
(setq visible-bell t)

;; enlarge kill-ring-max value
(setq kill-ring-max 200)

;; show file's full path in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
       
;; display time
(display-time-mode t)
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-format nil)

;; "DejaVu Sans Mono" is a nice open source font, good for programming
;;(if (display-graphic-p)
;;    (set-face-attribute 'default nil 
;;                        :font "DejaVu Sans Mono-10:weight=normal"
;;                        :height 100))
                        
;; "DejaVu Sans Mono" is a nice open source font family
(if (display-graphic-p)
    (set-face-attribute 'default nil
 		    :font "-misc-dejavu lgc sans mono-medium-r-normal--0-0-0-0-m-0-iso8859-1"
 		    :height 100))                        
                        
;; swap the foreground and background colors of face
(invert-face 'default)

;; set cursor color
(add-hook 'window-setup-hook '(lambda () (set-cursor-color "white")))
(add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "white"))))

;; save place in files between sessions
(require 'saveplace)
(setq-default save-place t)

(provide 'init-basics)
