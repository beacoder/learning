;;----------------------------------------------------------------------------
;; global key-bindings
;;----------------------------------------------------------------------------

;; bind Open File Path Under Cursor Fast
(global-set-key (kbd "C-c f") 'open-file-at-cursor)

;; @see http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
  (interactive)
  (let ( (path (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'filename) ) ))
    (if (string-match-p "\\`https?://" path)
        (browse-url path)
      (progn ; not starting “http://”
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" path) )
              (find-file path )) ) ) ) ) ))

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

;; ediff splits window horizontally
(setq ediff-split-window-function 'split-window-horizontally)

;; Overwrite flymake-display-warning so that no annoying dialog box is used.
(if (display-graphic-p)
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
(setq debug-on-error t)

(provide 'init-basics)
