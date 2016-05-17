;;----------------------------------------------------------------------------
;; ido configuration (use helm if possible)
;;----------------------------------------------------------------------------

;; use "C-f" during file selection to switch to regular find-file
(ido-mode t)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ;; ido-separator "\n" ;; display choices vertically
      ido-default-buffer-method 'selected-window)

;; Ido buffer intuitive navigation
(add-hook 'ido-setup-hook '(lambda ()
                             (define-key ido-completion-map "\C-p" 'ido-prev-match)
                             (define-key ido-completion-map "\C-r" 'ido-prev-match)
                             (define-key ido-completion-map "\C-s" 'ido-next-match)
                             (define-key ido-completion-map "\C-n" 'ido-next-match)))

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 99999)

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'init-ido)
