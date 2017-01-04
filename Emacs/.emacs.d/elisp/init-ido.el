;;----------------------------------------------------------------------------
;; ido configuration (use helm if possible)
;;----------------------------------------------------------------------------

(require-package 'flx-ido)
(flx-ido-mode 1)
;; @see https://github.com/lewang/flx
(setq flx-ido-threshold 10000)

;; use "C-f" during file selection to switch to regular find-file
(ido-mode t)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length -1
      ido-use-faces nil          ;; disable ido faces to see flx highlights.
      ;; ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ;; ido-separator "\n"      ;; display choices vertically
      ido-default-buffer-method 'selected-window)

(when (maybe-require-package 'ido-ubiquitous)
  (ido-ubiquitous-mode t))

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

;; disable ido for certain commands,
;; @see http://stackoverflow.com/questions/6771664/disable-ido-mode-for-specific-commands
(defadvice ido-read-buffer (around ido-read-buffer-possibly-ignore activate)
  "Check to see if use wanted to avoid using ido"
  (if (eq (get this-command 'ido) 'ignore)
      (let ((read-buffer-function nil))
        (run-hook-with-args 'ido-before-fallback-functions 'read-buffer)
        (setq ad-return-value (apply 'read-buffer (ad-get-args 0))))
    ad-do-it))
(put 'shell 'ido 'ignore)
(put 'ffap-alternate-file 'ido 'ignore)
(put 'tmm-menubar 'ido 'ignore)
(put 'dired-do-copy 'ido 'ignore)
(put 'dired-do-rename 'ido 'ignore)
(put 'vc-copy-file-and-rename-buffer 'ido 'ignore)
(put 'dired-create-directory 'ido 'ignore)
(put 'copy-file-and-rename-buffer 'ido 'ignore)
(put 'rename-file-and-buffer 'ido 'ignore)
(put 'w3m-goto-url 'ido 'ignore)
(put 'ido-find-file 'ido 'ignore)
(put 'ido-edit-input 'ido 'ignore)
(put 'read-file-name 'ido 'ignore)
(put 'dired-create-directory 'ido 'ignore)
(put 'minibuffer-completion-help 'ido 'ignore)
(put 'minibuffer-complete 'ido 'ignore)
(put 'c-set-offset 'ido 'ignore)
(put 'rgrep 'ido 'ignore)
(put 'dired-create-directory 'ido 'ignore)

(provide 'init-ido)
