;;----------------------------------------------------------------------------
;; init-highline setting
;;----------------------------------------------------------------------------
(require 'highline)

(defun highline-mode-on () (highline-mode 1))

;; Turn on local highlighting for Dired (C-x d)
(add-hook 'dired-after-readin-hook #'highline-mode-on)

;; Turn on local highlighting for list-buffers (C-x C-b)
(defadvice list-buffers (after highlight-line activate)
  (save-excursion
    ;; (set-buffer "*Buffer List*")
    (set-buffer "*Ibuffer*")
    (highline-mode-on)))

(provide 'init-highline)
