;;----------------------------------------------------------------------------
;; rtags setting
;;----------------------------------------------------------------------------

(require-package 'rtags)

;;; steps to index a project
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 /path/to/project
;; rc -J /path/to/project

;; start rdm if it's not running
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(require 'company)
(push 'company-rtags company-backends)
(global-company-mode)
;; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

;; fall back to other taggers like gtags if rtags doesn’t have a certain project indexed.
(defun use-rtags (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not (gtags-get-rootpath)) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-tag)))
(defun tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
      (gtags-find-rtag)))
(defun tags-find-symbol ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
(defun tags-find-references ()
  (interactive)
  (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
(defun tags-find-file ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
(defun tags-imenu ()
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
(define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
(define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
(define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

(define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
(define-key global-map (kbd "M-,") (function tags-find-references-at-point))
(define-key global-map (kbd "M-;") (function tags-find-file))
(define-key global-map (kbd "C-.") (function tags-find-symbol))
(define-key global-map (kbd "C-,") (function tags-find-references))
(define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
(define-key global-map (kbd "M-i") (function tags-imenu))

(provide 'init-rtags)
