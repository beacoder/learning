;;----------------------------------------------------------------------------
;; rtags setting
;;----------------------------------------------------------------------------

(require-package 'rtags)

;;; steps to index a project
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 /path/to/project
;; rc -J /path/to/project

;;; useful key-bindings
;; "C-c r ." rtags-find-symbol-at-point
;; "C-c r ," rtags-find-references-at-point
;; "C-c r /" rtags-find-all-references-at-point
;; "C-c r v" rtags-find-virtuals-at-point
;; "C-c r h" rtags-print-class-hierarchy
;; "C-c r A" rtags-find-functions-called-by-this-function
;; "C-c r T" rtags-taglist

;; start the rdm process unless the process is already running.
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

(require 'company)
(require 'rtags)

(rtags-enable-standard-keybindings c-mode-base-map)
(rtags-diagnostics)
(setq rtags-completions-enabled t
      rtags-autostart-diagnostics t)
(push 'company-rtags company-backends)

;; fall back to gtags if rtags doesn’t have a certain project indexed
(defun use-rtags ()
  (and (rtags-executable-find "rc")
       (rtags-is-indexed)))

(define-key rtags-mode-map (kbd "M-n") (lambda ()
                                         (interactive)
                                         (save-excursion
                                           (next-line)
                                           (rtags-select-other-window))))
(define-key rtags-mode-map (kbd "M-p") (lambda
                                         (interactive)
                                         (save-excursion
                                           (previous-line)
                                           (rtags-select-other-window))))

(provide 'init-rtags)
