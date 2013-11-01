;; run routine tasks when emacs is idle

;; define routine-task
(defun routine-task ()
  "show how to use the routine."
  (message "running routine tasks !"))

;; define how often the auto-save happens
(setq auto-save-default t
      auto-save-mode t
      auto-save-interval 300
      auto-save-timeout 10)

;; run routine-task after auto-save happens
(add-hook 'auto-save-hook 'routine-task)

;; perform an action the next time Emacs is idle for SECS seconds
;; (run-with-idle-timer 10 t 'routine-task)

;; perform an action after a delay of SECS seconds
;; (run-with-timer 0 10 'routine-task)

(provide 'init-routines)
