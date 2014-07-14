;; run routine tasks when emacs is idle

;; define a global variable as the counter
(defvar counter 0
  "used to count how many times some function have been executed.")

;; define routine-task
(defun routine-task ()
  "show how to use the routine."
  (setq counter (1+ counter))
  (message "running routine tasks for %d times !" counter))

;; define how often the auto-save happens
(setq auto-save-default t
      auto-save-mode t
      auto-save-interval 300
      auto-save-timeout 60)

;; run routine-task after auto-save happens
(add-hook 'auto-save-hook 'routine-task)

;; perform an action the next time Emacs is idle for SECS seconds
;; (run-with-idle-timer 60 t 'routine-task)

;; perform an action after a delay of SECS seconds
;; (run-with-timer 0 60 'routine-task)

(provide 'init-routines)