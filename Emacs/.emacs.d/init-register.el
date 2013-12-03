;; save register definitions

;; "C-x r j e" to open file ".emacs" 
(set-register ?e '(file . "~/.emacs"))

;; "C-x r j h" to open file ".bash_history" 
(set-register ?h '(file . "~/.bash_history"))

(provide 'init-register)
