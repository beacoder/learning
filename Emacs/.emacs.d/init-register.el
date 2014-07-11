;; save register definitions

;; "C-x r j e" to open file ".emacs"
(set-register ?e '(file . "~/.emacs"))

;; "C-x r j d" to open directory ".emacs.d"
(set-register ?d '(file . "~/.emacs.d"))

;; "C-x r j h" to open file "~/.bash_history"
(set-register ?h '(file . "~/.bash_history"))

(provide 'init-register)
