;; cperl mode setting

;; @see http://www.emacswiki.org/emacs/CPerlMode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(provide 'init-cperl-mode)
