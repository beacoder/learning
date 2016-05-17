;;----------------------------------------------------------------------------
;; ido configuration (use helm if possible)
;;----------------------------------------------------------------------------

;; use "C-f" during file selection to switch to regular find-file
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ;; ido-separator "\n" ;; display choices vertically
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

;; copied from https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-ido.el
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark (point))
      (goto-char position))))

(provide 'init-ido)
