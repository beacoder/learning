;;----------------------------------------------------------------------------
;; work related settings
;;----------------------------------------------------------------------------

(defun update-ttcn3-tags ()
  "Update ttcn3 tags"
  (interactive)
  (progn
    ;; generate ttcn3 tags
    (shell-command (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh tags"))
    (with-temp-file
        "~/my_tag_files/TAGS"
      (insert-file-contents (concat (getenv "TTCN3_BUILD_PATH") "/TAGS"))
      (goto-char (point-min))
      ;; replace relative path with abosolute path
      (while (re-search-forward "^\.\./" nil t)
        (replace-match (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/") t nil))
      (message "ttcn3 tags has been updated !"))))
(defalias 'ut 'update-ttcn3-tags)

(eval-after-load 'ttcn3
  '(progn
     ;; Allow my global binding of M-? to work when paredit is active
     (define-key ttcn3-mode-map (kbd "M-?") nil)))

;; key-bindings used in ttcn3-mode
(define-prefix-command 'ttcn3-map)
(define-key ttcn3-mode-map "\C-xt" ttcn3-map)
(define-key ttcn3-map (kbd "u") 'update-ttcn3-tags)

(provide 'init-work)
