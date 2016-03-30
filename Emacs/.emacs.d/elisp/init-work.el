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

(provide 'init-work)
