;;----------------------------------------------------------------------------
;; work related settings
;;----------------------------------------------------------------------------

(require 'async)

(defun async-update-ttcn3-tags ()
  "Async udpate TTCN3 tags"
  (interactive)
  (let ()
    (message "Updating TTCN3 tags...")
    (async-start

     ;; START-FUNC -> Building TTCN3 tags
     `(lambda ()
        (message "Updating TTCN3 tags...")
        (shell-command
         (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh tags")))

     ;; FINISH-FUNC -> Change relative path to absolute path
     `(lambda (&optional ignore)
        (with-temp-file
            "~/my_tag_files/TAGS"
          (insert-file-contents (concat (getenv "TTCN3_BUILD_PATH") "/TAGS"))
          (goto-char (point-min))
          ;; replace relative path with abosolute path
          (while (re-search-forward "^\.\./" nil t)
            (replace-match (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/") t nil))
          (message "TTCN3 tags has been updated..."))))))

(defun async-build-ttcn3-project ()
  "Async build TTCN3 project"
  (interactive)
  (let ()
    (message "Building TTCN3 project...")
    (async-start

     ;; START-FUNC -> Building TTCN3 project
     `(lambda ()
        (message "Building TTCN3 project...")
        (shell-command
         (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/compile_ttcn.sh build") "*compilation*")
        (switch-to-buffer "*compilation*")
        (buffer-substring-no-properties (point-min) (point-max)))

     ;; FINISH-FUNC -> Show compilation result
     `(lambda (&optional result)
        (switch-to-buffer "*compilation*")
        (insert result)
        (message "Successfully build TTCN3 project...")))))

(defun async-copy-ttcn3-project ()
  "Async copy TTCN3 binary to lab"
  (interactive)
  (let ()
    (message "Copying TTCN3 binary to lab...")
    (async-start

     ;; START-FUNC -> Copying TTCN3 binary
     `(lambda ()
        (message "Copying TTCN3 binary to lab...")
        (shell-command
         (concat (getenv "TTCN3_GGSN_ROOT_PATH") "/scripts/copy_ttcn3.sh") "*output*")
        (switch-to-buffer "*output*")
        (buffer-substring-no-properties (point-min) (point-max)))

     ;; FINISH-FUNC -> Show output result
     `(lambda (&optional result)
        (switch-to-buffer "*output*")
        (insert result)
        (message "Successfully copied TTCN3 binary to lab...")))))

(eval-after-load 'ttcn3
  '(progn
     ;; Allow my global binding of M-? to work when paredit is active
     (define-key ttcn3-mode-map (kbd "M-?") nil)))

;; key-bindings used in ttcn3-mode
(define-prefix-command 'ttcn3-map)
(define-key ttcn3-mode-map "\C-xt" ttcn3-map)
(define-key ttcn3-map (kbd "u") 'async-update-ttcn3-tags)
(define-key ttcn3-map (kbd "b") 'async-build-ttcn3-project)
(define-key ttcn3-map (kbd "c") 'async-copy-ttcn3-project)

(provide 'init-work)
