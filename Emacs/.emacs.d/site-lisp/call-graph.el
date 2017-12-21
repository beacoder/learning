;;; temporary place it here
;; add function gradually

;; to-do
;; make it work first
;; ui, will use hierarchy library


(defun get-function-caller (function-reference)
  (let* ((list (split-string function-reference ":"))
         (file (nth 0 list))
         (lineNb (nth 1 list)))

    (find-file file)
    (goto-line (string-to-number lineNb))
    (setq function-name (which-function))
    (kill-this-buffer)
    (message function-name)
    (setq tmpList (split-string function-name "::"))
    (if (> (length tmpList) 1)
        (nth 1 tmpList)
      (nth 0 tmpList))))


(defun call-graph ()
  "Generate a call-graph for the function func."
  (interactive)

  ;; find tag reference
  (message "building call-graph ...")
  (shell-command (concat "global -a --result=grep --color=always -r uncheckOut | grep .cpp:"))

  ;; get the reference
  (switch-to-buffer "*Shell Command Output*")
  (setq draft-tmp (buffer-substring-no-properties (point-min) (point-max)))
  (dolist (line (split-string draft-tmp "\r\n\t" t))
    (message (get-function-caller line))
    ))


(defun test-it()
  (interactive)
  (dolist (line (split-string draft-tmp "\n" t))
    (message (get-function-caller line))))


(defun call-tree()
  (interactive)
  (async-start

   ;; START-FUNC -> Building TTCN3 tags
   `(λ ()

      ;; correctly handle colors in shell output
      (require 'ansi-color)
      (defadvice display-message-or-buffer (before ansi-color activate)
        "Process ANSI color codes in shell output."
        (let ((buf (ad-get-arg 0)))
          (and (bufferp buf)
               (string= (buffer-name buf) "*Shell Command Output*")
               (with-current-buffer buf
                 (ansi-color-apply-on-region (point-min) (point-max))))))

      ;; find tag reference
      (message "building call-graph ...")
      (shell-command (concat "global -a --result=grep --color=always -r uncheckOut | grep .cpp:"))

      ;; get the reference
      (switch-to-buffer "*Shell Command Output*")
      (buffer-substring-no-properties (point-min) (point-max))
      (setq draft-buffer (buffer-substring-no-properties (point-min) (point-max))))

   ;; FINISH-FUNC -> Change relative path to absolute path
   `(λ (&optional output)
      (message output)

      (dolist (line (split-string output "\r\n\t" t))
        (message (get-function-caller line))
        ))))
