;;----------------------------------------------------------------------------
;; define some common functions
;;----------------------------------------------------------------------------

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
 If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
	(goto-char (point-min))
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))

;; trim string
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun getline ()
  "Return line contents as a string."
  (let (beg end line-string)
    (setq beg (line-beginning-position))
    (setq end (line-end-position))
    (setq line-string (buffer-substring-no-properties beg end))
    (trim-string line-string)))

(defun getline-nth (line-number)
  "Return specific line's contents as a string."
  (save-excursion
    ;; when in elisp program use the following two statements
    ;; instead of goto-line
    (goto-char (point-min))
    (forward-line (1- line-number))
    (getline)))

;; Inspired by apply-macro-to-region-lines
(defun apply-function-to-region-lines-with-args (fn)
  "Apply function to region lines."
  (interactive "aFunction to apply to lines in region: ")
  (setq eof (line-number-at-pos (region-end))
	beg (line-number-at-pos (region-beginning))
	cur (min beg eof)
	filelist nil)
  (save-excursion
    (while (<= cur eof)
      (setq line-string (getline-nth cur)
            ;; filelist (cons line-string filelist)
	    old (buffer-name))
      (funcall fn line-string)
      (switch-to-buffer old)
      (setq cur (1+ cur)))
    ;; (mapcar fn filelist)
    ))

;; Inspired by apply-macro-to-region-lines
(defun apply-function-to-region-lines-without-args (fn)
  (interactive "aFunction to apply to lines in region: ")
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
	  next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(let ((start nil)
	      (end nil))
	  (goto-char next-line-marker)
	  (save-excursion
	    (setq start (point))
	    (forward-line 1)
	    (set-marker next-line-marker (point))
	    (setq end (point)))
	  (save-excursion
	    (let ((mark-active nil))
	      (narrow-to-region start end)
	      (funcall fn)
	      (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))

;; operate shell command on current buffer file
(defun execute-command-on-file-core (command asynchronous)
  (require 'simple)
  (when (and (not (string= command ""))
             (buffer-file-name))
    (let ((final-command (concat command " " (buffer-file-name))))
      (if (not asynchronous)
          (progn
            (message "%s" final-command)
            (shell-command final-command))
        (progn
          (message "%s &" final-command)
          (async-shell-command final-command))))))

(defun execute-command-on-file (command)
  (interactive "sPlease input command name which will be executed on this file: ")
  (execute-command-on-file-core command nil))

(defun async-execute-command-on-file (command)
  (interactive "sPlease input command name which will be executed on this file: ")
  (execute-command-on-file-core command t))

(defvar Execute-Command-On-File nil)
(defun Switch-Command-Target ()
  (interactive)
  (if (not Execute-Command-On-File)
      (progn
        (message "Execute-Command-On-File disabled !")
        (global-set-key (kbd "M-!") 'shell-command)
        (global-set-key (kbd "M-&") 'async-shell-command)
        (setq Execute-Command-On-File t))
    (progn
      (message "Execute-Command-On-File enabled !")
      (global-set-key (kbd "M-!") 'execute-command-on-file)
      (global-set-key (kbd "M-&") 'async-execute-command-on-file)
      (setq Execute-Command-On-File nil))))

(global-set-key (kbd "<escape>") 'Switch-Command-Target)

(provide 'init-com-functions)
