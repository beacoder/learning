;; define some common functions

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
(defun apply-function-to-region-lines (fn)
  "Apply function to region lines."
  (interactive "aFunction to apply to lines in region: ")
  (setq eof (line-number-at-pos (region-end))
	beg (line-number-at-pos (region-beginning))
	cur (min beg eof)
	filelist nil)
  (while (<= cur eof)
    (setq line-string (getline-nth cur)
	  ;; filelist (cons line-string filelist)
	  old (buffer-name))
    (funcall fn line-string)
    (switch-to-buffer old)
    (setq cur (1+ cur))
    )
  ;; (mapcar fn filelist)
  )

(provide 'init-com-functions)
