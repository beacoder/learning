;; define some common functions

;; trim string
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )
  
;; get line contents as a string
(defun getline ()
  "Get line contents as a string."
  (let (beg end line-string)
    (setq beg (line-beginning-position))
    (setq end (line-end-position))
    (setq line-string (buffer-substring-no-properties beg end))
    )
  )

(defun getline-trim ()
  "Get line contents as a string and trim it."
  (trim-string (getline))
  )

(provide 'init-com-functions)
