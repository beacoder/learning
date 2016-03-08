;;----------------------------------------------------------------------------
;; define some common functions
;;----------------------------------------------------------------------------

;; @see http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html
(defun get-selection-or-unit (unit)
  "Return the string and boundary of text selection or UNIT under cursor.

If `use-region-p' is true, then the region is the unit.  Else,
it depends on the UNIT. See `unit-at-cursor' for detail about
UNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (get-selection-or-unit 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)
  (unless (region-active-p) (push-mark))
  (let ((p1 (region-beginning)) (p2 (region-end)))
    (if (use-region-p)
        (vector (buffer-substring-no-properties p1 p2) p1 p2 )
      (unit-at-cursor unit))))

(defun unit-at-cursor  (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:
• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.
• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, that doesn't have spaces in it.
• 'line — delimited by “\\n”.
• 'block — delimited by “\\n\\n” or beginning/end of buffer.
• 'buffer — whole buffer. (respects `narrow-to-region')
• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
    (setq bds (unit-at-cursor 'line))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2))

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:
• this function returns the text and the 2 boundaries as a vector in one shot.
• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.
• 'word does not depend on syntax table.
• 'block does not depend on syntax table."
  (let (p1 p2)
    (save-excursion
        (cond
         ( (eq unit 'word)
           (let ((wordcharset "-A-Za-zÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
             (skip-chars-backward wordcharset)
             (setq p1 (point))
             (skip-chars-forward wordcharset)
             (setq p2 (point))))

         ( (eq unit 'glyphs)
           (progn
             (skip-chars-backward "[:graph:]")
             (setq p1 (point))
             (skip-chars-forward "[:graph:]")
             (setq p2 (point))))

         ( (eq unit 'buffer)
           (progn
             (setq p1 (point-min))
             (setq p2 (point-max))))

         ((eq unit 'line)
          (progn
            (setq p1 (line-beginning-position))
            (setq p2 (line-end-position))))
         ((eq unit 'block)
          (progn
            (if (re-search-backward "\n\n" nil t)
                (progn (forward-char 2)
                       (setq p1 (point)))
              (setq p1 (line-beginning-position)))

            (if (re-search-forward "\n\n" nil t)
                (progn (backward-char)
                       (setq p2 (point)))
              (setq p2 (line-end-position)))))

         ((vectorp unit)
          (let (p0)
             (setq p0 (point))
             (skip-chars-backward (elt unit 0))
             (setq p1 (point))
             (goto-char p0)
             (skip-chars-forward (elt unit 1))
             (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2)))

;; find the directory containing a given library
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

;; define line-number-at-pos
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

(defconst *is-windows* (eq system-type 'windows-nt))

(defun is-modern-emacs ()
  "if emacs version is greater than 24.3, return true else false."
  (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3))
      t nil))

(provide 'init-utils)
