;;----------------------------------------------------------------------------
;; utilities for productivity
;;----------------------------------------------------------------------------

;; @see http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
(global-set-key (kbd "C-c j") 'open-file-at-cursor)
(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
  (interactive)
  (let ( (path (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'filename) ) ))
    (if (string-match-p "\\`https?://" path)
        (browse-url path)
      (progn ; not starting “http://”
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" path) )
              (find-file path )) ) ) ) ) ))

;; try to follow the "action-where-object" pattern when defining key-bindings,
;; and use as less keys as possible.

;; http://ergoemacs.org/emacs/elisp_modify_syntax_table_temporarily.html
;; "i b" => select text inside bracket.
(global-set-key (kbd "C-c i b") 'xah-select-text-in-bracket)
(defun xah-select-text-in-bracket ()
  "Select text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (with-syntax-table (standard-syntax-table)
    (modify-syntax-entry ?\« "(»")
    (modify-syntax-entry ?\» ")«")
    (modify-syntax-entry ?\‹ "(›")
    (modify-syntax-entry ?\› ")‹")
    (modify-syntax-entry ?\“ "(”")
    (modify-syntax-entry ?\” ")“")
    (modify-syntax-entry ?\‘ "(’")
    (modify-syntax-entry ?\’ ")‘")
    (let (pos p1 p2)
      (setq pos (point))
      (search-backward-regexp "\\s(" nil t )
      (setq p1 (point))
      (forward-sexp 1)
      (setq p2 (point))
      (goto-char (1+ p1))
      (set-mark (1- p2)))))

;; "a b" => select text with bracket
(global-set-key (kbd "C-c a b") 'select-text-with-bracket)
(defun select-text-with-bracket ()
  "Select text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (xah-select-text-in-bracket)
  (if (region-active-p)
      (progn
        (goto-char (1- (region-beginning)))
        (set-mark (1+ (region-end))))))

;; "d b" => delete text inside bracket.
(global-set-key (kbd "C-c d b") 'delete-text-in-bracket)
(defun delete-text-in-bracket ()
  "Delete text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (xah-select-text-in-bracket)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))))

;; "a p" => select a text paragraph (block).
(global-set-key (kbd "C-c a p") 'xah-select-current-block)
(defun xah-select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

;; "d p" => delete text paragraph (block).
(global-set-key (kbd "C-c d p") 'xah-delete-text-block)
(defun xah-delete-text-block ()
  "Delete the current text block (paragraph) and also put it to `kill-ring'."
  (interactive)
  (xah-select-current-block)
  (if (region-active-p)
      (progn (kill-region (region-beginning) (region-end))
             (delete-blank-lines))))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(global-set-key (kbd "M-8") 'extend-selection)
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (use-region-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(provide 'init-productivity)
