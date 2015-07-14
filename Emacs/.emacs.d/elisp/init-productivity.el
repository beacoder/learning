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

;;----------------------------------------------------------------------------
;; bracket operation
;;----------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/elisp_modify_syntax_table_temporarily.html
;; "i b" => select text inside bracket.
(global-set-key (kbd "C-c i b") 'xah-select-text-in-bracket)
(defun xah-select-text-in-bracket ()
  "Select text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (with-syntax-table (standard-syntax-table)
    ;; @see http://www.emacswiki.org/emacs/EmacsSyntaxTable
    (modify-syntax-entry ?\« "(»")
    (modify-syntax-entry ?\» ")«")
    (modify-syntax-entry ?\‹ "(›")
    (modify-syntax-entry ?\› ")‹")
    (modify-syntax-entry ?\“ "(”")
    (modify-syntax-entry ?\” ")“")
    (modify-syntax-entry ?\‘ "(’")
    (modify-syntax-entry ?\’ ")‘")
    ;; (modify-syntax-entry ?\< "(>")
    ;; (modify-syntax-entry ?\> ")<")
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

;; "w i b" => copy text inside bracket.
(global-set-key (kbd "C-c w i b") 'copy-text-in-bracket)
(defun copy-text-in-bracket ()
  "Copy text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (xah-select-text-in-bracket)
  (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))))

;; "y i b" => yank text into inside bracket.
(global-set-key (kbd "C-c y i b") 'yank-text-in-bracket)
(defun yank-text-in-bracket ()
  "Yank text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (xah-select-text-in-bracket)
  (if (region-active-p)
      (progn
        (delete-region (region-beginning) (region-end))
        (yank))))

;; "k i b" => kill text inside bracket.
(global-set-key (kbd "C-c k i b") 'kill-text-in-bracket)
(defun kill-text-in-bracket ()
  "Kill text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (xah-select-text-in-bracket)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))))

;;----------------------------------------------------------------------------
;; paragraph operation
;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------
;; extend-selection
;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------
;; register-usage
;;----------------------------------------------------------------------------

;; @see https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_editing_commands.el
(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line ))
         (inputStr (elt bds 0) )
         (p1 (elt bds 1) )
         (p2 (elt bds 2)))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." inputStr)
))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert-register ?1 t))

(global-set-key (kbd "C-c h 1") 'xah-copy-to-register-1)
(global-set-key (kbd "C-c h 0") 'xah-paste-from-register-1)

;;----------------------------------------------------------------------------
;; search multiple open buffers
;;----------------------------------------------------------------------------

;; @see http://stackoverflow.com/questions/2641211/emacs-interactively-search-open-buffers
(require 'cl)
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

(global-set-key [f8] 'search-all-buffers)

;;----------------------------------------------------------------------------
;; Copy Current Line If No Selection
;;----------------------------------------------------------------------------

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (p1 p2)
    (if current-prefix-arg
        (progn (setq p1 (point-min))
               (setq p2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq p1 (region-beginning))
                        (setq p2 (region-end)))
               (progn (setq p1 (line-beginning-position))
                      (setq p2 (line-end-position))))))
    (kill-ring-save p1 p2)
    (if current-prefix-arg
        (message "buffer text copied")
      (message "text copied"))))

(global-set-key (kbd "M-9") 'xah-copy-line-or-region)
(global-set-key (kbd "M-0") 'kill-whole-line)

(provide 'init-productivity)
