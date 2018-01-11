;;----------------------------------------------------------------------------
;; c/c++ programming mode setting
;;----------------------------------------------------------------------------

;; avoid default "gnu" style, use more popular one
(setq c-default-style "linux")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

;; personal settings
(defun my-c-mode-common-hook ()
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (setq compilation-window-height 8)
  (setq compilation-finish-function
        (lambda (buf str)
          (if (string-match "exited abnormally" str)
              ;;there were errors
              (message "compilation errors, press C-x ` to visit")
            ;;no errors, make the compilation window go away in 0.5 seconds
            (when (string-match "*compilation*" (buffer-name buf))
              ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
              (bury-buffer "*compilation*")
              (winner-undo)
              (message "NO COMPILATION ERRORS!")))))

  ;; other customizations
  (setq c-basic-offset 4)
  (setq tab-width 8)
  ;; show function name in mode-line
  (which-function-mode t)
  ;; enable flyspell for comments in source code
  ;; (flyspell-prog-mode)
  ;; improve performance
  (setq flyspell-issue-message-flag nil)

  ;; navigation between header and cpp/cc files
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'c-end-of-defun)
  (local-set-key (kbd "C-c C-j") 'imenu)

  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

  ;; allow global binding to work when c/c++-mode is active
  (define-key c++-mode-map (kbd "C-c C-e") nil)
  (define-key c++-mode-map (kbd "C-c C-s") nil)
  (define-key c-mode-map (kbd "C-c C-e") nil)
  (define-key c-mode-map (kbd "C-c C-s") nil))

;; use <tab> to indent region if anything is selected
;; fledermaus came up with this
(defun fledermaus-maybe-tab ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (indent-region (region-beginning) (region-end) nil)
    (c-indent-command)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook (lambda () (local-set-key [(tab)] 'fledermaus-maybe-tab)))

;; google-c-style
(when (is-modern-emacs)
  (require-package 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))


;; company-c-headers
(when (is-modern-emacs)
  (when (maybe-require-package 'company-c-headers)
    (after-load 'company
      (add-hook 'c-mode-common-hook
                (λ () (sanityinc/local-push-company-backend 'company-c-headers))))))


;; flymake
(when (is-modern-emacs)
  (defun my-flymake-google-init ()
    "Setup flymake-google-cpplint"
    (require-package 'flymake-google-cpplint)
    (custom-set-variables
     ;; use cpplint.py to ensure that C++ code conforms to Google's coding style guides
     ;; dos2unix "~/.emacs.d/cpplint.py" convert from dos to unix text format
     ;; chmod 775 "~/.emacs.d/cpplint.py" to make cpplint.py executable
     '(flymake-google-cpplint-command "~/.emacs.d/cpplint.py")
     '(flymake-googlelint-verbose "3")
     '(flymake-googlelint-filter "-whitespace,+whitespace/braces")
     '(flymake-googlelint-root "project/src")
     '(flymake-googlelint-linelength "120"))
    (flymake-google-cpplint-load))

  ;; flymake-error-navigation
  (defvar my-flymake-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\M-p" 'flymake-goto-prev-error)
      (define-key map "\M-n" 'flymake-goto-next-error)
      map)
    "Keymap for my flymake minor mode.")

  (define-minor-mode my-flymake-minor-mode
    "Simple minor mode which adds some key bindings for moving to the next and previous errors.
Key bindings:
\\{my-flymake-minor-mode-map}"
    nil
    nil
    :keymap my-flymake-minor-mode-map)

  ;; Enable this keybinding (my-flymake-minor-mode) by default
  ;; Added by Hartmut 2011-07-05
  (add-hook 'c-mode-common-hook 'my-flymake-minor-mode)
  (add-hook 'c-mode-common-hook 'flymake-mode-on)
  (add-hook 'c-mode-common-hook 'my-flymake-google-init))

;;----------------------------------------------------------------------------
;; generate the template c++ header and source files
;;----------------------------------------------------------------------------

(defun c++-create-class (name)
  "Insert a C++ class definition.
 It creates a matching header file, inserts the class definition and
 creates the  most important function templates in a named after the
 class name. This might still be somewhat buggy."
  (interactive "sclass name: ")
  (let* ((header-file-name (concat name ".hpp"))
		 (header-include-string (upcase (concat name "_H_INCLUDED")))
		 (def-file-name    (concat name ".cpp")))

	;; write header file
	(set-buffer (get-buffer-create header-file-name))
	(set-visited-file-name header-file-name)
	(c++-mode)
	(turn-on-font-lock)
	(insert (concat
			"/* -*-C++-*- */\n"
                         "#ifndef " header-include-string "\n"
                         "#define " header-include-string "\n\n"
                         "////////////////////////////////////////////////////////////////////////////////\n"
                         "//\n"
                         "// File:         " header-file-name "\n"
                         "// Description:   \n"
                         "// Created:      " (substring (current-time-string) -4) "\n"
                         "// Author: " (user-full-name) "\n"
                         "//\n"
                         "// (c) Copyright 2008, Advantest, all rights reserved.\n"
                         "//\n"
                         "////////////////////////////////////////////////////////////////////////////////\n\n"
                         "#include <iostream>\n\n"
                         "class " name "_I;\n\n"
                         "class " name "\n"
                         "{\n"
                         "public:\n"
                         name "();\n"
                         "virtual ~" name "();\n"
                         name "(const " name "& src);\n"
                         name "& operator=(const " name "& rv);\n\n"
                         "protected:\n"
                         "void init();\n"
                         "void reset();\n\n"
                         "private: \n"
                         name "_I * mImp;\n"
                         "};\n\n"
                         "# endif\n"))
	(beginning-of-buffer)
	(while (and (not (eobp)) (forward-line))
	  (indent-according-to-mode))

	;; create CC file
	(set-buffer (get-buffer-create def-file-name))
	(set-visited-file-name def-file-name)
	(switch-to-buffer (current-buffer))
	(c++-mode)
	(turn-on-font-lock)
	(insert (concat
			 "/* -*-C++-*- */\n"
                         "////////////////////////////////////////////////////////////////////////////////\n"
                         "//\n"
                         "// File:         " def-file-name "\n"
                         "// Description:   \n"
                         "// Created:      " (substring (current-time-string) -4) "\n"
                         "// Author: " (user-full-name) "\n"
                         "//\n"
                         "// (c) Copyright 2008, Advantest, all rights reserved.\n"
                         "//\n"
                         "////////////////////////////////////////////////////////////////////////////////\n\n"
                         "#include \"" header-file-name "\"\n"
                         "#include \"" name "_I.hpp\"\n\n"
                         name "::" name "()\n{\nmImp = new " name "_I();\n}\n\n"
                         name "::~" name "()\n{\nif (NULL != mImp)\n{\ndelete mImp;\nmImp = NULL;\n}\n}\n\n"
                         name "::" name "(const " name "& src)\n{\n\n}\n\n"
                         "void " name "::init()\n{\n\n}\n\n"
                         "void " name "::reset()\n{\n\n}\n\n"
                         name "& " name "::operator=(const " name "& src)\n{\nreturn (*this);\n}\n"))
	(beginning-of-buffer)
	(while (and (not (eobp)) (forward-line))
	  (indent-according-to-mode))
	(beginning-of-buffer)
	(search-forward "Description:")))


;; @see https://stackoverflow.com/questions/7299893/getting-rid-of-buffer-has-running-process-confirmation-when-the-process-is-a-f
;; Stop asking me all the time when killing the buffer
(defadvice flymake-start-syntax-check-process (after
                                               cheeso-advice-flymake-start-syntax-check-1
                                               (cmd args dir)
                                               activate compile)
  ;; set flag to allow exit without query on any
  ;;active flymake processes
  (set-process-query-on-exit-flag ad-return-value nil))


(provide 'init-cc-mode)
