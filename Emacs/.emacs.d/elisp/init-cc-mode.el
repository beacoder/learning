;; c/c++ programming mode setting

;; Customizations for all modes in CC Mode.

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

;; use google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; some personal settings
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'tab-indents-region)
(add-hook 'c-mode-common-hook 'my-flymake-google-init)

(defun my-flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   ;; use cpplint.py to ensure that C++ code conforms to Google's coding style guides
   ;; chmod 775 "~/.emacs.d/cpplint.py" to make cpplint.py executable
   '(flymake-google-cpplint-command "~/.emacs.d/cpplint.py"))
  (flymake-google-cpplint-load))

(defun my-c-mode-common-hook ()
  ;; other customizations
  (setq tab-width 8)
  ;; show function name in mode-line
  (which-function-mode t)
  ;; enable flyspell for comments in source code
  (flyspell-prog-mode)
  ;; improve performance
  (setq flyspell-issue-message-flag nil)
  ;; turn flymake mode on
  (flymake-mode-on)

  ;; navigation between header and cpp/cc files
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  ;; jump to the start of the function
  (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)
  ;; jump to the end of the function
  (local-set-key (kbd "C-M-e") 'c-end-of-defun)

  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1))

;; use <tab> to indent region if anything is selected
(defun tab-indents-region () (local-set-key [(tab)] 'fledermaus-maybe-tab))

;; fledermaus came up with this
(defun fledermaus-maybe-tab ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (indent-region (region-beginning) (region-end) nil)
    (c-indent-command)))

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
			 "/* -*-C++-*-\n"
			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
			 ";\n"
			 "; File:         " header-file-name "\n"
			 "; Description:   \n"
			 "; Created:      " (substring (current-time-string) -4) "\n"
			 "; Author: " (user-full-name) "\n"
			 ";\n"
			 "; (c) Copyright 2008, Advantest, all rights reserved.\n"
			 ";\n"
			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
			 "*/\n\n"
			 "#ifndef " header-include-string "\n"
			 "#define " header-include-string "\n\n"
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
			 "/* -*-C++-*-\n"
			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
			 ";\n"
			 "; File:         " def-file-name "\n"
			 "; Description:   \n"
			 "; Created:      " (substring (current-time-string) -4) "\n"
			 "; Author: " (user-full-name) "\n"
			 ";\n"
			 "; (c) Copyright 2008, Advantest, all rights reserved.\n"
			 ";\n"
			 ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
			 "*/\n\n"
			 "#include \"" header-file-name "\"\n"
			 "#include \"" name "_I.hpp\"\n\n"
			 name "::" name "()\n{\nmImp = new " name "_I();\n}\n\n"
			 name "::~" name "()\n{\nif (NULL != mImp)\n{\ndelete mImp;\nmImp = NULL;\n}\n}\n\n"
			 name "::" name "(const " name "& src)\n{\n\n}\n\n"
			 "void " name "::init()\n{\n\n}\n\n"
			 "void " name "::reset()\n{\n\n}\n\n"
			 name "& " name "::operator=(const " name "& src)\n{\nreturn (*this);\n}\n"
			 ))
	(beginning-of-buffer)
	(while (and (not (eobp)) (forward-line))
	  (indent-according-to-mode))
	(beginning-of-buffer)
	(search-forward "Description:")
	)
)

(provide 'init-cc-mode)