;; c/c++ programming mode setting

;; Here's a sample .emacs file that might help you along the way.
;; Just copy this region and paste it into your .emacs file.  You may
;; want to change some of the actual values.

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")

;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; show function name in mode-line
(add-hook 'c-mode-common-hook (lambda() (which-function-mode t)))

;;----------------------------------------------------------------------------
;; c++ mode specific key-bindings
;;----------------------------------------------------------------------------

;; navigation between header and cpp/cc files
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
    
;; jump to the start of the function    
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)))

;; jump to the end of the function    
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-M-e") 'c-end-of-defun)))
    
;; use <f5> to compile
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "<f5>") 'compile)))

;;----------------------------------------------------------------------------
;; generate the template c++ header and source files
;;----------------------------------------------------------------------------

(defun ska-skel-cc-class (name)
  "Insert a C++ class definition.
 It creates a matching header file, inserts the class definition and
 creates the  most important function templates in a named after the
 class name. This might still be somewhat buggy."
  (interactive "sclass name: ")
  (let* ((header-file-name (concat (downcase name) ".hh"))
		 (header-include-string (upcase (concat name "_HH_INCLUDED")))
		 (def-file-name    (concat (downcase name) ".cc")))

	;; write header file
	(set-buffer (get-buffer-create header-file-name))
	(set-visited-file-name header-file-name)
	(c++-mode)
	(turn-on-font-lock)
	(insert (concat
			 "// -*- C++ -*-\n"
			 "// File: " header-file-name "\n//\n"
			 "// Time-stamp: <>\n"
			 "// $Id: $\n//\n"
			 "// Copyright (C) "(substring (current-time-string) -4)
			 " by " "Brightc" "\n//\n"
			 "// Author: " (user-full-name) "\n//\n"
			 "// Description: \n// "
			 ;; get this point...
			 "\n\n" 
			 "# ifndef " header-include-string "\n"
			 "# define " header-include-string "\n\n"
			 "# include <iostream>\n\n"
			 "class " name ";\n\n"
			 "class " name " {\n"
			 "public:\n"
			 name "();" "\n"
			 name "(const " name "& src);\n"
			 "~" name "();" "\n"
			 name "& operator=(const " name "& rv);\n"
			 "\nprivate:\n"
			 "void init();\n"
			 "void reset();\n"
			 "void init_and_copy(const " name "& src);\n\n"
			 "protected: \n\n"
			 "};"
			 "\n\n# endif"))
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
			 "// -*- C++ -*-\n"
			 "// File: " def-file-name "\n//\n"
			 "// Time-stamp: <>\n"
			 "// $Id: $\n//\n"
			 "// Copyright (C) "(substring (current-time-string) -4)
			 " by " "Brightc" "\n//\n"
			 "// Author: " (user-full-name) "\n//\n"
			 "// Description: \n// "
			 "\n# include \"" header-file-name "\"\n\n"
			 name "::\n" name "() {\ninit();\n}\n\n"
			 name "::\n" name "(const " name "& src) {\ninit_and_copy(src);\n}\n\n"
			 name "::\n~" name "() {\nreset();\n}\n\n"
			 "void\n" name "::\ninit() {\n\n}\n\n"
			 "void\n" name "::\nreset() {\n\n}\n\n"
			 "void\n" name "::\ninit_and_copy(const " name "& src) {\n\n}\n\n"
			 name "&\n" name "::\noperator=(const " name "& src) {\n\n}\n\n"
			 ))
	(beginning-of-buffer)
	(while (and (not (eobp)) (forward-line))
	  (indent-according-to-mode))
	(beginning-of-buffer)
	(search-forward "Description:")
	)
)

(provide 'init-cc-mode)
