;;----------------------------------------------------------------------------
;; ruby && rails settings
;;----------------------------------------------------------------------------

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; Prevent emacs from adding the encoding line at the top of the file
(setq ruby-insert-encoding-magic-comment nil)

;; Functions to help with refactoring
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; Easily toggle ruby's hash syntax
(require 'ruby-hash-syntax)

;; Ruby rdoc helpers mostly
(require 'ruby-additional)

;; Helpers to deal with strings and symbols
(require 'ruby-tools)

;; Turn on eldoc in ruby files to display info about the
;; method or variable at point
(add-hook 'ruby-mode-hook 'eldoc-mode)

;; Switch the compilation buffer mode with C-x C-q (useful
;; when interacting with a debugger)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

;; Start projectile-rails
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)

;; minor mode for rails
(require 'rinari)
(global-rinari-mode)

;; RVM support
(require 'rvm)
(rvm-use-default)

;; Cucumber
(require 'feature-mode)
(setq feature-use-rvm t) ;; Tell cucumber to use RVM
(setq feature-cucumber-command "cucumber {options} {feature}")
;; .feature files should open in feature-mode
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Rspec
(require 'rspec-mode)
;; I want rspec instead of rake spec
(setq rspec-use-rake-when-possible nil)
;; Scroll to the first test failure
(setq compilation-scroll-output 'first-error)

;; Projectile mode
;; (require 'projectile)
;; (projectile-global-mode)
;; (setq projectile-completion-system 'grizzl)

;; Support for YARD
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

(provide 'init-ruby)
