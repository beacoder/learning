;;----------------------------------------------------------------------------
;; ruby && rails settings
;;----------------------------------------------------------------------------

(require 'ruby-mode)
(require 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

;; TODO: hippie-expand ignoring : for names in ruby-mode
;; TODO: hippie-expand adaptor for auto-complete sources

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))


;;; Inferior ruby
(require-package 'inf-ruby)
(require-package 'ac-inf-ruby)
(after-load 'auto-complete
  (add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(after-load 'inf-ruby
  (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))


;;; Ruby compilation
(require-package 'ruby-compilation)

(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m [S-f7] 'ruby-compilation-this-buffer)
    (define-key m [f7] 'ruby-compilation-this-test)))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))

(defun sanityinc/maybe-enable-robe-ac ()
  "Enable/disable robe auto-complete source as necessary."
  (if robe-mode
      (progn
        (add-hook 'ac-sources 'ac-source-robe nil t)
        (set-auto-complete-as-completion-at-point-function))
    (remove-hook 'ac-sources 'ac-source-robe)))

(after-load 'robe
  (add-hook 'robe-mode-hook 'sanityinc/maybe-enable-robe-ac))



;; Customise highlight-symbol to not highlight do/end/class/def etc.
(defun sanityinc/suppress-ruby-mode-keyword-highlights ()
  "Suppress highlight-symbol for do/end etc."
  (set (make-local-variable 'highlight-symbol-ignore-list)
       (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))
(add-hook 'ruby-mode-hook 'sanityinc/suppress-ruby-mode-keyword-highlights)



;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)



;;; YAML

(maybe-require-package 'yaml-mode)
(add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")



;;; ERB
(require-package 'mmm-mode)
(defun sanityinc/ensure-mmm-erb-loaded ()
  (require 'mmm-erb))

(require 'derived)

(defun sanityinc/set-up-mode-for-erb (mode)
  (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
  (dolist (mode html-erb-modes)
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

(mapc 'sanityinc/set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------

;; Needs to run after rinari to avoid clobbering font-lock-keywords?

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;(add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)











;; Prevent emacs from adding the encoding line at the top of the file
;; (setq ruby-insert-encoding-magic-comment nil)

;; Functions to help with refactoring
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

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

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2)

(provide 'init-ruby)
