;;----------------------------------------------------------------------------
;; auto-complete setting
;;
;; download auto-complete from http://cx4a.org/software/auto-complete/
;; install it using "emacs -batch -l etc/install.el", choose path "~/.emacs.d/"
;; add following scripts in .emacs
;;----------------------------------------------------------------------------

;; basic auto-complete configuration
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    
;; enable pos-tip
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)

;; show quick help of popup.el by real tooltip
(defadvice popup-menu-show-quick-help
  (around pos-tip-popup-menu-show-quick-help () activate)
  "Show quick help using `pos-tip-show'."
  (if (eq window-system 'x)
      (let ((doc (popup-menu-document
		  menu (or item
			   (popup-selected-item menu)))))
	(when (stringp doc)
	  (pos-tip-show doc nil
			(if (popup-hidden-p menu)
			    (or (plist-get args :point)
				(point))
			  (overlay-end (popup-line-overlay
					menu (+ (popup-offset menu)
						(popup-selected-line menu)))))
			nil 0)
	  nil))
    ad-do-it))

;; show tooltip
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)

;; TAB mode
(setq ac-dwim t)

;; auto complete using clang is CPU sensitive
(setq ac-auto-start nil)
(ac-set-trigger-key "<C-return>")

;; add function into ac-trigger-commands to trigger complete
(setq ac-trigger-commands
  (cons 'backward-delete-char-untabify ac-trigger-commands))
   
;; trigger complete even if your typing is wrong   
(setq ac-fuzzy-enable t)

(provide 'init-auto-complete)

