;;----------------------------------------------------------------------------
;; auto-complete setting
;;----------------------------------------------------------------------------

;; start auto-complete with emacs 
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

(global-auto-complete-mode t)  
    
;; enable pos-tip
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
;; (setq ac-fuzzy-enable t)

(provide 'init-auto-complete)

