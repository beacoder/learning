;; -*- coding: utf-8 -*-

;; learning emacs configuration

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;----------------------------------------------------------------------------
;; key-bindings for specific mode in emacs
;;----------------------------------------------------------------------------

;; the key definition only happen once        
;;(eval-after-load "coffee-mode"
;;    '(define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file)))
    
;; the key definition happens every time coffee-mode is enabled
;;(add-hook 'coffee-mode-hook
;;    (lambda ()
;;        (define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file))))

;;----------------------------------------------------------------------------
;; compilation setting
;;----------------------------------------------------------------------------

;; do not promp
(setq compilation-read-command nil)

;; set compile command
(setq compile-command "g++ -g HelloWorld.cpp TestMethod.cpp -o HelloWorld.exe")

;;----------------------------------------------------------------------------
;; using tags
;;----------------------------------------------------------------------------

(defun create-tags (dir-name)
  "Create tags file"
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s/TAGS -e -R %s" dir-name (directory-file-name dir-name)))
   (message "create-tags succeed !")
  )
(defalias 'ct 'create-tags)

;; set tags file lists
(setq tags-table-list
      '("/usr/include/c++"
        "~/my_include"
        )
      ) 

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-dired)
(require 'init-basics)
(require 'init-hippie-expand)
(require 'init-gnus)
(require 'init-cc-mode)
(require 'init-alias)
