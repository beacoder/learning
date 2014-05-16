;; -*- coding: utf-8 -*-

;; learning emacs configuration

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/google"))
(when (<= emacs-major-version 21)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-21")))

;;----------------------------------------------------------------------------
;; compilation setting
;;----------------------------------------------------------------------------

;; do not promp
;; (setq compilation-read-command nil)

;; set compile command
;; (setq compile-command "make clobber")
(setq compile-command "make debug")

;;----------------------------------------------------------------------------
;; using tags
;;----------------------------------------------------------------------------

;; create tags in specific directory for specific directory
(defun create-tags (dir-name1 tag-file-name dir-name2)
  "Create tags file"
  (interactive 
   "DDirectory in which tag-file will be created: \
    \nsName of the tag-file (TAGS): \
    \nDDirectory to be taged: ")
  (if (string= "" tag-file-name) (setq tag-file-name "TAGS))
  (shell-command
   ;; use Exuberant Ctags instead of Emacs Ctags
   (format "/usr/bin/ctags -f %s/%s -e -R %s" dir-name1 tag-file-name (directory-file-name dir-name2)))
   (message "create-tags succeed !")
  )
(defalias 'ct 'create-tags)

;; set tags file lists
(setq tags-table-list
      '("~/my_tag_files"
        "~/my_include"))

;;----------------------------------------------------------------------------
;; package setting
;;----------------------------------------------------------------------------

;; package-list-packages -> list packages
;; i -> mark for installation
;; U -> mark for upgrades
;; x -> execute installation or upgrades
(if (display-graphic-p)
    (progn 
      (require 'package)
      
      ;; Standard package repositories
      (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

      ;; We include the org repository for completeness, but don't normally use it.
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

      (when (< emacs-major-version 24)
        (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

      ;; Also use Melpa for most packages
      (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
      
      ;; fire up package.el
      (package-initialize)))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-com-functions)
(require 'init-dictionary)
(require 'init-routines)
(require 'init-register)
(require 'init-macros)
(require 'init-dired)
(require 'dired+)
(require 'init-hippie-expand)
(require 'init-gnus)
(require 'init-cc-mode)
(require 'tempo-c-cpp)
(require 'init-alias)
(if (> emacs-major-version 21)
    (progn
      (require 'init-3rd-party)
      (require 'init-lua))
  (progn
    (require 'ido)
    ))
(require 'buffer-move)
(require 'init-basics)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
