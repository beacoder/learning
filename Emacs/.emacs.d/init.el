;;----------------------------------------------------------------------------
;; Entry file for emacs configuration 
;;----------------------------------------------------------------------------

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(when (<= emacs-major-version 21)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-21")))
(require 'init-utils)

;;----------------------------------------------------------------------------
;; key-bindings for specific mode in emacs
;;----------------------------------------------------------------------------

;; the key definition only happen once
;; (eval-after-load "coffee-mode"
;;     '(define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file)))
;;
;; ;; the key definition happens every time coffee-mode is enabled
;; (add-hook 'coffee-mode-hook
;;     (lambda ()
;;         (define-key coffee-mode-map (kbd "C-c c" 'coffee-compile-file))))

;;----------------------------------------------------------------------------
;; compilation setting
;;----------------------------------------------------------------------------

;; do not promp
;; (setq compilation-read-command nil)

;; set compile command
;; (setq compile-command "make clobber")
(setq compile-command "make debug")

;;----------------------------------------------------------------------------
;; create tags
;;----------------------------------------------------------------------------

;; create tag files in specific directory
;; requires 'Exuberant Ctags' installed
(defun create-tags (dir-name1 tag-file-name dir-name2)
  "Create tags file."
  (interactive
   "DDirectory in which tag-file will be created: \
    \nsName of the tag-file (TAGS): \
    \nDDirectory to be taged: ")
  (if (string= "" tag-file-name) (setq tag-file-name "TAGS"))
  (shell-command
   ;; use Exuberant Ctags instead of emacs ctags
   (format "/usr/bin/ctags -f %s/%s -e -R %s" dir-name1 tag-file-name (directory-file-name dir-name2)))
  (message "create-tags succeed !")
  )
(defalias 'ct 'create-tags)

;; set tags file lists
(setq tags-table-list
      '(
        "/opt/hsm/src"                                      ;; MTP sources files
        "~/my_tag_files"                                    ;; boost and stl and linux headers
        ;;	"/opt/93000/src/segments/formatter/EventFormatter"  ;; eventFormatter sources files
        ;;	"/opt/93000/src/com/include"                        ;; common include files
        )
      )

;;----------------------------------------------------------------------------
;; package setting
;;----------------------------------------------------------------------------

(require 'package)

;; Standard package repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; We include the org repository for completeness, but don't normally use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; fire up package.el
(package-initialize)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-routines)
(require 'init-register)
(require 'init-macros)
(require 'init-hippie-expand)
(require 'init-cc-mode)
(require 'tempo-c-cpp)
(require 'init-alias)
(require 'init-modeline)
(if (> emacs-major-version 21)
    (progn
      (require 'init-3rd-party)
      (require 'init-lua)
      
      ;; use helm instead.
      ;; (require 'init-ido)
      )
  (progn
    (require 'ido)
    ))
(require 'init-windows)
(require 'init-nxml)
(require 'init-basics)
(require 'init-sessions)
(require 'init-cperl-mode)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
