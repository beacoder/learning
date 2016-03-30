;;----------------------------------------------------------------------------
;; Entry file for emacs configuration 
;;----------------------------------------------------------------------------

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
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
   "DDirectory where tag-file will be saved: \
    \nsName of the tag-file (TAGS): \
    \nDDirectory to be taged: ")
  (if (string= "" tag-file-name) (setq tag-file-name "TAGS"))
  (shell-command
   ;; use Exuberant Ctags instead of emacs ctags
   (format "/usr/bin/ctags -f %s/%s -e -R %s" dir-name1 tag-file-name (directory-file-name dir-name2)))
  (message "create-tags succeed !"))
(defalias 'ct 'create-tags)

;; set tags file lists
(setq tags-file-name nil
      tags-table-list
      '(
        "~/my_tag_files/TAGS"
        "~/my_tag_files/STL_TAGS"     ;; stl(gcc) headers
        "~/my_tag_files/BOOST_TAGS"   ;; boost headers
        )
      ;; add stl/boost/project path into ff-find-other-file's search dir
      cc-search-directories '("." "/usr/include" "/usr/local/include/*" "/usr/local/include/*"
                              "/usr/include/c++/4.4.7/*"
                              "  /usr/include/boost/*")
      ;; Don't ask before reverting the TAGS files
      tags-revert-without-query t
      ;; Do case-sensitive tag searches
      tags-case-fold-search nil)

(if *is-windows*
    ;; Don't warn unless TAGS files are bigger than 1GB
    (setq large-file-warning-threshold (expt 1024 3))
  ;; on win-32, set threshhold to 511MB
  (setq large-file-warning-threshold (* 511 (expt 1024 2))))

;;----------------------------------------------------------------------------
;; package setting
;;----------------------------------------------------------------------------

(when (is-modern-emacs)
  (require 'package)

  ;; Standard package repositories
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

  ;; We include the org repository for completeness, but don't normally use it.
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

  ;; Also use Melpa for most packages
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

  ;; fire up package.el
  (package-initialize))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------

(when (is-modern-emacs)
  (require 'server)
  (unless (server-running-p)
    (server-start)))
  
;;----------------------------------------------------------------------------
;; Kill all processes when closing emacs
;;----------------------------------------------------------------------------

(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

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
;; could use helm instead.
(require 'init-ido)
(when (= emacs-major-version 21)
  (require 'missing)
  (require 'syntax))
(when (is-modern-emacs)
  (require 'init-3rd-party)
  (require 'init-lua)
  (ido-everywhere t)
  (require 'init-windows)
  (require 'init-nxml))
(require 'init-basics)
(require 'init-dired)
(require 'init-sessions)
(require 'init-cperl-mode)
(require 'init-productivity)
(require 'init-shell)
(require 'init-work)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
