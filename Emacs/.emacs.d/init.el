;; learning emacs configuration

;; init load-path and start-time
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
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

;; create tags in specific directory
(defun create-tags (dir-name1 tag-file-name dir-name2)
  "Create tags file."
  (interactive
   "DDirectory in which tag-file will be created: \
    \nsName of the tag-file (TAGS): \
    \nDDirectory to be taged: ")
  (if (string= "" tag-file-name) (setq tag-file-name "TAGS"))
  (shell-command
   ;; use system ctags instead of emacs ctags
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

(if (file-exists-p
     ;; (concat (directory-of-library "package") "package.elc")
     "/usr/local/share/emacs/24.3/lisp/emacs-lisp/package.elc"
     )
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
      (package-initialize)
      )
  (progn
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140618.2217"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/auto-complete-20140618.2217/dict"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/buffer-move-20140522.58"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/connection-20131005.526"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/dictionary-20131005.526"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/dired+-20140710.2145"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/dired-details+-20131226.1832"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/dired-details-20130824.1158"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/flymake-cursor-20130822.1032"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/flymake-easy-20130907.131"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/flymake-google-cpplint-20140205.525"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/git-commit-mode-20140605.520"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/git-rebase-mode-20140605.520"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/google-c-style-20130412.1415"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/goto-chg-20131228.1459"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/helm-20140709.2306"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/link-20131005.526"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/magit-20140709.920"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/popup-20140207.1702"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/multiple-cursors-20140527.359"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/goto-chg-20131228.1459"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/tabulated-list-20120406.1351"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/undo-tree-20140509.522")))
  )

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
      (require 'init-lua))
  (progn
    (require 'ido)
    ))
(require 'init-basics)
(require 'init-sessions)
