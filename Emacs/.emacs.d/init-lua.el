;;;; This snippet enables lua-mode

;; This line is not necessary, if lua-mode.el is already on your load-path
;;(add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
