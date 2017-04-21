;;----------------------------------------------------------------------------
;; hydra setting
;;----------------------------------------------------------------------------

;; Major-mode usually goes with one specific file type
;; Minor-mode usually shared among different file types
;; Temporary-mode usually used on the fly, e.g: rectangle-mode, My-Mc-mode
;; Hydra usually used to group a family of related commands together, e.g: work related commands
(require-package 'hydra)

(defhydra hydra/window-movement ()
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("y" other-window "other")
  ("h" switch-window "switch-window")
  ("f" find-file "file")
  ("F" find-file-other-window "other file")
  ("v" (progn (split-window-right) (windmove-right)))
  ("o" delete-other-windows :color blue)
  ("a" ace-window)
  ("s" ace-swap-window)
  ("d" delete-window "delete")
  ("D" ace-delete-window "ace delete")
  ("i" ace-maximize-window "maximize")
  ("b" helm-buffers-list)
  ("q" nil))
(global-set-key (kbd "C-x w") 'hydra/window-movement/body)


(global-set-key (kbd "C-x c") 'hydra/rectangle/body)
(defhydra hydra/rectangle (:pre (rectangle-mark-mode 1)
                                :color pink
                                :hint nil)
  "
 _p_: paste   _r_: replace  _I_: insert
 _y_: copy    _o_: open     _R_: reset
 _d_: kill    _n_: number   _q_: quit
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle nil)
  ("x" clear-rectangle nil)
  ("o" open-rectangle nil)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("n" rectangle-number-lines)
  ("I" string-insert-rectangle)
  ("R" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("q" keyboard-quit :color blue))


(defhydra join-lines ()
  ("<up>" join-line)
  ("<down>" (join-line 1))
  ("t" join-line)
  ("n" (join-line 1)))
  
(provide 'init-hydra)
