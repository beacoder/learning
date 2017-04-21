;;----------------------------------------------------------------------------
;; hydra setting
;;----------------------------------------------------------------------------

;; Major-mode binds to one specific file type
;; Minor-mode shared among all file types
;; Hydra groups related commands together to act like a temporary minor mode
(require-package 'hydra)

(defhydra multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))
(global-set-key (kbd "C-x m")  'multiple-cursors-hydra/body)

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
(global-set-key (kbd "C-x c") 'hydra/rectangle/body)

  
(provide 'init-hydra)
