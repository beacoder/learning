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

(defhydra hydra-window ()
   "
    ^Move^        ^Split^             ^Switch^          ^Other^
-------------------------------------------------------------
[_h_] ←       [_v_] vertical      [_b_] buffer      [_q_] quit
[_j_] ↓       [_x_] horizontal    [_f_] find files
[_k_] ↑       [_z_] undo          [_S_] save
[_l_] →       [_Z_] reset         [_d_] delete
[_F_] follow  [_o_] only this
"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("S" save-buffer)
   ("d" delete-window)
   ("o" delete-other-windows)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo)))
   ("Z" winner-redo)
   ("q" nil))
(global-set-key (kbd "C-x w") 'hydra-window/body)

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
