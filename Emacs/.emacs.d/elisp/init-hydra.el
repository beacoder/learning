;;----------------------------------------------------------------------------
;; hydra setting
;;----------------------------------------------------------------------------

;; Major-mode binds to one specific file type
;; Minor-mode shared among all file types
;; Hydra groups related commands together to act like a temporary minor mode
(require-package 'hydra)

;; Don't treat 0-9 as digit-argument.
(after-load 'hydra
  (setq hydra-base-map (make-sparse-keymap)))

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_q_] Quit
[_P_]   Skip    [_N_]   Skip
[_M-p_] Unmark  [_M-n_] Unmark
^ ^             ^ ^
"
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))
(global-set-key (kbd "C-x m")  'hydra-multiple-cursors/body)

(defhydra hydra-window (:hint nil)
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

(defhydra hydra-quickness (:hint nil)
  "
                 ^Commands^
-----------------------------------------
[_s_] Swiper-at-point [_a_] Counsel-ag-project
[_l_] Counsel-locate  [_f_] Flymake-mode
[_u_] Update-GTAGS    [_c_] Mode-Compile
[_C_] Compile         [_r_] Recompile
[_q_] Quit
"
  ("s" sanityinc/swiper-at-point :exit t)
  ("a" sanityinc/counsel-ag-project :exit t)
  ("l" counsel-locate :exit t)
  ("f" flymake-mode :exit t)
  ("u" ggtags-update-tags :exit t)
  ("c" mode-compile :exit t)
  ("C" compile :exit t)
  ("r" recompile :exit t)
  ("q" nil))
(global-set-key (kbd "C-x q")  'hydra-quickness/body)


(provide 'init-hydra)
