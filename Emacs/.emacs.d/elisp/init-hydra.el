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
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←           _v_ertical      _b_uffer		_q_ X←
_j_ ↓           _x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑           _z_ undo        _a_ce 1		_e_ X↑
_l_ →           _Z_ reset       _s_wap		_r_ X→
_F_ollow		_D_lt Other     _S_ave		max_i_mize
_SPC_ cancel	_o_nly this     _d_elete
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("SPC" nil))
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
