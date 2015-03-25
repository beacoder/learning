;;----------------------------------------------------------------------------
;; config modeline format
;;----------------------------------------------------------------------------

;; config time format
(setq display-time-format "[%A %Y/%m/%d %H:%M Time-Zone:'%Z' %jth of %Y]"
      display-time-interval 60
      display-time-default-load-average nil
      display-time-mail-face 'custom-themed)

;; display time
(display-time-mode t)

(provide 'init-modeline)
