;;----------------------------------------------------------------------------
;; init-async setting
;;----------------------------------------------------------------------------

;; provide dired with asynchronous abilities
(eval-after-load "dired-aux"
  '(require 'dired-async))

;; sending emails asynchronously
(require 'smtpmail-async)
(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)

(provide 'init-async)
