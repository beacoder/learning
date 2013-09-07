;; my gnus configuration

;; enable fetch news function
;; (setq gnus-select-method '(nntp "news.gmane.org"))

;; enable fetch email function
;; (setq gnus-secondary-select-methods '((nnml "")))

;; we use gnus only to send and receive mails, 
;; so we don't use "gnus-secondary-select-methods"
(setq gnus-select-method '(nnml ""))

;; some other e-mail address
;; chenhuming@gmail.com
;; Bright_Chen@huatek.com
;; Bright.Chen@ptn.advantest.com

;; name and email address on your emails 
(setq user-full-name "Bright.Chen"
      user-mail-address "Bright.Chen@ptn.advantest.com")

;; receive mails from a pop server
(setq mail-sources
      '((pop :server "pop.ee-post.com"          ;; pop3 mail server
	     :user "bright_chen@huatek.com"     ;; user name
	     :port "pop3"
	     :password "5ihuatek")))            ;; password

;; send mails with a smtp server
(setq smtpmail-auth-credentials
     '(("smtp.ee-post.com"                      ;; smtp used to send mails
	      25                                ;; port
      	"bright_chen@huatek.com"                ;; user name
	      "5ihuatek"                        ;; password
	    )))

;; controls the default hostname of the server to use 
(setq smtpmail-default-smtp-server "smtp.ee-post.com"
      smtpmail-smtp-server "smtp.ee-post.com")

;; function used to send mail
(setq send-mail-function 'smtpmail-send-it
      ;; message-send-mail-function 'smtpmail-send-it
      )

;; show debug info where sending mails failed 
(setq smtpmail-debug-info t
      smtpmail-debug-verb t)

;; load smtpmail library
(require 'smtpmail)
