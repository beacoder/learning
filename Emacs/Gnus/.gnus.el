;; My Gnus Configuration

;; some other e-mail address
;; chenhuming@gmail.com
;; Bright_Chen@huatek.com

;; set your name and email address 
(setq user-full-name "Bright.Chen"
      user-mail-address "Bright.Chen@ptn.advantest.com")

;; fetch news from newsgroup
;; (setq gnus-select-method '(nntp "news.gmane.org"))

;; set "nnml" method and POP server configuration 
;; so that we could retrieve emails from POP server
(setq gnus-select-method '(nnml "pop-mail"))

;; do not delete emails on server when using POP3
(setq pop3-leave-mail-on-server t)

;; receive mails from a POP server
(setq mail-sources
      '((pop :server "pop.ee-post.com"          ;; pop3 mail server
	     :user "bright_chen@huatek.com"     ;; user name
 	     :port "pop3"                       ;; port
 	     :password "5ihuatek")))            ;; password

;; add this to be able to list all labels in gmail
(setq gnus-ignored-newsgroups "")

;; to be able to search within your gmail/imap mail
;; (require 'nnir)

;; add this to configure gmail imap 
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                             (nnimap-address "imap.gmail.com")
                                             (nnimap-server-port 993)
                                             (nnimap-stream ssl)
				             ;; (nnir-search-engine imap)
				             (nnimap-authinfo-file "~/.authinfo")))

;; send mails using huatek's smtp server
;; (setq smtpmail-auth-credentials
;;     '(("smtp.ee-post.com"                      ;; smtp used to send mails
;;	      25                                ;; port
;;    	"bright_chen@huatek.com"                ;; user name
;;	      "5ihuatek"                        ;; password
;;	    ))
;;      smtpmail-default-smtp-server "smtp.ee-post.com"
;;      smtpmail-smtp-server "smtp.ee-post.com")
      
;; send mails using gmail's smtp server
(setq ;; message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "chenhuming@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; smtpmail-local-domain "yourcompany.com"
      )

;; function used to send mail
(setq send-mail-function 'smtpmail-send-it
;;      message-send-mail-function 'smtpmail-send-it
      )

;; show debug info where sending mails failed 
(setq smtpmail-debug-info t
      smtpmail-debug-verb t)

;; load smtpmail library
(require 'smtpmail)