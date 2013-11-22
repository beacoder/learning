;; -*- coding: utf-8 -*-

;; install w3m and emacs-w3m first
;; then use emacs to view web pages

(require 'w3m-load)

(setq w3m-coding-system           'utf-8
      w3m-file-coding-system 	  'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system     'utf-8
      w3m-output-coding-system    'utf-8
      w3m-terminal-coding-system  'utf-8)
      
(setq w3m-use-cookies             t
      w3m-cookie-accept-bad-cookies t
      w3m-home-page               "http://www.google.com"
      w3m-use-toolbar             t
      ;; w3m-use-tab nil
      w3m-key-binding 		  'info
      w3m-command-arguments       '("-F" "-cookie")
      w3m-mailto-url-function     'compose-mail
      browse-url-browser-function 'w3m
      mm-text-html-renderer       'w3m)

;; show images in the browser
;; (setq w3m-default-display-inline-images t)

;; specify available search engines
(setq w3m-search-default-engine "g")
(eval-after-load "w3m-search" '(progn
				 ;; S g RET <search term> RET
                                 (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com/search?hl=en&q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("s" "http://stackoverflow.com/search?hl=en&q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("c" "http://code.ohloh.net/search?s=%s&browser=Default" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("b" "http://blogsearch.google.com/blogsearch?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("d" "http://dictionary.reference.com/search?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("wz" "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 ))

;; bind this function to A, which is the normal w3m bookmark binding:
(eval-after-load "w3m" '(progn
                          (define-key w3m-info-like-map "A" 'delicious-post)
                          ;; (w3m-lnum-mode 1)
                          ))

(defun w3m-search-google ()
  "search current symbol under cursor in google"
  (interactive)
  (require 'w3m)
  (let ((keyword (thing-at-point 'symbol)))
  	(w3m-search "g" keyword)
  	))
(global-set-key (kbd "C-c g") 'w3m-search-google)  	

;; external browser
(setq browse-url-generic-program
      (executable-find "firefox"))

;; use external browser to search programming stuff
(defun w3m-ext-hacker-search ()
  "search word under cursor in google code search and stackoverflow.com"
  (interactive)
  (require 'w3m)
  (let ((keyword (w3m-url-encode-string (thing-at-point 'symbol))))
  
    ;; google
    (browse-url-generic (concat "http://www.google.com/search?hl=en&q=%22" keyword "%22"
                                (if buffer-file-name 
                                (concat "+filetype%3A" (file-name-extension buffer-file-name)) 
                                "")
                                ))
    ;; stackoverflow.com				  
    (browse-url-generic (concat "http://stackoverflow.com/search?hl=en&q=" keyword))
    ;; koders.com
    (browse-url-generic (concat "http://code.ohloh.net/search?s=\"" keyword 
    				"\"&browser=Default&mp=1&ml=1&me=1&md=1&filterChecked=true" ))
    ))
(global-set-key (kbd "C-c e") 'w3m-ext-hacker-search)

;; open image or url with external browser
(defun w3m-ext-open-link-or-image-or-url ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (require 'w3m)
  (let (url)
    (if (or (string= major-mode "w3m-mode") (string= major-mode "gnus-article-mode"))
        (setq url (or (w3m-anchor) (w3m-image) w3m-current-url)))
    (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))
    ))
(global-set-key (kbd "C-c b") 'w3m-ext-open-link-or-image-or-url)

(provide 'init-emacs-w3m)
