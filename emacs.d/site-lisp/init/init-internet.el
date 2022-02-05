; -*- coding:utf-8 -*-

;; https://github.com/twlz0ne/with-proxy.el
(setenv "http_proxy" "http://127.0.0.1:7890")
(setenv "https_proxy" "http://127.0.0.1:7890")
(setenv "ALL_PROXY" "http://127.0.0.1:7890")
(setenv "all_proxy" "http://127.0.0.1:7890")

(setup (:pkg use-proxy)
   (:delay)
   ;; same as eaf-proxy-port
   (:option use-proxy-http-proxy "127.0.0.1:7890"
            use-proxy-https-proxy "127.0.0.1:7890"
            use-proxy-no-proxy "^\\(localhost\\|127\\.0\\.0\\.1\\|10\\..*\\|192\\.168\\..*\\)")
   (:when-loaded
     (defvar cow-proxy "127.0.0.1:7777")
     (defun proxy-http-show ()
       "Show http/https proxy."
       (interactive)
       (if url-proxy-services
           (message "Current HTTP proxy is \"%s\"" cow-proxy)
           (message "No proxy")))

     (defun proxy-http-enable ()
       "Enable http/https proxy."
       (interactive)
       (setq url-proxy-services `(("http" . ,cow-proxy)
                                  ("https" . ,cow-proxy)
                                  ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
       (proxy-http-show))

     (defun proxy-http-disable ()
       "Disable http/https proxy."
       (interactive)
       (setq url-proxy-services nil)
       (proxy-http-show))

     (defun proxy-http-toggle ()
       "Toggle http/https proxy."
       (interactive)
       (if url-proxy-services
           (proxy-http-disable)
           (proxy-http-enable)))

     (defvar socks-noproxy)
     (defvar socks-server)
     (defun proxy-socks-enable ()
       "Enable Socks proxy."
       (interactive)
       (setq url-gateway-method 'socks)
       (setq socks-noproxy '("localhost"))
       (setq socks-server '("Default server" "127.0.0.1" 9666 5))
       (message "Enable socks proxy."))

     (defun proxy-socks-disable ()
       "Disable Socks proxy."
       (interactive)
       (setq url-gateway-method 'native)
       (setq socks-noproxy nil)
       (message "Disable socks proxy."))

     (use-proxy-mode)
     (run-with-idle-timer 1 nil 'use-proxy-toggle-all-proxies)))

;; curl-for-url
;;   (curl-for-url-install)

; * elfeed 
;; elfeed-org
;;     (elfeed-org)
;;     (setq rmh-elfeed-org-files (list "~/.dotfiles/emacs/elfeed.org"))

(setup (:pkg elfeed)
  (:autoload elfeed)
  (:option elfeed-enclosure-default-dir "~/Downloads/"
           elfeed-show-enclosure-filename-function
           (lambda (entry url-enclosure)
             (let*
                 ((fname (file-name-nondirectory
                          (url-unhex-string
                           (car (url-path-and-query (url-generic-parse-url
                                                     url-enclosure))))))
                  (feed-title (elfeed-feed-title (elfeed-entry-feed entry))))
               (mapconcat 'directory-file-name (list (my-slugify feed-title) fname) "_"))))
  (:with-map elfeed-search-mode-map
    (:bind "d" #'my-download-podcast))
  (:when-loaded
    (setq elfeed-feeds '("http://www.bbc.co.uk/programmes/p002vsxs/episodes/downloads.rss"
                         "http://www.bbc.co.uk/programmes/p016tl04/episodes/downloads.rss"
                         "http://www.bbc.co.uk/programmes/p02nq0gn/episodes/downloads.rss"
                         "http://www.bbc.co.uk/programmes/p02tb8vq/episodes/downloads.rss"
                         ("http://www.bbc.co.uk/programmes/p02nq0lx/episodes/downloads.rss" comic)))

    (defun elfeed--download-enclosure (url path)
      "Download asynchronously the enclosure from URL to PATH."
      (if (require 'async nil :noerror)
          (with-no-warnings
            (async-start
             (lambda ()
               (url-copy-file url path t))
             (lambda (_)
               (message (format "%s downloaded" url)))))
          (url-copy-file url path t)))

    (defun my-trim-string (string)
      (replace-regexp-in-string
       "\\`[ \t\n]*" ""
       (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

    (defun my-slugify (str)
      (downcase
       (replace-regexp-in-string
        "[[:space:]-]+" "-"
        (my-trim-string
         (replace-regexp-in-string
          "[^[:word:][:space:]]+" "" str)))))

    (defun my-download-podcast ()
      (interactive)
      (elfeed-show-save-enclosure-single (elfeed-search-selected :ignore-region) 1))))

(provide 'init-internet)
