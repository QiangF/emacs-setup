; -*- coding:utf-8 -*-

;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-eaf.el

;; eaf dependencies
(setup (:pkg epc))
(setup (:pkg ctable))
(setup (:pkg deferred))
(setup (:pkg s))

(setup eaf
   (:with-map eaf-mode-map*
     (:bind "C-0" nil))
   (:with-map eaf-edit-mode
     (:bind "<C-return>" #'eaf-edit-buffer-confirm))
   (:autoload eaf-setq eaf-bind-key)
   (:when-loaded
     ;; use proxy for aria2 downloads
     (eaf-setq eaf-browser-aria2-proxy-host "127.0.0.1")
     (eaf-setq eaf-browser-aria2-proxy-port "6800")
     (defalias 'browse-web #'eaf-open-browser)
     (setq browse-url-browser-function 'eaf-open-browser)

     (eaf-setq eaf-browser-default-zoom (cond ((> (frame-pixel-width) 3000) 2.3)
                                              ((> (frame-pixel-width) 1500) 1.3)
                                              (t 1.2)))
     (eaf-setq eaf-browser-enable-adblocker t)
     (eaf-setq eaf-browser-enable-autofill t)
     (eaf-setq eaf-marker-letters "JKHLNMUIOYPFDSAVCRREW"))
  
   (defun eaf-install-dependencies ()
     (interactive)
     (let* ((eaf-dir (file-name-directory (locate-library "eaf")))
            (default-directory eaf-dir))
       (shell-command (concat "./install-eaf.sh --ignore-npm-deps" "&"))))

   ;; Improve EAF new page creation speed.
   (setq eaf-kill-process-after-last-buffer-closed nil))

(setup eaf-browser
  (:autoload eaf-open-browser)
  (:option eaf-browser-continue-where-left-off t
           eaf-browser-enable-adblocker t
           browse-url-browser-function 'eaf-open-browser)
  (:when-loaded
    (eaf-bind-key nil "C-0" eaf-browser-keybinding)
    (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki
    )
  (setq eaf-browser-dark-mode "force")
  ;; (defun eaf-get-theme-background-color () "#00FF00")
  ;; (defun eaf-get-theme-foreground-color () "#FF0000")

  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7890")
  (setq eaf-browser-continue-where-left-off t)
  (setq eaf-browser-remember-history nil))

(setup (:pkg google-this)
  (:delay 10)
  (:when-loaded
    (google-this-mode 1)))

(provide 'init-eaf)
