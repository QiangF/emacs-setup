; -*- coding:utf-8 -*-

(setup (:pkg shackle) ;; breaks winner-boring-buffers?
   (:delay)
   (:when-loaded
     ;; help-mode
     ;; (completion-list-mode :noselect t)
     ;; (compilation-mode :noselect t)
     ;; (grep-mode :noselect t)
     ;; (occur-mode :noselect t)
     ;; ("*Pp Macroexpand Output*" :noselect t)
     ;; "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
     ;; (" *undo-tree*" :width 60 :position right)
     ;; ("^\\*anything.*\\*$" :regexp t)
     ;; "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
     ;; ("*slime-compilation*" :noselect t)
     ;; "*slime-xref*"
     ;; (sldb-mode :stick t)
     ;; slime-repl-mode slime-connection-list-mode)
     ;; ("\\*Tex Help\\*" :regexp t :noselect t)
     (setq shackle-rules
           '((compilation-mode :select t)
             (TeX-errors-mode :select t)
             ("\\`\\*image-dired.*?\\*\\'" :regexp t :select nil) ; any buffer name begins with *image-dired
             ("\\`\\*Warnings.*?\\*\\'" :regexp t :select nil)
             (image-dired-thumbnail-mode :select nil) ; image-dired
             (image-dired-image-display-mode :select nil) ; image-dired
             ;; (append special-buffer-regexp (list :regexp t :select t))
             (help-mode :other t :select t)
             ("*exwmx-sendstring*" :select t :align t :size 0.15)
             (" *LV*" :select nil :size 0.2)
             ;; ("\\`\\*exwmx-sendstring\\*\\'" :regexp t :select t :size 0.2)
             (special-mode :other t :select t))
           shackle-default-rule '(:select t))
     (shackle-mode))
   ;; do I need to unquote it
   ;; (,special-buffer-regexp :regexp t :select t)
   ;; (push '("*Help*" :regexp t :noselect nil) popwin:special-display-config)
   ;; (push '(special-buffer-regexp :regexp t :noselect nil)
   ;;      popwin:special-display-config)
   )

(setup (:pkg popper)
   (:global "C-`" #'popper-toggle-latest
            "M-`" #'popper-cycle
            "C-M-`" #'popper-toggle-type)
   (:delay)
   (:when-loaded
     (setq popper-reference-buffers
           '("\\*Messages\\*"
             "\\*Warnings\\*"
             "\\*Backtrace\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             "^\\*eshell.*\\*$" eshell-mode
             "^\\*term.*\\*$" term-mode
             help-mode
             compilation-mode))
     ;; all popups are shown in the same location: At the bottom of the frame.
     ;; to use shackle rule:
     (setq popper-display-control nil)
     (popper-mode 1)
     (popper-echo-mode 1)))

(provide 'init-shackle)
