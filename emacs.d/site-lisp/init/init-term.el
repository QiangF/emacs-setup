; -*- coding:utf-8 -*-

;; * terminal
; two esc send esc, useful in vi editor
(setup term
   (:bind "M-p" #'term-send-up
          "M-n" #'term-send-down
          "C-\\" #'toggle-input-method
          "M-x" #'counsel-M-x
          "C-=" #'my-term-switch-line-char
          "C-c r" #'counsel-linux-app)
   (:bind-into term-raw-map
     ("M-p" #'term-send-up)
     ("M-n" #'term-send-down)
     ("M-x" #'counsel-M-x)
     ("C-\\" #'toggle-input-method)
     ("C-y" #'term-paste)
     ("<escape>" #'term-send-esc)
     ("C-m" #'term-send-C-m)
     ("C-c C-c" #'term-interrupt-subjob)
     ("C-=" #'my-term-switch-line-char)
     ("C-c r" #'counsel-linux-app))

   (:hook #'(lambda ()
              (progn (turn-off-my-mode)
                     (yas-minor-mode 0))))
   (:when-loaded
     (defun term-send-esc ()
       "Send ESC in term mode."
       (interactive)
       (term-send-raw-string "\e"))
     (defun term-send-C-m()
       (interactive)
       (term-send-raw-string "\C-m"))
     (defadvice term-handle-exit
         (after term-kill-buffer-on-exit activate)
       (kill-buffer))
     (setq explicit-shell-file-name "/bin/bash"
           term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;; C-c C-a to go to the end of prompt
     (defun my-term-switch-line-char ()
       "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
       (interactive)
       (cond
         ((term-in-line-mode)
          (term-char-mode)
          ;; (hl-line-mode -1)
          (when (bound-and-true-p evil-mode)
            (evil-emacs-state)))
         ((term-in-char-mode)
          (term-line-mode)(evil-exit-emacs-state))))))

(setup terminal-here
   (:autoload my-terminal-here-launch)

   ;; (setq terminal-here-terminal-command '("st" "-n" "scratch_term"))
   ;; todo try kitty
   ;; guake "--show" cause guake floating in i3wm
   ;; in guake, use drag to change tab position, will lauch in the first tab
   ;; (defun my-terminal-here-terminal-command (dir)
   ;;   ;; (list "guake" "-s" "0" "--rename-tab" "scratch" "-e"
   ;;   (list "guake" "-s" "0" "-e"
   ;;         (concat "cd " (shell-quote-argument (expand-file-name dir)))))
   (:when-loaded
     (defun my-terminal-here-terminal-command (dir)
       (list "tmux" "new-window"))

     (defun my-terminal-here-launch ()
       (interactive)
       (call-process-shell-command "jumpapp -R -f -m st")
       (terminal-here-launch))

     (setq terminal-here-terminal-command 'my-terminal-here-terminal-command)))

(provide 'init-term)
