;; -*- coding:utf-8 -*-

(setup (:pkg comment-dwim-2)
   (:global "M-;" #'comment-dwim-2))

(setup my-mode
   ;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
   ;; (defadvice load (after give-my-keybindings-priority)
   ;; "Try to ensure that my keybindings always have priority."
   ;; (if (not (eq (car (car minor-mode-map-alist)) 'my-mode))
   ;;     (let ((mykeys (assq 'my-mode minor-mode-map-alist)))
   ;;         (assq-delete-all 'my-mode minor-mode-map-alist)
   ;;         (add-to-list 'minor-mode-map-alist mykeys))))
   ;; (ad-activate 'load)

   (:hook-into prog-mode text-mode)
   (:global "C-6" #'switch-to-scratch-and-back
            "C-x C-e" #'my-eval
            "C-c h" #'mark-whole-buffer
            "<home>" #'my-home
            "M-q" #'my-toggle-fill-paragraph
            "S-<return>" #'sanityinc/newline-at-end-of-line
            "s-j" #'sow-scroll-other-window-down
            "s-k" #'sow-scroll-other-window
            "M-g" #'hydra-avy/body
            "C-h" #'hydra-help/body
            "S-SPC" #'hydra-pause-resume)
   (:bind
    "<tab>" #'my-tab
    "<backtab>" #'my-backtab)

   ;; my mode key binding, the first one behind major mode keybinding

   (require 'my-mode)
   (setq my-double-key-timeout 0.25)
   ;; (mapc (lambda (mode-hook) (add-hook mode-hook 'turn-off-my-mode))
   ;;       '(minibuffer-setup-hook inferior-python-mode-hook))
   (which-function-mode 1)
   (show-paren-mode 1)
   (column-number-mode 1)
   (electric-pair-mode 1))

;; ("S-<backspace>" . evil-shift-left-line)
;; ("M-;" . comment-dwim-2)
;; ("C-<tab>" . hydra-evil/body)
;; ("M-g" . hydra-error/body)

(provide 'init-my-mode)
