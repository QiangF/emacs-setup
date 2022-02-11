; -*- coding:utf-8 -*-
;; make using vanila emacs easier
;; https://github.com/magnars/multiple-cursors.el/
;; https://github.com/clemera/objed
;; https://github.com/casouri/isolate
;; thingatp library
;; https://github.com/mohkale/spaceleader

;; specify the whole key binding in god-local-mode-map
;; (define-key god-local-mode-map (kbd "C-/") 'isearch-forward)
(setup god-mode
   ;; :bind* (("<escape>" . god-local-mode))
   (:with-map god-local-mode-map
     (:bind "." #'repeat
            "i" #'god-local-mode))
   (:when-loaded
     (with-eval-after-load 'evil
       (define-key evil-emacs-state-map (kbd "<escape>") 'god-local-mode))

     ;; This mortal mode is designed to allow temporary departures from god mode
     ;; The idea is that within god-mode, you can hit shift-i, type in a few characters
     ;; and then hit enter to return to god-mode. To avoid clobbering the previous bindings,
     ;; we wrap up this behavior in a minor-mode.
     (define-minor-mode mortal-mode
       "Allow temporary departures from god-mode."
       :lighter " mortal"
       :keymap '(([return] . (lambda ()
                               "Exit mortal-mode and resume god mode." (interactive)
                               (god-local-mode-resume)
                               (mortal-mode 0))))
       (when mortal-mode
         (god-local-mode-pause)))

     ;; (define-key god-local-mode-map (kbd "I") 'mortal-mode)

     ;; (add-to-ordered-list 'emulation-mode-map-alists 'god-local-mode-map 50)

     (defun my-god-mode-update-cursor ()
       (if god-local-mode
           (progn
             (setq my-modeline-background "white")
             (setq evil-emacs-state-tag (propertize "G" 'face '((:background "SkyBlue2" :foreground "black"))))
             (setq evil-emacs-state-cursor 'box))
           (setq my-modeline-background "black")
           (setq evil-emacs-state-tag (propertize "E" 'face '((:background "SkyBlue2" :foreground "black"))))
           (setq evil-emacs-state-cursor 'bar))
       (evil-refresh-mode-line 'emacs))

     (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
     (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)))

;; (which-key-enable-god-mode-support)
;; make god local mode map has the highest priority
;; 1. (add-to-ordered-list 'emulation-mode-map-alists 'god-local-mode-map 50)
;; 2. (make-composed-keymap (current-active-maps)) to suck up all active keybindings just before entering God mode.
;; 3. set-keymap-parent to set this keymap as the parent keymap of god-local-mode-map.

(provide 'init-god-mode)
