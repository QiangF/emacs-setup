; -*- coding:utf-8 -*-

;; p  C-p  (`undo-tree-visualize-undo')
;; n  C-n  (`undo-tree-visualize-redo')
;; b  C-b  (`undo-tree-visualize-switch-branch-left')
;; f  C-f  (`undo-tree-visualize-switch-branch-right')

(setup (:pkg undo-tree)
   (global-undo-tree-mode 1)
   (add-to-list 'undo-tree-incompatible-major-modes 'exwm-mode))

;; C-c C-c : add the chain of undo’s as a single edit to the undo history.
;; C-c C-k : cancel
;; C-c C-d : ediff the proposed chain of undo
(setup (:pkg undo-propose)
   (:autoload undo-propose)
   (:when-loaded
     (undo-propose-wrap redo)))

;; (setup (:pkg) volatile-highlights
;;     :ensure t
;;     :defer 5
;;     :init
;;     (volatile-highlights-mode t)
;;     :config
;;    (with-eval-after-load 'evil
;;      (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
;;                            'evil-paste-pop 'evil-move)
;;      (vhl/install-extension 'evil))
;;     (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;     (vhl/install-extension 'undo-tree)
;; )

;; define goggles faces in theme 
;; (setup (:pkg) goggles
;;  :ensure t
;;  :demand t
;;  :config
;;    (goggles-mode)) 

(provide 'init-undo)
