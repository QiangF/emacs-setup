; -*- coding:utf-8 -*-

;; multiple inheritance?
;; check if hydra is active: hydra-curr-map 

;; :foreign-keys nil (the default) means that the hydra state will stop and the
;; foreign key will do whatever it was supposed to do if there was no hydra
;; state.

;; :foreign-keys warn will not stop the hydra state, but instead will issue a
;; warning without running the foreign key.

;;  will not stop the hydra state, and try to run the foreign key.

;; | color    | toggle                     |
;; |----------+----------------------------|
;; | red      |                            |
;; | blue     | :exit t                    |
;; | amaranth | :foreign-keys warn         | don't run foreign key
;; | teal     | :foreign-keys warn :exit t |
;; | pink     |           |

(setup (:pkg hydra)
   (:global*
    "M-'" #'hydra-evil/body)

   (defhydra hydra-help (:color blue)
     ;; ("a" apropos "helm-apropos")
     ("b" describe-bindings "bindings")
     ;; ("c" helpful-command "command")
     ("e" view-echo-area-messages "view message")
     ("f" counsel-describe-function "function")
     ("F" Info-goto-emacs-command-node "goto command")
     ;; ("g" ag-and-a-half "ag-and-a-half")
     ;; ("g" helm-ag "helm ag")
     ;; ("i" helm-info-emacs "info")
     ;; ("k" helpful-key "key")
     ("k" helpful-key "key")
     ("m" describe-mode "mode")
     ("p" describe-package "package")
     ("t" describe-theme "theme")
     ("v" counsel-describe-variable "variable")
     ("@" describe-face "face")         ; describe the face name under cursor, change face with set-face-attributes
     ("?" help-for-help "help")
     ("<escape>" nil "quit")
     ("d" find-name-dired "find name dired")
     ("r" revert-buffer "revert buffer")
     ("s" swiper "swiper"))

   (defhydra hydra-narrow (:color blue)
     ("d" narrow-to-defun "defun")      ;     M-h mark paragraph
     ("p" narrow-to-page "page")
     ("r" narrow-to-region)
     ("w" widen)
     ("s" org-narrow-to-subtree "org-subtree")
     ("b" org-narrow-to-block "org-block")
     ("<escape>" nil "quit"))

   (defhydra hydra-error ()
     "goto-error"
     ("<escape>" nil "quit") ;; quit with esc
     ("h" first-error "first")
     ("j" next-error "next")
     ("k" previous-error "prev")
     ("v" recenter-top-bottom "recenter")
     ("q" nil "quit"))

   (defhydra hydra-evil (:color blue)
     ("<escape>" nil "quit") ;; quit with esc
     ;; ("S-SPC" hydra-pause-resume "pause")
     ("cd" copy_work_path "copy work path" :color blue)
     ("x" er/expand-region "expand region" :color blue) ;; contract is z
     ("o" hydra-outline/body "outline" :color blue)
     ("h" hydra-highlight/body "highlight" :color blue) ;; make sure file is tracked
     ("n" hydra-narrow/body "narrowing")
     ("m" hydra-magit/body "magit")
     ("i" imenu "imenu")
     ("b" hydra-bookmark/body "bookmarks" :color blue)
     ("j" headlong-bookmark-jump "bmk")
     ;; hydra-avy M-g
     ;; ("a" hydra-avy/body "avy-jump" :color blue)
     ("k" my-kill-buffer "kill buffer" :color blue)
     ;; ("d" my-delete-window "delete window")
     ("s" my-other-window "other window" :color pink)
     ("cc" org-capture "org capture" :color blue)
     ("w" aya-create "create auto yas" :color blue)
     ("y" aya-expand "expand auto yas" :color blue))

   (setq hydra-hint-display-type 'lv)
   ;; (setq hydra-lv nil)
   ;; (defvar my-hydra-timer (timer-create))
   ;; (defvar my-hydra-hint nil)
   ;; (defun hydra-show-hint (hint caller)
   ;; (let ((verbosity (plist-get (cdr (assoc caller hydra-props-alist))
   ;;                             :verbosity)))
   ;;     (cond ((eq verbosity 0))
   ;;         ((eq verbosity 1)
   ;;         (message (eval hint)))
   ;;         (t
   ;;         (when hydra-is-helpful
   ;;             (if hydra-lv
   ;;                 (lv-message (eval hint))
   ;;               (setq my-hydra-hint (eval hint))
   ;;               (message my-hydra-hint)
   ;;               (setq my-hydra-timer
   ;;                     (run-with-idle-timer 0.5 t (lambda ()(message my-hydra-hint))))))))))
   ;; (defun hydra-keyboard-quit ()
   ;; "Quitting function similar to `keyboard-quit'."
   ;; (interactive)
   ;; (hydra-disable)
   ;; (cancel-timer hydra-timeout-timer)
   ;; (cancel-timer hydra-message-timer)
   ;; (setq hydra-curr-map nil)
   ;; (unless (and hydra--ignore
   ;;             (null hydra--work-around-dedicated))
   ;;     (if hydra-lv
   ;;         (lv-delete-window)
   ;;     (message "")(cancel-timer my-hydra-timer)))
   ;; nil)

   (when exwm_enable
     (defun exwm-passthrough (orig-fun keymap on-exit &optional foreign-keys)
       (setq exwm-input-line-mode-passthrough t)
       (let ((on-exit (lexical-let ((on-exit on-exit))
                        (lambda ()
                          (setq exwm-input-line-mode-passthrough nil)
                          (when on-exit (funcall on-exit))))))
         (apply orig-fun keymap on-exit (list foreign-keys))))

     (advice-add 'hydra-set-transient-map :around #'exwm-passthrough)))

(provide 'init-hydra)
