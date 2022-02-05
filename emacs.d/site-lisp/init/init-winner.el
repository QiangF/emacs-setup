; -*- coding:utf-8 -*-

; * winner eyebrowse

(setup (:pkg winner)
   ;; winner-boring-buffers, buffers to exclude in winner-undo
   (:global* "C-9" #'winner-redo
             "C-8" #'winner-undo)
   (:delay)
   (:when-loaded
    (setq winner-ring-size 200)
    (setq winner-boring-buffers
          '("*Apropos*" "*Buffer List*" "*Completions*" "*Compile-Log*" "*cvs*" "*Fuzzy Completions*"
            "*Help*" "*Ibuffer*" "*inferior-lisp*"
            "*helm mini*" "*helm projectile*" "*helm M-x*" "*helm resume*"))

    ;; (defun my-maybe-switch-frame (&rest args)
    ;;   (when (eq exwm-workspace--minibuffer (selected-frame)) (other-frame)))
    ;; (advice-add 'winner-undo :before #'my-maybe-switch-frame)

                                        ; * cycle buffer by name prefix
    ;; based on https://github.com/akermu/cbm.el
    ;; URL: https://github.com/jrosdahl/iflipb
    ;; URL: https://github.com/killdash9/buffer-flip.el
    ;; (global-set-key (kbd "C-;") #'cbn-cycle)
    ;; (global-set-key (kbd "C-'") #'cbn-switch-buffer)
    ;; This package provides useful commands for switching to similar
    ;; buffers. It's particularly handy for switching between buffers with the same name prefix.

    (setq my-buffer-remember-delay-time "2 sec"
          switch-to-buffer-in-dedicated-window t)

    (defun my-reset-winner-last-command (BUFFER-OR-NAME)
      (let ((buffer-switched-into (get-buffer BUFFER-OR-NAME)))
        ;; check if the target buffer is still current
        (when (eq buffer-switched-into (current-buffer))
          (with-current-buffer buffer-switched-into
            ;; reset winner-last-command, see winner-save-old-configurations
            ;; in winner repeat the same command doesn't insert new config
            ;; winner also save win configs before winner undo
            (setq winner-last-command nil)))))

    (defun my-winner-save-at-pause (WINDOW BUFFER-OR-NAME &optional KEEP-MARGINS)
      (run-at-time my-buffer-remember-delay-time nil #'my-reset-winner-last-command BUFFER-OR-NAME))

    (advice-add 'set-window-buffer :after #'my-winner-save-at-pause)

    (defun cbn-buffer-valid (buffer)
      (and (buffer-live-p buffer)
           (not (minibufferp buffer))
           (not (string-match-p "Vmware::" (buffer-name buffer)))
           (not (string-match-p "freerdp::" (buffer-name buffer)))
           ;; " *which-key*" can't be switch away with switch-buffer
           (not (string-prefix-p " " (buffer-name buffer)))))

    (defun cbn-buffer-p (buffer the-name-prefix the-major-mode)
      (when (cbn-buffer-valid buffer)
        (with-current-buffer buffer
          (or (string-prefix-p the-name-prefix (buffer-name))
              (eq major-mode the-major-mode)))))

    (defun cbn-cycle (&optional reverse)
      (interactive)
      ;; (derived-mode-p 'exwm-mode) exwm-mode buffers has the name pattern defined in exwmx-appconfig.el
      (let* ((name-splits (split-string (buffer-name) "::"))
             (the-name-prefix (concat (nth 0 name-splits) "::"))
             (the-major-mode major-mode))
        (unless reverse
          (bury-buffer))
        (let ((buffers (buffer-list)))
          (when reverse
            (setq buffers (nreverse buffers)))
          (catch 'loop
            (dolist (buf buffers)
              (when (cbn-buffer-p buf the-name-prefix the-major-mode)
                (switch-to-buffer buf)
                (throw 'loop nil)))))))

    (winner-mode)))

;; eyebrowse evil binding
;; C-c C-w <   Switch to previous window config
;; C-c C-w >   Switch to next window config
;; C-c C-w '   Switch to last window config
;; C-c C-w "   Close current window config
;; C-c C-w ,   Rename current window config
;; C-c C-w 0   Switch to window config 0

;; eyebrowse-keymap-prefix (defaults to C-c C-w)
;; insert the customization before enabling eyebrowse-mode.
(setup (:pkg eyebrowse)
   (:delay)
   (:option eyebrowse-new-workspace t
            eyebrowse-wrap-around t)
   (:global* "M-8" #'eyebrowse-last-window-config
             "M-9" #'eyebrowse-next-window-config
             "M-0" #'eyebrowse-create-window-config)
   (:when-loaded
     (eyebrowse-mode t)))

; * ace window switch window

(setup (:pkg ace-window)
   (:delay)
   (:global* "C-x o" #'my-cycle-buffer
             "s-o" #'mode-line-other-buffer)
   (:when-loaded
     (set-face-attribute 'aw-mode-line-face nil
                         :inherit 'mode-line-buffer-id
                         :foreground "green"
                         :weight 'bold
                         :height 2.0)
     (when exwm_enable
       (setq aw-make-frame-char nil))

     (with-eval-after-load 'mini-modeline
       (setq aw-display-mode-overlay nil)

       (defun my-set-ace-window (&rest args)
         (mini-modeline-mode -1)
         (set-default 'mode-line-format
                      `((:eval (window-parameter (selected-window) 'ace-window-path))
                        " " "%e" mode-line-process
                        (:eval (propertize (mini-modeline-buffer-name)
                                           'face 'toki-modeline-path-face))))
         (force-mode-line-update t)
         (when exwm_enable
           (exwm-layout--refresh)))

       (defun my-unset-ace-window (&rest args)
         (mini-modeline-mode 1)
         (setq-default mode-line-format (when (and mini-modeline-display-gui-line
                                                   (display-graphic-p))
                                          '(" ")))
         (when exwm_enable
           (exwm-layout--refresh)))

       (advice-add 'ace-window :before 'my-set-ace-window)
       (advice-add 'aw--done :after 'my-unset-ace-window))

     ;; use header-line
     ;; (defun my-set-ace-window (&rest args)
     ;;   (setq header-line-format
     ;;         '((ace-window-display-mode
     ;;            (:eval (window-parameter (selected-window) 'ace-window-path)))))
     ;;   (when exwm_enable
     ;;     (exwm-layout--refresh)))

     ;; (defun my-unset-ace-window (&rest args)
     ;;   (setq header-line-format nil)
     ;;   (when exwm_enable
     ;;     (exwm-layout--refresh)))

     (defun my-other-buffer ()
       (interactive)
       (switch-to-buffer (nth 2 (buffer-list))))

     (require 'cycle-buffer)

     ;; (defun my-cycle-buffer ()
     ;;     (interactive)
     ;;     (cond ((one-window-p) (progn (hydra-cycle-buffer/cycle-buffer-permissive)
     ;;                                  (hydra-cycle-buffer/body)))
     ;;           ((eq 2 (count-windows)) (other-window 1))
     ;;           (t (ace-window 1))))

     (defun my-cycle-buffer ()
       (interactive)
       (cond ((one-window-p) (progn (hydra-winner/winner-undo)
                                    (hydra-winner/winner-redo)))
             ((eq 2 (count-windows)) (other-window 1))
             (t (ace-window 1))))

     (defhydra hydra-winner (:color pink :verbosity 1 :idle 0.1)
       ("o" winner-undo "back")
       ("i" winner-redo "forward")
       ("<escape>" nil "quit"))

     (defhydra hydra-cycle-buffer (:color pink :verbosity 1 :idle 0.1)
       ("f" cycle-buffer "cycle-buffer")
       ("b" cycle-buffer-backward "cycle-buffer-back")
       ("o" cycle-buffer-permissive "cycle-all-buffer")
       ("i" cycle-buffer-backward-permissive "cycle-all-buffer-back")
       ("<escape>" nil "quit"))

     (setq aw-reverse-frame-list t)
     (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
           aw-dispatch-always nil
           aw-dispatch-alist            ; key prefix to window label
           '((?x aw-delete-window "Ace - Delete Window")
             (?c aw-swap-window "Ace - Swap Window")
             (?n aw-flip-window)
             (?v aw-split-window-vert "Ace - Split Vert Window")
             (?h aw-split-window-horz "Ace - Split Horz Window")
             (?m delete-other-windows "Ace - Maximize Window")
             (?g delete-other-windows)
             (?b balance-windows)
             (?u winner-undo)
             (?r winner-redo)))


     (defhydra hydra-window (:color blue :hint nil :idle 0.4 :timeout 5 :verbosity 1)
       "
[^w^] windows size [^r^] winner redo  [^u^] winner undo  [^g^] max. current
[^h^] split horz.  [^;^] swap window  [^x^] del. window  [^c^] swap window
[^v^] split vert.  [^b^] balance win. [^n^] last window  [^m^] max window")

     (defhydra hydra-window-size (:color red)
       "Windows size"
       ("h" shrink-window-horizontally "shrink horizontal")
       ("j" shrink-window "shrink vertical")
       ("k" enlarge-window "enlarge vertical")
       ("l" enlarge-window-horizontally "enlarge horizontal"))

     (defhydra hydra-window-scroll (:color red)
       "Scroll other window"
       ("n" joe-scroll-other-window "scroll")
       ("p" joe-scroll-other-window-down "scroll down"))
     (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
     (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t))

   (ace-window-display-mode t))

;; (setup (:pkg perspective)
;;    (("C-x b" #'persp-ivy-switch-buffer)
;;     ("C-x k" #'persp-kill-buffer*))
;;    (persp-mode)
;;    (setq display-buffer-alist
;;          '((".*" (display-buffer-reuse-window display-buffer-same-window))))

;;    (setq display-buffer-reuse-frames t) ; reuse windows in other frames
;;    (setq even-window-sizes nil)         ; display-buffer: avoid resizing
;;    (setq persp-sort 'access)
;;    (setq persp-state-default-file "~/.cache/emacs_perspectives")
;;    (add-hook 'kill-emacs-hook #'persp-state-save))

(radian-defadvice radian--advice-kill-buffer-maybe-kill-window
    (func &optional buffer-or-name kill-window-too)
  :around #'kill-buffer
  "Make it so \\[universal-argument] \\[kill-buffer] kills the window too."
  (interactive
   (lambda (spec)
     (append (or (advice-eval-interactive-spec spec) '(nil))
             current-prefix-arg)))
  (if kill-window-too
      (with-current-buffer buffer-or-name
        (kill-buffer-and-window))
      (funcall func buffer-or-name)))

(provide 'init-winner)
