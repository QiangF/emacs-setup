; -*- coding:utf-8 -*-

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold gcmh-low-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
;; C-M-v scroll other window down, C-M-V scroll the other window up
;; scroll up use negative arguments, type C-M-- C-M-v.
(setq max-mini-window-height 5)

(defun mf-workarea (&optional the-frame)
  "Return the workarea of THE-FRAME.
If left specified, then use the current frame."
  (let ((result ())
        (frame (or the-frame (window-frame (selected-window)))))
    (dolist (alist (display-monitor-attributes-list))
      (if (memq frame (cdr (assoc 'frames alist)))
          (push (assoc 'workarea alist) result)))
    (cdr (car (nreverse result)))))

(when (and exwm_enable (exwm-workspace--minibuffer-own-frame-p))
  (set-frame-width exwm-workspace--minibuffer
                   (nth 2 (mf-workarea exwm-workspace--minibuffer)) nil t))

;; (setq scroll-margin 1)
(defun my-minibuffer-setup ()
  (setq truncate-lines nil)
  (setq-local scroll-margin 0)
  (visual-line-mode 1)
  (setq resize-mini-windows t)
  (when (bound-and-true-p evil-mode)
    (turn-off-evil-mode)))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup t)

;; (defun my-minibuffer-exit ()
;;   (setq resize-mini-windows nil))
;; (add-hook 'minibuffer-exit-hook 'my-minibuffer-exit)

(defun my-abort-recursive-edit (&rest args)
    (when (active-minibuffer-window) (abort-recursive-edit)))

(defun my-exit-minibuffer (&rest args)
      (when (active-minibuffer-window)
        (exit-minibuffer)
       ;; (when (exwm-workspace--minibuffer-own-frame-p)
       ;;    (other-frame -1))
        ))

(advice-add 'switch-to-buffer :before #'my-exit-minibuffer)
(advice-add 'read-from-minibuffer :before #'my-abort-recursive-edit)
(advice-add 'read-string :before #'my-abort-recursive-edit)
(advice-add 'winner-undo :before #'my-abort-recursive-edit)

(add-hook 'mouse-leave-buffer-hook 'my-abort-recursive-edit)

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; (defadvice abort-recursive-edit (before switch-to-minibuffer activate)
;;       (switch-to-minibuffer-window))

(defun my-abort-minibuffer (orig-func &rest args)
  (switch-to-minibuffer-window)
  (apply orig-func args))

(advice-add 'abort-recursive-edit :around 'my-abort-minibuffer)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


(radian-defadvice radian--advice-keyboard-quit-minibuffer-first
    (keyboard-quit)
  :around #'keyboard-quit
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

(global-set-key [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; default TAB to page down *completion* buffer
;; (use-package icicles
;;     :ensure t
;;     :config
;;     (define-key minibuffer-local-map [tab] 'icicle-complete)
;; )

; C-x 3 split horizontally)
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
    (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(setup simple
   (:with-hook text-mode-hook
     (:hook visual-line-mode)))

(provide 'init-minibuffer)
