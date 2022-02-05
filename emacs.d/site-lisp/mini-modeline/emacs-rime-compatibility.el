;; take control of mini-buffer size
(add-hook 'minibuffer-setup-hook 'mini-modeline--minibuffer-setup t)
(add-hook 'minibuffer-exit-hook 'mini-modeline--minibuffer-exit)

(defun mini-modeline--minibuffer-setup ()
  (setq resize-mini-windows t))

(defun mini-modeline--minibuffer-exit ()
  (setq resize-mini-windows nil))
