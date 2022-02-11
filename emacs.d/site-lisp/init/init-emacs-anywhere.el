; -*- coding:utf-8 -*-

(when (display-graphic-p)
  (setq ea-copy t)

  (defun my-delete-frame ()
    (when (server-running-p)
      (delete-frame)))
  ;; (bind-key* "s-SPC" #'my-delete-frame)

  (defun popup-handler (app-name window-title x y w h)
    (set-frame-position (selected-frame) x (+ y (- h 200)))
    (unless (zerop w)
      (set-frame-size (selected-frame) w 200 t))
    (when evil-mode (evil-insert-state 1)))

  (add-hook 'ea-popup-hook 'popup-handler))

(provide 'init-emacs-anywhere)
