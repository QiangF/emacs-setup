;;; +ace-window.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ace-window)

;;;###autoload
(define-minor-mode +ace-window-display-mode
  "Minor mode for updating data for `+modeline-ace-window-display'."
  ;; This is stolen from ace-window.el but with the mode-line stuff ripped out.
  :global t
  (if +ace-window-display-mode
      (progn
        (aw-update)
        (force-mode-line-update t)
        (add-hook 'window-configuration-change-hook 'aw-update)
        (add-hook 'after-make-frame-functions 'aw--after-make-frame t)
        (advice-add 'aw--lead-overlay :override 'ignore))
    (remove-hook 'window-configuration-change-hook 'aw-update)
    (remove-hook 'after-make-frame-functions 'aw--after-make-frame)
    (advice-remove 'aw--lead-overlay 'ignore)))

(defun +ace-window@disable-overlay (_fn &rest _args)
  "ADVICE for FN `aw--lead-overlay' (and ARGS) to not show overlays.")

(provide '+ace-window)
;;; +ace-window.el ends here
